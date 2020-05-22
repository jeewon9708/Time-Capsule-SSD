// Copyright 2011 INDILINX Co., Ltd.
//
// This file is part of Jasmine.
//
// Jasmine is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Jasmine is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Jasmine. See the file COPYING.
// If not, see <http://www.gnu.org/licenses/>.
//
// GreedyFTL source file
//
// Author; Sang-Phil Lim (SKKU VLDB Lab.)
//
// - support POR
//  + fixed metadata area (Misc. block/Map block)
//  + logging entire FTL metadata when each ATA commands(idle/ready/standby) was issued
//

#include "jasmine.h"
//----------------------------------
// macro
//----------------------------------
#define VC_MAX              0xCDCD
#define VC_FREE             0xCECE
#define MISCBLK_VBN         0x1 // vblock #1 <- misc metadata
#define MAPBLKS_PER_BANK    (((PAGE_MAP_BYTES / NUM_BANKS) + BYTES_PER_PAGE - 1) / BYTES_PER_PAGE)
#define META_BLKS_PER_BANK  (1 + 1 + MAPBLKS_PER_BANK) // include block #0, misc block

// the number of sectors of misc. metadata info.
#define NUM_MISC_META_SECT  ((sizeof(misc_metadata) + BYTES_PER_SECTOR - 1)/ BYTES_PER_SECTOR)
#define NUM_VCOUNT_SECT     ((VBLKS_PER_BANK * sizeof(UINT16) + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR)

//지원:일단 이렇게 정해둠. 이거 나중에 바꿔야함
#define MAX_BLK 10 //bucket별 최대 블록개수
#define NUM_BUCKET 10 //총 bucket 개수
////////////////////////////////////////////////////////////////////
static void tc_write_seq(const UINT32 start_lsn, const UINT32 io_num, const UINT32 sector_size);
static void tc_write_rand(const UINT32 start_lsn, const UINT32 io_num, const UINT32 sector_size);
static void fillup_dataspace(void);
static void aging_with_rw(UINT32 io_cnt);
void ftl_test(void);
#define IO_LIMIT           (NUM_LSECTORS)
#define RANDOM_SEED        (IO_LIMIT)
#define NUM_PSECTORS_4KB   ((4 * 1024) / 512)
#define NUM_PSECTORS_8KB   (NUM_PSECTORS_4KB << 1)
#define NUM_PSECTORS_16KB  (NUM_PSECTORS_8KB << 1)
#define NUM_PSECTORS_32KB  (NUM_PSECTORS_16KB << 1)
#define NUM_PSECTORS_64KB  (NUM_PSECTORS_32KB << 1)
#define NUM_PSECTORS_128KB ((128 * 1024) / 512)
#define NUM_PSECTORS_256KB ((256 * 1024) / 512)

//////////////////////////////////////////////////////////////////////

//----------------------------------
// metadata structure
//----------------------------------

typedef struct _ftl_statistics
{
    UINT32 gc_cnt;
    UINT32 page_wcount; // page write count
}ftl_statistics;

//지원:time bucket in backup zone
/*
typedef struct _time_bucket{
    UINT32 expiration_time; //이 bucket의 블록들이 지워지는 순간
    UINT32 block_num; // 이 bucket에 닮긴 blocknum
    UINT32 blocks_in_bucket[MAX_BLK];//블록맨첫주소 in 배열
    UINT32 cur_write_vpn;//지금 이 블록의 몇번째 vpn 사용중
}time_bucket; 
//바꿔야하는 부분? 혹시 bank별로따로 분리시켜주어야하는지
*/

//지원: 여기서///////////////////////////@@@@@@@@@@@@@/////////////////

typedef struct _misc_metadata
{
    UINT32 cur_write_vpn; // physical page for new write
    UINT32 cur_miscblk_vpn; // current write vpn for logging the misc. metadata
    UINT32 cur_mapblk_vpn[MAPBLKS_PER_BANK]; // current write vpn for logging the age mapping info.
    UINT32 gc_vblock; // vblock number for garbage collection
    UINT32 free_blk_cnt; // total number of free block count
    UINT32 lpn_list_of_cur_vblock[PAGES_PER_BLK]; // logging lpn list of current write vblock for GC
    UINT32 old_vpn[PAGES_PER_BLK];
    UINT32 written_time[PAGES_PER_BLK];///////////////////@@@@@@@@@@@@@?////////
    UINT32 reservation_time[PAGES_PER_BLK];
}misc_metadata; // per bank

//----------------------------------
// FTL metadata (maintain in SRAM)
//----------------------------------
static misc_metadata  g_misc_meta[NUM_BANKS];
static ftl_statistics g_ftl_statistics[NUM_BANKS];
static UINT32		  g_bad_blk_count[NUM_BANKS];

//지원: backup zone에서 현 bank에 active 되어있는 블록
#define MAX_BUBLK     80
#define TIME_INT      600
static UINT32         backup_zone_active_blk[NUM_BANKS][MAX_BUBLK];
static UINT32         backup_zone_cur_write_pagenum[NUM_BANKS][MAX_BUBLK];
static UINT32         backup_zone_cur_blk=0;
//static UINT32         backup_zone_index=0;
//지원: time_bucket 개수를 정해야하는데//
//static time_bucket time_bucket_queue[NUM_BANKS][NUM_BUCKET];
//static UINT32 start_bucket=0; 

// SATA read/write buffer pointer id
UINT32 				  g_ftl_read_buf_id;
UINT32 				  g_ftl_write_buf_id;
UINT32                page_write_num=0;
UINT32                page_copy_num=0;
UINT32                erase_backup_num=0;

//static UINT32 uart_tp=0; //flush key temp
//static UINT32 uart_tp_pn=0;

//static UINT32 num_flush =0 ;
//static UINT32 num_logging = 0;
//----------------------------------
// NAND layout
//----------------------------------
// block #0: scan list, firmware binary image, etc.
// block #1: FTL misc. metadata
// block #2 ~ #31: page mapping table
// block #32: a free block for gc
// block #33~: user data blocks

//----------------------------------
// macro functions
//----------------------------------
#define is_full_all_blks(bank)  (g_misc_meta[bank].free_blk_cnt == 1)
#define inc_full_blk_cnt(bank)  (g_misc_meta[bank].free_blk_cnt--)
#define dec_full_blk_cnt(bank)  (g_misc_meta[bank].free_blk_cnt++)
#define inc_mapblk_vpn(bank, mapblk_lbn)    (g_misc_meta[bank].cur_mapblk_vpn[mapblk_lbn]++)
#define inc_miscblk_vpn(bank)               (g_misc_meta[bank].cur_miscblk_vpn++)

// page-level striping technique (I/O parallelism)
#define get_num_bank(lpn)             ((lpn) % NUM_BANKS)
#define get_bad_blk_cnt(bank)         (g_bad_blk_count[bank])
#define get_cur_write_vpn(bank)       (g_misc_meta[bank].cur_write_vpn)
#define set_new_write_vpn(bank, vpn)  (g_misc_meta[bank].cur_write_vpn = vpn)
#define get_gc_vblock(bank)           (g_misc_meta[bank].gc_vblock)
#define set_gc_vblock(bank, vblock)   (g_misc_meta[bank].gc_vblock = vblock)
#define set_lpn(bank, page_num, lpn)  (g_misc_meta[bank].lpn_list_of_cur_vblock[page_num] = lpn)
#define get_lpn(bank, page_num)       (g_misc_meta[bank].lpn_list_of_cur_vblock[page_num])
#define get_miscblk_vpn(bank)         (g_misc_meta[bank].cur_miscblk_vpn)
#define set_miscblk_vpn(bank, vpn)    (g_misc_meta[bank].cur_miscblk_vpn = vpn)
#define get_mapblk_vpn(bank, mapblk_lbn)      (g_misc_meta[bank].cur_mapblk_vpn[mapblk_lbn])
#define set_mapblk_vpn(bank, mapblk_lbn, vpn) (g_misc_meta[bank].cur_mapblk_vpn[mapblk_lbn] = vpn)
#define CHECK_LPAGE(lpn)              ASSERT((lpn) < NUM_LPAGES)
#define CHECK_VPAGE(vpn)              ASSERT((vpn) < (VBLKS_PER_BANK * PAGES_PER_BLK))
/////@@새로만든macro////
#define set_backup_written_time(bank,page_num)   (g_misc_meta[bank].written_time[page_num]=ptimer_record())
#define set_backup_zone_block(bank,vblock)   set_bit_dram(BACKUP_ZONE_BMP_ADDR + bank*(VBLKS_PER_BANK/8+1), vblock % VBLKS_PER_BANK);


///////////////////////
//----------------------------------
// FTL internal function prototype
//----------------------------------
static void   format(void);
static void   write_format_mark(void);
static void   sanity_check(void);
static void   load_pmap_table(void);
static void   load_misc_metadata(void);
static void   init_metadata_sram(void);
static void   load_metadata(void);
static void   logging_pmap_table(void);
static void   logging_misc_metadata(void);
static void   write_page(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors,UINT32 retention_duration);
static void   set_vpn(UINT32 const lpn, UINT32 const vpn);
static void   garbage_collection(UINT32 const bank);
static void   set_vcount(UINT32 const bank, UINT32 const vblock, UINT32 const vcount);
static BOOL32 is_bad_block(UINT32 const bank, UINT32 const vblock);
static BOOL32 check_format_mark(void);
static UINT32 get_vcount(UINT32 const bank, UINT32 const vblock);
static UINT32 get_vpn(UINT32 const lpn);
static UINT32 get_vt_vblock(UINT32 const bank);
static UINT32 assign_new_write_vpn(UINT32 const bank);
//////////////////////////지원://////////////
static void set_backup_data(UINT32 const bank, UINT32 const page_num, UINT32 const lpn, UINT32 const old_vpn);
static void move_to_backup_zone(UINT32 const bank, UINT32 const vpn, UINT32 const lpn);
static void erase_backupblk(UINT32 const bank,UINT32 const expired_bucket);
//static void check_backupzone(UINT32 const bank, UINT32 const vblock);
////////////////////////////////////////////////////////////
//key
//static UINT32 flush_num=0;
//static UINT32 write_num=0;
UINT32 gcnum=0;

static void sanity_check(void)
{
    UINT32 dram_requirement = RD_BUF_BYTES + WR_BUF_BYTES + COPY_BUF_BYTES + FTL_BUF_BYTES
        + HIL_BUF_BYTES + TEMP_BUF_BYTES + BAD_BLK_BMP_BYTES + PAGE_MAP_BYTES + VCOUNT_BYTES
        +FLUSH_MAP_BYTES+RECLAIMABILITY_BMP_BYTES+BACKUP_ZONE_BMP_BYTES+BACKUP_ZONE_LPN_BYTES;

    uart_printf("dram requirment: %d, dram size: %d \n",dram_requirement,DRAM_SIZE);
    if ((dram_requirement > DRAM_SIZE) || // DRAM metadata size check
        (sizeof(misc_metadata) > BYTES_PER_PAGE)) // misc metadata size check
    {
        uart_printf("dram requirment: %d\n",dram_requirement);
        uart_print("dram size is not enough!\n");
        led_blink();
        while (1);
    }
}
static void build_bad_blk_list(void)
{
	UINT32 bank, num_entries, result, vblk_offset;
	scan_list_t* scan_list = (scan_list_t*) TEMP_BUF_ADDR;

	mem_set_dram(BAD_BLK_BMP_ADDR, NULL, BAD_BLK_BMP_BYTES);

	disable_irq();

	flash_clear_irq();

	for (bank = 0; bank < NUM_BANKS; bank++)
	{
		SETREG(FCP_CMD, FC_COL_ROW_READ_OUT);
		SETREG(FCP_BANK, REAL_BANK(bank));
		SETREG(FCP_OPTION, FO_E);
		SETREG(FCP_DMA_ADDR, (UINT32) scan_list);
		SETREG(FCP_DMA_CNT, SCAN_LIST_SIZE);
		SETREG(FCP_COL, 0);
		SETREG(FCP_ROW_L(bank), SCAN_LIST_PAGE_OFFSET);
		SETREG(FCP_ROW_H(bank), SCAN_LIST_PAGE_OFFSET);

		SETREG(FCP_ISSUE, NULL);
		while ((GETREG(WR_STAT) & 0x00000001) != 0);
		while (BSP_FSM(bank) != BANK_IDLE);

		num_entries = NULL;
		result = OK;

		if (BSP_INTR(bank) & FIRQ_DATA_CORRUPT)
		{
			result = FAIL;
		}
		else
		{
			UINT32 i;

			num_entries = read_dram_16(&(scan_list->num_entries));

			if (num_entries > SCAN_LIST_ITEMS)
			{
				result = FAIL;
			}
			else
			{
				for (i = 0; i < num_entries; i++)
				{
					UINT16 entry = read_dram_16(scan_list->list + i);
					UINT16 pblk_offset = entry & 0x7FFF;

					if (pblk_offset == 0 || pblk_offset >= PBLKS_PER_BANK)
					{
						#if OPTION_REDUCED_CAPACITY == FALSE
						result = FAIL;
						#endif
					}
					else
					{
						write_dram_16(scan_list->list + i, pblk_offset);
					}
				}
			}
		}

		if (result == FAIL)
		{
			num_entries = 0;  // We cannot trust this scan list. Perhaps a software bug.
		}
		else
		{
			write_dram_16(&(scan_list->num_entries), 0);
		}

		g_bad_blk_count[bank] = 0;

		for (vblk_offset = 1; vblk_offset < VBLKS_PER_BANK; vblk_offset++)
		{
			BOOL32 bad = FALSE;

			#if OPTION_2_PLANE
			{
				UINT32 pblk_offset;

				pblk_offset = vblk_offset * NUM_PLANES;

                // fix bug@jasmine v.1.1.0
				if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, pblk_offset) < num_entries + 1)
				{
					bad = TRUE;
				}

				pblk_offset = vblk_offset * NUM_PLANES + 1;

                // fix bug@jasmine v.1.1.0
				if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, pblk_offset) < num_entries + 1)
				{
					bad = TRUE;
				}
			}
			#else
			{
                // fix bug@jasmine v.1.1.0
				if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, vblk_offset) < num_entries + 1)
				{
					bad = TRUE;
				}
			}
			#endif

			if (bad)
			{
				g_bad_blk_count[bank]++;
				set_bit_dram(BAD_BLK_BMP_ADDR + bank*(VBLKS_PER_BANK/8 + 1), vblk_offset);
			}
		}
	}
}


void ftl_open(void)
{
    // debugging example 1 - use breakpoint statement!
    /* *(UINT32*)0xFFFFFFFE = 10; */

    /* UINT32 volatile g_break = 0; */
    /* while (g_break == 0); */

	led(0);
    sanity_check();
    //----------------------------------------
    // read scan lists from NAND flash
    // and build bitmap of bad blocks
    //----------------------------------------
	build_bad_blk_list();
    ////지원: time_bucket 초기화 일단 이곳에 추가함//////
    
    mem_set_sram(backup_zone_active_blk, 0x00000000, sizeof(UINT32)*MAX_BUBLK);
    mem_set_sram(backup_zone_cur_write_pagenum, 0x00000000, sizeof(UINT32) * MAX_BUBLK);
    ///////////////////////////////////////////////////
    //----------------------------------------
	// If necessary, do low-level format
	// format() should be called after loading scan lists, because format() calls is_bad_block().
    //----------------------------------------
/* 	if (check_format_mark() == FALSE) */
	if (TRUE)
	{
        uart_print("do format");
		format();
        uart_print("end format");
	}
    // load FTL metadata
    else
    {
        load_metadata();
    }
	g_ftl_read_buf_id = 0;
	g_ftl_write_buf_id = 0;

    // This example FTL can handle runtime bad block interrupts and read fail (uncorrectable bit errors) interrupts
    flash_clear_irq();

    SETREG(INTR_MASK, FIRQ_DATA_CORRUPT | FIRQ_BADBLK_L | FIRQ_BADBLK_H);
	SETREG(FCONF_PAUSE, FIRQ_DATA_CORRUPT | FIRQ_BADBLK_L | FIRQ_BADBLK_H);

	enable_irq();
}
void ftl_flush(void)
{
    /* ptimer_start(); */
    //key
   // flush_num++;
    //uart_printf("flush num : %d write num : %d, page_num : %d",flush_num,write_num,PAGE_MAP_BYTES/BYTES_PER_PAGE);
    logging_pmap_table();
    logging_misc_metadata();
    /* ptimer_stop_and_uart_print(); */
}
// Testing FTL protocol APIs
void ftl_test_write(UINT32 const lba, UINT32 const num_sectors)
{
    ASSERT(lba + num_sectors <= NUM_LSECTORS);
    ASSERT(num_sectors > 0);

    ftl_write(lba, num_sectors,12*80000);
}
void ftl_read(UINT32 const lba, UINT32 const num_sectors)
{
    UINT32 remain_sects, num_sectors_to_read;
    UINT32 lpn, sect_offset;
    UINT32 bank, vpn;

    lpn          = lba / SECTORS_PER_PAGE;
    sect_offset  = lba % SECTORS_PER_PAGE;
    remain_sects = num_sectors;

    while (remain_sects != 0)
    {
        if ((sect_offset + remain_sects) < SECTORS_PER_PAGE)
        {
            num_sectors_to_read = remain_sects;
        }
        else
        {
            num_sectors_to_read = SECTORS_PER_PAGE - sect_offset;
        }
        bank = get_num_bank(lpn); // page striping
        vpn  = get_vpn(lpn);
        CHECK_VPAGE(vpn);

        if (vpn != NULL)
        {
            nand_page_ptread_to_host(bank,
                                     vpn / PAGES_PER_BLK,
                                     vpn % PAGES_PER_BLK,
                                     sect_offset,
                                     num_sectors_to_read);
        }
        // The host is requesting to read a logical page that has never been written to.
        else
        {
			UINT32 next_read_buf_id = (g_ftl_read_buf_id + 1) % NUM_RD_BUFFERS;

			#if OPTION_FTL_TEST == 0
			while (next_read_buf_id == GETREG(SATA_RBUF_PTR));	// wait if the read buffer is full (slow host)
			#endif

            // fix bug @ v.1.0.6
            // Send 0xFF...FF to host when the host request to read the sector that has never been written.
            // In old version, for example, if the host request to read unwritten sector 0 after programming in sector 1, Jasmine would send 0x00...00 to host.
            // However, if the host already wrote to sector 1, Jasmine would send 0xFF...FF to host when host request to read sector 0. (ftl_read() in ftl_xxx/ftl.c)
			mem_set_dram(RD_BUF_PTR(g_ftl_read_buf_id) + sect_offset*BYTES_PER_SECTOR,
                         0xFFFFFFFF, num_sectors_to_read*BYTES_PER_SECTOR);

            flash_finish();

			SETREG(BM_STACK_RDSET, next_read_buf_id);	// change bm_read_limit
			SETREG(BM_STACK_RESET, 0x02);				// change bm_read_limit

			g_ftl_read_buf_id = next_read_buf_id;
        }
        sect_offset   = 0;
        remain_sects -= num_sectors_to_read;
        lpn++;
    }
}
void ftl_write(UINT32 const lba, UINT32 const num_sectors,UINT32 const retention_duration)
{
    UINT32 remain_sects, num_sectors_to_write;
    UINT32 lpn, sect_offset;
    //uart_print("ftl_write\n");
    

    lpn          = lba / SECTORS_PER_PAGE;
    sect_offset  = lba % SECTORS_PER_PAGE;
    remain_sects = num_sectors;
    //uart_print("ftl write operation!\n");
    while (remain_sects != 0)
    {
        if ((sect_offset + remain_sects) < SECTORS_PER_PAGE)
        {
            num_sectors_to_write = remain_sects;
        }
        else
        {
            num_sectors_to_write = SECTORS_PER_PAGE - sect_offset;
        }
        // single page write individually
        write_page(lpn, sect_offset, num_sectors_to_write,retention_duration);

        sect_offset   = 0;
        remain_sects -= num_sectors_to_write;
        lpn++;
    }
}
static void write_page(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors,UINT32 retention_duration)
{
    CHECK_LPAGE(lpn);
    ASSERT(sect_offset < SECTORS_PER_PAGE);
    ASSERT(num_sectors > 0 && num_sectors <= SECTORS_PER_PAGE);

    UINT32 bank, old_vpn, new_vpn;
    UINT32 vblock, page_num, page_offset, column_cnt;

    //key
    //write_num++;

    bank        = get_num_bank(lpn); // page striping
    page_offset = sect_offset;
    column_cnt  = num_sectors;

    
    new_vpn  = assign_new_write_vpn(bank);
    old_vpn  = get_vpn(lpn);

    CHECK_VPAGE (old_vpn);
    CHECK_VPAGE (new_vpn);
    ASSERT(old_vpn != new_vpn);

    g_ftl_statistics[bank].page_wcount++;
    //uart_printf("write page! bank=%d new vpn=%d, oldvpn=%d, lpn=%d gcnum=%d time=%d\n",bank,new_vpn,old_vpn,lpn,gcnum,ptimer_record());

    // if old data already exist,
    if (old_vpn != NULL)
    {//////////////////@@overwrite@@@@/////////////-->backup data 저장 필요함
        vblock   = old_vpn / PAGES_PER_BLK;
        page_num = old_vpn % PAGES_PER_BLK;
        set_backup_data(bank,new_vpn, lpn,old_vpn);
        //--------------------------------------------------------------------------------------
        // `Partial programming'
        // we could not determine whether the new data is loaded in the SATA write buffer.
        // Thus, read the left/right hole sectors of a valid page and copy into the write buffer.
        // And then, program whole valid data
        //--------------------------------------------------------------------------------------
        if (num_sectors != SECTORS_PER_PAGE)
        {
            // Performance optimization (but, not proved)
            // To reduce flash memory access, valid hole copy into SATA write buffer after reading whole page
            // Thus, in this case, we need just one full page read + one or two mem_copy
            if ((num_sectors <= 8) && (page_offset != 0))
            {
                // one page async read
                nand_page_read(bank,
                               vblock,
                               page_num,
                               FTL_BUF(bank));
                // copy `left hole sectors' into SATA write buffer
                if (page_offset != 0)
                {
                    mem_copy(WR_BUF_PTR(g_ftl_write_buf_id),
                             FTL_BUF(bank),
                             page_offset * BYTES_PER_SECTOR);
                }
                // copy `right hole sectors' into SATA write buffer
                if ((page_offset + column_cnt) < SECTORS_PER_PAGE)
                {
                    UINT32 const rhole_base = (page_offset + column_cnt) * BYTES_PER_SECTOR;

                    mem_copy(WR_BUF_PTR(g_ftl_write_buf_id) + rhole_base,
                             FTL_BUF(bank) + rhole_base,
                             BYTES_PER_PAGE - rhole_base);
                }
            }
            // left/right hole async read operation (two partial page read)
            else
            {
                // read `left hole sectors'
                if (page_offset != 0)
                {
                    nand_page_ptread(bank,
                                     vblock,
                                     page_num,
                                     0,
                                     page_offset,
                                     WR_BUF_PTR(g_ftl_write_buf_id),
                                     RETURN_ON_ISSUE);
                }
                // read `right hole sectors'
                if ((page_offset + column_cnt) < SECTORS_PER_PAGE)
                {
                    nand_page_ptread(bank,
                                     vblock,
                                     page_num,
                                     page_offset + column_cnt,
                                     SECTORS_PER_PAGE - (page_offset + column_cnt),
                                     WR_BUF_PTR(g_ftl_write_buf_id),
                                     RETURN_ON_ISSUE);
                }
            }
        }
        // full page write
        page_offset = 0;
        column_cnt  = SECTORS_PER_PAGE;
        // invalid old page (decrease vcount)
        if(get_vcount(bank,vblock)==0xCECE){
            set_vcount(bank, vblock, 1);
        }
        else{
            set_vcount(bank, vblock, get_vcount(bank, vblock) - 1);
        }
    }
    vblock   = new_vpn / PAGES_PER_BLK;
    page_num = new_vpn % PAGES_PER_BLK;
   // ASSERT(get_vcount(bank,vblock) < (PAGES_PER_BLK - 1));

    // write new data (make sure that the new data is ready in the write buffer frame)
    // (c.f FO_B_SATA_W flag in flash.h)
    page_write_num++;
    nand_page_ptprogram_from_host(bank,
                                  vblock,
                                  page_num,
                                  page_offset,
                                  column_cnt);

    // update metadata
    set_lpn(bank, page_num, lpn);
    set_backup_written_time(bank, new_vpn % PAGES_PER_BLK);
    g_misc_meta[bank].reservation_time[page_num]=retention_duration;
   // uart_printf("lpn=%d old_vpn=%d written time=%d reservation time=%d\n",lpn,old_vpn,g_misc_meta[bank].written_time[page_num],retention_duration);
    set_vpn(lpn, new_vpn);

    if(get_vcount(bank,vblock)==0xCECE){
        set_vcount(bank, vblock, 1);
    }
    else{
        set_vcount(bank, vblock, get_vcount(bank, vblock) + 1);
    }
}


/////////////////@@@@@@@@@@@@@@@@@@@@@@@@?////////////////////////////////////////////////////////////////////////
//set backup data in SRAM
static void set_backup_data(UINT32 const bank, UINT32 const new_vpn, UINT32 const lpn, UINT32 const old_vpn){

//    uart_print("set backup data\n");

    //overwrite함과 동시에 바로바로 backup이 backup zone으로 가는경우

    //모았다가 backup 이 gc때 backupzone으로 가는경우


    g_misc_meta[bank].old_vpn[new_vpn % PAGES_PER_BLK]=old_vpn;
    
   // uart_printf("\nset backup data\nlpn:%d old_vpn:%d new_vpn:%d written_time:%d\n",lpn,old_vpn,page_num,g_misc_meta[bank].list_of_backup_data[page_num].written_time);
 //   uart_print("!!\n");
 //   ptimer_stop_and_uart_print();
}


//어떤 page를 backup zone으로 옮기는 과정
///////////////////////여기 다시 봐야함!!!!!!!!!!!!!!!!!!///////////////////////////////////////////////////
static void move_to_backup_zone(UINT32 const bank, UINT32 const vpn, UINT32 const lpn){
    

    UINT32 blocknum;
    UINT32 write_vpn;
    UINT32 written_time, reservation_time;
    UINT32 index;
    UINT32 page_num = vpn % PAGES_PER_BLK;
    UINT32 vblock = vpn / PAGES_PER_BLK;
    UINT32 old_vpn;
    UINT32 new_lpn;
    //UINT32 expr_time=TIME_INT;
    UINT32 invalidated_time;
    UINT32 i=0,j=0;
    UINT32 chain_num=0;
    

    //UINT32 lpn_temp[128];
    UINT32 reservation_time_temp[128];
    UINT32 written_time_temp[128];
    UINT32 old_vpn_temp[128];
    //한번에 다읽기
    nand_page_ptread(bank,vblock,PAGES_PER_BLK-1,0,(sizeof(UINT32)*PAGES_PER_BLK*4),FTL_BUF(bank),RETURN_WHEN_DONE);
   // mem_copy(lpn_temp, FTL_BUF(bank), sizeof(UINT32)*128);
    mem_copy(old_vpn_temp, FTL_BUF(bank)+sizeof(UINT32)*PAGES_PER_BLK, sizeof(UINT32)*128);
    mem_copy(written_time_temp, FTL_BUF(bank)+(sizeof(UINT32)*PAGES_PER_BLK*2), sizeof(UINT32)*128);
    mem_copy(reservation_time_temp, FTL_BUF(bank)+(sizeof(UINT32)*PAGES_PER_BLK*3), sizeof(UINT32)*128);

    reservation_time=reservation_time_temp[page_num];
    written_time=written_time_temp[page_num];
    old_vpn=old_vpn_temp[page_num];
    uart_print("move");
    //uart_printf("lpn=%d lpn_tmp:%d old_vpn=%d, written time=%d reservationtime=%d\n",lpn,lpn_temp[page_num],old_vpn,written_time,reservation_time);  

     
        
   // UINT32 old_vpn_array[100];
    //UINT32 written_time_array[100];
    
    //old_vpn_array[0] = vpn;
    //written_time_array[0]=written_time;
    if(reservation_time==0) return;
    /*if(old_vpn!=0){

     //old_vpn 정보 다 받아옴.
        for(i=1;;i++){

           // uart_printf("Traversing the chain! %d\n",old_vpn);
            vblock = old_vpn/PAGES_PER_BLK;
            page_num = old_vpn%PAGES_PER_BLK;
  
            //read OOB area
            nand_page_ptread(bank,vblock,PAGES_PER_BLK-1,0,(sizeof(UINT32)*PAGES_PER_BLK*4),FTL_BUF(bank),RETURN_WHEN_DONE);  
            //get lpn from oob area
            mem_copy(lpn_temp, FTL_BUF(bank), sizeof(UINT32)*128);
            mem_copy(old_vpn_temp, FTL_BUF(bank)+sizeof(UINT32)*PAGES_PER_BLK, sizeof(UINT32)*128);
            mem_copy(written_time_temp, FTL_BUF(bank)+(sizeof(UINT32)*PAGES_PER_BLK*2), sizeof(UINT32)*128);
            mem_copy(reservation_time_temp, FTL_BUF(bank)+(sizeof(UINT32)*PAGES_PER_BLK*3), sizeof(UINT32)*128);

           // uart_printf("lpn=%d lpn_tmp:%d old_vpn=%d, written time=%d reservationtime=%d\n",lpn,lpn_temp[page_num],old_vpn,written_time,reservation_time);  
            
            old_vpn_array[i]=old_vpn_temp[page_num];
            written_time_array[i]=written_time_array[page_num];


            if(lpn_temp[page_num]!=lpn)
                break;
            if( tst_bit_dram(RECLAIMABILITY_BMP_ADDR+old_vpn_array[i]/8,old_vpn_array[i]%8)){
              break;
            }        

        }
    }*/
    
    
    //chain_num=i+1;
    //uart_printf("move!chain num=%d\n",i+1);
    //erase_backup_num +=chain_num;

 //   chain_num=2;
    //이전 backup zone chain 첫 정보 받아오기
    /*
    old_vpn= read_dram_32(BACKUP_ZONE_LPN_MAP+lpn*sizeof(UINT32));  
    while(1){
        
        index=0;
       
        ///invalidated time 정함
        if(reservation_time==0){
            //만약에 reservation time=0이라면 굳이 copy할 필요 없으니깐 reclaimability만 check해줌
            set_bit_dram(RECLAIMABILITY_BMP_ADDR+old_vpn_array[chain_num]/8,old_vpn_array[chain_num]%8);
            if(chain_num==0) break;
            
            chain_num--;
            continue;
        }
        if(chain_num==0){
            invalidated_time=ptimer_record();
        }
        else{
            invalidated_time=written_time_array[chain_num-1];
        }
        //uart_printf("reservation_time=%d, invalidated time=%d\n",reservation_time,invalidated_time);

        index=(invalidated_time+reservation_time)/TIME_INT;
       
        write_vpn = get_cur_write_vpn(bank);
        vblock = write_vpn/PAGES_PER_BLK;
        if(backup_zone_cur_write_pagenum[index*2]==0){//backup block 1개도 안찬경우
            while(1){
                vblock++;
                if(get_vcount(bank,vblock)==VC_FREE) break;
            }
            backup_zone_active_blk[bank][index*2+1]=vblock;
            write_vpn=(backup_zone_cur_write_pagenum[bank][index*2]+backup_zone_active_blk[bank][index*2]*PAGES_PER_BLK);
            backup_zone_cur_write_pagenum[bank][index*2]++;

        }
        else if(backup_zone_cur_write_pagenum[bank][index*2]<=PAGES_PER_BLK-1 && backup_zone_cur_write_pagenum[bank][index*2+1]==0){//한개찼는데 아지 ㄱ꽉 안ㅊ남
            write_vpn=(backup_zone_cur_write_pagenum[bank][index*2]+backup_zone_active_blk[bank][index*2]*PAGES_PER_BLK);
            backup_zone_cur_write_pagenum[bank][index*2]++;

        }
        else if(backup_zone_cur_write_pagenum[bank][index*2+1]==0 && backup_zone_cur_write_pagenum[bank][index*2]==PAGES_PER_BLK){//한개는 꽉차고! 뒤에거 새로
             while(1){
                vblock++;
                if(get_vcount(bank,vblock)==VC_FREE) break;
            }
            backup_zone_active_blk[bank][index*2+1]=vblock;
            write_vpn=(backup_zone_cur_write_pagenum[bank][index*2+1] + backup_zone_active_blk[bank][index*2+1]*PAGES_PER_BLK);
            backup_zone_cur_write_pagenum[bank][index*2+1]++;    

        }
        else{//뒤에거 이어서
            write_vpn=(backup_zone_cur_write_pagenum[bank][index*2+1] + backup_zone_active_blk[bank][index*2+1]*PAGES_PER_BLK);
            backup_zone_cur_write_pagenum[bank][index*2+1]++;  
        } 


        nand_page_copyback(bank,old_vpn_array[chain_num]/PAGES_PER_BLK,old_vpn_array[chain_num]%PAGES_PER_BLK,write_vpn/PAGES_PER_BLK,write_vpn%PAGES_PER_BLK);
        page_copy_num++;
        page_write_num++;
        
       // write_dram_32(METADATA_BACKUPZONE_ADDR+ j*PAGES_PER_BLK*2*sizeof(UINT32)+ sizeof(UINT32)*(write_vpn%PAGES_PER_BLK),old_vpn);
       // write_dram_32(METADATA_BACKUPZONE_ADDR + j*PAGES_PER_BLK*2*sizeof(UINT32)+PAGES_PER_BLK*sizeof(UINT32)+sizeof(UINT32)*(write_vpn%PAGES_PER_BLK),written_time_array[i]);
        set_bit_dram(RECLAIMABILITY_BMP_ADDR+old_vpn_array[chain_num]/8,old_vpn_array[chain_num]%8);

        //old_vpn=write_vpn;
        if(chain_num==0) break;
        chain_num--;
    }
    uart_print("finish!");
 
    //lpn과 가장 첫번째 페이지 연결해주는 테이블 갱신
    write_dram_32(BACKUP_ZONE_LPN_MAP+lpn*sizeof(UINT32),write_vpn);  
    */
   

    if(backup_zone_cur_blk==0){
        while(1){
                vblock++;
                if(get_vcount(bank,vblock)==VC_FREE) break;
        }
        write_vpn=vblock;
        backup_zone_cur_write_pagenum[bank][backup_zone_cur_blk]++;

    }
    else if(backup_zone_cur_write_pagenum[backup_zone_cur_blk]==127){
        while(1){
                vblock++;
                if(get_vcount(bank,vblock)==VC_FREE) break;
        }
        backup_zone_cur_blk++;
        backup_zone_cur_write_pagenum[bank][backup_zone_cur_blk]=0;
        backup_zone_active_blk[bank][backup_zone_cur_blk]=vblock;
        write_vpn=vblock;
        backup_zone_cur_write_pagenum[bank][backup_zone_cur_blk]++;

    }
    else{
        write_vpn=backup_zone_active_blk[bank][backup_zone_cur_blk]+backup_zone_cur_write_pagenum[bank][backup_zone_cur_blk];
        backup_zone_cur_write_pagenum[bank][backup_zone_cur_blk]++;
    }

    nand_page_copyback(bank,vpn/PAGES_PER_BLK,vpn%PAGES_PER_BLK,write_vpn/PAGES_PER_BLK,write_vpn%PAGES_PER_BLK);
    page_copy_num++;
    page_write_num++;
/*
    if(backup_zone_cur_blk==0){
        while(1){
                vblock++;
                if(get_vcount(bank,vblock)==VC_FREE) break;
        }
        write_vpn=vblock;
        backup_zone_cur_write_pagenum[bank][backup_zone_cur_blk]++;

    }
    else if(backup_zone_cur_write_pagenum[backup_zone_cur_blk]==127){
        while(1){
                vblock++;
                if(get_vcount(bank,vblock)==VC_FREE) break;
        }
        backup_zone_cur_blk++;
        backup_zone_cur_write_pagenum[bank][backup_zone_cur_blk]=0;
        backup_zone_active_blk[bank][backup_zone_cur_blk]=vblock;
        write_vpn=vblock;
        backup_zone_cur_write_pagenum[bank][backup_zone_cur_blk]++;

    }
    else{
        write_vpn=backup_zone_active_blk[bank][backup_zone_cur_blk]+backup_zone_cur_write_pagenum[bank][backup_zone_cur_blk];
        backup_zone_cur_write_pagenum[bank][backup_zone_cur_blk]++;
    }
    nand_page_copyback(bank,old_vpn/PAGES_PER_BLK,old_vpn%PAGES_PER_BLK,write_vpn/PAGES_PER_BLK,write_vpn%PAGES_PER_BLK);
        page_copy_num++;
        page_write_num++;
    set_bit_dram(RECLAIMABILITY_BMP_ADDR+old_vpn/8,old_vpn%8);

*/

}

//////////////////////////////////////////////////////////////////////////////////////////

// get vpn from PAGE_MAP
static UINT32 get_vpn(UINT32 const lpn)
{
    CHECK_LPAGE(lpn);
    return read_dram_32(PAGE_MAP_ADDR + lpn * sizeof(UINT32));
}
// set vpn to PAGE_MAP
static void set_vpn(UINT32 const lpn, UINT32 const vpn)
{
    UINT8 new_flush;
    UINT32 map_entry_per_page =MAP_ENTRY_PER_PAGE;


    CHECK_LPAGE(lpn);
    ASSERT(vpn >= (META_BLKS_PER_BANK * PAGES_PER_BLK) && vpn < (VBLKS_PER_BANK * PAGES_PER_BLK));


//bitmap 이런식으로 만들기!


    //flush
    //    new_flush = 0x8000>>(lpn%16);
    //    write_dram_16(FLUSH_MAP_ADDR + lpn/16, new_flush);

    new_flush = read_dram_8(FLUSH_MAP_ADDR + (lpn/map_entry_per_page)/8);
  
  //  mem_copy(&new_flush, FLUSH_MAP_ADDR + (lpn/map_entry_per_page)/8, 1);
  //  new_flush = read_dram_8(FLUSH_MAP_ADDR + (lpn/map_entry_per_page)/8);
        //lpn/MAP_ENTRY_PER_PAGE -> 현재 맵핑되는 페이지 번호
        //lpn/MAP_ENTRY_PER_PAGE/8 -> flush table에서는 한 바이트당 8개의 페이지정보를 저장함
    //uart_printf(" lpn : %d, lpn/MAP_ENTRY_PER_PAGE : %d, MAP_ENTRY_PER_PAGE : %d", lpn, lpn/map_entry_per_page, map_entry_per_page);
  //  uart_printf("|set_vpn1| :%d %d, %d, new_flush : 0x%x: ",(lpn/MAP_ENTRY_PER_PAGE) ,(lpn%MAP_ENTRY_PER_PAGE),0x80>>(lpn/MAP_ENTRY_PER_PAGE%8),new_flush);

    new_flush = new_flush | (0x80>>((lpn/map_entry_per_page)%8));
/*
    if(uart_tp != FLUSH_MAP_ADDR + lpn/map_entry_per_page/8)
    {
      uart_tp_pn = lpn/map_entry_per_page;
      uart_printf("set_vpn1 : fmap_addr: %x, new_flush : %x, uart_to_pn: %d", FLUSH_MAP_ADDR + (lpn/map_entry_per_page)/8, new_flush, uart_tp_pn);

      uart_tp = FLUSH_MAP_ADDR + lpn/map_entry_per_page/8;
    }
    */
  //    uart_printf("set_vpn2 : %d, %d, %d, %d, %d",lpn, lpn/map_entry_per_page, lpn/map_entry_per_page/8,FLUSH_MAP_ADDR, FLUSH_MAP_ADDR + lpn/map_entry_per_page/8);
  //  uart_printf("set_vpn2 : lpn : %d, offset : %x", lpn, new_flush);
        //flush table내 바꿔야할 페이지의 비트를 1로 바꾼다.
      write_dram_8(FLUSH_MAP_ADDR + (lpn/map_entry_per_page)/8, new_flush);
  //  mem_copy(FLUSH_MAP_ADDR + (lpn/map_entry_per_page)/8, &new_flush, 1);
    //flush
    write_dram_32(PAGE_MAP_ADDR + lpn * sizeof(UINT32), vpn);







}
// get valid page count of vblock
static UINT32 get_vcount(UINT32 const bank, UINT32 const vblock)
{
    UINT32 vcount;

    ASSERT(bank < NUM_BANKS);
    ASSERT((vblock >= META_BLKS_PER_BANK) && (vblock < VBLKS_PER_BANK));

    vcount = read_dram_16(VCOUNT_ADDR + (((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16)));
    ASSERT((vcount < PAGES_PER_BLK) || (vcount == VC_MAX) || (vcount == VC_FREE));

    return vcount;
}
// set valid page count of vblock
static void set_vcount(UINT32 const bank, UINT32 const vblock, UINT32 const vcount)
{
    ASSERT(bank < NUM_BANKS);
    ASSERT((vblock >= META_BLKS_PER_BANK) && (vblock < VBLKS_PER_BANK));
    ASSERT((vcount < PAGES_PER_BLK) || (vcount == VC_MAX) || (vcount==VC_FREE));

    write_dram_16(VCOUNT_ADDR + (((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16)), vcount);
}
static UINT32 assign_new_write_vpn(UINT32 const bank)
{
    ASSERT(bank < NUM_BANKS);

    UINT32 write_vpn;
    UINT32 vblock;

    write_vpn = get_cur_write_vpn(bank);
    vblock    = write_vpn / PAGES_PER_BLK;

    // NOTE: if next new write page's offset is
    // the last page offset of vblock (i.e. PAGES_PER_BLK - 1),
    if ((write_vpn % PAGES_PER_BLK) == (PAGES_PER_BLK - 2))
    {
        // then, because of the flash controller limitation
        // (prohibit accessing a spare area (i.e. OOB)),
        // thus, we persistenly write a lpn list into last page of vblock.

        //////////////OOB 영역 //////////////////////////////////////////////////

        //lpnlist
        mem_copy(FTL_BUF(bank), g_misc_meta[bank].lpn_list_of_cur_vblock, sizeof(UINT32) * PAGES_PER_BLK);
        
        //backup list
        mem_copy(FTL_BUF(bank) + sizeof(UINT32) * PAGES_PER_BLK, g_misc_meta[bank].old_vpn,sizeof(UINT32)*PAGES_PER_BLK);
        mem_copy(FTL_BUF(bank) + sizeof(UINT32) * PAGES_PER_BLK*2, g_misc_meta[bank].written_time,sizeof(UINT32)*PAGES_PER_BLK);
        mem_copy(FTL_BUF(bank) + sizeof(UINT32) * PAGES_PER_BLK*3, g_misc_meta[bank].reservation_time,sizeof(UINT32)*PAGES_PER_BLK);
      
      
        
        // fix minor bug
        //이부분 아래와같이 수정함nand_page_ptprogram(bank, vblock, PAGES_PER_BLK - 1, 0,
        //                    ((sizefof(UINT32) * PAGES_PER_BLK + BYTES_PER_SECTOR - 1 ) / BYTES_PER_SECTOR), FTL_BUF(bank));
        nand_page_ptprogram(bank, vblock, PAGES_PER_BLK - 1, 0,
                            ((sizeof(UINT32) * 4 * PAGES_PER_BLK  + BYTES_PER_SECTOR - 1 ) / BYTES_PER_SECTOR), FTL_BUF(bank));
        // 


        mem_set_sram(g_misc_meta[bank].lpn_list_of_cur_vblock, 0x00000000, sizeof(UINT32) * PAGES_PER_BLK);
        mem_set_sram(g_misc_meta[bank].old_vpn, 0x00000000, sizeof(UINT32) * PAGES_PER_BLK);
        mem_set_sram(g_misc_meta[bank].written_time, 0x00000000, sizeof(UINT32) * PAGES_PER_BLK);
        mem_set_sram(g_misc_meta[bank].reservation_time, 0x00000000, sizeof(UINT32) * PAGES_PER_BLK);



        ///////////////////////////////////////////////////////////////////////
        
        
        inc_full_blk_cnt(bank);
        uart_printf("*freeblk,copy,write,backup)= %d %d %d %d\n",g_misc_meta[bank].free_blk_cnt,page_copy_num,page_write_num,erase_backup_num);
        // do garbage collection if necessary
        /*
        if(ptimer_record()>특정시간){
            garbage_collection();
        }
        */
        //if(is_full_all_blks(bank))

        if (g_misc_meta[bank].free_blk_cnt<=1910)//1880 
        {
            garbage_collection(bank);
            return get_cur_write_vpn(bank);
        }
        ////지원: 빈 블록 찾아서 계속 움직임. 같은 bank 안에서
        while(1){
            vblock++;
            ASSERT(vblock != VBLKS_PER_BANK);
            if(get_vcount(bank,vblock)!=VC_MAX){
                 if(tst_bit_dram(BACKUP_ZONE_BMP_ADDR + bank*(VBLKS_PER_BANK/8+1), vblock % VBLKS_PER_BANK)==0)
                    break;
            }
        }
        /*
        //빈블록 찾기
        do
        {
            vblock++;
            ASSERT(vblock != VBLKS_PER_BANK);
            uart_print("1");

        }while (get_vcount(bank, vblock) == VC_MAX)// || tst_bit_dram(BACKUP_ZONE_BMP_ADDR + bank*(VBLKS_PER_BANK/8+1), vblock % VBLKS_PER_BANK)==0);
        */
    }
    // write page -> next block
    if (vblock != (write_vpn / PAGES_PER_BLK))
    {
        write_vpn = vblock * PAGES_PER_BLK;
    }
    else
    {
        write_vpn++;
    }
    set_new_write_vpn(bank, write_vpn);

    return write_vpn;
}
static BOOL32 is_bad_block(UINT32 const bank, UINT32 const vblk_offset)
{
    if (tst_bit_dram(BAD_BLK_BMP_ADDR + bank*(VBLKS_PER_BANK/8 + 1), vblk_offset) == FALSE)
    {
        return FALSE;
    }
    return TRUE;
}

static void erase_backupblk(UINT32 const bank,UINT32 const expired_bucket){

    
    //uart_print("erase expired blocks!");
    if(backup_zone_active_blk[expired_bucket]!=0){
        nand_block_erase(bank, backup_zone_active_blk[expired_bucket]);
        set_vcount(bank, backup_zone_active_blk[expired_bucket], VC_FREE);
    }
    if(backup_zone_active_blk[expired_bucket+1]!=0){
        nand_block_erase(bank, backup_zone_active_blk[expired_bucket+1]);
        set_vcount(bank, backup_zone_active_blk[expired_bucket+1], VC_FREE);
    }
  
    
    //for(int i=0;i<time_bucket_queue[bank][expired_bucket].block_num;i++){
      //  nand_block_erase(bank, time_bucket_queue[bank][expired_bucket].blocks_in_bucket[i]);
        //clr_bit_dram(BACKUP_ZONE_BMP_ADDR + bank*(VBLKS_PER_BANK/8+1), time_bucket_queue[bank][expired_bucket].blocks_in_bucket[i] % VBLKS_PER_BANK);
   // }
    //start_bucket=expired_bucket+1;
     
}

//------------------------------------------------------------
// if all blocks except one free block are full,
// do garbage collection for making at least one free page
//-------------------------------------------------------------
/*
static void check_backupzone(UINT32 const bank, UINT32 const vblock){
    for(int i=0;i<MAX_BUBLK;i++){
        if(backup_zone_active_blk[bank][i]==vblock)
            return 1;
    }
    return 0;
}
*/
static void garbage_collection(UINT32 const bank)
{
    ASSERT(bank < NUM_BANKS);
    g_ftl_statistics[bank].gc_cnt++;
    gcnum++;

    UINT32 src_lpn;
    UINT32 vt_vblock;
    UINT32 free_vpn;
    UINT32 vcount; // valid page count in victim block
    UINT32 src_page;
    UINT32 gc_vblock;

    UINT8 reclaimability;
    UINT32 src_vpn;
    //uart_print("garbage_collection start!\n");
///////////////////////////GC 시작할 때 backup zone에 기간 지난 블ㄹ럭 있는지 확인해주자todo///////////////////////////////////
/*
    for(int i=0;i<MAX_BUBLK/2;i++){
        if(TIME_INT*i<ptimer_record()){
            if(backup_zone_active_blk[2*i]!=0){
                erase_backupblk(bank,2*i);
            
                //uart_print("There is a backup zone block to erase!\n");
                return;
            }
        }
    }
    */
/*
    for(i=start_bucket;;i++){
        
        if(time_bucket_queue[bank][i % NUM_BUCKET].expiration_time < ptimer_record()){
            break;
        }

        if((i+1)%NUM_BUCKET == start_bucket){
            i++;
            break;
        }
    }
*//*
    if(i%NUM_BUCKET!=start_bucket){
       erase_backupblk(bank,i);
       uart_print("There is a backup zone block to erase!\n");

       return;
     }
    uart_print("There is no backup zone block to erase!\n");

 */
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    g_ftl_statistics[bank].gc_cnt++;

    vt_vblock = get_vt_vblock(bank); 

  // get victim block
    vcount    = get_vcount(bank, vt_vblock);

    gc_vblock = get_gc_vblock(bank);

    free_vpn  = gc_vblock * PAGES_PER_BLK;

    //uart_printf("garbage_collection bank %d, vblock %d vcount %x write_num %d copy num %d",bank, vt_vblock,vcount,page_write_num,page_copy_num);

    ASSERT(vt_vblock != gc_vblock);
    ASSERT(vt_vblock >= META_BLKS_PER_BANK && vt_vblock < VBLKS_PER_BANK);

    //ASSERT(vcount < (PAGES_PER_BLK - 1));
   // ASSERT(get_vcount(bank, gc_vblock) == VC_MAX);
    ASSERT(!is_bad_block(bank, gc_vblock));

    // 1. load p2l list from last page offset of victim block (4B x PAGES_PER_BLK)
    //지원: 여기서 마지막 OOB 영역에 대한 복사가 이루어짐. 이곳에서 원래의 backup 정보 또한 복사가 되어야함. 
    // fix minor bug
    nand_page_ptread(bank, vt_vblock, PAGES_PER_BLK - 1, 0,
                     ((sizeof(UINT32) * PAGES_PER_BLK + BYTES_PER_SECTOR - 1 ) / BYTES_PER_SECTOR), FTL_BUF(bank), RETURN_WHEN_DONE);   

    
    mem_copy(g_misc_meta[bank].lpn_list_of_cur_vblock, FTL_BUF(bank), sizeof(UINT32) * PAGES_PER_BLK);
    //for(int i=0;i<128;i++){
      //  uart_printf("hhhh%d\n",g_misc_meta[bank].lpn_list_of_cur_vblock[i]);
   // }
 
    
   // uart_print("1!\n");
   UINT32 rt_temp[129];

    
    // 2. copy-back all valid pages to free space
    for (src_page = 0; src_page < (PAGES_PER_BLK - 1); src_page++)
    {
        
        // get lpn of victim block from a read lpn list
        src_lpn = get_lpn(bank, src_page);
        
        CHECK_VPAGE(get_vpn(src_lpn));
  

/*원래는 invalid와 valid를 나눠야하는데 이 경우에는 모든 페이지가 backup이고 이 페이지의 written 시간이 중요함
따라서 written time이 지나게 되면? 지우면됨! 
*/      src_vpn = get_vpn(src_lpn);//ftl

        //uart_printf("src_lpn=%d src_vpn=%d src_page=%d\n",src_lpn,src_vpn,(vt_vblock * PAGES_PER_BLK) + src_page);
        // determine whether the page is valid or not
        mem_copy(rt_temp, FTL_BUF(bank)+(sizeof(UINT32)*PAGES_PER_BLK*3), sizeof(UINT32)*128);

        if (src_vpn!=
            ((vt_vblock * PAGES_PER_BLK) + src_page))
        { //invalid pages
            //여기서 bitmap을 확인하고 만약 페이지가 reclaimable하다면! 그냥 지우고 아니라면? 백업이니까 백업존에 가야하는데 이때 chain 따라서 보내줘 
            //uart_print("invalid pages\n");
            if(rt_temp[src_page]==0){ 
               //erase_backup_num++;
               continue;
           }
            if(tst_bit_dram(RECLAIMABILITY_BMP_ADDR+src_vpn/8,src_vpn%8)){//reclaimable
                //uart_print("reclaimable!");
                continue;
            }
            else
            {//not reclaimable backup page
                move_to_backup_zone(bank,(vt_vblock*PAGES_PER_BLK)+src_page, src_lpn); //백업존에 적어주는 함수
                continue;
            }


        }
        else
        {//valid인 경우
            //uart_print("valid pages\n");
            ASSERT(get_lpn(bank, src_page) != INVALID);
            CHECK_LPAGE(src_lpn);
           // if the page is valid,
            // then do copy-back op. to free space
            nand_page_copyback(bank,
                               vt_vblock,
                               src_page,
                               free_vpn / PAGES_PER_BLK,
                               free_vpn % PAGES_PER_BLK);
           ASSERT((free_vpn / PAGES_PER_BLK) == gc_vblock);
           page_write_num++;
           page_copy_num++;
           //backup data에 대한 정보는 복사해주어야함. 
            g_misc_meta[bank].old_vpn[free_vpn % PAGES_PER_BLK]  = g_misc_meta[bank].old_vpn[src_page];
            
            set_vpn(src_lpn, free_vpn);
           
            set_lpn(bank, (free_vpn % PAGES_PER_BLK), src_lpn);
           
            free_vpn++;
        }

/*

        // update metadata

 //////////////////// 지원:이곳의 나머지 OOB(backup 정보) 또한 다시 옮겨준다 ////////////////////////////////  
 //   지원:여기서 get_lpn처럼 meta data를 갖고오는 함수 하나 만들어서 사용해보자
        set_vpn(src_lpn, free_vpn);
        set_lpn(bank, (free_vpn % PAGES_PER_BLK), src_lpn);
       
/////////////////////////////////////////////////////////////////////////////////////////////////

        free_vpn++;
        */
    }
#if OPTION_ENABLE_ASSERT
    if (vcount == 0)
    {
        ASSERT(free_vpn == (gc_vblock * PAGES_PER_BLK));
    }
#endif
    // 3. erase victim block
    nand_block_erase(bank, vt_vblock);
    ASSERT((free_vpn % PAGES_PER_BLK) < (PAGES_PER_BLK - 2));
    ASSERT((free_vpn % PAGES_PER_BLK == vcount));

/*     uart_printf("gc page count : %d", vcount); */

    // 4. update metadata
    
    set_vcount(bank, vt_vblock, VC_MAX);

    if(vcount==0){
        set_vcount(bank, gc_vblock, VC_FREE);
    }
    else{
        set_vcount(bank, gc_vblock, vcount);
    }
    set_new_write_vpn(bank, free_vpn); // set a free page for new write
    set_gc_vblock(bank, vt_vblock); // next free block (reserve for GC)
    dec_full_blk_cnt(bank); // decrease full block count
    uart_print("garbage_collection end"); 
}
//-------------------------------------------------------------
// Victim selection policy: Greedy
//
// Select the block which contain minumum valid pages
//-------------------------------------------------------------

static UINT32 get_vt_vblock(UINT32 const bank)
{
    ASSERT(bank < NUM_BANKS);

    UINT32 vblock;

    // search the block which has mininum valid pages
 //   uart_print("get victim block!\n");
    vblock = mem_search_min_max(VCOUNT_ADDR + (bank * VBLKS_PER_BANK * sizeof(UINT16)),
                                   sizeof(UINT16),
                                   VBLKS_PER_BANK,
                                   MU_CMD_SEARCH_MIN_DRAM,
                                   bank);
        
        
    
    //return MISCBLK_VBN;
    //return 0x1;
    //vblock=0x1;
    //while(1){
      //  if(get_vcount(bank,vblock)!=0)
        //    break;
        //vblock++;     
   // }
    ASSERT(is_bad_block(bank, vblock) == FALSE);
    ASSERT(vblock >= META_BLKS_PER_BANK && vblock < VBLKS_PER_BANK);
    //ASSERT(get_vcount(bank, vblock) < (PAGES_PER_BLK - 1));

    return vblock;
}
static void format(void)
{
    UINT32 bank, vblock, vcount_val;
    int i;
    ASSERT(NUM_MISC_META_SECT > 0);
    ASSERT(NUM_VCOUNT_SECT > 0);

    uart_printf("Total FTL DRAM metadata size: %d KB", DRAM_BYTES_OTHER / 1024);

    uart_printf("VBLKS_PER_BANK: %d", VBLKS_PER_BANK);
    uart_printf("LBLKS_PER_BANK: %d", NUM_LPAGES / PAGES_PER_BLK / NUM_BANKS);
    uart_printf("META_BLKS_PER_BANK: %d", META_BLKS_PER_BANK);
    uart_printf("PAGES_PER_BLK : %d", PAGES_PER_BLK);
    ///지원: 확인용//////////
    uart_printf("NUM_LPAGES: %d", NUM_LPAGES);
    uart_printf("NUM_BANKS : %d" , NUM_BANKS);
    ////////////////////////


    //----------------------------------------
    // initialize DRAM metadata
    //----------------------------------------
    mem_set_dram(PAGE_MAP_ADDR, NULL, PAGE_MAP_BYTES);
    mem_set_dram(VCOUNT_ADDR, 0xCECECECE, VCOUNT_BYTES);
    /*
    for(i=0; i<VCOUNT_BYTES; i+=1)
        uart_printf("!!!!!!!=%x\n",read_dram_8(VCOUNT_ADDR+i));
    while(1){

    }
    */
    mem_set_dram(FLUSH_MAP_ADDR, NULL, FLUSH_MAP_BYTES);
    mem_set_dram(RECLAIMABILITY_BMP_ADDR, NULL, RECLAIMABILITY_BMP_BYTES);
    mem_set_dram(BACKUP_ZONE_BMP_ADDR,NULL,BACKUP_ZONE_BMP_BYTES);
    mem_set_dram(BACKUP_ZONE_LPN_MAP,NULL,BACKUP_ZONE_LPN_BYTES);
    //----------------------------------------
    // erase all blocks except vblock #0
    //----------------------------------------
	for (vblock = MISCBLK_VBN; vblock < VBLKS_PER_BANK; vblock++)
	{
		for (bank = 0; bank < NUM_BANKS; bank++)
		{
            vcount_val = VC_MAX;
            if (is_bad_block(bank, vblock) == FALSE)
			{
				nand_block_erase(bank, vblock);
                vcount_val = VC_FREE;
            }
            write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16),
                          vcount_val);
        }
    }
    //----------------------------------------
    // initialize SRAM metadata
    //----------------------------------------
    init_metadata_sram();
    //uart_print("1");

    // flush metadata to NAND
    logging_pmap_table();
   // uart_print("1");
    
    logging_misc_metadata();
   // uart_print("1");
    
    write_format_mark();
	led(1);
    uart_print("format complete");
}
static void init_metadata_sram(void)
{
    UINT32 bank;
    UINT32 vblock;
    UINT32 mapblk_lbn;

    //----------------------------------------
    // initialize misc. metadata
    //----------------------------------------
    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        g_misc_meta[bank].free_blk_cnt = VBLKS_PER_BANK - META_BLKS_PER_BANK;
        g_misc_meta[bank].free_blk_cnt -= get_bad_blk_cnt(bank);
        // NOTE: vblock #0,1 don't use for user space
        write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + 0) * sizeof(UINT16), VC_MAX);
        write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + 1) * sizeof(UINT16), VC_MAX);

        //----------------------------------------
        // assign misc. block
        //----------------------------------------
        // assumption: vblock #1 = fixed location.
        // Thus if vblock #1 is a bad block, it should be allocate another block.
        set_miscblk_vpn(bank, MISCBLK_VBN * PAGES_PER_BLK - 1);
        ASSERT(is_bad_block(bank, MISCBLK_VBN) == FALSE);

        vblock = MISCBLK_VBN;

        //----------------------------------------
        // assign map block
        //----------------------------------------
        mapblk_lbn = 0;
        while (mapblk_lbn < MAPBLKS_PER_BANK)
        {
            vblock++;
            ASSERT(vblock < VBLKS_PER_BANK);
            if (is_bad_block(bank, vblock) == FALSE)
            {
                set_mapblk_vpn(bank, mapblk_lbn, vblock * PAGES_PER_BLK);
                write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16), VC_MAX);
                mapblk_lbn++;
            }
        }
        //----------------------------------------
        // assign free block for gc
        //----------------------------------------
        do
        {
            vblock++;
            // NOTE: free block should not be secleted as a victim @ first GC
            write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16), VC_MAX);
            // set free block
            set_gc_vblock(bank, vblock);

            ASSERT(vblock < VBLKS_PER_BANK);
        }while(is_bad_block(bank, vblock) == TRUE);
        //----------------------------------------

        // assign free vpn for first new write
        //----------------------------------------
        do
        {
            vblock++;
            // 현재 next vblock부터 새로운 데이터를 저장을 시작
            set_new_write_vpn(bank, vblock * PAGES_PER_BLK);
            //0이면 free 1이면 안에 뭐 있음
            ASSERT(vblock < VBLKS_PER_BANK);
        }while(is_bad_block(bank, vblock) == TRUE);
    }
}
// logging misc + vcount metadata
static void logging_misc_metadata(void)
{
    UINT32 misc_meta_bytes = NUM_MISC_META_SECT * BYTES_PER_SECTOR; // per bank
    UINT32 vcount_addr     = VCOUNT_ADDR;
    UINT32 vcount_bytes    = NUM_VCOUNT_SECT * BYTES_PER_SECTOR; // per bank
    UINT32 vcount_boundary = VCOUNT_ADDR + VCOUNT_BYTES; // entire vcount data
    UINT32 bank;

    flash_finish();

    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        inc_miscblk_vpn(bank);

        // note: if misc. meta block is full, just erase old block & write offset #0
        if ((get_miscblk_vpn(bank) / PAGES_PER_BLK) != MISCBLK_VBN)
        {
            nand_block_erase(bank, MISCBLK_VBN);
            set_miscblk_vpn(bank, MISCBLK_VBN * PAGES_PER_BLK); // vpn = 128
        }
        // copy misc. metadata to FTL buffer
        mem_copy(FTL_BUF(bank), &g_misc_meta[bank], misc_meta_bytes);

        // copy vcount metadata to FTL buffer
        if (vcount_addr <= vcount_boundary)
        {
            mem_copy(FTL_BUF(bank) + misc_meta_bytes, vcount_addr, vcount_bytes);
            vcount_addr += vcount_bytes;
        }
    }
    // logging the misc. metadata to nand flash
    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        nand_page_ptprogram(bank,
                            get_miscblk_vpn(bank) / PAGES_PER_BLK,
                            get_miscblk_vpn(bank) % PAGES_PER_BLK,
                            0,
                            NUM_MISC_META_SECT + NUM_VCOUNT_SECT,
                            FTL_BUF(bank));
    }
    flash_finish();
}
static void logging_pmap_table(void)
{
    UINT32 pmap_addr  = PAGE_MAP_ADDR;
    UINT32 pmap_bytes = BYTES_PER_PAGE; // per bank
    UINT32 mapblk_vpn;
    UINT32 bank;
    UINT32 pmap_boundary = PAGE_MAP_ADDR + PAGE_MAP_BYTES;
    BOOL32 finished = FALSE;
    //key flush




    UINT32 fmap_next = 0;
    UINT32 fmap_addr = FLUSH_MAP_ADDR;  //flush mapping table 비트단위로 맵핑이다.
    UINT8 cur_fmap;
//flush
   // num_flush++;
    for (UINT32 mapblk_lbn = 0; mapblk_lbn < MAPBLKS_PER_BANK; mapblk_lbn++)
    {
        flash_finish();

        for (bank = 0; bank < NUM_BANKS; bank++)
        {
            if (finished)
            {
                break;
            }
            else if (pmap_addr >= pmap_boundary)
            {
                finished = TRUE;
                break;
            }
            else if (pmap_addr + BYTES_PER_PAGE >= pmap_boundary)
            {
                finished = TRUE;
                pmap_bytes = (pmap_boundary - pmap_addr + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR ;
            }


            //flush


            if(fmap_next%8 == 0)
            {
                  if(fmap_next != 0)  //처음에는 update하면 안된다.
                      fmap_addr += 1;

                //  mem_copy(&cur_fmap, fmap_addr, 1);  //1바이트씩 가져온다.(8개의 페이지정보)
                  cur_fmap = read_dram_8(fmap_addr);
                  //if(uart_tp == fmap_addr)
                  //  uart_printf("1. uart_tp : %x, fmap_addr : %x , cur_fmap : %x", uart_tp, fmap_addr, cur_fmap);
            }
            fmap_next += 1;

          //  if(uart_tp == fmap_addr)
              //uart_printf("2. uart_tp : %x, fmap_addr : %x , cur_fmap : %x, fmap_next: %d, comp : %d", uart_tp, fmap_addr, cur_fmap, fmap_next%8,cur_fmap >> (8 - fmap_next%8 ));
            if( (cur_fmap >> (8 - fmap_next%8 )) & 0x01 != 0) //flush 해야하는 페이지의 경우
            {


                //  uart_printf("3. uart_tp : %x, fmap_addr : %x, fmap_num: %d , cur_fmap : %x, fmap_next : %d, set_vpn: page num : %d", uart_tp, fmap_addr, fmap_addr - FLUSH_MAP_ADDR, cur_fmap, fmap_next%8,  uart_tp_pn);

                  //uart_printf("bingo!");
                  //여기서부터가 실제 logging하는 부분임
                  //안쓴녀석한테 써야하므로 순차적으로 내려가는 모양이지.

                  inc_mapblk_vpn(bank, mapblk_lbn);

                  mapblk_vpn = get_mapblk_vpn(bank, mapblk_lbn);

                  // note: if there is no free page, then erase old map block first.
                  if ((mapblk_vpn % PAGES_PER_BLK) == 0)
                  {
                          // erase full map block
                          nand_block_erase(bank, (mapblk_vpn - 1) / PAGES_PER_BLK);

                          // next vpn of mapblk is offset #0
                          set_mapblk_vpn(bank, mapblk_lbn, ((mapblk_vpn - 1) / PAGES_PER_BLK) * PAGES_PER_BLK);
                          mapblk_vpn = get_mapblk_vpn(bank, mapblk_lbn);
                  }
                  // copy the page mapping table to FTL buffer
                  mem_copy(FTL_BUF(bank), pmap_addr, pmap_bytes);

            // logging update page mapping table into map_block
                  nand_page_ptprogram(bank,
                                  mapblk_vpn / PAGES_PER_BLK,
                                  mapblk_vpn % PAGES_PER_BLK,
                                  0,
                                  pmap_bytes / BYTES_PER_SECTOR,
                                  FTL_BUF(bank));
                 // num_logging++;
            }
            pmap_addr += pmap_bytes;
        }
        if (finished)
        {
            break;
        }
    }
    mem_set_dram(FLUSH_MAP_ADDR, 0,FLUSH_MAP_BYTES); //key flush
    flash_finish();

  //  uart_printf("num_flush : %d, num_logging : %d",num_flush, num_logging);
}
// load flushed FTL metadta
static void load_metadata(void)
{
    load_misc_metadata();
    load_pmap_table();
}
// misc + VCOUNT
static void load_misc_metadata(void)
{
    UINT32 misc_meta_bytes = NUM_MISC_META_SECT * BYTES_PER_SECTOR;
    UINT32 vcount_bytes    = NUM_VCOUNT_SECT * BYTES_PER_SECTOR;
    UINT32 vcount_addr     = VCOUNT_ADDR;
    UINT32 vcount_boundary = VCOUNT_ADDR + VCOUNT_BYTES;

    UINT32 load_flag = 0;
    UINT32 bank, page_num;
    UINT32 load_cnt = 0;

    flash_finish();

	disable_irq();
	flash_clear_irq();	// clear any flash interrupt flags that might have been set

    // scan valid metadata in descending order from last page offset
    for (page_num = PAGES_PER_BLK - 1; page_num != ((UINT32) -1); page_num--)
    {
        for (bank = 0; bank < NUM_BANKS; bank++)
        {
            if (load_flag & (0x1 << bank))
            {
                continue;
            }
            // read valid metadata from misc. metadata area
            nand_page_ptread(bank,
                             MISCBLK_VBN,
                             page_num,
                             0,
                             NUM_MISC_META_SECT + NUM_VCOUNT_SECT,
                             FTL_BUF(bank),
                             RETURN_ON_ISSUE);
        }
        flash_finish();

        for (bank = 0; bank < NUM_BANKS; bank++)
        {
            if (!(load_flag & (0x1 << bank)) && !(BSP_INTR(bank) & FIRQ_ALL_FF))
            {
                load_flag = load_flag | (0x1 << bank);
                load_cnt++;
            }
            CLR_BSP_INTR(bank, 0xFF);
        }
    }
    ASSERT(load_cnt == NUM_BANKS);

    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        // misc. metadata
        mem_copy(&g_misc_meta[bank], FTL_BUF(bank), sizeof(misc_metadata));

        // vcount metadata
        if (vcount_addr <= vcount_boundary)
        {
            mem_copy(vcount_addr, FTL_BUF(bank) + misc_meta_bytes, vcount_bytes);
            vcount_addr += vcount_bytes;

        }
    }
	enable_irq();
}
static void load_pmap_table(void)
{
    UINT32 pmap_addr = PAGE_MAP_ADDR;
    UINT32 temp_page_addr;
    UINT32 pmap_bytes = BYTES_PER_PAGE; // per bank
    UINT32 pmap_boundary = PAGE_MAP_ADDR + (NUM_LPAGES * sizeof(UINT32));
    UINT32 mapblk_lbn, bank;
    BOOL32 finished = FALSE;

    flash_finish();

    for (mapblk_lbn = 0; mapblk_lbn < MAPBLKS_PER_BANK; mapblk_lbn++)
    {
        temp_page_addr = pmap_addr; // backup page mapping addr

        for (bank = 0; bank < NUM_BANKS; bank++)
        {
            if (finished)
            {
                break;
            }
            else if (pmap_addr >= pmap_boundary)
            {
                finished = TRUE;
                break;
            }
            else if (pmap_addr + BYTES_PER_PAGE >= pmap_boundary)
            {
                finished = TRUE;
                pmap_bytes = (pmap_boundary - pmap_addr + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR;
            }
            // read page mapping table from map_block
            nand_page_ptread(bank,
                             get_mapblk_vpn(bank, mapblk_lbn) / PAGES_PER_BLK,
                             get_mapblk_vpn(bank, mapblk_lbn) % PAGES_PER_BLK,
                             0,
                             pmap_bytes / BYTES_PER_SECTOR,
                             FTL_BUF(bank),
                             RETURN_ON_ISSUE);
            pmap_addr += pmap_bytes;
        }
        flash_finish();

        pmap_bytes = BYTES_PER_PAGE;
        for (bank = 0; bank < NUM_BANKS; bank++)
        {
            if (temp_page_addr >= pmap_boundary)
            {
                break;
            }
            else if (temp_page_addr + BYTES_PER_PAGE >= pmap_boundary)
            {
                pmap_bytes = (pmap_boundary - temp_page_addr + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR;
            }
            // copy page mapping table to PMAP_ADDR from FTL buffer
            mem_copy(temp_page_addr, FTL_BUF(bank), pmap_bytes);

            temp_page_addr += pmap_bytes;
        }
        if (finished)
        {
            break;
        }
    }
}
static void write_format_mark(void)
{
	// This function writes a format mark to a page at (bank #0, block #0).

	#ifdef __GNUC__
	extern UINT32 size_of_firmware_image;
	UINT32 firmware_image_pages = (((UINT32) (&size_of_firmware_image)) + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
	#else
	extern UINT32 Image$$ER_CODE$$RO$$Length;
	extern UINT32 Image$$ER_RW$$RW$$Length;
	UINT32 firmware_image_bytes = ((UINT32) &Image$$ER_CODE$$RO$$Length) + ((UINT32) &Image$$ER_RW$$RW$$Length);
	UINT32 firmware_image_pages = (firmware_image_bytes + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
	#endif

	UINT32 format_mark_page_offset = FW_PAGE_OFFSET + firmware_image_pages;

	mem_set_dram(FTL_BUF_ADDR, 0, BYTES_PER_SECTOR);

	SETREG(FCP_CMD, FC_COL_ROW_IN_PROG);
	SETREG(FCP_BANK, REAL_BANK(0));
	SETREG(FCP_OPTION, FO_E | FO_B_W_DRDY);
	SETREG(FCP_DMA_ADDR, FTL_BUF_ADDR); 	// DRAM -> flash
	SETREG(FCP_DMA_CNT, BYTES_PER_SECTOR);
	SETREG(FCP_COL, 0);
	SETREG(FCP_ROW_L(0), format_mark_page_offset);
	SETREG(FCP_ROW_H(0), format_mark_page_offset);

	// At this point, we do not have to check Waiting Room status before issuing a command,
	// because we have waited for all the banks to become idle before returning from format().
	SETREG(FCP_ISSUE, NULL);

	// wait for the FC_COL_ROW_IN_PROG command to be accepted by bank #0
	while ((GETREG(WR_STAT) & 0x00000001) != 0);

	// wait until bank #0 finishes the write operation
	while (BSP_FSM(0) != BANK_IDLE);
}
static BOOL32 check_format_mark(void)
{
	// This function reads a flash page from (bank #0, block #0) in order to check whether the SSD is formatted or not.

	#ifdef __GNUC__
	extern UINT32 size_of_firmware_image;
	UINT32 firmware_image_pages = (((UINT32) (&size_of_firmware_image)) + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
	#else
	extern UINT32 Image$$ER_CODE$$RO$$Length;
	extern UINT32 Image$$ER_RW$$RW$$Length;
	UINT32 firmware_image_bytes = ((UINT32) &Image$$ER_CODE$$RO$$Length) + ((UINT32) &Image$$ER_RW$$RW$$Length);
	UINT32 firmware_image_pages = (firmware_image_bytes + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
	#endif

	UINT32 format_mark_page_offset = FW_PAGE_OFFSET + firmware_image_pages;
	UINT32 temp;

	flash_clear_irq();	// clear any flash interrupt flags that might have been set

	SETREG(FCP_CMD, FC_COL_ROW_READ_OUT);
	SETREG(FCP_BANK, REAL_BANK(0));
	SETREG(FCP_OPTION, FO_E);
	SETREG(FCP_DMA_ADDR, FTL_BUF_ADDR); 	// flash -> DRAM
	SETREG(FCP_DMA_CNT, BYTES_PER_SECTOR);
	SETREG(FCP_COL, 0);
	SETREG(FCP_ROW_L(0), format_mark_page_offset);
	SETREG(FCP_ROW_H(0), format_mark_page_offset);

	// At this point, we do not have to check Waiting Room status before issuing a command,
	// because scan list loading has been completed just before this function is called.
	SETREG(FCP_ISSUE, NULL);

	// wait for the FC_COL_ROW_READ_OUT command to be accepted by bank #0
	while ((GETREG(WR_STAT) & 0x00000001) != 0);

	// wait until bank #0 finishes the read operation
	while (BSP_FSM(0) != BANK_IDLE);

	// Now that the read operation is complete, we can check interrupt flags.
	temp = BSP_INTR(0) & FIRQ_ALL_FF;

	// clear interrupt flags
	CLR_BSP_INTR(0, 0xFF);

	if (temp != 0)
	{
		return FALSE;	// the page contains all-0xFF (the format mark does not exist.)
	}
	else
	{
		return TRUE;	// the page contains something other than 0xFF (it must be the format mark)
	}
}

// BSP interrupt service routine
void ftl_isr(void)
{
    UINT32 bank;
    UINT32 bsp_intr_flag;

    uart_print("BSP interrupt occured...");
    
    // interrupt pending clear (ICU)
    SETREG(APB_INT_STS, INTR_FLASH);

    for (bank = 0; bank < NUM_BANKS; bank++) {
        while (BSP_FSM(bank) != BANK_IDLE);
        // get interrupt flag from BSP
        bsp_intr_flag = BSP_INTR(bank);

        if (bsp_intr_flag == 0) {
            continue;
        }
        UINT32 fc = GETREG(BSP_CMD(bank));
        // BSP clear
        CLR_BSP_INTR(bank, bsp_intr_flag);

        // interrupt handling
		if (bsp_intr_flag & FIRQ_DATA_CORRUPT) {
            uart_printf("BSP interrupt at bank: 0x%x", bank);
            uart_print("FIRQ_DATA_CORRUPT occured...");
		}
		if (bsp_intr_flag & (FIRQ_BADBLK_H | FIRQ_BADBLK_L)) {
            uart_printf("BSP interrupt at bank: 0x%x", bank);
			if (fc == FC_COL_ROW_IN_PROG || fc == FC_IN_PROG || fc == FC_PROG) {
                uart_print("find runtime bad block when block program...");
			}
			else {
                uart_printf("find runtime bad block when block erase...vblock #: %d", GETREG(BSP_ROW_H(bank)) / PAGES_PER_BLK);
				ASSERT(fc == FC_ERASE);
			}
		}
    }
}






void ftl_test(void)
{
    
    uart_print("start ftl test...!!!!!!!!!");
/*     fillup_dataspace(); */
    //uart_print("!!!!!");
   // uart_print("start write");
    tc_write_seq(0, 1000000, NUM_PSECTORS_32KB); 
  //  move_to_backup_zone(0,10885,0);
  //  tc_write_rand(0, 200000, NUM_PSECTORS_4KB);
/*     tc_write_rand(0, 2000000, NUM_PSECTORS_4KB); */
    uart_print("ftl test passed!");
}
static void aging_with_rw(UINT32 io_cnt)
{
    UINT32 lba;
    UINT32 const num_sectors = SECTORS_PER_PAGE * NUM_BANKS;
    srand(RANDOM_SEED);

    uart_printf("start aging with random writes");
    while (io_cnt > 0) {
        do {
            lba = rand() % IO_LIMIT;
        }while (lba >= (NUM_LSECTORS - num_sectors));
        // page alignment
        lba = lba / SECTORS_PER_PAGE * SECTORS_PER_PAGE;
        ftl_write(lba, num_sectors,60);

        io_cnt--;
    }
    uart_printf("complete!");
}
// fill entire dataspace
static void fillup_dataspace(void)
{
    UINT32 lba = 0;
    UINT32 const num_sectors = SECTORS_PER_PAGE * NUM_BANKS;

    uart_printf("start fill entire data space");
    while (lba < (NUM_LSECTORS - num_sectors))
    {
        ftl_write(lba, num_sectors,60);
        lba += num_sectors;
    }
    uart_printf("complete!");
}
static void tc_write_seq(const UINT32 start_lsn, const UINT32 io_num, const UINT32 sector_size)
{
    UINT32 i, j, wr_buf_addr, rd_buf_addr, data;
    UINT32 lba, num_sectors = sector_size;
    UINT32 io_cnt = io_num;
    UINT32 const start_lba = start_lsn;

    /* UINT32 volatile g_barrier = 0; while (g_barrier == 0); */
    led(0);
    uart_printf("sequential write!");

    // STEP 1 - write
    for (UINT32 loop = 0; loop < 5; loop++)
    {
        wr_buf_addr = WR_BUF_ADDR;
        data = 0;
        lba  = start_lba;

        uart_print_32(loop); uart_print("");

        for (i = 0; i < io_cnt; i++)
        {
            wr_buf_addr = WR_BUF_PTR(g_ftl_write_buf_id) + ((lba % SECTORS_PER_PAGE) * BYTES_PER_SECTOR);
            for (j = 0; j < num_sectors; j++)
            {
                mem_set_dram(wr_buf_addr, data, BYTES_PER_SECTOR);

                wr_buf_addr += BYTES_PER_SECTOR;

                if (wr_buf_addr >= WR_BUF_ADDR + WR_BUF_BYTES)
                {
                    wr_buf_addr = WR_BUF_ADDR;
                }
                data++;
            }
            //ptimer_start();
          //  uart_print("ftl write start!");
            ftl_write(lba, num_sectors,60);
            //ptimer_stop_and_uart_print();

            lba += num_sectors;

            if (lba >= (UINT32)NUM_LSECTORS)
            {
                uart_print("adjust lba because of out of lba");
                lba = 0;
            }
        }

        // STEP 2 - read and verify
        rd_buf_addr = RD_BUF_ADDR;
        data = 0;
        lba  = start_lba;
        num_sectors = MIN(num_sectors, NUM_RD_BUFFERS * SECTORS_PER_PAGE);

        for (i = 0; i < io_cnt; i++)
        {
            rd_buf_addr = RD_BUF_PTR(g_ftl_read_buf_id) + ((lba % SECTORS_PER_PAGE) * BYTES_PER_SECTOR);
            /* ptimer_start(); */
            ftl_read(lba, num_sectors);

            flash_finish();
            /* ptimer_stop_and_uart_print(); */

            for (j = 0; j < num_sectors; j++)
            {
                UINT32 sample = read_dram_32(rd_buf_addr);

                if (sample != data)
                {
                    uart_printf("ftl test fail...io#: %d, %d", lba, num_sectors);
                    uart_printf("sample data %d should be %d", sample, data);
                    led_blink();
                }

                rd_buf_addr += BYTES_PER_SECTOR;

                if (rd_buf_addr >= RD_BUF_ADDR + RD_BUF_BYTES)
                {
                    rd_buf_addr = RD_BUF_ADDR;
                }
                data++;
            }

            lba += num_sectors;

            if (lba >= IO_LIMIT + num_sectors)
            {
                lba = 0;
            }
        }
    }
    ftl_flush();
}
static void tc_write_rand(const UINT32 start_lsn, const UINT32 io_num, const UINT32 sector_size)
{
    UINT32 i, j, wr_buf_addr, rd_buf_addr, data, r_data;
    UINT32 lba, num_sectors = sector_size;
    UINT32 io_cnt = io_num;
    uart_print("!!!!!!!!!!!!!!!!!!!!!!!");
    /* UINT32 volatile g_barrier = 0; while (g_barrier == 0); */
    led(0);
    srand(RANDOM_SEED);
  //  for (UINT32 loop = 0; loop < 1; loop++) {
        wr_buf_addr = WR_BUF_ADDR;
        data = 0;
        //uart_printf("test loop cnt: %d", loop);
        uart_print("test!!!!!!!!!!!!!!!!!!!!!!!!");
        for (i = 0; i < io_cnt; i++) {
            do {
                lba = rand() % IO_LIMIT;
            }while(lba + num_sectors >= IO_LIMIT);

            wr_buf_addr = WR_BUF_PTR(g_ftl_write_buf_id) + ((lba % SECTORS_PER_PAGE) * BYTES_PER_SECTOR);
            r_data = data;

            for (j = 0; j < num_sectors; j++) {
                mem_set_dram(wr_buf_addr, data, BYTES_PER_SECTOR);

                wr_buf_addr += BYTES_PER_SECTOR;

                if (wr_buf_addr >= WR_BUF_ADDR + WR_BUF_BYTES) {
                    wr_buf_addr = WR_BUF_ADDR;
                }
                data++;
            }
/*             ptimer_start(); */
            uart_print("ftl_write_start");
            ftl_write(lba, num_sectors,60);
/*             ptimer_stop_and_uart_print(); */
            rd_buf_addr = RD_BUF_PTR(g_ftl_read_buf_id) + ((lba % SECTORS_PER_PAGE) * BYTES_PER_SECTOR);
/*             ptimer_start(); */
            ftl_read(lba, num_sectors);
/*             ptimer_stop_and_uart_print(); */

            flash_finish();

            for (j = 0; j < num_sectors; j++) {
                UINT32 sample = read_dram_32(rd_buf_addr);

                if (sample != r_data) {
                    uart_printf("ftl test fail...io#: %d, %d", lba, num_sectors);
                    uart_printf("sample data %d should be %d", sample, r_data);
                    led_blink();
                }
                rd_buf_addr += BYTES_PER_SECTOR;

                if (rd_buf_addr >= RD_BUF_ADDR + RD_BUF_BYTES) {
                    rd_buf_addr = RD_BUF_ADDR;
                }
                r_data++;
            }
        } // end for
   // }
    ftl_flush();
}
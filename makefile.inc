
VERILOGDIR:=build/hw/verilog

BSVBUILDDIR:=build/hw/intermediate

BSVOUTDIR:=bin

BSCCMD:=bsc -u -verilog -elab -vdir build/hw/verilog -bdir build/hw/intermediate -info-dir build/hw/intermediate +RTS -K4000M -RTS -check-assert  -keep-fires -opt-undetermined-vals -remove-false-rules -remove-empty-rules -remove-starved-rules -remove-dollar -unspecified-to X -show-schedule -show-module-use -cross-info 

BSC_DEFINES:=Addr_space=26 xlen=64 flen=64 elen=64 bypass_sources=2 debug debug_bus_sz=64 ASSERT rtldump static_check VERBOSITY=0 isb_s0s1=2 isb_s1s2=2 isb_s2s3=1 isb_s3s4=8 isb_s4s5=8 RV64 ibuswidth=64 dbuswidth=64 resetpc=4096 paddr=32 vaddr=64 CORE_AXI4 iesize=2 desize=1 num_harts=1 microtrap_support no_wawstalls wawid=4 simulate mhpm_eventcount=32 atomic reservation_sz=8 spfpu dpfpu compressed muldiv MULSTAGES_IN=1 MULSTAGES_OUT=1 MULSTAGES_TOTAL=2 DIVSTAGES=32 zicsr user supervisor itlbsize=4 dtlbsize=4 asidwidth=16 sv39 non_m_traps bpu gshare btbdepth=32 bhtdepth=512 histlen=8 histbits=5 rasdepth=8 bpu_ras iwords=4 iblocks=16 iways=4 isets=64 ifbsize=4 icache_onehot=0 icache ifence irepl=0 dwords=8 dblocks=8 dways=4 dsets=64 dfbsize=9 dsbsize=2 dlbsize=4 dibsize=2 dcache_1rw dcache_onehot=0 dcache drepl=1 csr_low_latency perfmonitors pmp pmpentries=4 pmp_grain=1 reset_cycles=512 max_int_cause=16 max_ex_cause=32 causesize=7 stage0_noinline stage1_noinline stage2_noinline stage3_noinline stage4_noinline stage5_noinline mbox_noinline mbox_mul_noinline mbox_div_noinline registerfile_noinline bpu_noinline riscv_noinline csrbox_noinline scoreboard_noinline bypass_noinline base_alu_noinline decoder_noinline decompress_noinline

BSVINCDIR:=.:%/Libraries:src/:csrbox_bsv/:src/predictors:src/mbox:src/fpu/:src/fpu/hardfloat:src/fpu/bsv_float:test_soc/:benchmarks/:fabrics/axi4:fabrics/axi4lite:caches_mmu/src/icache_1rw:caches_mmu/src/dcache:caches_mmu/src/tlbs:caches_mmu/src/pmp:devices/bootrom:devices/uart_v2:devices/clint:devices/bram:devices/riscvdebug100:devices/jtagdtm/:devices/err_slave:common_bsv/:common_verilog/bsvwrappers/

BS_VERILOG_LIB:=/opt/tools/bsc/latest/lib/Verilog/

TOP_MODULE:=mkTbSoc

TOP_DIR:=test_soc

TOP_FILE:=TbSoc.bsv

VERILATOR_FLAGS:= --no-timing -O3 -LDFLAGS "-static" --x-assign fast  --x-initial fast --noassert sim_main.cpp --bbox-sys -Wno-STMTDLY  -Wno-UNOPTFLAT -Wno-WIDTH -Wno-lint -Wno-COMBDLY -Wno-INITIALDLY  --autoflush   --threads 1 -DBSV_RESET_FIFO_HEAD  -DBSV_RESET_FIFO_ARRAY --output-split 20000  --output-split-ctrace 10000

VERILATOR_SPEED:= OPT_FAST="-O3" OPT_SLOW="-O3" OPT="-O3 "

SHAKTI_HOME:=/home/rishi/Desktop/ASSIGNMENTS/C-Class/c-class_repo

XLEN:=64

TOP_BIN=TbSoc.bo

include depends.mk

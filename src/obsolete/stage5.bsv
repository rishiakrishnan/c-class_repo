// See LICENSE.iitm for license details
/*

Author: IIT Madras
Details:

--------------------------------------------------------------------------------------------------
*/
package stage5;
  import TxRx::*;
  import GetPut::*;
  import ccore_types::*;
  `include "ccore_params.defines"
  `include "Logger.bsv"
  `include "trap.defines"
  import ConfigReg::*;

  import FIFO::*;
  import FIFOF::*;
  import GetPut :: * ;
  import Connectable :: * ;
  
  import csrbox   :: * ;
  import csr_types :: * ;
  import DReg::*;
  import Vector::*;
  import Clocks :: *;
  import dcache_types::*;


  interface Ifc_stage5;
    interface RXe#(PIPE4) rx_in;
    `ifdef rtldump
      interface RXe#(CommitLogPacket) rx_inst;
    `endif
    method Maybe#(CommitData) commit_rd;
  `ifdef supervisor
    method Tuple4#(Bool, Bit#(`vaddr), Bool, Bool) flush;
  `else
    method Tuple3#(Bool, Bit#(`vaddr), Bool) flush;
  `endif
    (*always_ready*)
    method CSRtoDecode mv_csrs_to_decode;
   	method Action ma_clint_msip(Bit#(1) intrpt);
    method Action ma_clint_mtip(Bit#(1) intrpt);
	  method Action ma_clint_mtime(Bit#(64) c_mtime);
    //This method returns value of csr_reg which enables/disables arith_exceptions
    `ifdef arith_trap
      method Bit#(1) mv_arith_excep;
   `endif
   `ifdef rtldump
     method Maybe#(CommitLogPacket) commitlog;
     interface Sbread sbread;
   `endif
		`ifdef supervisor
			method Bit#(XLEN) mv_csr_satp;
		`endif
  	method Action ma_set_meip(Bit#(1) ex_i);
  `ifdef supervisor
  	method Action ma_set_seip(Bit#(1) ex_i);
  `endif
  `ifdef usertraps
  	method Action ma_set_ueip(Bit#(1) ex_i);
  `endif
    method Bit#(1) mv_csr_misa_c;
  `ifdef dcache
    method Bit#(1) mv_initiate_store;
    method Bit#(1) mv_initiate_ioop;
  `endif
    method Action ma_io_response(Maybe#(DMem_core_response#(TMul#(`dwords,8),`desize)) r);
    (*always_enabled*)
    method Bit#(3) mv_cacheenable;
    method Bit#(2) mv_curr_priv;
    method Bit#(XLEN) mv_csr_mstatus;
		/*doc:method: This method indicates if the hart should resume from a WFI*/
		method Bool mv_resume_wfi ();
  `ifdef pmp
    method Vector#(`pmpentries, Bit#(8)) mv_pmp_cfg;
    method Vector#(`pmpentries, Bit#(`paddr)) mv_pmp_addr;
  `endif
  `ifdef debug
    method Bit#(64) mv_csr_dcsr;
    method Action ma_debug_interrupt (Bit#(1) _int);
    method Bit#(1) mv_debug_mode;
    method Bit#(1) mv_core_debugenable;
    method Bit#(1) mv_stop_timer;
    method Bit#(1) mv_stop_count;
  `endif
  `ifdef triggers
    method Vector#(`trigger_num, TriggerData) trigger_data1;
    method Vector#(`trigger_num, Bit#(XLEN)) trigger_data2;
    method Vector#(`trigger_num, Bool) trigger_enable;
  `endif
  `ifdef perfmonitors
      method Action ma_events (Bit#(`mhpm_eventcount) e);
 		  /*doc:method: */
   		method Bit#(1) mv_count_exceptions;
   		method Bit#(1) mv_count_interrupts;
   		method Bit#(1) mv_count_csrops;
 	`endif
	`ifdef dtim
	  /*doc:method: */
	  method Bit#(XLEN) mv_csr_dtim_base ();
    /*doc:method: */
    method Bit#(XLEN) mv_csr_dtim_bound ();
  `endif
  `ifdef itim
    /*doc:method: */
    method Bit#(XLEN) mv_csr_itim_base ();
    /*doc:method: */
    method Bit#(XLEN) mv_csr_itim_bound ();
  `endif
  endinterface

  (*synthesize*)
  module mkstage5#(parameter Bit#(XLEN) hartid) (Ifc_stage5);

    let curr_reset <- exposeCurrentReset;
    RX#(PIPE4) rx<-mkRX;
  `ifdef rtldump
    RX#(CommitLogPacket) rxinst <-mkRX;
  `endif
    Ifc_csrbox csr <- mk_csrbox();

    // wire that carries the commit data that needs to be written to the integer register file.
    Wire#(Maybe#(CommitData)) wr_commit <- mkDWire(tagged Invalid);
    Wire#(Bool) wr_increment_minstret <- mkDWire(False);

    // wire which signals the entire pipe to be flushed.
  `ifdef supervisor
    Wire#(Tuple4#(Bool, Bit#(`vaddr), Bool, Bool)) wr_flush <- mkDWire(tuple4(False, ?, False, False));
  `else
    Wire#(Tuple3#(Bool, Bit#(`vaddr), Bool)) wr_flush <- mkDWire(tuple3(False, ?, False));
  `endif

  `ifdef perfmonitors
    /*doc:wire: */
    Wire#(Bit#(1)) wr_count_exceptions <- mkDWire(0);
    Wire#(Bit#(1)) wr_count_interrupts <- mkDWire(0);
    Wire#(Bit#(1)) wr_count_csrops <- mkDWire(0);
  `endif
    // the local epoch register
    Reg#(Bit#(1)) rg_epoch <- mkReg(0);
    
    Reg#(Bool) rg_csr_wait <- mkReg(False);
  `ifdef rtldump
    Reg#(Maybe#(CommitLogPacket)) rg_commitlog <- mkDReg(tagged Invalid);
    let prv=csr.mv_prv;
  `endif

    Reg#(Bool) rg_ioop_init <- mkReg(False);
    Reg#(Bool) rg_atomic_readdone <- mkReg(False);
    Reg#(Bit#(ELEN)) rg_atomic_data <- mkReg(0);
    Wire#(Maybe#(DMem_core_response#(TMul#(`dwords,8),`desize))) wr_ioop_response <- mkDWire(tagged Invalid);
  `ifdef dcache
    Wire#(Bit#(1)) wr_commit_cacheop <- mkWire();
    Wire#(Bit#(1)) wr_commit_ioop <- mkWire();
  `endif    

  `ifdef debug
    mkConnection(csr.ma_stop_count, csr.mv_stop_count);
  `endif

    let csr_resp = csr.mv_core_resp;
  `ifdef triggers
    Reg#(TriggerStatus) rg_take_trigger <- mkConfigReg(unpack(0));
    Reg#(Bit#(`vaddr)) rg_flush_pc <- mkReg(0);

    rule check_triggers;
      let {commit, epoch} = rx.u.first;

      let trigger_data1 = csr.trigger_data1;
      let trigger_data2 = csr.trigger_data2;
      let trigger_enable = csr.trigger_enable;

      Bool trap = False;
      Bit#(`causesize) cause = `Breakpoint;
      Bool chain = False;

      Bool exception = False;
      Bool interrupt = False;
      Bit#(TSub#(`causesize,1)) code=?;

      if(commit matches tagged TRAP .t)begin
        code = truncate(t.cause);
        if(code != `Rerun && code != `IcacheFence && code != `SFence && t.cause[`causesize-1] == 0 )begin
          exception = True;
        end
        if( code <= `Machine_external_int  && t.cause[`causesize-1] == 1 )begin
          interrupt = True;
        end
      end

      for(Integer i=0; i<`trigger_num; i=i+1)begin
        if(trigger_enable[i] && !trap )begin
          if(trigger_data1[i] matches tagged ETRIGGER .et &&& exception)begin
            if( trigger_data2[i][code] == 1) begin
              trap = True;
            `ifdef debug
              if(et.action_ == 1)begin
                cause = `HaltTrigger;
                cause[`causesize - 1] = 1;
              end
            `endif
            end
          end
          else if(trigger_data1[i] matches tagged ITRIGGER .it &&& interrupt)begin
            if( trigger_data2[i][code] == 1) begin
              trap = True;
              if(it.action_ == 1)begin
                cause = `HaltTrigger;
                cause[`causesize - 1] = 1;
              end
            end
          end
        end
      end

      rg_take_trigger <= TriggerStatus{trap: trap, cause:cause};

    endrule
  `endif

    rule instruction_commit;
      let {commit, epoch}=rx.u.first;
    `ifdef rtldump
      let clogpkt =rxinst.u.first;
      clogpkt.mode = prv;
    `endif
      Bool fenceI=False;
    `ifdef supervisor
      Bool sFence=False;
    `endif
      Bit#(`vaddr) jump_address=?;
      Bool fl = False;
      `ifdef rtldump
        `logLevel( stage5, 0, $format("[%2d]STAGE5: PC: %h: inst: %h epoch:%b rg_epoch:%b commit: ",hartid,
                clogpkt.pc,clogpkt.instruction,epoch,rg_epoch,fshow(commit)))
      `endif
      if(rg_epoch==epoch)begin
      `ifdef triggers
        if(rg_take_trigger.trap)begin
          let newpc <- csr.mav_upd_on_trap(rg_take_trigger.cause, rg_flush_pc, ?);
          fl=True;
          jump_address=newpc;
          rx.u.deq;
        `ifdef rtldump
          rxinst.u.deq;
        `endif
          `logLevel( stage5, 0, $format("[%2d]STAGE5: Trigger TRAP:%d NewPC:%h fl:%b",hartid,
                                                         rg_take_trigger.cause,jump_address,fl))
        end
        else
      `endif
        if(commit matches tagged TRAP .t)begin
          if(t.cause==`Rerun || t.cause==`IcacheFence `ifdef supervisor || t.cause==`SFence `endif )begin
            `logLevel( stage5, 0, $format("[%2d]STAGE5: Rerun initiated",hartid))
            fl=True;
            jump_address=t.pc;
            fenceI=(t.cause==`IcacheFence );
            `ifdef supervisor
              sFence = (t.cause==`SFence);
            `endif
          end
          else begin
            let newpc <- csr.mav_upd_on_trap(t.cause, t.pc, t.badaddr);
            fl=True;
            jump_address=newpc;
          `ifdef perfmonitors
            if(truncateLSB(t.cause) == 1'b1)
              wr_count_interrupts <= 1;
            else
              wr_count_exceptions <= 1;
          `endif
          end
          rx.u.deq;
        `ifdef rtldump
          rxinst.u.deq;
        `endif
          `logLevel( stage5, 0, $format("[%2d]STAGE5: Received TRAP:%d NewPC:%h fl:%b",hartid,t.cause,jump_address,fl))
        end
        else if (commit matches tagged MEMOP .s)begin
        `ifdef rtldump
          CommitLogMem _pkt = ?;
          if (clogpkt.inst_type matches tagged MEM. cmem) 
            _pkt = cmem;
        `endif

          if (!s.io) begin // Cacheable Store or Atomic op
            wr_commit_cacheop <= rg_epoch;
            wr_increment_minstret <= True;
          `ifdef atomic
            let cache_resp = s.atomic_rd_data;
            if (s.memaccess == Atomic) begin
              wr_commit <= tagged Valid CommitData{addr: s.rd, data: cache_resp
                                              `ifdef spfpu , rdtype: IRF `endif } ;
            end
          `else
            Bit#(ELEN) cache_resp = 0;
          `endif
            rx.u.deq;
          `ifdef rtldump
            _pkt.commit_data = cache_resp;
            clogpkt.inst_type = tagged MEM _pkt;
            rg_commitlog <= tagged Valid (clogpkt);
            rxinst.u.deq;
          `endif
          end
          else if(s.io) begin // Non-Cacheable Memory op
            if (!rg_ioop_init) begin
              rg_ioop_init <= True;
              wr_commit_ioop <= rg_epoch;
            end
            else if(wr_ioop_response matches tagged Valid .ioresp)begin
              rg_ioop_init <= False;
              rg_atomic_readdone <= False;
              if (ioresp.trap) begin
                let newpc <- csr.mav_upd_on_trap(ioresp.cause, s.pc, ioresp.word);
                fl=True;
                jump_address=newpc;
              end
              else begin
                wr_increment_minstret <= True;
                let data = ioresp.word;
              `ifdef dpfpu 
                if (s.nanboxing) data[63:32] = '1;
              `endif
                  wr_commit <= tagged Valid CommitData{addr: s.rd, data: data
                                                  `ifdef spfpu , rdtype: s.rdtype `endif } ;
              `ifdef rtldump
                _pkt.commit_data = data;
                clogpkt.inst_type = tagged MEM _pkt;
                rg_commitlog <= tagged Valid (clogpkt);
              `endif
              end
              rx.u.deq;
            `ifdef rtldump
              rxinst.u.deq;
            `endif
            end
            else begin
              if (s.io)
                `logLevel( stage5, 0, $format("[%2d]STAGE5: Waiting for IO response",hartid))
              else
                `logLevel( stage5, 0, $format("[%2d]STAGE5: Waiting for Cache to be ready",hartid))
            end
          end
        end
        else if(commit matches tagged SYSTEM .sys)begin
          //let {drain, newpc}<-csr.system_instruction(sys.csraddr, sys.rs1, sys.func3, sys.lpc);
          wr_increment_minstret<=True;
          Bool drain = False;
          Bit#(`vaddr) newpc = ?;
          if (sys.func3 == 0) begin // URET, SRET, MRET
            let temp <- csr.mav_upd_on_ret( truncateLSB(sys.csraddr));
            newpc = temp;
            drain = True;
          end
          else if(!rg_csr_wait) begin
            csr.ma_core_req(CSRReq{csr_address: sys.csraddr, writedata: sys.rs1,
               funct3: truncate(sys.func3) `ifdef compressed , pc_1:sys.lpc[1] `endif });
            `logLevel( stage5, 0, $format("STAGE5: Sending req to CSRBOX"))
          end

          if ((sys.func3 != 0 && csr_resp.hit) || (sys.func3==0)) begin
            rg_csr_wait <= False;
            `logLevel( stage5, 0, $format("STAGE5: CSRBOX Responded"))
          end
          else begin
            rg_csr_wait <= True;
            `logLevel( stage5, 0, $format("STAGE5: Waiting for CSRBOX response"))
          end

          let dest= csr_resp.data;
          if(drain || csr_resp.hit) begin
            jump_address=newpc;
            fl=drain;
            wr_commit <= tagged Valid CommitData{addr: sys.rd, data: zeroExtend(dest)
                                      `ifdef spfpu, rdtype: IRF `endif };
            `ifdef rtldump
              CommitLogCSR _pkt = ?;
              if (clogpkt.inst_type matches tagged CSR .pcsr)
                _pkt = pcsr;
              if (sys.func3 == 0) begin
                _pkt.csr_address = 'h300;
                _pkt.wdata = csr.sbread.mv_csr_mstatus;
              end
              _pkt.rdata = dest;
              clogpkt.inst_type = tagged CSR _pkt;
              rg_commitlog <= tagged Valid (clogpkt);
              rxinst.u.deq;
            `endif
              rx.u.deq;
            `ifdef perfmonitors
              wr_count_csrops <= 1;
            `endif
          end
        end
        else if(commit matches tagged REG .r)begin
          // in case of regular instruction simply update RF and forward the data.
          `logLevel( stage5, 0, $format("[%2d]STAGE5: Regular commit",hartid))
          wr_increment_minstret<=True;
        `ifdef spfpu
          csr.ma_set_fflags(r.fflags);
        `endif
          wr_commit <= tagged Valid CommitData{addr:r.rd, data:r.commitvalue
                                      `ifdef spfpu , rdtype: r.rdtype `endif };
          rx.u.deq;
        `ifdef rtldump
          rxinst.u.deq;
          rg_commitlog <= tagged Valid (clogpkt);
        `endif
        end

        // if it is a branch/JAL_R instruction generate a flush signal to the pipe.
      `ifdef supervisor
        wr_flush<=tuple4(fl, jump_address, fenceI, sFence);
      `else
        wr_flush<=tuple3(fl, jump_address, fenceI);
      `endif
      `ifdef triggers
        rg_flush_pc <= jump_address;
      `endif
        if(fl)begin
          rg_epoch <= ~rg_epoch;
          `logLevel( stage5, 0, $format("STAGE5: Flush to PC:%h",jump_address))
        end
      end
      else begin
        `logLevel( stage5, 0, $format("[%2d]STAGE5: Dropping instruction",hartid))
         rx.u.deq;
        `ifdef rtldump
          rxinst.u.deq;
        `endif
        if(commit matches tagged MEMOP.s) begin
          if(!s.io)
            wr_commit_cacheop <= rg_epoch;
          else
            wr_commit_ioop <= rg_epoch;
        end
      end
    endrule

    rule increment_instruction_counter(wr_increment_minstret);
      csr.ma_incr_minstret;
    endrule
    interface rx_in=rx.e;
  `ifdef rtldump
    interface rx_inst=rxinst.e;
  `endif
    method Maybe#(CommitData) commit_rd();
      return wr_commit;
    endmethod
    method flush=wr_flush;
    method mv_csrs_to_decode = CSRtoDecode{prv: csr.mv_prv, 
      csr_mip: truncate(csr.sbread.mv_csr_mip), 
      csr_mie: truncate(csr.sbread.mv_csr_mie), 
      csr_mstatus: truncate(csr.sbread.mv_csr_mstatus) `ifdef debug & {'1,csr.mv_debug_mode,17'd0} `endif , 
      csr_misa: truncate(csr.sbread.mv_csr_misa), frm: truncate(csr.sbread.mv_csr_frm)
    `ifdef debug
      ,csr_dcsr: truncate(csr.sbread.mv_csr_dcsr)
    `endif
    `ifdef non_m_traps 
      ,csr_mideleg: truncate(csr.sbread.mv_csr_mideleg)
    `endif };
      
	  method ma_clint_msip = csr.ma_set_mip_msip;
		method ma_clint_mtip = csr.ma_set_mip_mtip;
		method ma_clint_mtime = csr.ma_set_time;
		method mv_resume_wfi = unpack( |((csr.sbread.mv_csr_mip)& (csr.sbread.mv_csr_mie) ));
    `ifdef rtldump
      method commitlog = rg_commitlog;
      interface sbread = csr.sbread;
    `endif
		`ifdef supervisor
			method mv_csr_satp=csr.sbread.mv_csr_satp;
		`endif
  	method ma_set_meip = csr.ma_set_mip_meip;
  `ifdef supervisor
  	method ma_set_seip = csr.ma_set_mip_seip;
  `endif
  `ifdef usertraps
  	method ma_set_ueip = csr.ma_set_ueip;
  `endif
    method mv_csr_misa_c=csr.sbread.mv_csr_misa[2];
  `ifdef dcache
    method mv_initiate_store=wr_commit_cacheop;
    method mv_initiate_ioop = wr_commit_ioop;
  `endif
    method Action ma_io_response(Maybe#(DMem_core_response#(TMul#(`dwords,8),`desize)) r);
      wr_ioop_response<=r;
    endmethod
    method mv_cacheenable = truncate(csr.sbread.mv_csr_customcontrol);
    method mv_curr_priv = pack(csr.mv_prv);    
    method mv_csr_mstatus= csr.sbread.mv_csr_mstatus;
  `ifdef pmp
    method mv_pmp_cfg = csr.mv_pmpcfg;
    method mv_pmp_addr=csr.mv_pmpaddr;
  `endif
  `ifdef debug
    method mv_csr_dcsr = csr.sbread.mv_csr_dcsr;
    method ma_debug_interrupt= csr.ma_set_mip_debug_interrupt;
    method mv_debug_mode= csr.mv_debug_mode;
    method mv_core_debugenable = csr.sbread.mv_csr_customcontrol[4];
    method mv_stop_timer = csr.mv_stop_timer;
    method mv_stop_count = csr.mv_stop_count;
  `endif

    `ifdef arith_trap
      method mv_arith_excep = csr.mv_arith_excep;
  `endif
  `ifdef triggers
    method trigger_data1 = csr.trigger_data1;
    method trigger_data2 = csr.trigger_data2;
    method trigger_enable = csr.trigger_enable;
  `endif
  `ifdef perfmonitors
    method ma_events = csr.ma_events;
   	method mv_count_exceptions = wr_count_exceptions;
   	method mv_count_interrupts = wr_count_interrupts;
   	method mv_count_csrops = wr_count_csrops;
	`endif
	`ifdef dtim
	  /*doc:method: */
	  method  mv_csr_dtim_base = csr.sbread.mv_csr_dtim_base;
    /*doc:method: */
    method  mv_csr_dtim_bound  = csr.sbread.mv_csr_dtim_bound;
  `endif
  `ifdef itim
    /*doc:method: */
    method mv_csr_itim_base  = csr.sbread.mv_csr_itim_base;
    /*doc:method: */
    method mv_csr_itim_bound = csr.sbread.mv_csr_itim_bound;
  `endif
  endmodule
endpackage

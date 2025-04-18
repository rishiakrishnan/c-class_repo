// See LICENSE.iitm for license details
/*

Author  : IIT Madras
Details : This module contains the methods and functions which will perform tasks related to CSRs:
Trap handling and Updating the CSRs for system - instruction

--------------------------------------------------------------------------------------------------
*/

package csr;
  // project related imports
  import ccore_types::*;
  `include "ccore_params.defines"
  import ConcatReg::*;
//  import csrfile::*;
  import csr_daisy :: * ;
  import Vector::*;
  import DReg :: * ;
  `include "Logger.bsv"

`ifdef debug
  import debug_types::*;
`endif

  // package imports
  import ConfigReg::*;

  interface Ifc_csr;
	  method ActionValue#(Tuple2#(Bool, Bit#(`vaddr))) system_instruction(
            Bit#(12) csr_address, Bit#(XLEN) op1, Bit#(3) funct3, Bit#(2) lpc);
  	//(*doc = "method : response from the CSRs forwarded to the core"*)
  	method CSRResponse mv_resp_to_core;
    method CSRtoDecode mv_csrs_to_decode;
		/*doc:method: This method indicates if the hart should resume from a WFI*/
		method Bool mv_resume_wfi ();
    method ActionValue#(Bit#(`vaddr)) take_trap(Bit#(`causesize) type_cause, Bit#(`vaddr) pc, Bit#(`vaddr) badaddr);
	  method Action ma_clint_msip(Bit#(1) intrpt);
		method Action ma_clint_mtip(Bit#(1) intrpt);
		method Action ma_clint_mtime(Bit#(64) c_mtime);
    method Action ma_incr_minstret;
		`ifdef supervisor
			method Bit#(XLEN) mv_csr_satp;
		`endif
    `ifdef spfpu
      method Action ma_update_fflags(Bit#(5) flags);
    `endif
  	method Action ma_set_meip(Bit#(1) ex_i);
  `ifdef supervisor
  	method Action ma_set_seip(Bit#(1) ex_i);
  `endif
  `ifdef usertraps
  	method Action ma_set_ueip(Bit#(1) ex_i);
  `endif
    method Bit#(1) mv_csr_misa_c;
    method Bit#(3) mv_cacheenable;
  `ifdef arith_trap
   //This method returns value of csr_reg which enables or disables arithmetic exceptions
    method Bit#(1) mv_arith_excep;
  `endif
    method Bit#(2) mv_curr_priv;
    method Bit#(XLEN) mv_csr_mstatus;
  `ifdef pmp
    method Vector#(`pmpentries, Bit#(8)) mv_pmp_cfg;
    method Vector#(`pmpentries, Bit#(TSub#(`paddr, `pmp_grainbits) )) mv_pmp_addr;
  `endif

  `ifdef debug
    method Action ma_debug_access_csrs(AbstractRegOp cmd);
    method Action ma_debug_halt_request(Bit#(1) ip);
    method Action ma_debug_resume_request(Bit#(1) ip);
    method Bit#(1) mv_core_is_halted;
    method Bit#(1) mv_step_is_set;
    method Bit#(1) mv_step_ie;
    method Bit#(1) mv_core_debugenable;
  `endif
  `ifdef triggers
    method Vector#(`trigger_num, TriggerData) trigger_data1;
    method Vector#(`trigger_num, Bit#(XLEN)) trigger_data2;
    method Vector#(`trigger_num, Bool) trigger_enable;
  `endif
  `ifdef perfmonitors
    `ifdef csr_grp4
    	//(*doc = "method : whenever event corresponding to the group-4 occur, the method is to be \
   		//          called, so that corresponding counters are incremented"*)
   		method Action ma_events_grp4(Bit#(SizeOf#(Events_grp4)) e);
    `endif

    `ifdef csr_grp5
    	//(*doc = "method : whenever event corresponding to the group-5 occur, the method is to be \
   		//          called, so that corresponding counters are incremented"*)
 			method Action ma_events_grp5(Bit#(SizeOf#(Events_grp5)) e);
 		`endif

 		`ifdef csr_grp6
 			//(*doc = "method : whenever event corresponding to the group-6 occur, the method is to be \
   		//          called, so that corresponding counters are incremented"*)
 			method Action ma_events_grp6(Bit#(SizeOf#(Events_grp6)) e);
 		`endif

 		`ifdef csr_grp7
 			//(*doc = "method : whenever event corresponding to the group-7 occur, the method is to be \
   		//          called, so that corresponding counters are incremented"*)
 			method Action ma_events_grp7(Bit#(SizeOf#(Events_grp7)) e);
 		`endif
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
  endinterface : Ifc_csr


  (*synthesize*)
  (*mutually_exclusive="system_instruction, take_trap"*)
  (*mutually_exclusive="system_instruction,mv_resp_to_core"*)
  (*mutually_exclusive="take_trap,mv_resp_to_core"*)
`ifdef debug
  (*preempts="ma_debug_access_csrs, take_trap"*)
  (*preempts="ma_debug_access_csrs,system_instruction"*)
`endif
  module mkcsr#(parameter Bit#(XLEN) hartid) (Ifc_csr);

    //Ifc_csrfile csrfile <- mkcsrfile(hartid);
    Ifc_csr_daisy csrfile <- mk_csr_daisy;
    Reg#(Bool) rg_csr_wait <- mkDReg(False);
	  method ActionValue#(Tuple2#(Bool, Bit#(`vaddr))) system_instruction(
         Bit#(12) csr_address, Bit#(XLEN) op1, Bit#(3) funct3, Bit#(2) lpc);
      Bool flush = False;
      Bit#(`vaddr) jump_add = ?;
      Bit#(XLEN) destination_value = ?;
	  	case(funct3)
        'd0 : case (csr_address[11 : 8])
              'h0, `ifdef supervisor 'h1, `endif 'h3 : begin // URET, SRET, MRET
                let temp <- csrfile.mav_upd_on_ret( `ifdef non_m_traps unpack(csr_address[9 : 8]) `endif );
                jump_add = temp;
                flush = True;
                `logLevel( csr, 1, $format("[%2d]CSR : RET Function: %h",hartid,csr_address))
              end
	  		    endcase
        default : begin
          if(!rg_csr_wait) begin
            rg_csr_wait <= True;
            csrfile.ma_core_req(CSRReq{csr_address: csr_address, writedata: op1,
                                      funct3: truncate(funct3) `ifdef compressed ,lpc:lpc `endif });
          end
          else if(csrfile.mv_resp_to_core.hit)
            rg_csr_wait <= False;
          else
            rg_csr_wait <= True;
        end
      endcase
	  	return tuple2(flush, jump_add);
	  endmethod

    method ActionValue#(Bit#(`vaddr)) take_trap(Bit#(`causesize) type_cause, Bit#(`vaddr) pc, Bit#(`vaddr) badaddr);
      let jump_address <- csrfile.mav_upd_on_trap(type_cause, pc, badaddr);
		  return jump_address;
  	endmethod

  	method mv_resp_to_core = csrfile.mv_resp_to_core;

    method mv_csrs_to_decode = csrfile.mv_csrs_to_decode;
	  method ma_clint_msip = csrfile.ma_clint_msip;
	  method ma_clint_mtip = csrfile.ma_clint_mtip;
	  method ma_clint_mtime = csrfile.ma_clint_mtime;
    method ma_incr_minstret = csrfile.ma_incr_minstret;
    method mv_resume_wfi = csrfile.mv_resume_wfi;
		`ifdef supervisor
			method mv_csr_satp = csrfile.mv_csr_satp;
		`endif
    `ifdef spfpu
      method ma_update_fflags = csrfile.ma_update_fflags;
    `endif
  	method ma_set_meip = csrfile.ma_set_meip;
  `ifdef supervisor
  	method ma_set_seip = csrfile.ma_set_seip;
  `endif
  `ifdef usertraps
  	method ma_set_ueip = csrfile.ma_set_ueip;
  `endif
    method mv_csr_misa_c = csrfile.mv_csr_misa_c;
    method mv_cacheenable = csrfile.mv_cacheenable;

  `ifdef arith_trap
    method mv_arith_excep = csrfile.mv_arith_excep;
  `endif
    method mv_curr_priv = csrfile.mv_curr_priv;
    method mv_csr_mstatus = csrfile.mv_csr_mstatus;
  `ifdef pmp
    method mv_pmp_cfg = csrfile.mv_pmp_cfg;
    method mv_pmp_addr = csrfile.mv_pmp_addr;
  `endif
  `ifdef debug
    method Action ma_debug_access_csrs(AbstractRegOp cmd);
      Bit#(2) funct3 = 'b01; // write op
      Bit#(ELEN) writedata = cmd.writedata;
      if(!cmd.read_write) begin // read op
        funct3 = 'b10;
        writedata = 0;
      end
      csrfile.ma_core_req(CSRReq{csr_address: truncate(cmd.address), writedata: writedata,
                                      funct3: funct3 `ifdef compressed ,lpc:0 `endif });
    endmethod
    method ma_debug_halt_request = csrfile.ma_debug_halt_request;
    method ma_debug_resume_request = csrfile.ma_debug_resume_request ;
    method mv_core_is_halted = csrfile.mv_core_is_halted ;
    method mv_step_is_set = csrfile.mv_step_is_set;
    method mv_step_ie = csrfile.mv_step_ie;
    method mv_core_debugenable = csrfile.mv_core_debugenable;
  `endif
  `ifdef triggers
    method trigger_data1 = csrfile.trigger_data1;
    method trigger_data2 = csrfile.trigger_data2;
    method trigger_enable = csrfile.trigger_enable;
  `endif
  `ifdef perfmonitors
  	`ifdef csr_grp4
 			method ma_events_grp4 = csrfile.ma_events_grp4;
 		`endif

 		`ifdef csr_grp5
 			method ma_events_grp5 = csrfile.ma_events_grp5;
 		`endif

 		`ifdef csr_grp6
 			method ma_events_grp6 = csrfile.ma_events_grp6;
 		`endif

 		`ifdef csr_grp7
 			method ma_events_grp7 = csrfile.ma_events_grp7;
 		`endif
	`endif
	`ifdef dtim
	  /*doc:method: */
	  method  mv_csr_dtim_base = csrfile.mv_csr_dtim_base;
    /*doc:method: */
    method  mv_csr_dtim_bound  = csrfile.mv_csr_dtim_bound;
  `endif
  `ifdef itim
    /*doc:method: */
    method mv_csr_itim_base  = csrfile.mv_csr_itim_base;
    /*doc:method: */
    method mv_csr_itim_bound = csrfile.mv_csr_itim_bound;
  `endif
  endmodule
endpackage

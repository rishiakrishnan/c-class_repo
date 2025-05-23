// See LICENSE.iitm for license details
/*

Author : IIT Madras
Details:

--------------------------------------------------------------------------------------------------
 */

package csr_grp2;

	//standard library imports
	import Vector :: *;
	import FIFOF :: * ;
	import DReg :: *;
  import UniqueWrappers :: * ;
	import ConcatReg :: *;

  //project imports
  `include "csrgrp.defines"
  `include "ccore_params.defines"
  `include "Logger.bsv"
	import ccore_types :: * ;

	//typedef struct{
  	//Bit#(12) csr_address;
  	//Bit#(XLEN) writedata;
    //Bit#(3) funct3;
	//} CSRReq deriving(Bits, FShow, Eq);

	interface Ifc_csr_grp2;

		///*doc = "method : to receive the request from the core or previous node" */
    method Action ma_core_req(CSRReq req);

    ///*doc = "method : to send response to core on a hit in this node" */
    method CSRResponse mv_core_resp;

    ///*doc = "method : to forward the request to the next node on a miss in current node" */
   	method ActionValue#(CSRReq) mav_fwd_req;

   	//intergroup sideband connections
   	(*always_ready, always_enabled*)
    ///*doc = "method: sideband connection to grp1 to send misa value from here"*/
    method Bit#(XLEN) mv_csr_misa;

    //sideband connections from core
  	///*doc = "method : returns the last 3-bits of rg_customcontrol"*/
  	method Bit#(3) mv_cacheenable;

  `ifdef arith_trap
  	///*doc = "method : returns the last 4th bit of rg_customcontrol"*/
    method Bit#(1) mv_arith_excep;
  `endif
	  ///*doc = "method : returns the value of misa_c in the MISA register"*/
	  method Bit#(1) mv_csr_misa_c;

	`ifdef pmp
		//(*doc = "method : returns the vector of values stored in PMPCFG registers in grp-2"*)
    method Vector#(`pmpentries, Bit#(8)) mv_pmp_cfg; //tested
    //(*doc = "method : returns the vector of values stored in PMPADDR registers in grp-2"*)
    method Vector#(`pmpentries, Bit#(TSub#(`paddr, `pmp_grainbits ))) mv_pmp_addr; //tested
  `endif

  endinterface

  (*noinline*)
  ///*doc = "func : returns the value to be written onto the registers involved the CSR-operations"*/
	function Bit#(XLEN) fn_csr_op (Bit#(XLEN) writedata, Bit#(XLEN) readdata, Bit#(2) op);
		if(op == 'd1)
    	return writedata;
    else if(op == 'd2)
      return (writedata|readdata);
    else
      return (~writedata & readdata);
  endfunction

  ///*doc = "func : returns and interface of a register, however with read-only functionality"*/
  function Reg#(t) readOnlyReg(t r);
    return (interface Reg;
       method t _read = r;
       method Action _write(t x) = noAction;
    endinterface);
  endfunction

  (*synthesize*)
	/*doc = "module : implementing read and write methods for group - 2 csrs and related side band \
	         access"*/
  module mk_csr_grp2(Ifc_csr_grp2);
  // common registers
  `ifdef csr_low_latency
  	/* doc = "wire : holds the response of this group for a csr operation request, \
  					  for one cycle, wire is used for low latency"*/
    Wire#(CSRResponse) rg_resp_to_core <- mkDWire(CSRResponse{hit:False, data:0});
	`else
		/* doc = "reg : register to hold the response of this group for a csr operation request"*/
		Reg#(CSRResponse) rg_resp_to_core <- mkDReg(CSRResponse{hit:False, data:0});
	`endif

    /*doc = "fifo : fifo to forward the core - req to the next group on a miss in the \
             current group"*/
    FIFOF#(CSRReq) ff_fwd_request <- mkLFIFOF();

    ///*doc = note: mkUniqueWrapper is used to avoid multiple instantiation of same function"*/
    let csr_op <- mkUniqueWrapper3(fn_csr_op);
		//////////////////////////////////////machine type registers////////////////////////////////////
		//MISA fields
    //Reg#(Bit#(2)) rg_mxl <- mkReg(fromInteger(valueOf(TDiv#(XLEN, 32))));
    Bit#(2) mxl = fromInteger(valueOf(TDiv#(XLEN, 32)));
  `ifdef atomic
  	/*doc = "reg : a-Atomic, indicates wether atomic extension is supported or not"*/
    Reg#(Bit#(1)) rg_misa_a <- mkReg(1);
  `else
    Bit#(1) rg_misa_a = 0;
  `endif
  `ifdef compressed
	  /*doc = "reg : c-Compressed, indicates wether compressed extension is supported or not"*/
    Reg#(Bit#(1)) rg_misa_c <- mkReg(1);
  `else
    Bit#(1) rg_misa_c = 0;
  `endif
  `ifdef dpfpu
  	/*doc = "reg : d-Double, indicates wether double precision FP-operations are supported or not"*/
    Reg#(Bit#(1)) rg_misa_d <- mkReg(1);
  `else
    Bit#(1) rg_misa_d = 0;
  `endif
  `ifdef spfpu
  	/*doc = "reg : F-float, indicates wether single precision FP-operations are supported or not"*/
    Reg#(Bit#(1)) rg_misa_f <- mkReg(1);
  `else
    Bit#(1) rg_misa_f = 0;
  `endif
 	 	/*doc = "reg : i - base ISA, indicates RV32I/RV64I/RV128I base ISA is supported or not"*/
    Reg#(Bit#(1)) rg_misa_i <- mkReg(1);
  `ifdef muldiv
  	/*doc = "reg : m -Integer Multiply/Divide, indicates support for integer Multiply/Divide \
  	         operations"*/
    Reg#(Bit#(1)) rg_misa_m <- mkReg(1);
  `else
    Bit#(1) rg_misa_m = 0;
  `endif
  `ifdef usertraps
  	/*doc = "reg : n - User level interrupt supported or not"*/
    Reg#(Bit#(1)) rg_misa_n <- mkReg(1);
  `else
    Bit#(1) rg_misa_n = 0;
  `endif
  `ifdef supervisor
  	/*doc = "reg : s - Supervisor mode implemented/not"*/
    Reg#(Bit#(1)) rg_misa_s <- mkReg(1);
  `else
  	`ifdef rtldump
    	Bit#(1) rg_misa_s = 1;
  	`else
    	Bit#(1) rg_misa_s = 0;
  	`endif
  `endif
  `ifdef user
  	/*doc = "u - User mode implemented/not"*/
    Reg#(Bit#(1)) rg_misa_u <- mkReg(1);
  `else
    Bit#(1) rg_misa_u = 0;
  `endif
  	///*doc = "note : misa - Machine ISA support"*/
   	Bit#(26) misa = {5'd0, rg_misa_u, 1'd0, rg_misa_s, 4'd0, rg_misa_n, rg_misa_m, 3'd0, rg_misa_i,
   									 1'd0, /*rg_misa_i & rg_misa_m & rg_misa_a & rg_misa_f & rg_misa_d*/ 1'b0,
                     rg_misa_f, 1'd0, rg_misa_d, rg_misa_c, 1'd0, rg_misa_a};

    //MSCRATCH
    /* doc = "reg : Machine Scratch register, available for usage in M-mode only" */
    Reg#(Bit#(XLEN)) rg_mscratch <- mkReg(0);

    //=============================PHYSICAL MEMORY PROTECTION=======================================
    `ifdef pmp
    	///*doc = " reg : Vector to hold all the PMPCFG - Physical Memory Protection ConFiGuration \
    	//          registers"*/
      Vector#(`pmpentries, Reg#(Bit#(8))) v_pmp_cfg <- replicateM(mkReg(0));
      ///*doc = " reg : Vector to hold all the PMPADDR - Physical Memory Protection ADDRess \
    	//          registers"*/
      Vector#(`pmpentries, Reg#(Bit#(XLEN))) v_pmp_addr <- replicateM(mkReg(0));
    `ifdef RV64
      Bit#(XLEN) lv_csr_pmpcfg0 = 0;
      Bit#(XLEN) lv_csr_pmpcfg2 = 0;
      for(Integer i = 0;i<`pmpentries ;i = i+1)begin
        if(i<8)
          lv_csr_pmpcfg0[i * 8+7 : i*8] = v_pmp_cfg[i];
        else
          lv_csr_pmpcfg2[(i - 8) * 8+7 : (i - 8) * 8] = v_pmp_cfg[i];
      end

   `elsif RV32
      Bit#(XLEN) lv_csr_pmpcfg0 = 0;
      Bit#(XLEN) lv_csr_pmpcfg1 = 0;
      Bit#(XLEN) lv_csr_pmpcfg2 = 0;
      Bit#(XLEN) lv_csr_pmpcfg3 = 0;
      for(Integer i = 0;i<`pmpentries ;i = i+1)begin
        if(i<4)
          lv_csr_pmpcfg0[i * 8+7 : i*8] = v_pmp_cfg[i];
        else if(i<8)
          lv_csr_pmpcfg1[(i - 4) * 8+7 : (i - 4) * 8] = v_pmp_cfg[i];
        else if(i<12)
          lv_csr_pmpcfg2[(i - 8) * 8+7 : (i - 8) * 8] = v_pmp_cfg[i];
        else
          lv_csr_pmpcfg3[(i - 12) * 8+7 : (i - 12) * 8] = v_pmp_cfg[i]; //?
      end
    `endif
    `endif
    //==============================================================================================
		////////////////////////////////////////////////////////////////////////////////////////////////
	 	///////////////////////////////////supervisor type registers////////////////////////////////////
	 	//SSCRATCH
	`ifdef supervisor
		/*doc = "reg : Supervisor Scratch register, available for usage in s-mode only" */
	 	Reg#(Bit#(XLEN)) rg_sscratch <- mkReg(0);
	`endif
		////////////////////////////////////////////////////////////////////////////////////////////////
	 	/////////////////////////////////////user type registers////////////////////////////////////////
		//USCRATCH
	`ifdef usertraps
		/*doc = "reg : User Scratch register, available for usage in u-mode only" */
		Reg#(Bit#(XLEN)) rg_uscratch <- mkReg(0);
	`endif

	 	////////////////////////////////////////////////////////////////////////////////////////////////
	 	///////////////////////////// None Standard User RW CSRs /////////////////////////////////
    // Address : 'h800
  `ifdef icache
    // 0 - bit is cache enable for instruction cache
    /*doc = "reg : bit for cache-enable of instruction cache, part of rg_customcontrol"*/
    Reg#(Bit#(1)) rg_ienable <- mkReg(fromInteger(valueOf(`icachereset)));
  `else
  	/*doc = "reg : bit for cache-enable of instruction cache, part of rg_customcontrol"*/
    Reg#(Bit#(1)) rg_ienable = readOnlyReg(0);
  `endif

  `ifdef dcache
    // 1 - bit is cache enable for data cache
    /*doc = "reg : bit for cache-enable of data cache, part of rg_customcontrol"*/
    Reg#(Bit#(1)) rg_denable <- mkReg(fromInteger(valueOf(`dcachereset)));
  `else
    /*doc = "reg : bit for cache-enable of data cache, part of rg_customcontrol"*/
    Reg#(Bit#(1)) rg_denable = readOnlyReg(0);
  `endif
  `ifdef bpu
    // 2 - bit is branch predictor enable
    /*doc = "reg : bit for enabling branch predictor unit, part of rg_customcontrol"*/
    Reg#(Bit#(1)) rg_bpuenable <- mkReg(fromInteger(valueOf(`bpureset)));
  `else
  	/*doc = "reg : bit for enabling branch predictor unit, part of rg_customcontrol"*/
    Reg#(Bit#(1)) rg_bpuenable = readOnlyReg(0);
  `endif

  `ifdef arith_trap
    // 3 - bit if to enable traps on arithmetic ops
    /*doc = "reg : bit for enabling arithmetic exceptions, part of rg_customcontrol"*/
    Reg#(Bit#(1)) rg_arith_excep <-mkReg(0);
  `else
  	/*doc = "reg : bit for enabling arithmetic exceptions, part of rg_customcontrol"*/
    Reg#(Bit#(1)) rg_arith_excep = readOnlyReg(0);
  `endif
		//*doc = "reg : the register holds enable bits for arithmetic exceptions, \
		//         branch predictor unit, i-cache, d-cache units"*/
    Reg#(Bit#(4)) rg_customcontrol = concatReg4(rg_arith_excep, rg_bpuenable, rg_denable, rg_ienable);
	  //////////////////////////////////////////////////////////////////////////////////////////
	 	method Action ma_core_req(CSRReq req);

			Bit#(2) op = req.funct3;

 			case (req.csr_address)
 				`MISA : begin
 					//read previous value

					Bit#(XLEN) readdata = 0;
					readdata[25 : 0] = {5'd0, rg_misa_u, 1'd0, rg_misa_s, 4'd0, rg_misa_n, rg_misa_m, 3'd0,
														  rg_misa_i, 2'd0,
														  /*rg_misa_i & rg_misa_m & rg_misa_a & rg_misa_f & rg_misa_d,*/
														  rg_misa_f, 1'd0, rg_misa_d, rg_misa_c, 1'd0, rg_misa_a};

 				`ifdef RV64
          readdata[63 : 62] = mxl;
        `else
          readdata[31 : 30] = mxl;
        `endif
         	rg_resp_to_core <= CSRResponse{ hit : True, data : readdata};
 					//form the new value to be written and write
 					let word <- csr_op.func(req.writedata,readdata,op);

 				`ifdef atomic
 					rg_misa_a <= word[0];
 				`endif
        `ifdef compressed
          if(word[2] == 1 || (word[2] == 0 && req.lpc == 0)) begin
          	rg_misa_c <= word[2];
          end
        `endif
        `ifdef dpfpu
        	rg_misa_d <= word[3];
        `endif
        `ifdef spfpu
        	rg_misa_f <= word[5];
        `endif
          rg_misa_i <= word[8];
        `ifdef muldiv
        	rg_misa_m <= word[12];
        `endif
        `ifdef usertraps
        	rg_misa_n <= word[13];
        `endif
        `ifdef supervisor
        	rg_misa_s <= word[18];
        `endif
        `ifdef user
        	rg_misa_u <= word[20];
        `endif
 				end

				`MSCRATCH : begin
					//read previous value
 					rg_resp_to_core <= CSRResponse{ hit : True, data : rg_mscratch};
 					Bit#(XLEN) readdata = rg_mscratch;
 					//form the new value to be written and write
 					let word <- csr_op.func(req.writedata,readdata,op);
 					rg_mscratch <= word;
				end
    `ifdef pmp
			`ifdef RV64
			  `PMPCFG0: begin
          Bit#(XLEN) readdata = lv_csr_pmpcfg0;
 					rg_resp_to_core <= CSRResponse{ hit : True, data : lv_csr_pmpcfg0};
 					let word <- csr_op.func(req.writedata, readdata, op);
 					Vector#(8, Bit#(8)) _temp;
 					Vector#(8, Bool) _valid = replicate(False);
 					for (Integer i = 0; i< 8; i = i + 1) begin
 					  _temp[i] = word[i*8 + 7: i*8 ];
 					  if(_temp[i][1:0] != 'b10 && (`pmp_grainbits > 2 && _temp[i][4:3] != 2))
 					    _valid[i] = True;
 					end
 					for (Integer i = 0; i<8 && i < `pmpentries ; i = i + 1) begin
          	if(v_pmp_cfg[i][7] == 0 && _valid[i])
            	v_pmp_cfg[i] <= _temp[i];
 					end
			  end
			  `PMPCFG2: begin
          Bit#(XLEN) readdata = lv_csr_pmpcfg2;
 					rg_resp_to_core <= CSRResponse{ hit : True, data : lv_csr_pmpcfg2};
 					let word <- csr_op.func(req.writedata, readdata, op);
 					Vector#(8, Bit#(8)) _temp;
 					Vector#(8, Bool) _valid = replicate(False);
 					for (Integer i = 0; i< 8; i = i + 1) begin
 					  _temp[i] = word[i*8 + 7: i*8 ];
 					  if(_temp[i][1:0] != 'b10 && (`pmp_grainbits > 2 && _temp[i][4:3] != 2))
 					    _valid[i] = True;
 					end
 					for (Integer i = 8; i<16 && i < `pmpentries ; i = i + 1) begin
          	if(v_pmp_cfg[i][7] == 0 && _valid[i-8]) 
            	v_pmp_cfg[i] <= _temp[i-8];
 					end
			  end
			`endif

			`ifdef RV32
			  `PMPCFG0: begin
          Bit#(XLEN) readdata = lv_csr_pmpcfg0;
 					rg_resp_to_core <= CSRResponse{ hit : True, data : lv_csr_pmpcfg0};
 					let word <- csr_op.func(req.writedata, readdata, op);
 					Vector#(4, Bit#(8)) _temp;
 					Vector#(4, Bool) _valid = replicate(False);
 					for (Integer i = 0; i< 4; i = i + 1) begin
 					  _temp[i] = word[i*8 + 7: i*8 ];
 					  if(_temp[i][1:0] != 'b10 && (`pmp_grainbits > 2 && _temp[i][4:3] != 2))
 					    _valid[i] = True;
 					end
 					for (Integer i = 0; i<4 && i < `pmpentries ; i = i + 1) begin
          	if(v_pmp_cfg[i][7] == 0 && _valid[i]) 
            	v_pmp_cfg[i] <= _temp[i];
 					end
			  end
			  `PMPCFG1: begin
          Bit#(XLEN) readdata = lv_csr_pmpcfg1;
 					rg_resp_to_core <= CSRResponse{ hit : True, data : lv_csr_pmpcfg1};
 					let word <- csr_op.func(req.writedata, readdata, op);
 					Vector#(4, Bit#(8)) _temp;
 					Vector#(4, Bool) _valid = replicate(False);
 					for (Integer i = 0; i< 4; i = i + 1) begin
 					  _temp[i] = word[i*8 + 7: i*8 ];
 					  if(_temp[i][1:0] != 'b10 && (`pmp_grainbits > 2 && _temp[i][4:3] != 2) )
 					    _valid[i] = True;
 					end
 					for (Integer i = 4; i<8 && i < `pmpentries ; i = i + 1) begin
          	if(v_pmp_cfg[i][7] == 0 && _valid[i-4]) 
            	v_pmp_cfg[i] <= _temp[i-4];
 					end
			  end
			  `PMPCFG2: begin
          Bit#(XLEN) readdata = lv_csr_pmpcfg2;
 					rg_resp_to_core <= CSRResponse{ hit : True, data : lv_csr_pmpcfg2};
 					let word <- csr_op.func(req.writedata, readdata, op);
 					Vector#(4, Bit#(8)) _temp;
 					Vector#(4, Bool) _valid = replicate(False);
 					for (Integer i = 0; i< 4; i = i + 1) begin
 					  _temp[i] = word[i*8 + 7: i*8 ];
 					  if(_temp[i][1:0] != 'b10 && (`pmp_grainbits > 2 && _temp[i][4:3] != 2) )
 					    _valid[i] = True;
 					end
 					for (Integer i = 8; i<12 && i < `pmpentries ; i = i + 1) begin
          	if(v_pmp_cfg[i][7] == 0 && _valid[i-8]) 
            	v_pmp_cfg[i] <= _temp[i-8];
 					end
			  end
			  `PMPCFG3: begin
          Bit#(XLEN) readdata = lv_csr_pmpcfg3;
 					rg_resp_to_core <= CSRResponse{ hit : True, data : lv_csr_pmpcfg3};
 					let word <- csr_op.func(req.writedata, readdata, op);
 					Vector#(4, Bit#(8)) _temp;
 					Vector#(4, Bool) _valid = replicate(False);
 					for (Integer i = 0; i< 4; i = i + 1) begin
 					  _temp[i] = word[i*8 + 7: i*8 ];
 					  if(_temp[i][1:0] != 'b10 && (`pmp_grainbits > 2 && _temp[i][4:3] != 2) )
 					    _valid[i] = True;
 					end
 					for (Integer i = 12; i<16 && i < `pmpentries ; i = i + 1) begin
          	if(v_pmp_cfg[i][7] == 0 && _valid[i-12]) 
            	v_pmp_cfg[i] <= _temp[i-12];
 					end
			  end
			`endif

				`PMPADDR0 : begin
					if (`pmpentries > 0) begin
						//read previous value
 						Bit#(XLEN) readdata = v_pmp_addr[0];
 						rg_resp_to_core <= CSRResponse{ hit : True, data : readdata};
 						//form the new value to be written and write
 						let word <- csr_op.func(req.writedata,readdata,op);
 						if(v_pmp_cfg[0][7] == 0) begin
 							v_pmp_addr[0] <= truncate(word);
 						end
 					end
 					else
 						rg_resp_to_core <= CSRResponse{ hit : True, data : 0};
				end

				`PMPADDR1 : begin
					if(`pmpentries > 1) begin
						//read previous value
 						Bit#(XLEN) readdata = v_pmp_addr[1];
 						rg_resp_to_core <= CSRResponse{ hit : True, data : readdata};
 						//form the new value to be written and write
 						let word <- csr_op.func(req.writedata,readdata,op);
 						if(v_pmp_cfg[1][7] == 0) begin
 							v_pmp_addr[1] <= truncate(word);
 						end
 					end
 					else
 						rg_resp_to_core <= CSRResponse{ hit : True, data : 0};
				end

				`PMPADDR2 : begin
					if(`pmpentries > 2) begin
						//read previous value
 						Bit#(XLEN) readdata = v_pmp_addr[2];
 						rg_resp_to_core <= CSRResponse{ hit : True, data : readdata};
 						//form the new value to be written and write
 						let word <- csr_op.func(req.writedata,readdata,op);
 						if(v_pmp_cfg[2][7] == 0) begin
 							v_pmp_addr[2] <= truncate(word);
 						end
 					end
 					else
 						rg_resp_to_core <= CSRResponse{ hit : True, data : 0};
				end

				`PMPADDR3 : begin
					if(`pmpentries > 3) begin
						//read previous value
 						Bit#(XLEN) readdata = v_pmp_addr[3];
 						rg_resp_to_core <= CSRResponse{ hit : True, data : readdata};
 						//form the new value to be written and write
 						let word <- csr_op.func(req.writedata,readdata,op);
 						if(v_pmp_cfg[3][7] == 0) begin
 							v_pmp_addr[3] <= truncate(word);
 						end
 					end
 					else
 						rg_resp_to_core <= CSRResponse{ hit : True, data : 0};
				end

				`PMPADDR4 : begin
					if(`pmpentries > 4) begin
						//read previous value
 						Bit#(XLEN) readdata = v_pmp_addr[4];
 						rg_resp_to_core <= CSRResponse{ hit : True, data : readdata};
 						//form the new value to be written and write
 						let word <- csr_op.func(req.writedata,readdata,op);
 						if(v_pmp_cfg[4][7] == 0) begin
 							v_pmp_addr[4] <= truncate(word);
 						end
 					end
 					else
 						rg_resp_to_core <= CSRResponse{ hit : True, data : 0};
				end

				`PMPADDR5 : begin
					if(`pmpentries > 5) begin
						//read previous value
 						Bit#(XLEN) readdata = v_pmp_addr[5];
 						rg_resp_to_core <= CSRResponse{ hit : True, data : readdata};
 						//form the new value to be written and write
 						let word <- csr_op.func(req.writedata,readdata,op);
 						if(v_pmp_cfg[5][7] == 0) begin
 							v_pmp_addr[5] <= truncate(word);
 						end
 					end
 					else
 						rg_resp_to_core <= CSRResponse{ hit : True, data : 0};
				end

				`PMPADDR6 : begin
					if(`pmpentries > 6) begin
						//read previous value
 						Bit#(XLEN) readdata = v_pmp_addr[6];
 						rg_resp_to_core <= CSRResponse{ hit : True, data : readdata};
 						//form the new value to be written and write
 						let word <- csr_op.func(req.writedata,readdata,op);
 						if(v_pmp_cfg[6][7] == 0) begin
 							v_pmp_addr[6] <= truncate(word);
 						end
 					end
 					else
 						rg_resp_to_core <= CSRResponse{ hit : True, data : 0};
				end

				`PMPADDR7 : begin
					if(`pmpentries > 7) begin
						//read previous value
 						Bit#(XLEN) readdata = v_pmp_addr[7];
 						rg_resp_to_core <= CSRResponse{ hit : True, data : readdata};
 						//form the new value to be written and write
 						let word <- csr_op.func(req.writedata,readdata,op);
 						if(v_pmp_cfg[7][7] == 0) begin
 							v_pmp_addr[7] <= truncate(word);
 						end
 					end
 					else
 						rg_resp_to_core <= CSRResponse{ hit : True, data : 0};
				end

				`PMPADDR8 : begin
					if(`pmpentries > 8) begin
						//read previous value
 						Bit#(XLEN) readdata = v_pmp_addr[8];
 						rg_resp_to_core <= CSRResponse{ hit : True, data : readdata};
 						//form the new value to be written and write
 						let word <- csr_op.func(req.writedata,readdata,op);
 						if(v_pmp_cfg[8][7] == 0) begin
 							v_pmp_addr[8] <= truncate(word);
 						end
 					end
 					else
 						rg_resp_to_core <= CSRResponse{ hit : True, data : 0};
				end

				`PMPADDR9 : begin
					if(`pmpentries > 9) begin
						//read previous value
 						Bit#(XLEN) readdata = v_pmp_addr[9];
 						rg_resp_to_core <= CSRResponse{ hit : True, data : readdata};
 						//form the new value to be written and write
 						let word <- csr_op.func(req.writedata,readdata,op);
 						if(v_pmp_cfg[9][7] == 0) begin
 							v_pmp_addr[9] <= truncate(word);
 						end
 					end
 					else
 						rg_resp_to_core <= CSRResponse{ hit : True, data : 0};
				end

				`PMPADDR10 : begin
					if(`pmpentries > 10) begin
						//read previous value
 						Bit#(XLEN) readdata = v_pmp_addr[10];
 						rg_resp_to_core <= CSRResponse{ hit : True, data : readdata};
 						//form the new value to be written and write
 						let word <- csr_op.func(req.writedata,readdata,op);
 						if(v_pmp_cfg[10][7] == 0) begin
 							v_pmp_addr[10] <= truncate(word);
 						end
 					end
 					else
 						rg_resp_to_core <= CSRResponse{ hit : True, data : 0};
				end

				`PMPADDR11 : begin
					if(`pmpentries > 11) begin
						//read previous value
 						Bit#(XLEN) readdata = v_pmp_addr[11];
 						rg_resp_to_core <= CSRResponse{ hit : True, data : readdata};
 						//form the new value to be written and write
 						let word <- csr_op.func(req.writedata,readdata,op);
 						if(v_pmp_cfg[11][7] == 0) begin
 							v_pmp_addr[11] <= truncate(word);
 						end
 					end
 					else
 						rg_resp_to_core <= CSRResponse{ hit : True, data : 0};
				end

				`PMPADDR12 : begin
					if(`pmpentries > 12) begin
						//read previous value
 						Bit#(XLEN) readdata = v_pmp_addr[12];
 						rg_resp_to_core <= CSRResponse{ hit : True, data : readdata};
 						//form the new value to be written and write
 						let word <- csr_op.func(req.writedata,readdata,op);
 						if(v_pmp_cfg[12][7] == 0) begin
 							v_pmp_addr[12] <= truncate(word);
 						end
 					end
 					else
 						rg_resp_to_core <= CSRResponse{ hit : True, data : 0};
				end

				`PMPADDR13 : begin
					if(`pmpentries > 13) begin
						//read previous value
 						Bit#(XLEN) readdata = v_pmp_addr[13];
 						rg_resp_to_core <= CSRResponse{ hit : True, data : readdata};
 						//form the new value to be written and write
 						let word <- csr_op.func(req.writedata,readdata,op);
 						if(v_pmp_cfg[13][7] == 0) begin
 							v_pmp_addr[13] <= truncate(word);
 						end
 					end
 					else
 						rg_resp_to_core <= CSRResponse{ hit : True, data : 0};
				end

				`PMPADDR14 : begin
					if(`pmpentries > 14) begin
						//read previous value
 						Bit#(XLEN) readdata = v_pmp_addr[14];
 						rg_resp_to_core <= CSRResponse{ hit : True, data : readdata};
 						//form the new value to be written and write
 						let word <- csr_op.func(req.writedata,readdata,op);
 						if(v_pmp_cfg[14][7] == 0) begin
 							v_pmp_addr[14] <= truncate(word);
 						end
 					end
 					else
 						rg_resp_to_core <= CSRResponse{ hit : True, data : 0};
				end

				`PMPADDR15 : begin
					if(`pmpentries > 15) begin
						//read previous value
 						Bit#(XLEN) readdata = v_pmp_addr[15];
 						rg_resp_to_core <= CSRResponse{ hit : True, data : readdata};
 						//form the new value to be written and write
 						let word <- csr_op.func(req.writedata,readdata,op);
 						if(v_pmp_cfg[15][7] == 0) begin
 							v_pmp_addr[15] <= truncate(word);
 						end
 					end
 					else
 						rg_resp_to_core <= CSRResponse{ hit : True, data : 0};
 				end
  		`endif

			`ifdef supervisor
				`SSCRATCH : begin
					//read previous value
 					rg_resp_to_core <= CSRResponse{ hit : True, data : rg_sscratch};
 					Bit#(XLEN) readdata = rg_sscratch;
 					//form the new value to be written and write
 					let word <- csr_op.func(req.writedata,readdata,op);
 					rg_sscratch <= word;
				end
			`endif

			`ifdef usertraps
				`USCRATCH : begin
					//read previous value
 					rg_resp_to_core <= CSRResponse{ hit : True, data : rg_uscratch};
 					Bit#(XLEN) readdata = rg_uscratch;
 					//form the new value to be written and write
 					let word <- csr_op.func(req.writedata,readdata,op);
 					rg_uscratch <= word;
				end
			`endif

 				`CUSTOMCNTRL : begin
 				 	//read previous value
 					rg_resp_to_core <= CSRResponse{ hit : True, data : zeroExtend(rg_customcontrol)};
 					Bit#(XLEN) readdata = zeroExtend(rg_customcontrol);
 					//form the new value to be written and write
 					let word <- csr_op.func(req.writedata,readdata,op);
 					rg_customcontrol <= truncate(word);
 				end


 				default : begin
          ff_fwd_request.enq(req);
          rg_resp_to_core <= CSRResponse{ hit : False, data : 0};
        end
 			endcase
 		endmethod

	 	method CSRResponse mv_core_resp = rg_resp_to_core;

   	method ActionValue#(CSRReq) mav_fwd_req;
      ff_fwd_request.deq;
      return ff_fwd_request.first();
    endmethod

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // Inter group sideband connections
    method Bit#(XLEN) mv_csr_misa;
    	Bit#(XLEN) misa_val;
			misa_val[25 : 0] = {5'd0, rg_misa_u, 1'd0, rg_misa_s, 4'd0, rg_misa_n, rg_misa_m, 3'd0,
			                    rg_misa_i, 2'd0,
			                    /*rg_misa_i & rg_misa_m & rg_misa_a & rg_misa_f & rg_misa_d,*/ rg_misa_f,
 					                1'd0, rg_misa_d, rg_misa_c, 1'd0, rg_misa_a};

 		`ifdef RV64
      misa_val[63 : 62] = mxl;
    `else
      misa_val[31 : 30] = mxl;
    `endif

    	return misa_val;
    endmethod
    ////////////////////////////////////////////////////////////////////////////////////////////////
    //sideband connecitons from core
     method mv_cacheenable = truncate(rg_customcontrol);

  `ifdef arith_trap
    method Bit#(1) mv_arith_excep = rg_customcontrol[3];
  `endif

  	method mv_csr_misa_c = rg_misa_c;

	`ifdef pmp
    method mv_pmp_cfg = readVReg(v_pmp_cfg);
    method Vector#(`pmpentries, Bit#(TSub#(`paddr, `pmp_grainbits ))) mv_pmp_addr;
      Vector#(`pmpentries, Bit#(TSub#(`paddr, `pmp_grainbits))) _t;
      for (Integer i = 0; i<`pmpentries; i = i + 1) begin
        _t[i] = truncate(v_pmp_addr[i] >> (`pmp_grainbits -2 ));
      end
      return _t;
    endmethod
  `endif

  endmodule : mk_csr_grp2
endpackage : csr_grp2

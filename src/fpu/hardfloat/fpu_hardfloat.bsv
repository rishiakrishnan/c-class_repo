// See LICENSE.iitm for license details
/* 
Author: IIT Madras
Details: Top level module for the FPU. This module interfaces with the FPU hardfloat modules and the FPU wrapper modules.
--------------------------------------------------------------------------------------------------
*/
package fpu_hardfloat;
  import FIFO :: * ;
  import FIFOF :: * ;
`ifdef async_rst
  import SpecialFIFOs_Modified :: * ;
`else
  import SpecialFIFOs :: * ;
`endif
  import DReg :: * ;
  import wrapper_fma :: * ;
  import wrapper_ftoi :: * ;
  import wrapper_itof :: * ;
  import wrapper_ftof :: * ;
  import bsvmkdiv_sqrt :: * ;
  import comparison :: * ;
  import fclass :: * ;
  import sign_inject :: * ;
  import ccore_types::*;
  import TxRx     :: *;
  
  `include "ccore_params.defines"
  `include "fpu.defines"
  `include "Logger.bsv"

  `define expwidth 11
  `define sigwidth 53

//   `define verbose

`ifdef fpu_clockgate
  (*synthesize,gate_all_clocks*)
`else
	(*synthesize*)
`endif
  module mkspfma_instance(Ifc_wrapper_fma#(8,24));
    let ifc();
    mkwrapper_fma fma(ifc);
    return (ifc);
  endmodule

`ifdef fpu_clockgate
  (*synthesize,gate_all_clocks*)
`else
	(*synthesize*)
`endif
  module mkdpfma_instance(Ifc_wrapper_fma#(11,53));
    let ifc();
    mkwrapper_fma fma(ifc);
    return (ifc);
  endmodule

`ifdef fpu_clockgate
  (*synthesize,gate_all_clocks*)
`else
	(*synthesize*)
`endif
  module mkspdiv_sqrt_instance(Ifc_div_sqrt#(8,24));
    let ifc();
    mkdiv_sqrt div_sqrt(ifc);
    return (ifc);
  endmodule
  
`ifdef fpu_clockgate
  (*synthesize,gate_all_clocks*)
`else
	(*synthesize*)
`endif
  module mkdpdiv_sqrt_instance(Ifc_div_sqrt#(11,53));
    let ifc();
    mkdiv_sqrt div_sqrt(ifc);
    return (ifc);
  endmodule
  
`ifdef fpu_clockgate
  (*synthesize,gate_all_clocks*)
`else
	(*synthesize*)
`endif
  module mkitosp_instance(Ifc_wrapper_itof#(8,24,ELEN));
    let ifc();
    mkwrapper_itof itof(ifc);
    return (ifc);
  endmodule
  
`ifdef fpu_clockgate
  (*synthesize,gate_all_clocks*)
`else
	(*synthesize*)
`endif
  module mkitodp_instance(Ifc_wrapper_itof#(11,53,ELEN));
    let ifc();
    mkwrapper_itof itof(ifc);
    return (ifc);
  endmodule
  
`ifdef fpu_clockgate
  (*synthesize,gate_all_clocks*)
`else
	(*synthesize*)
`endif
  module mksptoi_instance(Ifc_wrapper_ftoi#(8,24,ELEN));
    let ifc();
    mkwrapper_ftoi ftoi(ifc);
    return (ifc);
  endmodule
  
`ifdef fpu_clockgate
  (*synthesize,gate_all_clocks*)
`else
	(*synthesize*)
`endif
  module mkdptoi_instance(Ifc_wrapper_ftoi#(11,53,ELEN));
    let ifc();
    mkwrapper_ftoi ftoi(ifc);
    return (ifc);
  endmodule
  
`ifdef fpu_clockgate
  (*synthesize,gate_all_clocks*)
`else
	(*synthesize*)
`endif
  module mksptodp_instance(Ifc_wrapper_ftof#(8,24,11,53));
    let ifc();
    mkwrapper_ftof ftof(ifc);
    return (ifc);
  endmodule
  
`ifdef fpu_clockgate
  (*synthesize,gate_all_clocks*)
`else
	(*synthesize*)
`endif
  module mkdptosp_instance(Ifc_wrapper_ftof#(11,53,8,24));
    let ifc();
    mkwrapper_ftof ftof(ifc);
    return (ifc);
  endmodule
  
`ifdef fpu_clockgate
  (*synthesize,gate_all_clocks*)
`else
	(*synthesize*)
`endif
  module mkspcmp_instance(Ifc_comparison#(8,24));
    let ifc();
    mkcomparison cmp(ifc);
    return (ifc);
  endmodule
  
`ifdef fpu_clockgate
  (*synthesize,gate_all_clocks*)
`else
	(*synthesize*)
`endif
  module mkdpcmp_instance(Ifc_comparison#(11,53));
    let ifc();
    mkcomparison cmp(ifc);
    return (ifc);
  endmodule
  
`ifdef fpu_clockgate
  (*synthesize,gate_all_clocks*)
`else
	(*synthesize*)
`endif
  module mkspfclass_instance(Ifc_fclass#(8,24));
    let ifc();
    mkfclass fclass(ifc);
    return (ifc);
  endmodule
  
`ifdef fpu_clockgate
  (*synthesize,gate_all_clocks*)
`else
	(*synthesize*)
`endif
  module mkdpfclass_instance(Ifc_fclass#(11,53));
    let ifc();
    mkfclass fclass(ifc);
    return (ifc);
  endmodule
  
`ifdef fpu_clockgate
  (*synthesize,gate_all_clocks*)
`else
	(*synthesize*)
`endif
  module mkspsign_inject_instance(Ifc_sign_inject#(8,24));
    let ifc();
    mksign_inject sign_inject(ifc);
    return (ifc);
  endmodule
  
`ifdef fpu_clockgate
  (*synthesize,gate_all_clocks*)
`else
	(*synthesize*)
`endif
  module mkdpsign_inject_instance(Ifc_sign_inject#(11,53));
    let ifc();
    mksign_inject sign_inject(ifc);
    return (ifc);
  endmodule
  
  interface Ifc_fpu;
    method Action _start(Input_Packet m);
    method TXe#(XBoxOutput) tx_output;
    method Action flush;
    method Bit#(1) fpu_ready;
  endinterface

  (*synthesize,gate_all_clocks*)
  module mkfpu_hardfloat(Ifc_fpu);

    let spfma <- mkspfma_instance;
    let spdiv_sqrt <- mkspdiv_sqrt_instance;
    let itosp <- mkitosp_instance;
    let sptoi <- mksptoi_instance;
    let spcmp <- mkspcmp_instance;
    let spfclass <- mkspfclass_instance;
    let spsign_inject <- mkspsign_inject_instance;
    
`ifdef dpfpu
	let dpfma <- mkdpfma_instance;
	let dpdiv_sqrt <- mkdpdiv_sqrt_instance;
	let itodp <- mkitodp_instance;
	let dptoi <- mkdptoi_instance;
	let dpcmp <- mkdpcmp_instance;
	let dpfclass <- mkdpfclass_instance;
	let dpsign_inject <- mkdpsign_inject_instance;
	let sptodp <- mksptodp_instance;
	let dptosp <- mkdptosp_instance;
`endif
    
	TX#(XBoxOutput) tx_fbox_out <- mkTX;
	FIFOF# (Input_Packet) ff_input   <- mkFIFOF1;	
   
	function Tuple2#(Bit#(ELEN), Bit#(5)) compensate_for_32bit_int (Bit#(ELEN) out, Bit#(5) flag, Bit#(1) long, Bit#(1) is_signed);

		if(long==0 && flag[4]==1 && is_signed==0) begin  	//if 32-bit unsigned int and result overflows
			Bit#(32) all_ones= '1;
			out= zeroExtend(all_ones);
			flag[0]= 0; //inexact
			flag[2]= 0; //overflow
			flag[4]= 1; //invalid
		end
		else if(long==0 && flag[4]==1 && is_signed==1) begin	//OR if result is invalid for a 32-bit signed int
			Bit#(31) all_ones= '1;
			out= zeroExtend(all_ones);
			flag[0]= 0; //inexact
			flag[2]= 0; //overflow
			flag[4]= 1; //invalid
		end
		else if(long==1 && (flag[4]==1 || flag[2]==1) && is_signed==1) begin	//Convert to long and result is invalid or has overflown
			out= {1'b0,'1};
		end
		return tuple2(out, flag);

	endfunction

	function Tuple3#(Bool, Bit#(n), Bit#(5)) fn_ExtendFpuRes (Tuple3#(Bool, Bit#(m), Bit#(5)) inp, Bool zeroextend)
	  provisos (Add#(m, a__, n));
	  Bit#(n) lv2= (zeroextend) ? zeroExtend(tpl_2(inp)) : {'1,tpl_2(inp)};
	  return tuple3(tpl_1(inp), lv2, tpl_3(inp));
	endfunction

`ifdef dpfpu
    function Bool isNaNBox(Bit#(64) op);
        return (&(op[63:32])==1);
    endfunction
`endif

    function Tuple3#(Bit#(ELEN),Bit#(ELEN),Bit#(ELEN)) setCanonicalNaN (Bit#(ELEN) op1, Bit#(ELEN) op2,
                                                            		  Bit#(ELEN) op3);
`ifdef dpfpu
        return tuple3(isNaNBox(op1)? truncate(op1) : zeroExtend(32'h7fc00000),
					  isNaNBox(op2)? truncate(op2) : zeroExtend(32'h7fc00000),
				      isNaNBox(op3)? truncate(op3) : zeroExtend(32'h7fc00000));
`else
        return tuple3(truncate(op1),truncate(op2),truncate(op3));
`endif
    endfunction


    Wire#(Bit#(1)) wr_inValid <- mkDWire(0);
    Wire#(Bit#(1)) wr_sqrtop  <- mkDWire(0);
    Wire#(Bit#(3)) wr_f3      <- mkDWire(0);
    Wire#(Bit#(ELEN)) wr_op1  <- mkDWire(0);
    Wire#(Bit#(ELEN)) wr_op2  <- mkDWire(0);
    Wire#(Bool) wr_issp       <- mkDWire(True);
    Reg#(Bool)  rg_issp       <- mkReg(False);
    Reg#(Bool) rg_multicycle_op <-mkReg(False);
   
`ifdef dpfpu
    (*conflict_free="start, output_spfma, output_dpfma, output_spdiv_sqrt, output_dpdiv_sqrt, output_sptoi, output_dptoi, output_itosp, output_itodp, output_sptodp, output_dptosp"*)
`else
    (*conflict_free="start, output_spfma, output_spdiv_sqrt, output_sptoi, output_itosp"*)
`endif

    rule rl_send_spdiv_sqrt_inputs(wr_issp);
		`ifdef verbose if(wr_inValid==1) $display($time,"\tSPDIV_SQRT"); `endif
		spdiv_sqrt.request(1'b1, wr_inValid,  wr_sqrtop,  wr_f3,  truncate(wr_op1), truncate(wr_op2));
		rg_issp<= True;
    endrule
    
`ifdef dpfpu
    rule rl_send_dpdiv_sqrt_inputs(!wr_issp);
		`ifdef verbose if(wr_inValid==1) $display($time,"\tDPDIV_SQRT"); `endif
		dpdiv_sqrt.request(1'b1, wr_inValid,  wr_sqrtop,  wr_f3,  wr_op1, wr_op2);
		rg_issp<= False;
    endrule
`endif
    
    rule start;
	    let input_packet = ff_input.first;
      Bit#(ELEN) op1 = input_packet.operand1;
      Bit#(ELEN) op2 = input_packet.operand2;
      Bit#(ELEN) op3 = input_packet.operand3;
      Bit#(4) opcode       = input_packet.opcode;
      Bit#(7) f7       = input_packet.funct7;
      Bit#(3) f3       = input_packet.funct3;
      Bit#(2) imm          = input_packet.imm;
		  Bit#(3) fsr        =  input_packet.fsr;
      Bool    issp         = input_packet.issp;

		  ff_input.deq;
      
      f3 = (f3 == 'b111) ? fsr : f3;
	    `ifdef verbose $display($time,"\tfpu Input to FPU: op1: %h, op2: %h, op3: %h", op1, op2, op3); `endif
      `ifdef verbose $display($time,"\tfpu opcode:%b, funct7:%b, funct3:%b issp: %b imm:%b",opcode, f7, f3, issp, imm); `endif  

			if( (issp && !(f7[6:2] ==`FCVT_F_I_f5 && opcode == `FP_OPCODE) && 	//Except int to fp and fmv, rest of SP ops
			    !(((f7 == `FMV_X_S_f7 || f7 == `FMV_S_X_f7 `ifdef dpfpu || f7 == `FMV_X_D_f7 || f7 == `FMV_D_X_f7 `endif )
			    && f3 == 'b000) && opcode == `FP_OPCODE) && !((f7[6:2] == `FCVT_S_D_f5) && opcode == `FP_OPCODE)) ||	//Except DPTOSP
		      (!issp && ((f7[6:2] == `FCVT_S_D_f5) && opcode == `FP_OPCODE)) ) begin	//or if it is a SPtoDP op
      	
				{op1, op2, op3} = setCanonicalNaN(op1,op2,op3);
	    	`ifdef verbose $display($time,"\tfpu After NaN boxing: op1: %h, op2: %h, op3: %h", op1, op2, op3); `endif
		 	end

      
	  	//Compare Operations
    	if((f7[6:2]==`FCMP_f5 || f7[6:2] == `FMMAX_f5) && opcode == `FP_OPCODE) begin
    	  `ifdef verbose $display($time,"\tfpu Input comparison: op1: %h, op2: %h, f3: %b, f7: %b",op1, op2, f3, f7[2]); `endif
			  if(issp) begin
    	   	let x <- spcmp.request(truncate(op1), truncate(op2), f3, f7[2]);
				  Tuple3#(Bool, Bit#(ELEN), Bit#(5))  u = fn_ExtendFpuRes(x, `ifdef dpfpu (f7[2]==0) `else True `endif );
		      let y = XBoxOutput{valid:  tpl_1(u),data:   tpl_2(u),	fflags: tpl_3(u) };
	        tx_fbox_out.u.enq(y);
			  end
			`ifdef dpfpu
			  else begin
    	    let x <- dpcmp.request(op1, op2, f3, f7[2]);
    	    Tuple3#(Bool, Bit#(ELEN), Bit#(5))  u = fn_ExtendFpuRes(x, `ifdef dpfpu (f7[2]==0) `else True `endif );
			    let y = XBoxOutput{valid:  tpl_1(u),data:   tpl_2(u),	fflags: tpl_3(u) };
			    tx_fbox_out.u.enq(y);
			  end
			`endif   
    	end

	  	//Convert INT to Floating Point
	  	else if(f7[6:2] ==`FCVT_F_I_f5 && opcode == `FP_OPCODE) begin
	  	  `ifdef verbose $display($time,"\tfpu Input ITOF: op1: %h, f3: %b, imm[1:0]: %b", op1, f3, imm[1:0]); `endif
				//Check           long?              signed?
				Bit#(ELEN) inp= (imm[1]==1) ? op1 : ((imm[0]==0) ? signExtend(op1[31:0]) : zeroExtend(op1[31:0]));
			  if(issp) begin
			    itosp.request(inp, f3, ~imm[0]);
			  end
			  `ifdef dpfpu
			  else begin
			    itodp.request(inp, f3, ~imm[0]);
			  end
			  `endif
			  rg_multicycle_op<=True;
	  	end

	  	//Sign Injection
	  	else if(f7[6:2] == `FSGNJN_f5 && opcode == `FP_OPCODE) begin
			  if(issp) begin
			  	let out= spsign_inject.request(truncate(op1), op2[31], f3);
	  	  	`ifdef verbose  $display($time,"\tfpu SP sign injection: op1: %h, op2[31]: %b, f3: %b out: %h", op1, op2[31], f3, out);  `endif
				  let y = XBoxOutput {valid:True,data: `ifdef dpfpu {'1,out} `else zeroExtend(out) `endif , fflags:0 `ifdef arith_trap ,arith_trap_en: wr_arith_en `endif };
				  tx_fbox_out.u.enq(y);
				end
			`ifdef dpfpu
			  else begin
				let out= dpsign_inject.request(op1, op2[valueOf(ELEN)-1], f3);
	  	  	`ifdef verbose $display($time,"\tfpu DP sign injection: op1: %h, op2[31]: %b, f3: %b out: %h", op1, op2[valueOf(ELEN)-1], f3, out); `endif
			    let y = XBoxOutput {valid:True, fflags:0, data:out `ifdef arith_trap ,arith_trap_en: wr_arith_en `endif };
				  tx_fbox_out.u.enq(y);
			  end
			`endif
	  	end

	  	//Convert to Integer
	  	else if(f7[6:2] == `FCVT_I_F_f5 && opcode == `FP_OPCODE) begin
	  	  `ifdef verbose $display($time,"\tfpu Input FTOI: op1: %h, f3: %b, imm[1:0]: %b", op1, f3, imm[1:0]); `endif
			  if(issp) begin
	  	   	sptoi.request(truncate(op1), f3, ~imm[0], imm[1]);
			  end
			`ifdef dpfpu
			  else begin
	  	   	dptoi.request(op1, f3, ~imm[0], imm[1]);
			  end
			`endif
			  rg_multicycle_op<=True;
	  	end

		//FP to FP convert
		`ifdef dpfpu
		else if((f7[6:2] == `FCVT_S_D_f5) && opcode == `FP_OPCODE)begin
      if(!issp) begin
				`ifdef verbose $display("Giving inputs to Convert SP to DP: %x", op1); `endif
				sptodp.request(truncate(op1), f3); 
			end
			else begin
				`ifdef verbose $display("Giving inputs to Convert DP to SP: %x", op1); `endif
				dptosp.request(op1, f3); 
			end
			rg_multicycle_op<=True;
		end
		`endif

	  	//FCLASS
	  	else if((f7[6:2] == `FCLASS_f5 && f3=='b001) && opcode == `FP_OPCODE) begin
	  	  `ifdef verbose $display($time,"\tfpu Input classify: op1: %h", op1); `endif
			  if(issp) begin
	  	   	//rg_output <= tuple3(True, zeroExtend(spfclass.request(truncate(op1))), 0);
				 let y = XBoxOutput {valid:True,data:zeroExtend(spfclass.request(truncate(op1))), fflags:0 `ifdef arith_trap ,arith_trap_en: wr_arith_en `endif }; 
			     tx_fbox_out.u.enq(y);
				end
			  `ifdef dpfpu
			  else begin
	  	   	// rg_output <= tuple3(True, dpfclass.request(op1), 0);
				 let y = XBoxOutput {valid:True,data:dpfclass.request(op1), fflags:0 `ifdef arith_trap ,arith_trap_en: wr_arith_en `endif }; 
				 tx_fbox_out.u.enq(y);
				end
			  `endif
	  	end

	  	//Move
	  	else if(((f7 == `FMV_X_S_f7 || f7 == `FMV_S_X_f7 `ifdef dpfpu || f7 == `FMV_X_D_f7 || f7 == `FMV_D_X_f7 `endif )
	  	&& f3 == 'b000) && opcode == `FP_OPCODE) begin
	  	  Bit#(ELEN) final_result=0;
	  	  if(f7==`FMV_X_S_f7) // sp to integer FMV.X.W
	  	    final_result = signExtend(op1[31:0]);
				else if(f7==`FMV_S_X_f7) begin // integer to sp FMV.W.X
	  	  	`ifdef dpfpu
	  	    	final_result = {'1,op1[31:0]};
	  	  	`else
	  	    	final_result= zeroExtend(op1[31:0]);
	  	  	`endif
				end

				`ifdef dpfpu
				else begin
					final_result= op1;
				end
				`endif
	  	  //rg_output <= tuple3(True, final_result, 0);
		   let y = XBoxOutput {valid:True,fflags:0, data:final_result `ifdef arith_trap ,arith_trap_en: wr_arith_en `endif }; 
		   tx_fbox_out.u.enq(y);
	  	end

	  	//ADD SUB
	  	else if((f7[6:2] == `FADD_f5 || f7[6:2] == `FSUB_f5) && opcode == `FP_OPCODE) begin
	  		`ifdef verbose $display($time,"\tfpu Input Add/Sub: f7[3:2]: %b, op1: %h, op2: %h, f3: %b",f7[3:2], op1, op2, f3); `endif
				if(issp) begin
	  			Bit#(7) exp_ones = '1;
	  			Bit#(23) sig_zero = '0;
	  			Bit#(32) _op2 = {1'b0, 1'b0, exp_ones, sig_zero};
	  			spfma.request(f7[3:2], truncate(op1), _op2, truncate(op2), f3);
				end
				`ifdef dpfpu
				else begin
	  			Bit#(10) exp_ones = '1;
	  			Bit#(52) sig_zero = '0;
	  			Bit#(ELEN) _op2 = {1'b0, 1'b0, exp_ones, sig_zero};
	  			dpfma.request(f7[3:2], op1, _op2, op2, f3);
				end
				`endif
				rg_multicycle_op<=True;
	  	end
			//MUL
	  	else if(f7[6:2] == `FMUL_f5 && opcode == `FP_OPCODE) begin
	  	  `ifdef verbose $display($time,"\tfpu Input Mul: op1: %h, op2: %h, f3: %b", op1, op2, f3); `endif
				if(issp) begin
	  	    Bit#(ELEN) _op3 = 0;
	  	    _op3[31] = op1[31] ^ op2[31];
	  	    spfma.request(2'b00, truncate(op1), truncate(op2), truncate(_op3), f3);
				end
				`ifdef dpfpu
				else begin
	  	    Bit#(ELEN) _op3 = 0;
	  	    _op3[valueOf(ELEN)-1] = op1[valueOf(ELEN)-1] ^ op2[valueOf(ELEN)-1];
	  	    dpfma.request(2'b00, op1, op2, _op3, f3);
				end
				`endif
				rg_multicycle_op<=True;
	  	end
			//Fused MUL ADD/SUB
	  	else if(opcode == `FMADD || opcode == `FMSUB || opcode == `FNMSUB || opcode == `FNMADD) begin
	  	  `ifdef verbose $display($time,"\tfpu Input FMA: op1: %h, op2: %h, op3: %h f3: %b", op1, op2, op3, f3); `endif
				if(issp)
	  	  	spfma.request(opcode[1:0], truncate(op1), truncate(op2), truncate(op3), f3);
				`ifdef dpfpu
				else
	  	  	dpfma.request(opcode[1:0], op1, op2, op3, f3);
				`endif
				rg_multicycle_op<=True;
	  	end

			//DIV SQRT
	  	else if((f7[6:2] == `FDIV_f5 || f7[6:2] == `FSQRT_f5) && opcode == `FP_OPCODE) begin
    		wr_inValid <=  1;
    		wr_f3 <=  f3;
    		wr_op1 <=  op1;
    		wr_op2 <=  op2;
				wr_issp <= issp;
				if(f7[6:2] == `FDIV_f5) begin
    			wr_sqrtop <=  0;
	  	  	`ifdef verbose $display($time,"\tfpu Input Division: op1: %h, op2: %h, f3: %b", op1, op2, f3); `endif
				end
				else begin
    			wr_sqrtop <=  1;
	  	  	`ifdef verbose $display($time,"\tfpu Input Sqrt: op1: %h, f3: %b", op1, f3); `endif
				end
				rg_multicycle_op<=True;
	  	end
    endrule
    
    rule output_spfma(spfma.resp_valid);
      let res = spfma.response;
	  let flag= tpl_2(res);
	  Bit#(ELEN) out;
	`ifdef dpfpu
	  out={'1,tpl_1(res)[31:0]};
	`else
	  out=zeroExtend(tpl_1(res));
	`endif
      `ifdef verbose $display($time,"\tfpu Output SPFMA: out: %h, flag: %b", out, flag); `endif
      let u = tuple3(True, out, flag);
	  let y = XBoxOutput{valid:  tpl_1(u),data:   tpl_2(u),	fflags: tpl_3(u) };
	  tx_fbox_out.u.enq(y);
	  rg_multicycle_op<=False;
    endrule
   
`ifdef dpfpu
    rule output_dpfma(dpfma.resp_valid);
      let {out,flag} = dpfma.response;
      `ifdef verbose $display($time,"\tfpu Output DPFMA: out: %h, flag: %b", out, flag); `endif
      let u = tuple3(True, out, flag);
	  let y = XBoxOutput{valid:  tpl_1(u),data:   tpl_2(u),	fflags: tpl_3(u) };
	  tx_fbox_out.u.enq(y);
	  rg_multicycle_op<=False;
    endrule
`endif
   
	rule rl_dummy_outp(spdiv_sqrt.ooutValid);
	  let res = spdiv_sqrt.oout;
      `ifdef verbose $display($time,"\tfpu Dummy Output SPDiv Sqrt: out: %h", res); `endif
	endrule

    rule output_spdiv_sqrt(spdiv_sqrt.ooutValid && rg_issp);
      let res = spdiv_sqrt.oout;
      let flag = spdiv_sqrt.oexceptionFlags;
	  Bit#(ELEN) out;
	`ifdef dpfpu
	  out={'1,res[31:0]};
	`else
	  out=zeroExtend(res);
	`endif
      `ifdef verbose $display($time,"\tfpu Output SPDiv Sqrt: out: %h, flag: %b", out, flag); `endif
      let u = tuple3(True, out, flag);
	  let y = XBoxOutput{valid:  tpl_1(u),data:   tpl_2(u),	fflags: tpl_3(u) };
	  tx_fbox_out.u.enq(y);
	  rg_multicycle_op<=False;
    endrule
   
		`ifdef dpfpu
    rule output_dpdiv_sqrt(dpdiv_sqrt.ooutValid && !rg_issp);
      let out = dpdiv_sqrt.oout;
      let flag = dpdiv_sqrt.oexceptionFlags;
      `ifdef verbose $display($time,"\tfpu Output DPDiv Sqrt: out: %h, flag: %b", out, flag); `endif
      let u = tuple3(True, out, flag);
	  let y = XBoxOutput{valid:  tpl_1(u),data:   tpl_2(u),	fflags: tpl_3(u) };
	  tx_fbox_out.u.enq(y);
	  rg_multicycle_op<=False;
    endrule
		`endif

    rule output_sptoi(sptoi.resp_valid);
      //let resp = sptoi.response;
			let {lv_out, lv_flag, lv_meta}= sptoi.response;
      `ifdef verbose $display($time,"\tfpu Output from hardfloat: out: %h, flag: %b meta: %b", lv_out, lv_flag, lv_meta); `endif
			// let {out, flag} = compensate_for_32bit_int(lv_out, lv_flag, lv_meta[0], lv_meta[1]);
    //   `ifdef verbose $display($time,"\tfpu Output SPTOI: out: %h, flag: %b", out, flag); `endif
      let u = tuple3(True, lv_out, lv_flag);
	  let y = XBoxOutput{valid:  tpl_1(u),data:   tpl_2(u),	fflags: tpl_3(u) };
	  tx_fbox_out.u.enq(y);
	  rg_multicycle_op<=False;
    endrule
   
	`ifdef dpfpu
    rule output_dptoi(dptoi.resp_valid);
			let {lv_out, lv_flag, lv_meta}= dptoi.response;
      `ifdef verbose $display($time,"\tfpu Output from hardfloat: out: %h, flag: %b meta: %b", lv_out, lv_flag, lv_meta); `endif
			let {out, flag} = compensate_for_32bit_int(lv_out, lv_flag, lv_meta[0], lv_meta[1]);
      `ifdef verbose $display($time,"\tfpu Output DPTOI: out: %h, flag: %b", out, flag); `endif
      let u = tuple3(True, out, flag);
	  let y = XBoxOutput{valid:  tpl_1(u),data:   tpl_2(u),	fflags: tpl_3(u) };
	  tx_fbox_out.u.enq(y);
	  rg_multicycle_op<=False;
    endrule
	`endif
   
    rule output_itosp(itosp.resp_valid);
      let {out,flag} = itosp.response;
      `ifdef verbose $display($time,"\tfpu Output ITOSP: out: %h, flag: %b", out, flag); `endif
	  	//let u = tuple3(True, `ifdef dpfpu {'1,out} `else zeroExtend(out) `endif , flag);
		  let y = XBoxOutput{valid:  True,data:   `ifdef dpfpu {'1,out} `else zeroExtend(out) `endif ,	fflags: flag };
		  tx_fbox_out.u.enq(y);
		  rg_multicycle_op<=False;
    endrule
    
	`ifdef dpfpu
    rule output_itodp(itodp.resp_valid);
      let {out,flag} = itodp.response;
      `ifdef verbose $display($time,"\tfpu Output ITODP: out: %h, flag: %b", out, flag); `endif
	  let u = tuple3(True, out, flag);
	  let y = XBoxOutput{valid:  tpl_1(u),data:   tpl_2(u),	fflags: tpl_3(u) };
	  tx_fbox_out.u.enq(y);
	  rg_multicycle_op<=False;
    endrule

    rule output_sptodp(sptodp.resp_valid);
      let {out,flag} = sptodp.response;
      `ifdef verbose $display($time,"\tfpu Output SPTODP: out: %h, flag: %b", out, flag); `endif
	  	let u = tuple3(True, out, flag);
		  let y = XBoxOutput{valid:  tpl_1(u),data:   tpl_2(u),	fflags: tpl_3(u) };
		  tx_fbox_out.u.enq(y);
		  rg_multicycle_op<=False;
    endrule

    rule output_dptosp(dptosp.resp_valid);
      let {out,flag} = dptosp.response;
      `ifdef verbose $display($time,"\tfpu Output DPTOSP: out: %h, flag: %b", out, flag); `endif
		  let y = XBoxOutput{valid:  True,data:   {'1,out},	fflags: flag };
		  tx_fbox_out.u.enq(y);
		  rg_multicycle_op<=False;
    endrule
	`endif
    
	method Action _start(Input_Packet m) if(!rg_multicycle_op);
		ff_input.enq (m);
    endmethod


	method tx_output = tx_fbox_out.e;

	method fpu_ready = pack(!(rg_multicycle_op || ff_input.notEmpty));

	method Action flush;
		$display("Flush");
	//   wr_flush<=True;
	endmethod
	  
  endmodule
  
endpackage

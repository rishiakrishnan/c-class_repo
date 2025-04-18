// See LICENSE.iitm for license details
/* 

Module name: Multiplier module.  
author name: IIT Madras
*/
package muldiv_fpga;
  `ifdef simulate
    import muldiv_fpga_sim::*;
  `else
  import multiplier::*;
  `endif
  import restoring_div::*;
  import ccore_types::*;
  `include "ccore_params.defines"

	interface Ifc_muldiv;
		method ActionValue#(ALU_OUT) get_inputs(Bit#(XLEN) operand1, Bit#(XLEN) operand2, 
        Bit#(3) funct3 `ifdef RV64 , Bool word32 `endif );
		method ActionValue#(ALU_OUT) delayed_output;//returning the result
   `ifdef arith_trap
      method Action rd_arith_excep_en(Bit#(1) arith_en);
   `endif
	endinterface:Ifc_muldiv

  (*synthesize*)
	module mkmuldiv(Ifc_muldiv);
    Ifc_multiplier#(XLEN) mult <- mkmultiplier;
    Ifc_restoring_div divider <-mkrestoring_div();
    Reg#(Bit#(TLog#(TAdd#(TMax#(`MULSTAGES, `DIVSTAGES), 1)))) rg_count <-mkReg(0);
    Reg#(Bool) mul_div <-mkReg(False); // False = Mul, True = Div.
    Reg#(Bool) rg_upperbits <- mkReg(False);
    Reg#(Bool) rg_complement <- mkReg(False);
    `ifdef RV64 Reg#(Bool) rg_word32 <- mkReg(False); `endif
    Reg#(Bit#(1)) rg_sign_op1 <- mkReg(0);
    
    
    `ifdef arith_trap
    Wire#(Bit#(1)) wr_arith_en <-mkDWire(0);
    `endif

    rule increment_counter(rg_count!=0);
      if((rg_count== fromInteger(`MULSTAGES) && !mul_div) || 
          (rg_count== fromInteger(`DIVSTAGES)+ 1 && mul_div)) begin
        rg_count<= 0;
      end
      else begin
        rg_count<= rg_count+ 1;
      end
    endrule

		method ActionValue#(ALU_OUT) get_inputs(Bit#(XLEN) operand1, Bit#(XLEN) operand2,
        Bit#(3) funct3 `ifdef RV64 , Bool word32 `endif ) if(rg_count==0);
      // logic to choose the upper bits
      // in case of division,  this variable is set is the operation is a remainder operation
      mul_div<=unpack(funct3[2]);
      `ifdef RV64
        if(word32)begin
          operand1=funct3[0]==0? signExtend(operand1[31:0]):zeroExtend(operand1[31:0]);
          operand2=funct3[0]==0? signExtend(operand2[31:0]):zeroExtend(operand2[31:0]);
        end
      `endif
      Bool lv_upperbits = funct3[2]==0?unpack(|funct3[1:0]):unpack(funct3[1]);

      Bool invert_op1=False;
      Bool invert_op2=False;
      // in multiplication operations
      if(funct3[2]==0 && (funct3[0]^funct3[1])==1 && operand1[valueOf(XLEN)-1]==1)
        invert_op1=True;
      else if(funct3[2]==1 && funct3[0]==0 && operand1[valueOf(XLEN)-1]==1) // in case of division operations.
        invert_op1=True;
      
      if(funct3[2]==0 && funct3[1:0]==1 && operand2[valueOf(XLEN)-1]==1)// in multiplication operations
          invert_op2=True;
      else if(funct3[2]==1 && funct3[0]==0 && operand2[valueOf(XLEN)-1]==1)// in case of division operations.
        invert_op2=True;

      Bit#(XLEN) t1=signExtend(pack(invert_op1));
      Bit#(XLEN) t2=signExtend(pack(invert_op2));
      Bit#(XLEN) op1= (t1^operand1)+ zeroExtend(pack(invert_op1));
      Bit#(XLEN) op2= (t2^operand2)+ zeroExtend(pack(invert_op2));

	    Bool lv_take_complement = False;
    	if(funct3==1 || funct3==4) // in case of MULH or DIV
		    lv_take_complement=unpack(operand1[valueOf(XLEN)-1]^operand2[valueOf(XLEN)-1]);
    	else if(funct3==2)
		    lv_take_complement=unpack(operand1[valueOf(XLEN)-1]);
      else if(funct3==6)
        lv_take_complement=True;
      rg_sign_op1<= operand1[valueOf(XLEN)-1];	

      Bit#(XLEN) default_out='1;
      Bool result_avail=False;
      if(funct3[2]==0)begin // multiplication operation
        mult.iA(op1);
        mult.iB(op2);
        if(`MULSTAGES==0)begin
          result_avail=True;
          if(lv_upperbits)
            default_out=truncateLSB(mult.oP);
          else
            default_out=truncate(mult.oP);
        end
      end
      else begin
        if(|operand2==0)begin
          result_avail=True;
          if(funct3[1]==1)
            default_out=operand1;
        end
        else
          divider.get_inputs(op1, op2, unpack(funct3[1])); // send inputs to the divider
      end
      `ifdef RV64
        if(word32)
          default_out=signExtend(default_out[31:0]);
      `endif
    `ifdef arith_trap
     let is_mul = ~funct3[2];
     if(is_mul==0 && operand2 == 0 && wr_arith_en==1'b1)
      return ALU_OUT{done:True, cmtype :TRAP,aluresult :'b1,effective_addr:?,cause:17,redirect:False};//DIV_BY_ZER0 trap
      else
      `endif
      begin
      if((funct3[2]==0 && `MULSTAGES!=0) || (funct3[2]==1 && `DIVSTAGES!=0) && !result_avail)begin
        rg_count<= rg_count+ 1;
        rg_upperbits<= lv_upperbits;
        rg_complement<= lv_take_complement;
        `ifdef RV64 rg_word32<= word32; `endif
      end
      return ALU_OUT{done : result_avail, cmtype : REGULAR, aluresult : zeroExtend(default_out), 
                      effective_addr:?, cause:?, redirect : False
                    `ifdef bpu , branch_taken: ?, redirect_pc: ? `endif };
      end
    endmethod
		method ActionValue#(ALU_OUT) delayed_output if((rg_count== fromInteger(`MULSTAGES) && !mul_div)
                                            || (rg_count==(fromInteger(`DIVSTAGES)+ 1) && mul_div));
      Bit#(TMul#(2, XLEN)) reslt=mul_div?zeroExtend(divider.quo_rem):mult.oP;
      if( (mul_div && rg_upperbits && rg_complement && reslt[valueOf(XLEN)-1]!=rg_sign_op1) || 
                      (mul_div && rg_complement && !rg_upperbits) ||(!mul_div && rg_complement))
        reslt=~reslt+ 1;
      Bit#(XLEN) product=`ifdef RV64 rg_word32?signExtend(reslt[31:0]): `endif 
          (!mul_div && rg_upperbits)? truncateLSB(reslt): truncate(reslt);
      return ALU_OUT{done : True, cmtype : REGULAR, aluresult : zeroExtend(product), 
                    effective_addr:?, cause:?, redirect : False
                    `ifdef bpu , branch_taken: ?, redirect_pc: ? `endif };
    endmethod



   `ifdef arith_trap
      method  Action rd_arith_excep_en(Bit#(1) arith_en);
      wr_arith_en<=arith_en;
      endmethod
   `endif

	endmodule:mkmuldiv
endpackage:muldiv_fpga

// See LICENSE.iitm for license details
/*
Module Name   	: Sequential Integer multiplier unit
Author's Name 	: IIT Madras

*/

package integermultiplier;
  import DReg::*;
  interface Ifc_integermultiplier#(numeric type regwidth, numeric type loop);
    method ActionValue#(Maybe#(Bit#(TMul#(2,regwidth)))) _start(Bit#(regwidth) inp1, Bit#(regwidth) inp2); //_div_name 00 : DIV/REM 01: DIVU/REMU
  endinterface
  //(*synthesize*)
  module mkintegermultiplier(Ifc_integermultiplier#(regwidth,loop))
     provisos ( Add#(regwidth,1,regwidth1),
                Add#(regwidth,regwidth,regwidth_twice),
                Add#(1,TMul#(2,regwidth),regwidth_twice1),
                Add#(1,TLog#(regwidth),regwidth_log1),
                //per request of bsc
                Add#(regwidth1,regwidth,regwidth_twice1)
              );

    Reg#(Bit#(regwidth_twice1)) partial_prod <-mkReg(0); 
    Reg#(Bit#(regwidth_log1)) rg_state_counter <-mkDReg(0);//Register for state machine counter
    let rEGWIDTH = valueOf(regwidth);
    let lOOP = valueOf(loop); 
    
    method ActionValue#(Maybe#(Bit#(regwidth_twice))) _start(Bit#(regwidth) inp1, Bit#(regwidth) inp2); 
        //`ifdef verbose $display("Taken inputs in multiplier. rs1: %h rs2: %h",inp1,inp2); `endif
      //`ifdef verbose $display("Register State Counter %h", rg_state_counter);`endif
      //`ifdef verbose $display("partial_prod %h", partial_prod);`endif
      if(rg_state_counter==0)begin
	     partial_prod<=zeroExtend(inp2);
         rg_state_counter<=rg_state_counter+1;
         return tagged Invalid;
      end
      else begin
	      Bit#(regwidth) temp=(partial_prod[lOOP-1:0])*inp1[rEGWIDTH-1:0];
	      Bit#(regwidth1) accum=partial_prod[2*rEGWIDTH:rEGWIDTH]+zeroExtend(temp);
          Bit#(regwidth) partial_prod_temp = partial_prod[rEGWIDTH-1:0];
	      Bit#(regwidth_twice1) temp1 ={accum,partial_prod_temp}>>lOOP;
	      //`ifdef verbose $display("multiplication. Partial :%h Counter: %d",temp1,rg_state_counter);`endif
	      if(rg_state_counter==(fromInteger(rEGWIDTH)/fromInteger(lOOP)))begin
	         rg_state_counter<=0;
	         return tagged Valid temp1[2*rEGWIDTH-1:0];
	      end
	      else begin
	         partial_prod<=temp1;
	         rg_state_counter<=rg_state_counter+1;
	         return tagged Invalid;
	      end
	  end
      endmethod
  endmodule

  module mkTb(Empty);
   Ifc_integermultiplier#(8,4) mul <- mkintegermultiplier();
   Reg#(Bit#(8)) inp1 <- mkReg(8'b1100);
   Reg#(Bit#(8)) inp2 <- mkReg(8'b1010);

   rule give_inputs;
   let x <- mul._start(inp1,inp2);
   if(x matches tagged Valid .res) begin
       //`ifdef verbose $display("Output is %b",res);`endif
       $finish(0);
   end
   endrule

   endmodule
endpackage

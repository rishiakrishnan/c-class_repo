// See LICENSE.iitm for license details
/*
Author: IIT Madras
Description: Wrapper for HardFloat ftof Module
*/
package wrapper_ftof;

  import bsvmkftof:: * ;
  import ConfigReg::*;
  import FIFO::*;
  import UniqueWrappers::*;
  import Vector :: * ;
  import DReg :: * ;
  `include "fpu.defines"
  
  interface Ifc_wrapper_ftof#(numeric type expWidthIn, numeric type sigWidthIn, numeric type expWidthOut, numeric type sigWidthOut);
    method Action request(Bit#(TAdd#(expWidthIn, sigWidthIn)) a, Bit#(3) rm);
    method Tuple2#(Bit#(TAdd#(expWidthOut, sigWidthOut)), Bit#(5)) response;
    method Bool resp_valid;
  endinterface
  
  module mkwrapper_ftof(Ifc_wrapper_ftof#(expWidthIn, sigWidthIn, expWidthOut, sigWidthOut));
    Ifc_ftof#(expWidthIn, sigWidthIn, expWidthOut, sigWidthOut) ftof <- mkftof;
    
    Vector#(`ftof_stages, Reg#(Tuple2#(Bit#(TAdd#(expWidthOut, sigWidthOut)), Bit#(5))))  rg_output  <- replicateM(mkReg(unpack(0)));
    Vector#(`ftof_stages, Reg#(Bool))  rg_valid  <- replicateM(mkDReg(False));
   
		if(`ftof_stages > 1) begin
    	rule rl_start;
    	  for(Integer i = 1; i <=  `ftof_stages-1 ; i = i+ 1) begin
    	    rg_output[i] <= rg_output[i - 1];
    	    rg_valid[i] <= rg_valid[i-1];
    	  end   
    	endrule
		end
   
    method Action request(Bit#(TAdd#(expWidthIn, sigWidthIn)) a, Bit#(3) rm);
      ftof.request(1, rm, a);
      rg_output[0] <=  tuple2(ftof.oout,  ftof.oexceptionFlags);
	  $display($time,"\tFTOF: inp: %x out: %x flag: %b", a, ftof.oout, ftof.oexceptionFlags);
      rg_valid[0] <=  True;
    endmethod
    
    method Tuple2#(Bit#(TAdd#(expWidthOut, sigWidthOut)), Bit#(5)) response;
      let {out, flg} = rg_output[`ftof_stages -1];
      //Bit#(5) fnew = {flg[2],1'b0,flg[1],1'b0,flg[0]};
      return tuple2(out, flg);
    endmethod
    
    method Bool resp_valid;
      return rg_valid[`ftof_stages-1];
    endmethod
     
    
  endmodule
endpackage

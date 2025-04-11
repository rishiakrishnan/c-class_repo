// See LICENSE.iitm for license details
/*
Author: IIT Madras
Description: Wrapper for HardFloat ftoi Module
*/
package wrapper_ftoi;

  import bsvmkftoi:: * ;
  import ConfigReg::*;
  import FIFO::*;
  import UniqueWrappers::*;
  import Vector :: * ;
  import DReg :: * ;
  `include "fpu.defines"
  
  interface Ifc_wrapper_ftoi#(numeric type expWidth, numeric type sigWidth, numeric type intWidth);
    method Action request(Bit#(TAdd#(expWidth, sigWidth)) a, Bit#(3) rm, Bit#(1) signedOut, Bit#(1) long);
    method Tuple3#(Bit#(intWidth), Bit#(5), Bit#(2)) response;
    method Bool resp_valid;
  endinterface
  
  module mkwrapper_ftoi(Ifc_wrapper_ftoi#(expWidth, sigWidth, intWidth));
    Ifc_ftoi#(expWidth, sigWidth, intWidth) ftoi <- mkftoi;
    
    Vector#(`ftoi_stages, Reg#(Tuple3#(Bit#(intWidth), Bit#(3), Bit#(2))))  rg_inputs  <- replicateM(mkReg(unpack(0)));
    Vector#(`ftoi_stages, Reg#(Bool))  rg_valid  <- replicateM(mkDReg(False));
   
		if(`ftoi_stages > 1) begin
    	rule rl_start;
    	  for(Integer i = 1; i <=  `ftoi_stages-1 ; i = i+ 1) begin
    	    rg_inputs[i] <= rg_inputs[i - 1];
    	    rg_valid[i] <= rg_valid[i-1];
    	  end   
    	endrule
		end
   
    method Action request(Bit#(TAdd#(expWidth, sigWidth)) a, Bit#(3) rm, Bit#(1) signedOut, Bit#(1) long);
      ftoi.request(1, rm, signedOut, a);
      rg_inputs[0] <=  tuple3(ftoi.oout,  ftoi.oexceptionFlags, {signedOut, long});
			//$display($time,"\tFTOI: inp: %x signedOut: %b long: %b out: %x flag: %b", a, signedOut, ftoi.oout, ftoi.oexceptionFlags);
      rg_valid[0] <=  True;
    endmethod
    
    method Tuple3#(Bit#(intWidth), Bit#(5), Bit#(2)) response;
      let {out, flg, long} = rg_inputs[`ftoi_stages -1];
      Bit#(5) fnew = {flg[2],1'b0,flg[1],1'b0,flg[0]};
      return tuple3(out, fnew, long);
    endmethod
    
    method Bool resp_valid;
      return rg_valid[`ftoi_stages-1];
    endmethod
     
    
  endmodule
endpackage

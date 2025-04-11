// See LICENSE.iitm for license details
/*
Author: IIT Madras
Description: Wrapper for HardFloat FMA Module
*/
package wrapper_fma;

  import bsvmkmulAdd:: * ;
  import ConfigReg::*;
  import FIFO::*;
  import UniqueWrappers::*;
  import Vector :: * ;
  import DReg :: * ;
  `include "fpu.defines"

  interface Ifc_wrapper_fma#(numeric type expWidth, numeric type sigWidth);
    method Action request(Bit#(2) op, Bit#(TAdd#(expWidth, sigWidth)) a, Bit#(TAdd#(expWidth, sigWidth)) b, Bit#(TAdd#(expWidth, sigWidth)) c, Bit#(3) rm);
    method Tuple2#(Bit#(TAdd#(expWidth, sigWidth)), Bit#(5)) response;
    method Bool resp_valid;
  endinterface
  
  module mkwrapper_fma(Ifc_wrapper_fma#(expWidth, sigWidth));
    Ifc_mulAdd#(expWidth, sigWidth) fma <- mkmulAdd;
    
    Vector#(`fma_stages, Reg#(Tuple2#(Bit#(TAdd#(expWidth, sigWidth)), Bit#(5))))  rg_inputs  <- replicateM(mkReg(unpack(0)));
    Vector#(`fma_stages, Reg#(Bool))  rg_valid  <- replicateM(mkDReg(False));
    
    rule rl_start;
      for(Integer i = 1; i <=  `fma_stages-1 ; i = i+ 1) begin
        rg_inputs[i] <= rg_inputs[i - 1];
        rg_valid[i] <= rg_valid[i-1];
      end
      
    endrule
    
    method Action request(Bit#(2) op, Bit#(TAdd#(expWidth, sigWidth)) a, Bit#(TAdd#(expWidth, sigWidth)) b, Bit#(TAdd#(expWidth, sigWidth)) c, Bit#(3) rm);
      fma.request(1, op, rm, a, b, c);
			$display("FMA wrapper: a: %x b: %x c: %x op: %b rm: %b res: %x flag: %b", a, b, c, op, rm, fma.oout, fma.oexceptionFlags);
      rg_inputs[0] <=  tuple2(fma.oout,  fma.oexceptionFlags);
      rg_valid[0] <=  True;
    endmethod
    
    method Tuple2#(Bit#(TAdd#(expWidth, sigWidth)), Bit#(5)) response;
      return rg_inputs[`fma_stages -1];
    endmethod
    
    method Bool resp_valid;
      return rg_valid[`fma_stages-1];
    endmethod
    
  endmodule
endpackage

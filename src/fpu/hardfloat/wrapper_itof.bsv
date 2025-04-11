// See LICENSE.iitm for license details
/*
Author: IIT Madras
Description: Wrapper for HardFloat itof Module
*/
package wrapper_itof;

  import bsvmkitof:: * ;
  import ConfigReg::*;
  import FIFO::*;
  import UniqueWrappers::*;
  import Vector :: * ;
  import DReg :: * ;
  `include "fpu.defines"

  interface Ifc_wrapper_itof#(numeric type expWidth, numeric type sigWidth, numeric type intWidth);
    method Action request(Bit#(intWidth) a, Bit#(3) rm, Bit#(1) signedOut);
    method Tuple2#(Bit#(TAdd#(expWidth, sigWidth)), Bit#(5)) response;
    method Bool resp_valid;
  endinterface
  
  module mkwrapper_itof(Ifc_wrapper_itof#(expWidth, sigWidth, intWidth));
    Ifc_itof#(expWidth, sigWidth, intWidth) itof <- mkitof;
    
    Vector#(`itof_stages, Reg#(Tuple2#(Bit#(TAdd#(expWidth, sigWidth)), Bit#(5))))  rg_inputs  <- replicateM(mkReg(unpack(0)));
    Vector#(`itof_stages, Reg#(Bool))  rg_valid  <- replicateM(mkDReg(False));
    
    rule rl_start;
      for(Integer i = 1; i <=  `itof_stages-1 ; i = i+ 1) begin
        rg_inputs[i] <= rg_inputs[i - 1];
        rg_valid[i] <= rg_valid[i-1];
      end
    endrule
    
    method Action request(Bit#(intWidth) a, Bit#(3) rm, Bit#(1) signedOut);
      itof.request(1, rm, signedOut, a);
      rg_inputs[0] <=  tuple2(itof.oout,  itof.oexceptionFlags);
      rg_valid[0] <=  True;
    endmethod
    
    method Tuple2#(Bit#(TAdd#(expWidth, sigWidth)), Bit#(5)) response;
      return rg_inputs[`itof_stages -1];
    endmethod
    
    method Bool resp_valid;
      return rg_valid[`ftoi_stages-1];
    endmethod
    
  endmodule
endpackage

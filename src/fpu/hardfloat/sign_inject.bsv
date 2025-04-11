// See LICENSE.iitm for license details
/*
Author: IIT Madras
Description: Sign Injection Module
*/

package sign_inject;
  
  interface Ifc_sign_inject#(numeric type expWidth, numeric type sigWidth);
    method Bit#(TAdd#(expWidth, sigWidth)) request(Bit#(TAdd#(expWidth, sigWidth)) a, Bit#(1) s_b, Bit#(3) operation);
  endinterface
  
  module mksign_inject(Ifc_sign_inject#(expWidth, sigWidth));
    
    method Bit#(TAdd#(expWidth, sigWidth)) request(Bit#(TAdd#(expWidth, sigWidth)) a, Bit#(1) s_b, Bit#(3) operation);
      let f = valueOf(expWidth) + valueOf(sigWidth);
      if(operation == 3'b000) //FSGNJ
	      a[f-1] = s_b;
	    else if(operation == 3'b001) //FSNGNJN
	      a[f-1] = ~s_b;
	    else //FSGNJX
	      a[f-1] = a[f-1]^s_b;
      
      return a;
    
    endmethod
    
  endmodule

endpackage

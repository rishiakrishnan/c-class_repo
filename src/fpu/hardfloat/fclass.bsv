// See LICENSE.iitm for license details
/*
Author: IIT Madras
Description: Classify Module
*/

package fclass;

  function Bit#(5) condFlags(Bit#(expWidth) exponent, Bit#(sigWidth) mantissa);
  
    let s = valueOf(sigWidth);   
    Bit#(5) flags;
     
    bit man0 = |mantissa;
    bit exp0 = |exponent;
    bit exp1 = &exponent;
    
    Bool expZ = (exp0 == 0);
    Bool manZ = (man0  == 0);
    Bool expOne = (exp1 == '1);
    Bool topB = (mantissa[s-1] == 1);
    
    flags = {pack(expZ && !manZ),pack(expZ && manZ),pack(expOne && topB),pack(expOne && manZ),pack(expOne && !topB && !manZ)}; //Denormal, isZero, QNaN, Infinity, SNaN
    return flags;
    
  endfunction
  
  interface Ifc_fclass#(numeric type expWidth, numeric type sigWidth);
    method Bit#(TAdd#(expWidth, sigWidth)) request(Bit#(TAdd#(expWidth, sigWidth)) a);
  endinterface
  
  module mkfclass(Ifc_fclass#(expWidth, sigWidth));
    
    method Bit#(TAdd#(expWidth, sigWidth)) request(Bit#(TAdd#(expWidth, sigWidth)) a);
    
      Bit#(TAdd#(expWidth, sigWidth)) fclass = 0;
      
      let e = valueOf(expWidth);
      let s = valueOf(sigWidth);
      
      Bit#(expWidth) exponent = a[e+s-2:s-1];
      Bit#(TSub#(sigWidth,1)) mantissa = a[s-2:0];      
      
      Bit#(5) flags = condFlags(exponent, mantissa);
      Bool sbit = (a[e+s-1] == 1);
      Bool inf  = (flags[1] == 1);
      Bool normal = (flags == '0);
      Bool subnormal = (flags[4] == 1);
      Bool zero    = (flags[3] == 1);
		  
		  if(sbit && inf) fclass[0] = 'b1;  //negtive infinity
		  else if(sbit && normal) fclass[1] = 'b1; //negative normal
		  else if(sbit && subnormal) fclass[2] = 'b1; //negative subnormal
		  else if(sbit && zero) fclass[3] = 'b1; //-0
		  else if(!sbit && zero) fclass[4] = 'b1; // +0
		  else if( !sbit && subnormal) fclass[5] = 'b1; //positive subnormal
		  else if(!sbit && normal) fclass[6] = 'b1; //positive normal
		  else if(!sbit && inf) fclass[7] = 'b1; //positive infinity
		  else if(flags[0]==1) fclass[8] = 'b1; //Signaling NaN
		  else if(flags[2]==1) fclass[9] = 'b1; //quiet NaN
		  else fclass = 0;
		  
		  return fclass;
    endmethod
    
  endmodule

endpackage

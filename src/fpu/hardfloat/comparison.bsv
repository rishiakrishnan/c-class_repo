// See LICENSE.iitm for license details
/*
Author: IIT Madras
Description: Floating point comparison Module
*/

package comparison;

  
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
    
    flags = {pack(expZ && !manZ),pack(expZ && manZ),pack(expOne && topB),pack(expOne && manZ),pack(expOne && !topB && !manZ)}; 
    //Denormal, isZero, QNaN, Infinity, SNaN
    return flags;
    
  endfunction
  
  function Bit#(2) fn_comparator(bit sign1, Bit#(expWidth) exponent1, Bit#(sigWidth) mantissa1, bit sign2, Bit#(expWidth) exponent2, Bit#(sigWidth) mantissa2);

	  Bit#(2) magnitude;  //01: inp2 > inp1, 10: inp1 > inp2, 11: means inp2 = inp1
	  if(exponent1<exponent2)
		  magnitude= 2'b01;
	  else if(exponent1==exponent2)
	  begin
		  if(mantissa1<mantissa2)
			  magnitude = 2'b01;
		  else if(mantissa1==mantissa2)
			  magnitude = 2'b11;
		  else magnitude = 2'b10;
	  end
	  else
		  magnitude = 2'b10;

	  if(sign1==0) begin
		  if(sign2==1)
			  return 2'b10;
		  else 
			  return magnitude;
	  end
	  else begin
		  if(sign2==1)
			  return {magnitude[0],magnitude[1]};
		  else
			  return 2'b01;
	  end
  endfunction
  
  interface Ifc_comparison#(numeric type expWidth, numeric type sigWidth);
    method ActionValue#(Tuple3#(Bool, Bit#(TAdd#(expWidth, sigWidth)), Bit#(5))) request(Bit#(TAdd#(expWidth, sigWidth)) a, Bit#(TAdd#(expWidth, sigWidth)) b, Bit#(3) which_cmp_instr, bit cmp_or_min_max);
  endinterface
  
  module mkcomparison(Ifc_comparison#(expWidth, sigWidth))
    provisos (Add#(1, a__, TAdd#(expWidth, sigWidth)),
              Add#(expWidth, b__, a__))
              ;
    
    method ActionValue#(Tuple3#(Bool, Bit#(TAdd#(expWidth, sigWidth)), Bit#(5))) request(Bit#(TAdd#(expWidth, sigWidth)) a, Bit#(TAdd#(expWidth, sigWidth)) b, Bit#(3) which_cmp_instr, bit cmp_or_min_max);
    
      let e = valueOf(expWidth);
      let s = valueOf(sigWidth);
      
      Bit#(expWidth) exponent1 = a[e+s-2:s-1];
      Bit#(TSub#(sigWidth,1)) mantissa1 = a[s-2:0];
      
      Bit#(expWidth) exponent2 = b[e+s-2:s-1];
      Bit#(TSub#(sigWidth,1)) mantissa2 = b[s-2:0]; 
      
      Bit#(5) flags1 = condFlags(exponent1, mantissa1);
      Bit#(5) flags2 = condFlags(exponent2, mantissa2);
      
      bit sign1 = a[e+s-1];
      bit sign2 = b[e+s-1];
 	    
 	    Bit#(5) lv_exception = 0;
 	    Bit#(TAdd#(expWidth, sigWidth)) lv_result = 0;
	    
	    bit lv_invalid = 0;
		  bit lv_zero = 0;

      bit lv_op1_is_qNan=flags1[2];
      bit lv_op2_is_qNan=flags2[2];
   	  bit lv_op1_is_sNan=flags1[0];
      bit lv_op2_is_sNan=flags2[0];
      bit lv_op1_Nan=lv_op1_is_qNan | lv_op1_is_sNan;
      bit lv_op2_Nan=lv_op2_is_qNan | lv_op2_is_sNan;
      bit or_sign = sign1 | sign2;
	 	  bit and_sign = sign1 & sign2;

     	if(lv_op1_is_qNan==1 || lv_op1_is_sNan==1 || lv_op2_is_qNan==1 || lv_op2_is_sNan==1)
        lv_invalid=1;

		  if(flags1[3]==1 && flags2[3]==1)
			  lv_zero = 1;
    
		  Bit#(2) lv_compare_result = fn_comparator(sign1,exponent1,mantissa1,sign2,exponent2,mantissa2);
            
			if(cmp_or_min_max=='b0) begin //compare instruction
    		
    		if(lv_invalid==1)begin          
          if(which_cmp_instr!=3'b010)
    			  lv_exception[4] = 1;
          else if((flags1[0] | flags2[0])==1)
            lv_exception[4] = 1;
					if((lv_op1_is_sNan | lv_op2_is_sNan)==1)
						lv_result=0;
          if(((lv_op1_is_qNan | lv_op2_is_qNan)==1) && which_cmp_instr!=3'b010)
            lv_result=0;
    		end
				
				else if(which_cmp_instr==3'b010) begin
					if(lv_compare_result==2'b11 || lv_zero==1)
						lv_result[0]=1;
				end
				
				else if(which_cmp_instr==3'b001) begin
					if(lv_compare_result==2'b01 && lv_zero==0)
						lv_result[0]=1;
				end
				
				else if(which_cmp_instr==3'b000) begin
					if(lv_compare_result[0]==1'b1 || lv_zero==1)
						lv_result[0]=1;
				end
				
			end
			
			else begin // min max instruction
      
        Bit#(expWidth) exp_all_ones = '1;
        Bit#(TSub#(sigWidth,2)) man_all_zeros = '0;
        if(lv_op1_is_sNan==1 || lv_op2_is_sNan==1)
          lv_exception[4] = 1;
        if((lv_op1_Nan&lv_op2_Nan)==1) begin
          lv_result = {1'b0, exp_all_ones, 1'b1, man_all_zeros};
        end
        else if(lv_op1_Nan==1) begin
          lv_result=b;
        end
        else if(lv_op2_Nan==1) begin
          lv_result=a;
        end
		    else if((which_cmp_instr[0]==0 && lv_zero==1))
		      lv_result= {or_sign, b[e+s-2:0]};
		    else if((which_cmp_instr[0]==1 && lv_zero==1))
		      lv_result= {and_sign, a[e+s-2:0]};
		    else if((which_cmp_instr[0]==0 && lv_compare_result==2'b01) || (which_cmp_instr[0]==1 && lv_compare_result==2'b10))
		      lv_result= a;
		    else
			    lv_result= b;
		  end
		  
		  return tuple3(True, lv_result, lv_exception);
    
    endmethod
    
  endmodule

endpackage

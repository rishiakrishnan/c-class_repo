// See LICENSE.iitm for license details
package tb_fpu;

import RegFile::*;
import fpu::*;

module mktb_fpu1();
	
	Ifc_fpu fpu <- mk_fpu();	
	Reg#(int) cycle <- mkReg(0);
	
	rule count;
		cycle<=cycle+1;
	endrule
	
	rule give_input(cycle==10);
		Bit#(32) op1 = 'h40800000;
		Bit#(32) op2 = 'h40400000;
		Bit#(32) op3 = 'h40800000;
		Bit#(32) ones = '1;
		Bit#(32) zeros = 0;
		$display($time,"\tcycle: %0d op1:%h",cycle,op1[31:0]);
		$display($time,"\tcycle: %0d op2:%h",cycle,op2[31:0]);
		$display($time,"\tcycle: %0d op3:%h",cycle,op3[31:0]);
		
		//fpu.req(op1,op2,op3,4'b0100,7'b0000000,0,0,0,True); //Add
		//fpu.req(op1,op2,op3,4'b0100,7'b0000100,0,0,0,True); //Sub
		//fpu.req(op1,op2,op3,4'b0100,7'b0001000,0,0,0,True); //Mul
		//fpu.req(op1,op2,op3,4'b0000,7'b0,0,0,0,True); //fma	
		//fpu.req(op1,op2,op3,4'b0001,7'b0000000,0,0,0,True); //fmsub
		//fpu.req(op1,op2,op3,4'b0010,7'b0000000,0,0,0,True); //fnmsub
		//fpu.req(op1,op2,op3,4'b0011,7'b0000000,0,0,0,True); //fnmadd
		//fpu.req(op1,op2,op3,4'b0100,7'b0001100,0,0,0,True); //Div
		fpu.req(op1,op2,op3,4'b0100,7'b0101100,0,0,0,True); //Sqrt		
		//fpu.req(op1,op2,op3,4'b0100,7'b1010000,2,0,0,True); //FCMP_f5 (FEQ.S, FLT.S, FLE.S)
		//fpu.req(op1,op2,op3,4'b0100,7'b0010100,1,0,0,True); //FMMAX_f5 (FMIN.S, FMAX.S)
		//fpu.req(op1,op2,op3,4'b0100,7'b1101000,0,2'b00,0,True); //FCVT_F_I_f5 int to float
    //fpu.req(op1,op2,op3,4'b0100,7'b1101000,0,2'b01,0,True); //FCVT_F_I_f5 int to float		
		//fpu.req(op1,op2,op3,4'b0100,7'b0010000,2,0,0,True); //FSGNJN_f5
		//fpu.req(op1,op2,op3,4'b0100,7'b1100000,0,2'b01,0,True); //FCVT_I_F_f5 float to int
		//fpu.req(op1,op2,op3,4'b0100,7'b0100000,0,0,0,True); //FCVT_S_D_f5 single to double
		//fpu.req(op1,op2,op3,4'b0100,7'b1110000,0,0,0,True); //FMV_X_S_f7
		//fpu.req(op1,op2,op3,4'b0100,7'b1111000,0,0,0,True); //FMV_S_X_f7
		//fpu.req(op1,op2,op3,4'b0100,7'b1110001,0,0,0,True); //FMV_X_D_f7
		//fpu.req(op1,op2,op3,4'b0100,7'b1111001,0,0,0,True); //FMV_D_X_f7
		
	endrule
	
	rule get_output(cycle>10);
		let {valid, result, flags} = fpu.resp();
		if(valid) begin
		  $display($time,"\tcycle: %0d Result: %h Flags: %b",cycle,result, flags);
		  $finish;
		end
	endrule
	
endmodule

//module mktb_fpu(); //bulk mode test
//
//	Ifc_fpu fpu <- mk_fpu();
//	Reg#(int) count <- mkReg(0);
//	Reg#(Bit#(16)) index <- mkReg(0);
//	Reg#(Bit#(16)) out_index <- mkReg(0);
//	RegFile#(Bit#(16),Bit#(96))  input_data <- mkRegFileFullLoad("random-tests/random_hex");
//	let write_file <- mkReg(InvalidFile);
//	
//	rule open_file(count==0);
//		
//		File wfile <- $fopen( "random-tests/ftest","w");
//		if ( wfile == InvalidFile ) begin
//			$display("cannot open write file");
//			$finish(0);
//		end
//		write_file <= wfile;
//		count <= 1;
//	endrule
//	
//	rule give_input(count ==1 && index <= 10);
//		//count<=2;
//		let in = index;
//		Bit#(32) op1 = input_data.sub(in)[95:64];
//		Bit#(32) op2 = input_data.sub(in)[63:32];
//		Bit#(32) op3 = input_data.sub(in)[31:0];
//		Bit#(32) ones = '1;
//		`ifdef verbose $display("op1:%b",op1); `endif
//		`ifdef verbose $display("op2:%b",op2); `endif
//		`ifdef verbose $display("op3:%b",op3); `endif
//	
//		//fpu.req(op1,op2,op3,4'b0100,7'b0000000,0,0,0,True); //Add
//		//fpu.req(op1,op2,op3,4'b0100,7'b0000100,0,0,0,True); //Sub
//		//fpu.req(op1,op2,op3,4'b0100,7'b0001000,0,0,0,True); //Mul
//		//fpu.req(op1,op2,op3,4'b0000,7'b0001000,0,0,0,True); //fma		
//		//fpu.req(op1,op2,op3,4'b0001,7'b0000000,0,0,0,True); //fmsub
//		//fpu.req(op1,op2,op3,4'b0010,7'b0000000,0,0,0,True); //fnmsub
//		//fpu.req(op1,op2,op3,4'b0011,7'b0000000,0,0,0,True); //fnmadd
//		//fpu.req(op1,op2,op3,4'b0100,7'b0001100,0,0,0,True); //Div
//		fpu.req(op1,op2,op3,4'b0100,7'b0101100,0,0,0,True); //Sqrt
//		index <= index + 1;
//	endrule
//	
//	
//	rule get_output;//(count>=1);
//		let {valid, result, flags} = fpu.resp();
//		if(valid) begin
//		  //count<=1;
//		  $display($time,"\tResult: %h Flags: %b",result, flags);
//		  $fwrite(write_file,"%h %b\n", result, flags);
//		  out_index <= out_index + 1;
//	  	if(out_index == 10) begin $fclose(write_file);	$finish; end
//		end
//	endrule
//	
//endmodule

endpackage

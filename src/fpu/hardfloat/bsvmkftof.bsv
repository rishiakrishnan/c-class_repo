// See LICENSE.iitm for license details

interface Ifc_ftof#(numeric type expWidthIn, numeric type sigWidthIn, numeric type expWidthOut, numeric type sigWidthOut);
	(*always_enabled*)
	method Bit#(TAdd#(expWidthOut, sigWidthOut)) oout ();
	(*always_enabled*)
	method Bit#(5) oexceptionFlags ();
	(*always_ready , always_enabled*)
	method Action request (Bit#(1) control, Bit#(3) roundingmode, Bit#(TAdd#(expWidthIn, sigWidthIn)) a);
endinterface

import "BVI" ftof =
module mkftof (Ifc_ftof#(expWidthIn, sigWidthIn, expWidthOut, sigWidthOut));

	default_clock clk_clk;
	default_reset rst;
	
	parameter expWidthIn  = valueOf(expWidthIn);
  parameter sigWidthIn  = valueOf(sigWidthIn);
	parameter expWidthOut = valueOf(expWidthOut);
  parameter sigWidthOut = valueOf(sigWidthOut);

	input_clock clk_clk (clk,gate)  <- exposeCurrentClock;
	input_reset rst (/* empty */) clocked_by(clk_clk)  <- exposeCurrentReset;


	method out /* (expWidthOut+sigWidthOut-1) : 0 */ oout ()
		 clocked_by(clk_clk) reset_by(rst);
	method exceptionFlags /* 4 : 0 */ oexceptionFlags ()
		 clocked_by(clk_clk) reset_by(rst);
	method request (control , roundingMode /*2:0*/, a /*(expWidthIn+sigWidthIn-1):0*/)
		 enable((*inhigh*)request_enable) clocked_by(clk_clk) reset_by(rst);

	schedule oout CF oout;
	schedule oout CF oexceptionFlags;
	schedule oout SB request;
	schedule oexceptionFlags CF oexceptionFlags;
	schedule oexceptionFlags SB request;
	schedule request C request;
endmodule



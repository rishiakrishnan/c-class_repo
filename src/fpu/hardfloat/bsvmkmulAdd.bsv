// See LICENSE.iitm for license details
// Bluespec wrapper, created by Import BVI Wizard
// Created on: Fri Dec 27 18:28:43 IST 2019
// Created by: IIT Madras
// Bluespec version: 2019.05.beta2 2019-05-24 a88bf40db


interface Ifc_mulAdd#(numeric type expWidth, numeric type sigWidth);
	// (*always_enabled*)
	method Bit#(TAdd#(expWidth, sigWidth)) oout ();
	// (*always_enabled*)
	method Bit#(5) oexceptionFlags ();
	// (*always_ready*)
	method Action request (Bit#(1) control, Bit#(2) op, Bit#(3) roundingmode, Bit#(TAdd#(expWidth, sigWidth)) a, Bit#(TAdd#(expWidth, sigWidth)) b, Bit#(TAdd#(expWidth, sigWidth)) c);
endinterface

import "BVI" mulAdd =
module mkmulAdd  (Ifc_mulAdd#(expWidth, sigWidth));

	default_clock clk_clk;
	default_reset rst;
	
	parameter expWidth = valueOf(expWidth);
  parameter sigWidth = valueOf(sigWidth);

	input_clock clk_clk (clk,gate)  <- exposeCurrentClock;
	input_reset rst (/* empty */) clocked_by(clk_clk)  <- exposeCurrentReset;


	method out /* (expWidth + sigWidth-1) : 0 */ oout ()
		 clocked_by(clk_clk) reset_by(rst);
	method exceptionFlags /* 4 : 0 */ oexceptionFlags ()
		 clocked_by(clk_clk) reset_by(rst);
	method request (control , op /*1:0*/, roundingMode /*2:0*/, a /*(expWidth+sigWidth-1):0*/, b /*(expWidth+sigWidth-1):0*/, c /*(expWidth+sigWidth-1):0*/)
		 enable((*inhigh*)request_enable) clocked_by(clk_clk) reset_by(rst);

	schedule oout CF oout;
	schedule oout CF oexceptionFlags;
	schedule oout SB request;
	schedule oexceptionFlags CF oexceptionFlags;
	schedule oexceptionFlags SB request;
	schedule request C request;
endmodule



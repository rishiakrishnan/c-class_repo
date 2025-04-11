// See LICENSE.iitm for license details
// Bluespec wrapper, created by Import BVI Wizard
// Created on: Fri Dec 27 22:05:26 IST 2019
// Created by: IIT Madras
// Bluespec version: 2019.05.beta2 2019-05-24 a88bf40db


interface Ifc_ftoi#(numeric type expWidth, numeric type sigWidth, numeric type intWidth);
	(*always_enabled*)
	method Bit#(intWidth) oout ();
	(*always_enabled*)
	method Bit#(3) oexceptionFlags ();
	(*always_ready , always_enabled*)
	method Action request (Bit#(1) control, Bit#(3) roundingmode, Bit#(1) signedout, Bit#(TAdd#(expWidth, sigWidth)) a);
endinterface

import "BVI" ftoi =
module mkftoi  (Ifc_ftoi#(expWidth, sigWidth, intWidth));

	default_clock clk_clk;
	default_reset rst;
	
	parameter expWidth = valueOf(expWidth);
  parameter sigWidth = valueOf(sigWidth);
  parameter intWidth = valueOf(intWidth);

	input_clock clk_clk (clk,gate)  <- exposeCurrentClock;
	input_reset rst (/* empty */) clocked_by(clk_clk)  <- exposeCurrentReset;


	method out /* (intWidth-1) : 0 */ oout ()
		 clocked_by(clk_clk) reset_by(rst);
	method exceptionFlags /* 3 : 0 */ oexceptionFlags ()
		 clocked_by(clk_clk) reset_by(rst);
	method request (control , roundingMode /*2:0*/, signedOut , a /*(expWidth+sigWidth-1):0*/)
		 enable((*inhigh*)request_enable) clocked_by(clk_clk) reset_by(rst);

	schedule oout CF oout;
	schedule oout CF oexceptionFlags;
	schedule oout SB request;
	schedule oexceptionFlags CF oexceptionFlags;
	schedule oexceptionFlags SB request;
	schedule request C request;
endmodule



// See LICENSE.iitm for license details

// Bluespec wrapper, created by Import BVI Wizard
// Created on: Fri Dec 27 19:08:13 IST 2019
// Created by: IIT Madras
// Bluespec version: 2019.05.beta2 2019-05-24 a88bf40db


interface Ifc_div_sqrt#(numeric type expWidth, numeric type sigWidth);
	(*always_enabled*)
	method Bool oinReady ();
	(*always_enabled*)
	method Bool ooutValid ();
	(*always_enabled*)
	method Bool osqrtOpOut ();
	(*always_enabled*)
	method Bit#(TAdd#(expWidth, sigWidth)) oout ();
	(*always_enabled*)
	method Bit#(5) oexceptionFlags ();
	method Action request (Bit#(1) control, Bit#(1) invalid, Bit#(1) sqrtop, Bit#(3) roundingmode, Bit#(TAdd#(expWidth, sigWidth)) a, Bit#(TAdd#(expWidth, sigWidth)) b);
endinterface

import "BVI" div_sqrt =
module mkdiv_sqrt  (Ifc_div_sqrt#(expWidth, sigWidth));

	default_clock clk_clock;
	default_reset rst_nReset;
	
	parameter expWidth = valueOf(expWidth);
  parameter sigWidth = valueOf(sigWidth);

	input_clock clk_clock (clock,gate)  <- exposeCurrentClock;
	input_reset rst_nReset (nReset) clocked_by(clk_clock)  <- exposeCurrentReset;


	method inReady oinReady ()
		 clocked_by(clk_clock) reset_by(rst_nReset);
	method outValid ooutValid ()
		 clocked_by(clk_clock) reset_by(rst_nReset);
	method sqrtOpOut osqrtOpOut ()
		 clocked_by(clk_clock) reset_by(rst_nReset);
	method out /* (expWidth + sigWidth-1) : 0 */ oout ()
		 clocked_by(clk_clock) reset_by(rst_nReset);
	method exceptionFlags /* 4 : 0 */ oexceptionFlags ()
		 clocked_by(clk_clock) reset_by(rst_nReset);
	method request (control /*0:0*/, inValid /*0:0*/, sqrtOp /*0:0*/, roundingMode /*2:0*/, a /*(expWidth+sigWidth-1):0*/, b /*(expWidth+sigWidth-1):0*/)
		 enable((*inhigh*)request_enable) clocked_by(clk_clock) reset_by(rst_nReset);

	schedule oinReady CF oinReady;
	schedule oinReady CF ooutValid;
	schedule oinReady CF osqrtOpOut;
	schedule oinReady CF oout;
	schedule oinReady CF oexceptionFlags;
	schedule oinReady SB request;
	schedule ooutValid CF ooutValid;
	schedule ooutValid CF osqrtOpOut;
	schedule ooutValid CF oout;
	schedule ooutValid CF oexceptionFlags;
	schedule ooutValid SB request;
	schedule osqrtOpOut CF osqrtOpOut;
	schedule osqrtOpOut CF oout;
	schedule osqrtOpOut CF oexceptionFlags;
	schedule osqrtOpOut SB request;
	schedule oout CF oout;
	schedule oout CF oexceptionFlags;
	schedule oout SB request;
	schedule oexceptionFlags CF oexceptionFlags;
	schedule oexceptionFlags SB request;
	schedule request C request;
endmodule



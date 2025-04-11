// See LICENSE.iitm for license details
/*
Author: IIT Madras
Description: Int to Float Module using Hardfloat
*/

module itof#(parameter expWidth = 3, parameter sigWidth = 3, parameter intWidth = 1) (
        clk,
	gate,
        control,
        a,
        roundingMode,
        signedOut,
        out,
        exceptionFlags
    );
    
    input clk;
    input gate;
    input control;
    input [intWidth-1:0] a;
    input [2:0] roundingMode;
    input signedOut;
    output [(expWidth + sigWidth-1):0] out;
    output [4:0] exceptionFlags;
    
    wire [(expWidth + sigWidth):0] recOut;

    iNToRecFN#(intWidth, expWidth, sigWidth) iNToRecFN(control, signedOut, a, roundingMode, recOut, exceptionFlags);
    
    recFNToFN#(expWidth, sigWidth) recFNToFN(recOut, out);
    
endmodule

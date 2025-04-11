// See LICENSE.iitm for license details
/*
Author: IIT Madras 
Description: Float to Float Module using Hardfloat
*/

module ftof#(parameter expWidthIn = 3, parameter sigWidthIn = 3, parameter expWidthOut = 3, parameter sigWidthOut = 3) (
        clk,
        control,
        a,
        roundingMode,
        signedOut,
        out,
        exceptionFlags
    );
    
    input clk;
    input control;
    input [(expWidthIn + sigWidthIn -1):0] a;
    input [2:0] roundingMode;
    input signedOut;
    output [(expWidthOut + sigWidthOut -1):0] out;
    output [4:0] exceptionFlags;
    
    wire [(expWidthIn + sigWidthIn):0] recA;
    wire [(expWidthOut + sigWidthOut):0] recOut;
    
    fNToRecFN#(expWidthIn, sigWidthIn) fNToRecFN_a(a, recA);

		recFNToRecFN#(expWidthIn, sigWidthIn, expWidthOut, sigWidthOut) recFN_to_RecFN (control, recA, roundingMode, recOut, exceptionFlags);

		recFNToFN#(expWidthOut, sigWidthOut) recFN_to_FN (recOut, out);
    
endmodule

package fpu;

`ifdef hardfloat 
	import fpu_hardfloat::*;
`else
	import fpu_bsvfloat::*;
`endif

export Ifc_fpu;
export mkfpu;
`ifdef fpu_clockgate
(*synthesize,gate_all_clocks*)
`else
(*synthesize*)
`endif
module mkfpu(Ifc_fpu);
	let ifc();
	`ifdef hardfloat
	mkfpu_hardfloat fpu(ifc);
	`else
	mkfpu_bsvfloat fpu(ifc);
	`endif

	return (ifc);
endmodule

endpackage

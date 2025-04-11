// See LICENSE.iitm for license details
/*

Author: IIT Madras
Details:

--------------------------------------------------------------------------------------------------
*/
package fwding;
  import ccore_types::*;
  import GetPut::*;
  import BUtils::*;
  `include "Logger.bsv"

  interface Ifc_fwding;
    (*always_ready, always_enabled*)
    method Action ma_bypass_youngest (FwdType fwd);
    (*always_ready, always_enabled*)
    method Action ma_bypass_youngestp1 (FwdType fwd);
    (*always_enabled, always_ready*)
    /*doc:method: This method receives the current scoreboard datastructure */
    method Action ma_scoreboard (SBD sbd);

    method ActionValue#(Tuple2#(Bool,Bit#(ELEN))) read_rs1(Bit#(ELEN) val, Bit#(5) addr
                                                  `ifdef spfpu , RFType rftype `endif );
    method ActionValue#(Tuple2#(Bool,Bit#(ELEN))) read_rs2(Bit#(ELEN) val, Bit#(5) addr
                                                  `ifdef spfpu , RFType rftype `endif );
  `ifdef spfpu
    method ActionValue#(Tuple2#(Bool,Bit#(ELEN))) read_rs3(Bit#(ELEN) val, Bit#(5) addr, RFType rftype);
  `endif
  endinterface

  function Tuple2#(Bool, Bit#(ELEN)) forward_data (Bit#(ELEN) rfval, Bit#(5) addr,
                                                Bit#(1) sb_lock,
                                              `ifdef spfpu  RFType rftype, `endif
                                                FwdType youngest,
                                                FwdType youngestp1);
    Bit#(1) choose_y = pack ( youngest.valid && youngest.addr == addr 
                      `ifdef spfpu && youngest.rftype == rftype `endif );
    Bit#(1) choose_yp1 =  pack(youngestp1.valid && youngestp1.addr == addr 
                      `ifdef spfpu && youngestp1.rftype == rftype `endif );
    Bool raw_stall = sb_lock==1;
    case ({choose_y, choose_yp1}) matches
      'b1?: return tuple2(youngest.available, youngest.data);
      'b01: return tuple2(youngestp1.available, youngestp1.data);
      default : return tuple2(!raw_stall, rfval);
    endcase
    /*if (youngest.valid && youngest.addr == addr `ifdef spfpu && youngest.rftype == rftype `endif )
      return tuple2(youngest.available, youngest.data);
    else if (youngestp1.valid && youngestp1.addr == addr
                                              `ifdef spfpu && youngestp1.rftype == rftype `endif )
      return tuple2(youngestp1.available, youngestp1.data);
    else
      return tuple2(True, rfval);*/
  endfunction

  (*synthesize*)
  module mkfwding#(parameter Bit#(XLEN) hartid) (Ifc_fwding);
    Wire#(FwdType) wr_from_pipe3        <- mkWire();
    Wire#(FwdType) wr_from_pipe4_first  <- mkWire();
    /*doc:wire: */
    Wire#(SBD) wr_sboard <- mkWire();
    method ActionValue#(Tuple2#(Bool,Bit#(ELEN))) read_rs1(Bit#(ELEN) val, Bit#(5) addr
      `ifdef spfpu , RFType rftype `endif );
      Bit#(1) sb_lock = `ifdef spfpu rftype==FRF?wr_sboard.frf_board[addr]: `endif wr_sboard.irf_board[addr];
      return forward_data(val, addr, sb_lock, `ifdef spfpu rftype, `endif  wr_from_pipe3,
                                                                           wr_from_pipe4_first);
    endmethod
    method ActionValue#(Tuple2#(Bool,Bit#(ELEN))) read_rs2(Bit#(ELEN) val, Bit#(5) addr
                                                              `ifdef spfpu , RFType rftype `endif );
      Bit#(1) sb_lock = `ifdef spfpu rftype==FRF?wr_sboard.frf_board[addr]: `endif wr_sboard.irf_board[addr];
      return forward_data(val, addr, sb_lock, `ifdef spfpu rftype, `endif  wr_from_pipe3,
                                                                  wr_from_pipe4_first);
    endmethod
  `ifdef spfpu
    method ActionValue#(Tuple2#(Bool,Bit#(ELEN))) read_rs3(Bit#(ELEN) val, Bit#(5) addr
                                                              `ifdef spfpu , RFType rftype `endif );
      Bit#(1) sb_lock = rftype==FRF?wr_sboard.frf_board[addr]: wr_sboard.irf_board[addr];
      return forward_data(val, addr, sb_lock, `ifdef spfpu rftype, `endif  wr_from_pipe3,
                                                                  wr_from_pipe4_first);
    endmethod
  `endif
    method Action ma_bypass_youngest (FwdType fwd);
      `logLevel( fwding, 2, $format("[%2d]FWDING: from PIPE3: ",hartid,fshow(fwd)))
      wr_from_pipe3 <= fwd;
    endmethod
    method Action ma_bypass_youngestp1 (FwdType fwd);
      `logLevel( fwding, 2, $format("[%2d]FWDING: from PIPE4-first: ",hartid,fshow(fwd)))
      wr_from_pipe4_first<= fwd;
    endmethod
    method Action ma_scoreboard (SBD sbd);
      wr_sboard <= sbd;
    endmethod
  endmodule
endpackage

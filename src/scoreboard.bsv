// See LICENSE.iitm for license details
/*
Author: IIT Madras
Created on: Friday 18 June 2021 12:34:39 PM

*/
/*doc:overview
This module implements the scoreboard which currently simply implements a single bit per register
indicating if there exists an instruction in the pipeline which has that particular register as its
destination register.

*/
package scoreboard ;
  import FIFOF        :: * ;
  import Vector       :: * ;
`ifdef async_rst
  import SpecialFIFOs_Modified :: * ;
`else
  import SpecialFIFOs :: * ;
`endif
  import FIFOF        :: * ;

  `include "Logger.bsv"
  import ccore_types  :: * ;

  interface Ifc_scoreboard;
    method ActionValue#(Bit#(`wawid)) ma_lock_rd (SBDUpd lock);
    method Action ma_release_rd (SBDUpd rls);
    method SBD mv_board;
  endinterface: Ifc_scoreboard

  /*doc:module: */
`ifdef scoreboard_noinline
  (*synthesize*)
`endif
  module mkscoreboard#(parameter Bit#(`xlen) hartid)(Ifc_scoreboard);

  `ifdef spfpu
    Vector#(64, Array#(Reg#(SBEntry))) rg_rf_board <- replicateM(mkCReg(2,unpack(0)));
  `else
    Vector#(32, Array#(Reg#(SBEntry))) rg_rf_board <- replicateM(mkCReg(2,unpack(0)));
  `endif
  `ifdef no_wawstalls
    /*doc:reg: */
    Reg#(Bit#(`wawid)) rg_renameid <- mkReg(0);
  `endif
    /*doc:method: This method is used to lock a destination register. WAW is prevented by ensuring
    * that the rd of the new instruction is not already locked*/
    method ActionValue#(Bit#(`wawid)) ma_lock_rd (SBDUpd lock);
      `logLevel( sboard, 0, $format("[%2d]SBoard Lock for : ",hartid,fshow(lock)))
      let index =  { `ifdef spfpu pack(lock.rdtype), `endif lock.rd};
      let entry = rg_rf_board[index][0];
    `ifdef no_wawstalls
      entry.id = rg_renameid;
      rg_renameid <= rg_renameid + 1;
    `endif
      entry.lock = 1;
      if (index !=0 ) 
        rg_rf_board[index][0] <= entry;
    `ifdef no_wawstalls
      return rg_renameid;
    `else
      return ?;
    `endif
    endmethod
    /*doc:method: This method is used to release the lock of a destination register when the
     * instruction has committed.*/
    method Action ma_release_rd (SBDUpd rls);
      let index =  { `ifdef spfpu pack(rls.rdtype), `endif rls.rd};
      let entry = rg_rf_board[index][1];
      `ifdef no_wawstalls if (rls.id == entry.id) `endif entry.lock = 0;
      if (index !=0  ) 
        rg_rf_board[index][1] <=  entry;
      `logLevel( sboard, 0, $format("[%2d]SBoard release for : ",hartid,fshow(rls)))
      `logLevel( sboard, 0, $format("[%2d]SBoard release entry : ",hartid,fshow(entry)))
    endmethod
    /*doc:method: This method provides a peek into the current score-board status */
    method SBD mv_board;
      Bit#(`ifdef spfpu 64 `else 32 `endif ) _rflock;
    `ifdef no_wawstalls
      Vector#(`ifdef spfpu 64 `else 32 `endif , Bit#(`wawid)) _id;
    `endif
      for (Integer i = 0; i< `ifdef spfpu 64 `else 32 `endif ; i = i + 1) begin
        _rflock[i] = rg_rf_board[i][0].lock;
      `ifdef no_wawstalls
        _id[i] = rg_rf_board[i][0].id;
      `endif
      end
      return SBD{rf_lock: _rflock `ifdef no_wawstalls ,v_id: _id `endif };
    endmethod
  endmodule:mkscoreboard
endpackage: scoreboard


// See LICENSE.iitm for license details
/*

Author : IIT Madras
Details : This stage implements the execute stage of the pipe.

--------------------------------------------------------------------------------------------------
*/
package stage3;

  // -- package imports --//
  import GetPut::*;
  import FIFOF::*;
  import SpecialFIFOs::*;
  import DReg::*;
	import TxRx:: *;
  import Vector :: *;
  import ConfigReg :: * ;

  // -- project imports --//
  import alu::*;                // implements the ALU function
  import fwding::*;       // provides the operand bypassing logic
  import ccore_types::*;       // for pipe - line types
  `include "ccore_params.defines"  // for core parameters
  `include "Logger.bsv"         // for logging display statements.
  `include "trap.defines"
  import dcache_types :: *;          // for dmem request types


  interface Ifc_stage3;

    /*doc:subifc: interface to receive the meta information from the decode stage*/
    interface RXe#(Stage3Meta)    rx_meta_from_stage2;

    /*doc: subifc: interface to receive the mtval value from stage2 incase of a trap */
    interface RXe#(Bit#(XLEN))    rx_mtval_from_stage2;
    
    /*doc: subifc: interface to receive the mtval value from stage2 incase of a trap */
    interface RXe#(Instruction_type)    rx_instrtype_from_stage2;

    /*doc:method: receive op1 and its meta info from previous stage (stage2/decode)*/
    (*always_ready*)
    method Action ma_op1 (RFOp1 i);

    /*doc:method: receive op2 and its meta info from previous stage (stage2/decode)*/
    (*always_ready*)
    method Action ma_op2 (RFOp2 i);

    /*doc:method: receive op2 and its meta info from previous stage (stage2/decode)*/
    (*always_ready*)
    method Action ma_op3 (RFOp3 i);

    /*doc:subifc: interface to send common meta information to the memory stage.*/
		interface TXe#(Stage4Common) tx_common_to_stage4;

    /*doc:subifc: interface to send the result of the current stage to the memory stage.*/
		interface TXe#(Stage4Type)   tx_type_to_stage4;

  `ifdef rtldump
    // interface to receive the instruction sequence for the rtl dump feature
    interface TXe#(CommitLogPacket) tx_inst;
    interface RXe#(CommitLogPacket) rx_inst;
  `endif

    (*always_ready*)
    // method to update epochs on redirection from write - back stage
    method Action update_wEpoch;

    // this method will send out the redirection caused by branches / jumps to all previous stages.
    (*always_ready*)
    method Tuple2#(Bool, Bit#(`vaddr)) flush_from_exe;

    // interface to send memory requests to the dmem subsystem
    interface Get#(DMem_request#(`vaddr, ELEN, 1)) memory_request;
    (*always_enabled*)
    method Action cache_is_available(Bool avail);
   `ifdef arith_trap
      method  Action ma_arith_trap_en(Bit#(1) en);
   `endif

    // method to receive the current status of the misa_c bit
    (*always_enabled, always_ready*)
    method Action csr_misa_c (Bit#(1) m);

    // method to identify if the store - buffer in the memory stage is empty or not.
    (*always_enabled*)
    method Action storebuffer_empty(Bool e);

    // methods to receive the operands from the proceeding stages. A max of three instructions can
    // exist in the pipe that will require to be looked up for bypass.
    (*always_enabled, always_ready*)
    method Action fwd_from_pipe3 (FwdType fwd);
    (*always_enabled, always_ready*)
    method Action fwd_from_pipe4_first (FwdType fwd);

  `ifdef bpu
    // this method receives the pc of the next instruction present in the pipe. This is driven by
    // the decode stage.
    method Action next_pc (Bit#(`vaddr) npc);

    // This method sends out the training information to the BTB for conditional branches.
    method Training_data mv_train_bpu;

    // This method sends out the return - address to be pushed on top of the stack.
    `ifdef gshare
      method Tuple2#(Bool, Bit#(`histlen)) mv_mispredict;
    `endif
  `endif

  `ifdef triggers
    method Action trigger_data1(Vector#(`trigger_num, TriggerData) t);
    method Action trigger_data2(Vector#(`trigger_num, Bit#(XLEN)) t);
    method Action trigger_enable(Vector#(`trigger_num, Bool) t);
  `endif
  `ifdef perfmonitors
    /*doc:method: */
    `ifdef spfpu
      method Bit#(1) mv_count_floats;
    `endif
    `ifdef muldiv
      method Bit#(1) mv_count_muldiv;
    `endif
    method Bit#(1) mv_count_jumps;
    method Bit#(1) mv_count_branches;
    method Bit#(1) mv_count_rawstalls ;
    method Bit#(1) mv_count_exestalls ;
  `endif
  endinterface

  (*synthesize*)
  module mkstage3#(parameter Bit#(XLEN) hartid) (Ifc_stage3);

    String stage3=""; // defined for logger

    // --------------------- Start instantiations ------------------------//

    // rx fifos to receive the decoded information and the operands from the RF.
    RX#(Stage3Meta)   rx_meta   <- mkRX;

    RX#(Bit#(XLEN))   rx_mtval   <- mkRX;
    
    RX#(Instruction_type)   rx_instrtype   <- mkRX;

    Wire#(RFOp1) wr_op1 <- mkWire();
    Wire#(RFOp2) wr_op2 <- mkWire();
    Wire#(RFOp3) wr_op3 <- mkWire();

  `ifdef rtldump
    // rx fifo to receive the instruction sequence for rtl.dump feature.
    RX#(CommitLogPacket) rxinst <- mkRX;
  `endif

  `ifdef bpu
    /*doc:wire: wire holding the PC value of the next instruction fetched into the pipe*/
    Wire#(Maybe#(Bit#(`vaddr))) wr_next_pc <- mkDWire(tagged Invalid);

    /*doc:reg: Register to send the training for the BTB on conditional branches.*/
	  Reg#(Maybe#(Training_data)) wr_training_data <- mkDReg(tagged Invalid);
    // Wire to send the return - address on the stack.
    `ifdef gshare
      Reg#(Maybe#(Tuple2#(Bool, Bit#(`histlen)))) wr_mispredict_ghr
                                                                       <- mkDReg( tagged Invalid);
    `endif
  `endif

    TX#(Stage4Common)   tx_common <- mkTX;
    TX#(Stage4Type)     tx_type   <- mkTX;
  `ifdef rtldump
    TX#(CommitLogPacket) txinst <- mkTX;
  `endif

    // module implementing the operand bypass behavior
    Ifc_fwding fwding <- mkfwding(hartid);

    // The following registers are use to the maintain epochs from various pipeline stages:
    // writeback and execute stage.
		Reg#(Bit#(1)) rg_eEpoch <- mkConfigReg(0);
		Reg#(Bit#(1)) rg_wEpoch <- mkReg(0);

    // Wire sending redirection indication to the previous stages.
    Reg#(Bool) wr_flush_from_exe <- mkDWire(False);

    // Wire holding the new pc to be redirected to due to branches / jumps
    Reg#(Bit#(`vaddr)) wr_redirect_pc <- mkDWire(0);

    Wire#(DMem_request#(`vaddr, ELEN, 1)) wr_memory_request <- mkWire;
    Wire#(Bool) wr_cache_avail <- mkWire;

    // wire holding the compressed bit of the misa csr
    Wire#(Bit#(1)) wr_misa_c <- mkWire();

    // Wire indicating if the store buffer in the memory stage or dcache is empty or not?
    Wire#(Bool) wr_storebuffer_empty <- mkWire();

    // This variable holds the current epoch values of the pipe
    let curr_epochs = {rg_eEpoch, rg_wEpoch};

  `ifdef triggers
    Vector#(`trigger_num, Wire#(TriggerData)) v_trigger_data1 <- replicateM(mkWire());
    Vector#(`trigger_num, Wire#(Bit#(XLEN))) v_trigger_data2 <- replicateM(mkWire());
    Vector#(`trigger_num, Wire#(Bool)) v_trigger_enable <- replicateM(mkWire());
  `endif

  `ifdef multicycle
    Reg#(Bool) rg_stall <- mkReg(False);
    Ifc_multicycle_alu multicycle_alu <- mkmulticycle_alu(hartid);
  `endif

  `ifdef perfmonitors
    /*doc:wire: */
    Wire#(Bit#(1)) wr_count_floats <- mkDWire(0);
    Wire#(Bit#(1)) wr_count_muldiv <- mkDWire(0);
    Wire#(Bit#(1)) wr_count_branches <- mkDWire(0);
    Wire#(Bit#(1)) wr_count_jumps <- mkDWire(0);
    Wire#(Bit#(1)) wr_count_rawstalls <- mkDWire(0);
    Wire#(Bit#(1)) wr_count_exestalls <- mkDWire(0);
  `endif
    // ---------------------- End Instatiations --------------------------//

    // ---------------------- Start local function definitions ----------------//

    // this function will deque the response received from the previous stage
    function Action deq_rx = action
      rx_meta.u.deq;
      rx_mtval.u.deq;
      rx_instrtype.u.deq;
    `ifdef rtldump
      rxinst.u.deq;
    `endif
    endaction;

    // function to send the memory request to the data memory subsystem
    function Action send_memory_request(Bit#(`vaddr) address, Bit#(1) epochs, Bit#(3) funct3,
                                        Access_type memaccess, Bit#(4) fn, Bit#(ELEN) data) = action
        let req = DMem_request{address      : address,
                               epochs       : epochs,
                               size         : funct3
                               ,fence       : memaccess == FenceI || memaccess == Fence
                               ,access      : truncate(pack(memaccess))
                               ,writedata   : data
                            `ifdef atomic
                               ,atomic_op   : {funct3[0], fn}
                            `endif
                            `ifdef supervisor
                               ,sfence      : memaccess == SFence
                               ,ptwalk_req  : False
                               ,ptwalk_trap : False
                            `endif } ;
        wr_memory_request <= req;
    endaction;
    // ---------------------- End local function definitions ------------------//
    Bool rule_condition = wr_cache_avail `ifdef multicycle && !rg_stall `endif ;

    rule rl_capture_memory_stalls(rx_meta.u.notEmpty && !wr_cache_avail 
                    `ifdef multicycle && !rg_stall `endif );
      `logLevel( stage3, 4, $format("[%2d]STAGE3: DCACHE is busy",hartid))
    endrule

    // RuleName : execute_operation
    // Implicit Conditions : all rx fifo should not be empty. All tx fifos should not be full. next
    //                     pc from the previous stage should be made available.
    // Explicit Conditions : dcache should be available and all previous multicycle operations should
    //                      be over (variable rule_condition captures this condition)
    // Description : This rule performs the alu operation of the core - pipe. This rule calls the
    // fwding logic to check if any of the following stages in the pipe hold an updated value of
    // the operands fetched from the RF in the previous stage. Only when the latest value of all
    // operands is available can the instruction execute. All arigthmetic, branch - compare,
    // logical operations are performed by the "alu" module. This module will also generate the
    // effective address required for load / stores. This module will also generate the redirection
    // Pc in case of branches / JUMPs. Misaligned traps are also detected in the alu module itself.
    // In case of multicycle operations like MULDIV / FPU the inputs are latched in the first cycle
    // and the execute stage is stalled untill the operation is over.
    // An instruction tagged as a TRAP in the previous stage will simply bypass the execute
    // stage.

    /*doc:rule:
    This rule receives the decoded information from the previous stage.
    The operand forwarding circuit is also used in this rule to capture the latest operand values.
    */
    rule rl_capture_latest_ops ( rule_condition );
    //---------------------- capturing the inputs from the previous stage. -------------------- //
      let meta    = rx_meta.u.first;
      let mtval   = rx_mtval.u.first;
      Instruction_type inst_type = rx_instrtype.u.first;
      Bit#(3) funct3  = truncate(meta.funct);
      Bit#(4) fn      = truncateLSB(meta.funct);
    `ifdef bpu
      let btbresponse = meta.btbresponse;
    `endif
    `ifdef rtldump
      let clogpkt = rxinst.u.first;
    `endif
      Bit#(`vaddr) pc = meta.pc;
      let s4common = Stage4Common{pc    : meta.pc,
                                  rd    : meta.rd,
                                  epochs : meta.epochs[0]
                                `ifdef spfpu
                                  ,rdtype : meta.rdtype
                                `endif } ;
      Bool epochs_match = (curr_epochs == meta.epochs);

      Bool trap = inst_type == TRAP;
      Bit#(`causesize) cause = truncate(meta.funct);
      Bit#(`vaddr) badaddr = truncate(mtval);

      `logLevel( stage3, 0, $format("[%2d]STAGE3: PC:%h",hartid, meta.pc))
      `logLevel( stage3, 0, $format("[%2d]STAGE3: ",hartid, fshow(meta)))
      // ---------------------------------------------------------------------------------------- //
      // ------------------ check if operands are available for execution -----------------------//
    `ifdef spfpu
      RFType rf1type = `ifdef spfpu wr_op1.optype == FloatingRF ? FRF : `endif IRF;
      RFType rf2type = `ifdef spfpu wr_op2.optype == FloatingRF ? FRF : `endif IRF;
    `endif
      let {rs1avail, rs1}     <- fwding.read_rs1(wr_op1.data, wr_op1.addr
                                            `ifdef spfpu, rf1type `endif );

      let {rs2avail, rs2}     <- fwding.read_rs2(wr_op2.data, wr_op2.addr
                                            `ifdef spfpu, rf2type `endif );
    `ifdef spfpu
      let {rs3avail, rs3_imm} <- fwding.read_rs3(signExtend(wr_op3.data), wr_op3.addr,
                                             wr_op3.optype);
    `else
      let rs3_imm = wr_op3.data;
    `endif
      Bool operands_available = (rs1avail && rs2avail `ifdef spfpu && rs3avail `endif );
      // -------------------------------------------------------------------------------------- //

      // --------------------- Mux the operands according to the ALU arguments  --------------- //
      // the following mux is required only for AUIPC
      Bit#(ELEN)    arg1 = wr_op1.optype == PC ? zeroExtend(meta.pc) : rs1;
      Bit#(ELEN)    arg2 = rs2;
      // in case of JALR only the rs1 is used as the base else the PC is the base for all other
      // control instructions
      Bit#(`vaddr)  arg3 = (inst_type == JALR) ? truncate(rs1) : meta.pc;
      Bit#(TMax#(`vaddr,FLEN))  arg4 = rs3_imm;
      // -------------------------------------------------------------------------------------- //

      // the split between memory address and jump_address has been done to reduce the path to cache
      // and tlbs
      Bit#(`vaddr) memory_address = rs1 + truncate(arg4);
      Bit#(`vaddr) jump_address = arg3 + truncate(arg4);
      if(inst_type == JALR)
        jump_address[0] = 0;
      let aluout = fn_alu(fn, arg1, arg2, arg3, jump_address, inst_type, truncate(funct3),
                    `ifdef RV64 meta.word32, `elsif dpfpu meta.word32, `endif wr_misa_c
                    `ifdef triggers
                      ,readVReg(v_trigger_data1), readVReg(v_trigger_data2)
                      ,readVReg(v_trigger_enable)
                    `endif
                    `ifdef bpu
                      , fromMaybe(?,wr_next_pc), btbresponse.prediction[`statesize-1]
                      `ifdef compressed ,meta.compressed `endif
                    `endif );
      // ---------------------------------- Check for traps ------------------------------------//
      Bool memory_trap = inst_type == MEMORY && ((funct3[1 : 0] == 1 && memory_address[0] != 0)
                              || (funct3[1 : 0] == 2 && memory_address[1 : 0] != 0)
                  `ifdef RV64 || (funct3[1 : 0] == 3 && memory_address[2 : 0] != 0) `endif );
      Bit#(`causesize) memory_cause = meta.memaccess == Load? `Load_addr_misaligned:
                                                             `Store_addr_misaligned ;
      Bool control_trap = aluout.trap;
      Bit#(`causesize) contro_cause = `Inst_addr_misaligned;

      if(!trap) begin
        if(memory_trap)begin
          trap = True;
          cause = memory_cause;
          badaddr = memory_address;
        end
        else begin
          trap = control_trap;
          cause = contro_cause;
          badaddr = jump_address;
        end
      end
      // -------------------------------------------------------------------------------------- //

    `ifdef dpfpu
      Bit#(1) nanboxing = pack(inst_type == MEMORY
                               && funct3[1 : 0] == 2
                               && meta.rdtype == FRF);
    `endif
    `ifdef bpu
      let td = Training_data{pc : meta.pc,
                             target : jump_address,
                             state  : ?
                          `ifdef gshare
                             ,history   : btbresponse.history
                          `endif
                          `ifdef compressed
                             ,instr16 : meta.compressed
                          `endif
                             ,ci         : ?
                             ,btbhit     : btbresponse.btbhit
                          };
      if((inst_type == JAL || inst_type == JALR) && meta.rd[0]==1)
        td.ci = Call;
      else if(inst_type == JALR && wr_op1.addr == 'b00001) // TODO add x5 check
        td.ci = Ret;
      else if(inst_type == JAL || inst_type == JALR)
        td.ci = JAL;
      else
        td.ci = Branch;

    `endif
      // --------------- In case of Memory operation send request to Dmem ----------------//
      if(operands_available && epochs_match && inst_type == MEMORY && !memory_trap) begin
        send_memory_request(memory_address, meta.epochs[0], funct3, meta.memaccess, fn, arg2);
      end
      // ---------------------------------------------------------------------------------- //

      // -------------------------- Derive types for Next stage --------------------------- //
      Stage4Trap s4trap = Stage4Trap {cause   : cause,
                                      badaddr : badaddr};
      let s4system = Stage4System {funct3     : funct3,
                                   lpc        : truncate(meta.pc),
                                   rs1_imm    : funct3[2] == 1?zeroExtend(arg4[19 : 15]):
                                                               truncate(aluout.aluresult),
                                   csr_address : truncate(arg4) };

      let s4regular = Stage4Regular { rdvalue   : aluout.aluresult
                              `ifdef spfpu
                                    ,fflags    :0
                              `endif };
      let s4memory = Stage4Memory{  memaccess   : meta.memaccess
                              `ifdef atomic
                                   ,atomicop    : {funct3[0], fn}
                              `endif
                              `ifdef triggers
                                  ,address     : memory_address
                                  ,size        : funct3[1:0]
                              `endif
                              `ifdef dpfpu
                                  ,nanboxing   : nanboxing
                              `endif } ;

      Bool execute = True;
    /*
      // TODO instead of just store - buffer for loads will need to check if pipe is empty
      // TODO add more comments here
      if(inst_type == MEMORY && !wr_storebuffer_empty
                                && (meta.memaccess == FenceI || meta.memaccess == Fence
                              `ifdef atomic     || meta.memaccess == Atomic `endif
                              `ifdef supervisor || meta.memaccess == SFence `endif ) )
        execute = False;
    */
    `ifdef bpu
      if((inst_type == BRANCH || inst_type == JALR || inst_type == JAL) &&
                                                                              !isValid(wr_next_pc))
        execute = False;
    `endif
    `ifdef perfmonitors
      if(epochs_match && !operands_available)
        wr_count_rawstalls <= 1;
    `endif
      // --------------------- logger statements ----------------------------------------------- //
      if(!operands_available && inst_type != TRAP)
        `logLevel( stage3, 4, $format("[%2d]STAGE3: RAW STALL PC:%h",hartid,meta.pc))
      if( (operands_available || inst_type == TRAP) && !execute)
        `logLevel( stage3, 4, $format("[%2d]STAGE3: NPC not Available PC:%h",hartid, meta.pc))
      // --------------------------------------------------------------------------------------- //
      if(!epochs_match)begin
        `logLevel( stage3, 0, $format("[%2d]STAGE3: Dropping Instructions",hartid))
        deq_rx;
      end
      // here the trap could be because the misprediction from the previous jump.branch might
      // have caused the cpu to fetch an illegal instruction. So trap check should happen after the
      // redirection has been checked.
      else if(execute &&  (inst_type == TRAP || operands_available )) begin
        `logLevel( stage3, 0, $format("[%2d]STAGE3: Sending ops",hartid))
        `logLevel( stage3, 1, $format("[%2d]STAGE3: rs1avail:%b rs1:%h",hartid, rs1avail, arg1))
        `logLevel( stage3, 1, $format("[%2d]STAGE3: rs2avail:%b rs2:%h",hartid, rs2avail, arg2))
        `logLevel( stage3, 1, $format("[%2d]STAGE3: arg3:%h",hartid, arg3))
        `logLevel( stage3, 1, $format("[%2d]STAGE3: arg4:%h",hartid, arg4))
        `logLevel( stage3, 0, $format("[%2d]STAGE3: Result: ",hartid,fshow(aluout)))
        deq_rx;
        wr_redirect_pc    <= aluout.redirect_pc;
        wr_flush_from_exe <= aluout.redirect;
        rg_eEpoch         <= pack(aluout.redirect )^rg_eEpoch;
        // -------------------------- sending training updates to predictors ----------------- //
      `ifdef bpu
        if(inst_type == BRANCH && !control_trap)begin
          /*if(aluout.branch_taken)begin
            if(btbresponse.prediction < 3)begin
              btbresponse.prediction = btbresponse.prediction + 1;
            end
          end
          else begin
            if(btbresponse.prediction > 0) begin
              btbresponse.prediction = btbresponse.prediction - 1;
            end
          end*/
          if(aluout.branch_taken)begin
            case(btbresponse.prediction)
              'b00: btbresponse.prediction= 'b01;
              'b01: btbresponse.prediction= 'b11;
              'b10: btbresponse.prediction= 'b11;
            endcase
          end
          else begin
            case(btbresponse.prediction)
              'b01: btbresponse.prediction= 'b00;
              'b10: btbresponse.prediction= 'b00;
              'b11: btbresponse.prediction= 'b10;
            endcase
          end
          td.state = btbresponse.prediction;
          wr_training_data <= tagged Valid td;
        end
        else if(inst_type == JAL || inst_type == JALR) begin
          td.state = 3;
          wr_training_data <= tagged Valid td;
        end

        if(aluout.redirect && !control_trap) begin
          `logLevel( stage3, 4, $format("[%2d]STAGE3: Misprediction. Inst: ",hartid,
                                        fshow(inst_type), " PC:%h Target:%h NextPC:%h",
                                        meta.pc, aluout.redirect_pc, fromMaybe(?,wr_next_pc)))
          `ifdef gshare
            wr_mispredict_ghr <= tagged Valid tuple2(btbresponse.btbhit, btbresponse.history);
          `endif
        end
      `endif
    // --------------------------------------------------------------------------------------- //

      `ifdef supervisor
        if( inst_type == MEMORY && meta.memaccess == SFence ) begin
          inst_type = ALU;
        end
      `endif
        if(trap)
          inst_type = TRAP;
        Stage4Type s4type = case(inst_type) matches
                              SYSTEM_INSTR  : tagged System s4system;
                              TRAP          : tagged Trap s4trap;
                              MEMORY        : tagged Memory s4memory;
                              default       : tagged Regular s4regular;
                            endcase;
      // ---------------------------------------------------------------------------------- //
      `ifdef perfmonitors
        if(inst_type == JALR || inst_type == JAL)
          wr_count_jumps <= 1;
        if(inst_type == BRANCH)
          wr_count_branches <= 1;
        `ifdef spfpu
          if(inst_type == FLOAT)
            wr_count_floats <= 1;
        `endif
        `ifdef muldiv
          if(inst_type == MULDIV)
            wr_count_muldiv <= 1;
        `endif
      `endif
      `ifdef multicycle
        Bool is_multicyle = `ifdef muldiv inst_type == MULDIV  || `endif
                            `ifdef spfpu  inst_type == FLOAT || `endif False;
        rg_stall <= is_multicyle;
        if(is_multicyle) begin
          multicycle_alu.ma_inputs(fn, funct3, arg1, arg2, arg4, inst_type
                                  `ifdef RV64 ,meta.word32 `endif );
          `logLevel( stage3, 1, $format("[%2d]STAGE3: Sending multicyle ops",hartid))
        end
        else  begin
          `logLevel( stage3, 1, $format("[%2d]STAGE3: Enquing: ",hartid,fshow(s4type)))
          tx_type.u.enq(s4type);
        end
      `else
        tx_type.u.enq(s4type);
      `endif
        tx_common.u.enq(s4common);
      `ifdef rtldump
        if (s4type matches tagged Memory .mem)begin
          CommitLogMem _pkt = ?;
          if (clogpkt.inst_type matches tagged MEM .cmem) 
            _pkt = cmem;
          _pkt.address= memory_address;
          _pkt.data= arg2;
          _pkt.atomic_op = {funct3[0],fn};
          clogpkt.inst_type = tagged MEM _pkt;
        end
        else if (s4type matches tagged Regular .x) begin
          CommitLogReg _pkt = ?;
          if (clogpkt.inst_type matches tagged REG .creg)
            _pkt = creg;
          else begin
            _pkt.rd = meta.rd;
            _pkt.irf = (meta.rdtype==IRF);
          end
          _pkt.wdata = x.rdvalue;
          clogpkt.inst_type = tagged REG _pkt;
        end
        txinst.u.enq(clogpkt);
      `endif
      end
    endrule

  `ifdef multicycle
    rule delayed_output(rg_stall);
      let delayed_result = multicycle_alu.mv_delayed_output;
      if(delayed_result.valid) begin
      `ifdef arith_trap
        if(delayed_result.trap)
          tx_type.u.enq(tagged Trap Stage4Trap{cause: delayed_result.cause, badaddr:?});
        else
      `endif
        tx_type.u.enq(tagged Regular Stage4Regular { rdvalue   : delayed_result.data
                                                    `ifdef spfpu
                                                          ,fflags    :delayed_result.fflags
                                                    `endif });
        `logLevel( stage3, 1, $format("[%2d]STAGE3: Enquing Delayed Result :%h ",hartid,delayed_result.data))
        rg_stall <= False;
      end
      else begin
        `logLevel( stage3, 4, $format("MULDELAYED"))
      `ifdef perfmonitors
        wr_count_exestalls <= 1;
      `endif
      end
    endrule
  `endif

    //--------------- interfaces to receive the decoded info from the previous stage. ------------//
    interface rx_meta_from_stage2   = rx_meta.e;
    interface rx_mtval_from_stage2  = rx_mtval.e;
    interface rx_instrtype_from_stage2 = rx_instrtype.e;
  `ifdef rtldump
    interface rx_inst = rxinst.e;
    interface tx_inst = txinst.e;
  `endif
    method Action ma_op1 (RFOp1 i);
      wr_op1 <= i;
    endmethod
    method Action ma_op2 (RFOp2 i);
      wr_op2 <= i;
    endmethod
    method Action ma_op3 (RFOp3 i);
      wr_op3 <= i;
    endmethod
    // -------------------------------------------------------------------------------------------//

    // ------------------ interfaces to send the executed result to the next stage --------------//
		interface tx_common_to_stage4 = tx_common.e;
		interface tx_type_to_stage4   = tx_type.e;
    // -------------------------------------------------------------------------------------------//

    // Description : This method fires when there is a flush from the write - back stage.
    method Action update_wEpoch;
      rg_wEpoch<= ~rg_wEpoch;
    endmethod

    // Thhis is method is fired when a branch / jump redicrection is detected from this stage.
    method flush_from_exe = tuple2(wr_flush_from_exe, wr_redirect_pc);

    // Description : captures if the dmem subsystem is available for sending requests
    method Action cache_is_available(Bool avail);
      wr_cache_avail <= avail;
    endmethod

    // Description : interface to send memory requests.
    interface memory_request = interface Get
      method ActionValue#(DMem_request#(`vaddr, ELEN, 1)) get;
        return wr_memory_request;
      endmethod
    endinterface;

    // Description : captures the current setting of the "C" bit in the misa csr.
    method Action csr_misa_c (Bit#(1) m);
      wr_misa_c <= m;
    endmethod

    // Description : captures if the storebuffer in the dmem or stage4 is empty. This used for
    // blocking speculative loads
    method Action storebuffer_empty(Bool e);
      wr_storebuffer_empty <= e;
    endmethod

    // Description : receives the bypass values from the pipe3
    method Action fwd_from_pipe3 (FwdType fwd);
      fwding.fwd_from_pipe3(fwd);
    endmethod

    // Description : receives the bypass values from the first element of pipe4
    method Action fwd_from_pipe4_first (FwdType fwd);
      fwding.fwd_from_pipe4_first(fwd);
    endmethod

  `ifdef bpu
    // Description : captures the next_pc in the pipe
    method Action next_pc (Bit#(`vaddr) npc);
      wr_next_pc <= tagged Valid npc;
    endmethod
  `endif
   `ifdef arith_trap
      method  Action ma_arith_trap_en(Bit#(1) en);
        multicycle_alu.ma_arith_trap_en(en);
      endmethod
   `endif
  `ifdef bpu
    // Description : method to train the branch predictor BTB
    method Training_data mv_train_bpu if(wr_training_data matches tagged Valid .x);
      return x;
    endmethod
    `ifdef gshare
      method Tuple2#(Bool, Bit#(`histlen)) mv_mispredict
                                                    if(wr_mispredict_ghr matches tagged Valid .x);
        return x;
      endmethod
    `endif
  `endif
  `ifdef triggers
    method Action trigger_data1(Vector#(`trigger_num, TriggerData) t);
      for(Integer i=0; i<`trigger_num; i=i+1)
        v_trigger_data1[i] <= t[i];
    endmethod
    method Action trigger_data2(Vector#(`trigger_num, Bit#(XLEN)) t);
      for(Integer i=0; i<`trigger_num; i=i+1)
        v_trigger_data2[i] <= t[i];
    endmethod
    method Action trigger_enable(Vector#(`trigger_num, Bool) t);
      for(Integer i=0; i<`trigger_num; i=i+1)
        v_trigger_enable[i] <= t[i];
    endmethod
  `endif
  `ifdef perfmonitors
    /*doc:method: */
    `ifdef spfpu
      method mv_count_floats = wr_count_floats;
    `endif
    `ifdef muldiv
      method mv_count_muldiv = wr_count_muldiv;
    `endif
    method mv_count_jumps = wr_count_jumps;
    method mv_count_branches = wr_count_branches;
    method mv_count_rawstalls = wr_count_rawstalls;
    method mv_count_exestalls = wr_count_exestalls;
  `endif
  endmodule
endpackage

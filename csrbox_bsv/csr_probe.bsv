
  function Bit#(`xlen) fn_probe_csr (Bit#(12) csr_addr);
    case (csr_addr)

        `MISA : return zeroExtend(soc.soc_sb.sbread.mv_csr_misa);
        `MVENDORID : return zeroExtend(soc.soc_sb.sbread.mv_csr_mvendorid);
        `STVEC : return zeroExtend(soc.soc_sb.sbread.mv_csr_stvec);
        `MTVEC : return zeroExtend(soc.soc_sb.sbread.mv_csr_mtvec);
        `MSTATUS : return zeroExtend(soc.soc_sb.sbread.mv_csr_mstatus);
        `MARCHID : return zeroExtend(soc.soc_sb.sbread.mv_csr_marchid);
        `MIMPID : return zeroExtend(soc.soc_sb.sbread.mv_csr_mimpid);
        `MHARTID : return zeroExtend(soc.soc_sb.sbread.mv_csr_mhartid);
        `MIP : return zeroExtend(soc.soc_sb.sbread.mv_csr_mip);
        `SIP : return zeroExtend(soc.soc_sb.sbread.mv_csr_sip);
        `MIE : return zeroExtend(soc.soc_sb.sbread.mv_csr_mie);
        `SIE : return zeroExtend(soc.soc_sb.sbread.mv_csr_sie);
        `MSCRATCH : return zeroExtend(soc.soc_sb.sbread.mv_csr_mscratch);
        `SSCRATCH : return zeroExtend(soc.soc_sb.sbread.mv_csr_sscratch);
        `SEPC : return zeroExtend(soc.soc_sb.sbread.mv_csr_sepc);
        `STVAL : return zeroExtend(soc.soc_sb.sbread.mv_csr_stval);
        `SCAUSE : return zeroExtend(soc.soc_sb.sbread.mv_csr_scause);
        `MEPC : return zeroExtend(soc.soc_sb.sbread.mv_csr_mepc);
        `MTVAL : return zeroExtend(soc.soc_sb.sbread.mv_csr_mtval);
        `MCAUSE : return zeroExtend(soc.soc_sb.sbread.mv_csr_mcause);
        `MCYCLE : return zeroExtend(soc.soc_sb.sbread.mv_csr_mcycle);
        `MINSTRET : return zeroExtend(soc.soc_sb.sbread.mv_csr_minstret);
        `FCSR : return zeroExtend(soc.soc_sb.sbread.mv_csr_fcsr);
        `TIME : return zeroExtend(soc.soc_sb.sbread.mv_csr_time);
        `MIDELEG : return zeroExtend(soc.soc_sb.sbread.mv_csr_mideleg);
        `MEDELEG : return zeroExtend(soc.soc_sb.sbread.mv_csr_medeleg);
        `PMPCFG0 : return zeroExtend(soc.soc_sb.sbread.mv_csr_pmpcfg0);
        `PMPADDR0 : return zeroExtend(soc.soc_sb.sbread.mv_csr_pmpaddr0);
        `PMPADDR1 : return zeroExtend(soc.soc_sb.sbread.mv_csr_pmpaddr1);
        `PMPADDR2 : return zeroExtend(soc.soc_sb.sbread.mv_csr_pmpaddr2);
        `PMPADDR3 : return zeroExtend(soc.soc_sb.sbread.mv_csr_pmpaddr3);
        `MCOUNTEREN : return zeroExtend(soc.soc_sb.sbread.mv_csr_mcounteren);
        `SCOUNTEREN : return zeroExtend(soc.soc_sb.sbread.mv_csr_scounteren);
        `SATP : return zeroExtend(soc.soc_sb.sbread.mv_csr_satp);
        `MCOUNTINHIBIT : return zeroExtend(soc.soc_sb.sbread.mv_csr_mcountinhibit);
        `FFLAGS : return zeroExtend(soc.soc_sb.sbread.mv_csr_fflags);
        `FRM : return zeroExtend(soc.soc_sb.sbread.mv_csr_frm);
        `CUSTOMCONTROL : return zeroExtend(soc.soc_sb.sbread.mv_csr_customcontrol);
        `MHPMCOUNTER3 : return zeroExtend(soc.soc_sb.sbread.mv_csr_mhpmcounter3);
        `MHPMCOUNTER4 : return zeroExtend(soc.soc_sb.sbread.mv_csr_mhpmcounter4);
        `MHPMCOUNTER5 : return zeroExtend(soc.soc_sb.sbread.mv_csr_mhpmcounter5);
        `MHPMCOUNTER6 : return zeroExtend(soc.soc_sb.sbread.mv_csr_mhpmcounter6);
        `MHPMEVENT3 : return zeroExtend(soc.soc_sb.sbread.mv_csr_mhpmevent3);
        `MHPMEVENT4 : return zeroExtend(soc.soc_sb.sbread.mv_csr_mhpmevent4);
        `MHPMEVENT5 : return zeroExtend(soc.soc_sb.sbread.mv_csr_mhpmevent5);
        `MHPMEVENT6 : return zeroExtend(soc.soc_sb.sbread.mv_csr_mhpmevent6);
        `DCSR : return zeroExtend(soc.soc_sb.sbread.mv_csr_dcsr);
        `DPC : return zeroExtend(soc.soc_sb.sbread.mv_csr_dpc);
        `DSCRATCH0 : return zeroExtend(soc.soc_sb.sbread.mv_csr_dscratch0);
        `DSCRATCH1 : return zeroExtend(soc.soc_sb.sbread.mv_csr_dscratch1);
    default: return 0;
    endcase
  endfunction

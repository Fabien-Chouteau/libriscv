------------------------------------------------------------------------------
--                                                                          --
--                   Copyright (C) 2019, Fabien Chouteau                    --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with ESF;

package LibRISCV.CSR is

   type Id is mod 2 ** 12
     with Size => 12;

   subtype User is Id
     with Dynamic_Predicate =>
       (User and 2#00_11_00000000#) = 2#00_00_00000000#;

   subtype Supervisor is Id
     with Dynamic_Predicate =>
       (Supervisor and 2#00_11_00000000#) = 2#00_01_00000000#;

   subtype Hypervisor is Id
     with Dynamic_Predicate =>
       (Hypervisor and 2#00_11_00000000#) = 2#00_10_00000000#;

   subtype Machine is Id
     with Dynamic_Predicate =>
       (Machine and 2#00_11_00000000#) = 2#00_11_00000000#;

   subtype Read_Only is Id
     with Dynamic_Predicate =>
       (Read_Only and 2#11_0000000000#) = 2#11_0000000000#;

   function Hex is new ESF.Hex_Image (Id);

   type Name is
     (
      ustatus,        -- URW: User status register
      fflags,         -- URW: Floating-Point Accrued Exceptions
      frm,            -- URW: Floating-Point Dynamic Rounding Mode
      fcsr,           -- URW: Floating-Point Control and St
      uie,            -- URW: User interrupt-enable register
      utvec,          -- URW: User trap handler base address
      uscratch,       -- URW: Scratch register for user trap handlers
      uepc,           -- URW: User exception program counter
      ucause,         -- URW: User trap cause
      utval,          -- URW: User bad address or instruction
      uip,            -- URW: User interrupt pending
      sstatus,        -- SRW: Supervisor status register
      sedeleg,        -- SRW: Supervisor exception delegation register
      sideleg,        -- SRW: Supervisor interrupt delegation register
      sie,            -- SRW: Supervisor interrupt-enable register
      stvec,          -- SRW: Supervisor trap handler base address
      scounteren,     -- SRW: Supervisor counter enable
      sscratch,       -- SRW: Scratch register for supervisor trap handlers
      sepc,           -- SRW: Supervisor exception program counter
      scause,         -- SRW: Supervisor trap cause
      stval,          -- SRW: Supervisor bad address or instruction
      sip,            -- SRW: Supervisor interrupt pending
      satp,           -- SRW: Supervisor address translation and protection
      vsstatus,       -- HRW: Virtual supervisor status register
      vsie,           -- HRW: Virtual supervisor interrupt-enable register
      vstvec,         -- HRW: Virtual supervisor trap handler base address
      vsscratch,      -- HRW: Virtual supervisor scratch register
      vsepc,          -- HRW: Virtual supervisor exception program counter
      vscause,        -- HRW: Virtual supervisor trap cause
      vstval,         -- HRW: Virtual supervisor bad address or instruction
      vsip,           -- HRW: Virtual supervisor interrupt pending
      vsatp,          -- HRW: Virtual supervisor address translation and protection
      mstatus,        -- MRW: Machine status register
      misa,           -- MRW: ISA and extension
      medeleg,        -- MRW: Machine exception delegation register
      mideleg,        -- MRW: Machine interrupt delegation register
      mie,            -- MRW: Machine interrupt-enable register
      mtvec,          -- MRW: Machine trap-handler base address
      mcounteren,     -- MRW: Machine counter enable
      mstatush,       -- MRW: Additional machine status register, RV32 only
      mcountinhibit,  -- MRW: Machine counter-inhibit register
      mhpmevent3,     -- MRW: Machine performance-monitoring event selector
      mhpmevent4,     -- MRW: Machine performance-monitoring event selector
      mhpmevent31,    -- MRW: Machine performance-monitoring event selector
      mscratch,       -- MRW: Scratch register for machine trap handlers
      mepc,           -- MRW: Machine exception program counter
      mcause,         -- MRW: Machine trap cause
      mtval,          -- MRW: Machine bad address or instruction
      mip,            -- MRW: Machine interrupt pending
      mbase,          -- MRW: Base register
      mbound,         -- MRW: Bound register
      mibase,         -- MRW: Instruction base register
      mibound,        -- MRW: Instruction bound register
      mdbase,         -- MRW: Data base register
      mdbound,        -- MRW: Data bound register
      pmpcfg0,        -- MRW: Physical memory protection configuration
      pmpcfg1,        -- MRW: Physical memory protection configuration, RV32 only
      pmpcfg2,        -- MRW: Physical memory protection configuration
      pmpcfg3,        -- MRW: Physical memory protection configuration, RV32 only
      pmpaddr0,       -- MRW: Physical memory protection address register
      pmpaddr1,       -- MRW: Physical memory protection address register
      pmpaddr15,      -- MRW: Physical memory protection address register
      hstatus,        -- HRW: Hypervisor status register
      hedeleg,        -- HRW: Hypervisor exception delegation register
      hideleg,        -- HRW: Hypervisor interrupt delegation register
      hcounteren,     -- SRW: Hypervisor counter enable
      hgatp,          -- HRW: Hypervisor guest address translation and protection
      tselect,        -- MRW: Debug/Trace trigger register select
      tdata1,         -- MRW: First Debug/Trace trigger data register
      tdata2,         -- MRW: Second Debug/Trace trigger data register
      tdata3,         -- MRW: Third Debug/Trace trigger data register
      dcsr,           -- DRW: Debug control and status register
      dpc,            -- DRW: Debug PC
      dscratch0,      -- DRW: Debug scratch register 0
      dscratch1,      -- DRW: Debug scratch register 1
      mcycle,         -- MRW: Machine cycle counter
      minstret,       -- MRW: Machine instructions-retired counter
      mhpmcounter3,   -- MRW: Machine performance-monitoring counter
      mhpmcounter4,   -- MRW: Machine performance-monitoring counter
      mhpmcounter31,  -- MRW: Machine performance-monitoring counter
      mcycleh,        -- MRW: Upper 32 bits of mcycle, RV32I only
      minstreth,      -- MRW: Upper 32 bits of minstret, RV32I only
      mhpmcounter3h,  -- MRW: Upper 32 bits of mhpmcounter3, RV32I only
      mhpmcounter4h,  -- MRW: Upper 32 bits of mhpmcounter4, RV32I only
      mhpmcounter31h, -- MRW: Upper 32 bits of mhpmcounter31, RV32I only
      cycle,          -- URO: Cycle counter for RDCYCLE instruction
      time,           -- URO: Timer for RDTIME instruction
      instret,        -- URO: Instructions-retired counter for RDINSTRET instruction
      hpmcounter3,    -- URO: Performance-monitoring counter
      hpmcounter4,    -- URO: Performance-monitoring counter
      hpmcounter31,   -- URO: Performance-monitoring counter
      cycleh,         -- URO: Upper 32 bits of cycle, RV32I only
      timeh,          -- URO: Upper 32 bits of time, RV32I only
      instreth,       -- URO: Upper 32 bits of instret, RV32I only
      hpmcounter3h,   -- URO: Upper 32 bits of hpmcounter3, RV32I only
      hpmcounter4h,   -- URO: Upper 32 bits of hpmcounter4, RV32I only
      hpmcounter31h,  -- URO: Upper 32 bits of hpmcounter31, RV32I only
      mvendorid,      -- MRO: Vendor ID
      marchid,        -- MRO: Architecture ID
      mimpid,         -- MRO: Implementation ID
      mhartid,        -- MRO: Hardware thread ID
      Not_Implemented -- Disgnate all CSR for which we do not have a name
     );

   function To_Id (N : Name) return Id;
   function To_Name (I : Id) return Name;
   function Img (I : Id) return String;
   function Img (N : Name) return String;

   for Name use
     (
      ustatus        => 16#000#,
      fflags         => 16#001#,
      frm            => 16#002#,
      fcsr           => 16#003#,
      uie            => 16#004#,
      utvec          => 16#005#,
      uscratch       => 16#040#,
      uepc           => 16#041#,
      ucause         => 16#042#,
      utval          => 16#043#,
      uip            => 16#044#,
      sstatus        => 16#100#,
      sedeleg        => 16#102#,
      sideleg        => 16#103#,
      sie            => 16#104#,
      stvec          => 16#105#,
      scounteren     => 16#106#,
      sscratch       => 16#140#,
      sepc           => 16#141#,
      scause         => 16#142#,
      stval          => 16#143#,
      sip            => 16#144#,
      satp           => 16#180#,
      vsstatus       => 16#200#,
      vsie           => 16#204#,
      vstvec         => 16#205#,
      vsscratch      => 16#240#,
      vsepc          => 16#241#,
      vscause        => 16#242#,
      vstval         => 16#243#,
      vsip           => 16#244#,
      vsatp          => 16#280#,
      mstatus        => 16#300#,
      misa           => 16#301#,
      medeleg        => 16#302#,
      mideleg        => 16#303#,
      mie            => 16#304#,
      mtvec          => 16#305#,
      mcounteren     => 16#306#,
      mstatush       => 16#310#,
      mcountinhibit  => 16#320#,
      mhpmevent3     => 16#323#,
      mhpmevent4     => 16#324#,
      mhpmevent31    => 16#33F#,
      mscratch       => 16#340#,
      mepc           => 16#341#,
      mcause         => 16#342#,
      mtval          => 16#343#,
      mip            => 16#344#,
      mbase          => 16#380#,
      mbound         => 16#381#,
      mibase         => 16#382#,
      mibound        => 16#383#,
      mdbase         => 16#384#,
      mdbound        => 16#385#,
      pmpcfg0        => 16#3A0#,
      pmpcfg1        => 16#3A1#,
      pmpcfg2        => 16#3A2#,
      pmpcfg3        => 16#3A3#,
      pmpaddr0       => 16#3B0#,
      pmpaddr1       => 16#3B1#,
      pmpaddr15      => 16#3BF#,
      hstatus        => 16#600#,
      hedeleg        => 16#602#,
      hideleg        => 16#603#,
      hcounteren     => 16#606#,
      hgatp          => 16#680#,
      tselect        => 16#7A0#,
      tdata1         => 16#7A1#,
      tdata2         => 16#7A2#,
      tdata3         => 16#7A3#,
      dcsr           => 16#7B0#,
      dpc            => 16#7B1#,
      dscratch0      => 16#7B2#,
      dscratch1      => 16#7B3#,
      mcycle         => 16#B00#,
      minstret       => 16#B02#,
      mhpmcounter3   => 16#B03#,
      mhpmcounter4   => 16#B04#,
      mhpmcounter31  => 16#B1F#,
      mcycleh        => 16#B80#,
      minstreth      => 16#B82#,
      mhpmcounter3h  => 16#B83#,
      mhpmcounter4h  => 16#B84#,
      mhpmcounter31h => 16#B9F#,
      cycle          => 16#C00#,
      time           => 16#C01#,
      instret        => 16#C02#,
      hpmcounter3    => 16#C03#,
      hpmcounter4    => 16#C04#,
      hpmcounter31   => 16#C1F#,
      cycleh         => 16#C80#,
      timeh          => 16#C81#,
      instreth       => 16#C82#,
      hpmcounter3h   => 16#C83#,
      hpmcounter4h   => 16#C84#,
      hpmcounter31h  => 16#C9F#,
      mvendorid      => 16#F11#,
      marchid        => 16#F12#,
      mimpid         => 16#F13#,
      mhartid        => 16#F14#,

      Not_Implemented => 16#1000#
     );

end LibRISCV.CSR;

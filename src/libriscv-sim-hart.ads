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

with LibRISCV.Sim.Memory_Bus;

with LibRISCV.Except;

private with LibRISCV.Instructions;
private with LibRISCV.CSR;

package LibRISCV.Sim.Hart
with SPARK_Mode => On
is

   type Instance
   is tagged private;

   subtype Class is Instance'Class;

   procedure Reset (This : in out Instance);

   procedure Cycle (This : in out Instance;
                    Bus  : in out Sim.Memory_Bus.Class);

   type State_Kind is (Reset, Running);

   function State (This : Instance) return State_Kind;

   procedure Dump (This : Instance);

   type Privilege_Level is (User, Machine)
     with Size => 2;

private

   for Privilege_Level use (User    => 0,
                            Machine => 3);

   type GPR_Array is array (GPR_Id) of Register;
   type CSR_Array is array (CSR.Name) of Register;

   type Instance
   is tagged record
      Privilege : Privilege_Level := Machine;
      State     : State_Kind := Reset;
      PC        : Register;
      Next_PC   : Register;

      Mcause : Except.Kind;
      Mepc   : U_Register;

      GPR   : GPR_Array;
      CSRs : CSR_Array;
   end record
     with Type_Invariant => Instance.GPR (0).U = 0;

   type Branch_Condition is (Cond_EQ, Cond_NE, Cond_LT, Cond_GE, Cond_GEU,
                             Cond_LTU);

   procedure Fetch (This     : in out Instance;
                    Bus      : in out Sim.Memory_Bus.Class;
                    Raw_Insn :    out Word;
                    Success  :    out Boolean);

   function Evaluate (Cond   : Branch_Condition;
                      R1, R2 : Register)
                      return Boolean;


   procedure Raise_Exception (This : in out Instance;
                              E    : Except.Kind;
                              tval : U_Register := 0);

   procedure Read_CSR (This : in out Instance;
                       Data :    out Register;
                       Id   :        CSR.Id);

   procedure Write_CSR (This : in out Instance;
                        Data :        Register;
                        Id   :        CSR.Id);

   procedure Exec_Branch (This     : in out Instance;
                          Insn     : Instructions.Instruction)
     with Pre => Insn.Kind in Instructions.B_Insn_Kind;

   procedure Exec_Instruction (This : in out Instance; Raw : Word; Bus : in out Sim.Memory_Bus.Class);
   procedure Exec_Invalid (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_SLLI (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_SRLI (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_SRAI (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_ADD (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_SUB (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_SLL (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_SLT (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_SLTU (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_XOR (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_SRL (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_SRA (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_OR (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_AND (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_SB (This : in out Instance; Insn : Instructions.Instruction; Bus : in out Sim.Memory_Bus.Class);
   procedure Exec_SH (This : in out Instance; Insn : Instructions.Instruction; Bus : in out Sim.Memory_Bus.Class);
   procedure Exec_SW (This : in out Instance; Insn : Instructions.Instruction; Bus : in out Sim.Memory_Bus.Class);
   procedure Exec_JALR (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_LB (This : in out Instance; Insn : Instructions.Instruction; Bus : Sim.Memory_Bus.Class);
   procedure Exec_LH (This : in out Instance; Insn : Instructions.Instruction; Bus : Sim.Memory_Bus.Class);
   procedure Exec_LW (This : in out Instance; Insn : Instructions.Instruction; Bus : Sim.Memory_Bus.Class);
   procedure Exec_LBU (This : in out Instance; Insn : Instructions.Instruction; Bus : Sim.Memory_Bus.Class);
   procedure Exec_LHU (This : in out Instance; Insn : Instructions.Instruction; Bus : Sim.Memory_Bus.Class);
   procedure Exec_ADDI (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_SLTI (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_SLTIU (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_XORI (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_ORI (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_ANDI (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_FENCE (This : in out Instance);
   procedure Exec_FENCE_I (This : in out Instance);
   procedure Exec_ECALL (This : in out Instance);
   procedure Exec_EBREAK (This : in out Instance);
   procedure Exec_CSRRW (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_CSRRS (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_CSRRC (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_CSRRWI (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_CSRRSI (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_CSRRCI (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_LUI (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_AUIPC (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_JAL (This : in out Instance; Insn : Instructions.Instruction);
   procedure Exec_XRET (This : in out Instance; Insn : Instructions.Instruction);
end LibRISCV.Sim.Hart;

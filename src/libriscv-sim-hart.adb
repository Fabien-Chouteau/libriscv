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

with LibRISCV.Instructions; use LibRISCV.Instructions;
with LibRISCV.Sim.Log;

package body LibRISCV.Sim.Hart is

   use type Sim.Memory_Bus.Access_Result;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Instance) is
   begin
      This.Privilege := Machine;
      This.PC.U := 16#80000000#;
      This.GPR := (others => (U => 0, others => <>));
      This.CSRs (CSR.mhartid).U := 0;
      This.CSRs (CSR.mtvec).U := 0;
   end Reset;

   -----------
   -- Cycle --
   -----------

   procedure Cycle (This : in out Instance;
                    Bus  : in out Sim.Memory_Bus.Class)
   is
      Raw : Word;
      Fetch_Success : Boolean := False;
   begin

      if This.State = Debug_Halt then
         return;
      end if;

      This.Next_PC.U := This.PC.U + 4;

      This.Fetch (Bus, Raw, Fetch_Success);

      if Fetch_Success then
         This.Exec_Instruction (Raw, Bus);
      end if;

      This.PC := This.Next_PC;

      if This.State = Single_Step then
         This.Halt_Src := Single_Step;
         This.State := Debug_Halt;
      end if;

   end Cycle;

   ----------------------
   -- Exec_Instruction --
   ----------------------

   procedure Exec_Instruction (This : in out Instance;
                               Raw  :        Word;
                               Bus  : in out Sim.Memory_Bus.Class)
   is
      Insn : constant Instruction := Decode (Raw);
   begin

      if Sim.Log.Decode then
         Sim.Log.Put_Line (Img (Insn, This.PC.U));
      end if;
      if Sim.Log.Exec then
         This.Dump;
      end if;

      case Insn.Kind is
         when Invalid      => This.Exec_Invalid (Insn);
         when Insn_SLLI    => This.Exec_SLLI (Insn);
         when Insn_SRLI    => This.Exec_SRLI (Insn);
         when Insn_SRAI    => This.Exec_SRAI (Insn);
         when Insn_ADD     => This.Exec_ADD (Insn);
         when Insn_SUB     => This.Exec_SUB (Insn);
         when Insn_SLL     => This.Exec_SLL (Insn);
         when Insn_SLT     => This.Exec_SLT (Insn);
         when Insn_SLTU    => This.Exec_SLTU (Insn);
         when Insn_XOR     => This.Exec_XOR (Insn);
         when Insn_SRL     => This.Exec_SRL (Insn);
         when Insn_SRA     => This.Exec_SRA (Insn);
         when Insn_OR      => This.Exec_OR (Insn);
         when Insn_AND     => This.Exec_AND (Insn);
         when Insn_BEQ     => This.Exec_Branch (Insn);
         when Insn_BNE     => This.Exec_Branch (Insn);
         when Insn_BLT     => This.Exec_Branch (Insn);
         when Insn_BGE     => This.Exec_Branch (Insn);
         when Insn_BLTU    => This.Exec_Branch (Insn);
         when Insn_BGEU    => This.Exec_Branch (Insn);
         when Insn_SB      => This.Exec_SB (Insn, Bus);
         when Insn_SH      => This.Exec_SH (Insn, Bus);
         when Insn_SW      => This.Exec_SW (Insn, Bus);
         when Insn_JALR    => This.Exec_JALR (Insn);
         when Insn_LB      => This.Exec_LB (Insn, Bus);
         when Insn_LH      => This.Exec_LH (Insn, Bus);
         when Insn_LW      => This.Exec_LW (Insn, Bus);
         when Insn_LBU     => This.Exec_LBU (Insn, Bus);
         when Insn_LHU     => This.Exec_LHU (Insn, Bus);
         when Insn_ADDI    => This.Exec_ADDI (Insn);
         when Insn_SLTI    => This.Exec_SLTI (Insn);
         when Insn_SLTIU   => This.Exec_SLTIU (Insn);
         when Insn_XORI    => This.Exec_XORI (Insn);
         when Insn_ORI     => This.Exec_ORI (Insn);
         when Insn_ANDI    => This.Exec_ANDI (Insn);
         when Insn_FENCE   => This.Exec_FENCE;
         when Insn_FENCE_I => This.Exec_FENCE_I;
         when Insn_ECALL   => This.Exec_ECALL;
         when Insn_EBREAK  => This.Exec_EBREAK;
         when Insn_CSRRW   => This.Exec_CSRRW (Insn);
         when Insn_CSRRS   => This.Exec_CSRRS (Insn);
         when Insn_CSRRC   => This.Exec_CSRRC (Insn);
         when Insn_CSRRWI  => This.Exec_CSRRWI (Insn);
         when Insn_CSRRSI  => This.Exec_CSRRSI (Insn);
         when Insn_CSRRCI  => This.Exec_CSRRCI (Insn);
         when Insn_LUI     => This.Exec_LUI (Insn);
         when Insn_AUIPC   => This.Exec_AUIPC (Insn);
         when Insn_JAL     => This.Exec_JAL (Insn);
         when Insn_URET |
              Insn_SRET |
              Insn_MRET    => This.Exec_XRET (Insn);
      end case;
   end Exec_Instruction;

   -----------
   -- State --
   -----------

   function State (This : Instance) return State_Kind
   is (This.State);

   -----------------
   -- Halt_Source --
   -----------------

   function Halt_Source (This : in out Instance) return Halt_Source_Kind is
      Res : constant Halt_Source_Kind := This.Halt_Src;
   begin
      This.Halt_Src := None;
      return Res;
   end Halt_Source;

   ----------
   -- Halt --
   ----------

   procedure Halt (This : in out Instance) is
   begin
      This.State := Debug_Halt;
   end Halt;

   ------------
   -- Resume --
   ------------

   procedure Resume (This : in out Instance) is
   begin
      This.State := Running;
   end Resume;

   -----------------
   -- Single_Step --
   -----------------

   procedure Single_Step (This : in out Instance) is
   begin
      This.State := Single_Step;
   end Single_Step;

   --------------
   -- Read_GPR --
   --------------

   function Read_GPR (This : Instance; Id : GPR_Id) return Register
   is (This.GPR (Id));

   -------------
   -- Read_PC --
   -------------

   function Read_PC (This : Instance) return Register
   is (This.PC);

   --------------
   -- Write_PC --
   --------------

   procedure Write_PC (This : in out Instance; Addr : Register) is
   begin
      This.PC := Addr;
   end Write_PC;

   ---------------------------
   -- Set_Debugger_Attached --
   ---------------------------

   procedure Set_Debugger_Attached (This     : in out Instance;
                                    Attached : Boolean := True)
   is
   begin
      This.Debugger_Attached := Attached;
   end Set_Debugger_Attached;

   --------------------
   -- Debug_Read_CSR --
   --------------------

   function Debug_Read_CSR (This : Instance; Id : CSR.Id) return Register is
   begin
      return This.CSRs (CSR.To_Name (Id));
   end Debug_Read_CSR;

   --------------------
   -- Add_Breakpoint --
   --------------------

   procedure Add_Breakpoint (This    : in out Instance;
                             Addr    :        Address;
                             Success :    out Boolean)
   is
   begin
      for Index in This.Breakpoints'Range loop
         if not This.Breakpoints (Index).Enabled then
            This.Breakpoints (Index).Enabled := True;
            This.Breakpoints (Index).Addr := Addr;
            Success := True;
         end if;
      end loop;
      Success := False;
   end Add_Breakpoint;

   -----------------------
   -- Remove_Breakpoint --
   -----------------------

   procedure Remove_Breakpoint (This    : in out Instance;
                                Addr    :        Address;
                                Success :    out Boolean)
   is
   begin
      for Index in This.Breakpoints'Range loop
         if This.Breakpoints (Index).Enabled
           and then
             This.Breakpoints (Index).Addr = Addr
         then
            This.Breakpoints (Index).Enabled := False;
            Success := True;
         end if;
      end loop;
      Success := False;
   end Remove_Breakpoint;

   ---------------
   -- Write_GPR --
   ---------------

   procedure Write_GPR (This  : in out Instance;
                        Id    :        GPR_Id;
                        Value :        Register)
   is
   begin
      if Id /= 0 then
         This.GPR (Id) := Value;
      end if;
   end Write_GPR;

   ----------
   -- Dump --
   ----------

   procedure Dump (This : Instance) is
   begin
      Sim.Log.Put_Line ("PC: 0x" & Hex (This.PC.U));

      for Id in GPR_Id loop
         Sim.Log.Put (Img (Id) & ": 0x" & Hex (This.GPR (Id).U));

         if Id mod 5 = 4 then
            Sim.Log.New_Line;
         else
            Sim.Log.Put (" ");
         end if;
      end loop;
      Sim.Log.New_Line;
   end Dump;

   -----------
   -- Fetch --
   -----------

   procedure Fetch (This     : in out Instance;
                    Bus      : in out Sim.Memory_Bus.Class;
                    Raw_Insn :    out Word;
                    Success  :    out Boolean)
   is
      Mem_Access : Sim.Memory_Bus.Access_Result;
   begin
      if (This.PC.U and 2#11#) /= 0 then
         This.Raise_Exception (Except.Instruction_Address_Misaligned,
                               This.PC.U);
         Success := False;
         return;
      end if;

      for Index in This.Breakpoints'Range loop
         if This.Breakpoints (Index).Enabled
           and then
             This.Breakpoints (Index).Addr = This.PC.U
         then
            if This.Debugger_Attached then
               This.State := Debug_Halt;
               This.Halt_Src := Breakpoint;
            else
               This.Raise_Exception (Except.Breakpoint);
            end if;
            Success := True;
            return;
         end if;
      end loop;

      Bus.Load_W (This.PC.U, Raw_Insn, Mem_Access);

      if Mem_Access /= Sim.Memory_Bus.Success then
         This.Raise_Exception (Except.Instruction_Access_Fault,
                               This.PC.U);
         Success := False;
      else
         Success := True;
      end if;
   end Fetch;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Cond   : Branch_Condition;
                      R1, R2 : Register)
                      return Boolean
   is
   begin
      case Cond is
         when Cond_EQ =>
            return R1.U = R2.U;
         when Cond_NE =>
            return R1.U /= R2.U;
         when Cond_LT =>
            return R1.S < R2.S;
         when Cond_LTU =>
            return R1.U < R2.U;
         when Cond_GE =>
            return R1.S >= R2.S;
         when Cond_GEU =>
            return R1.U >= R2.U;
      end case;
   end Evaluate;

   -----------------
   -- Exec_Branch --
   -----------------

   procedure Exec_Branch (This : in out Instance;
                          Insn : Instructions.Instruction)
   is
      Actual_Offset : Register;
      Cond          : constant Branch_Condition :=
        (case Insn.Kind is
            when Insn_BEQ  => Cond_EQ,
            when Insn_BGE  => Cond_GE,
            when Insn_BGEU => Cond_GEU,
            when Insn_BLT  => Cond_LT,
            when Insn_BLTU => Cond_LTU,
            when Insn_BNE  => Cond_NE,
            when others    =>
               raise Program_Error with "not a branch instruction");
   begin
      if Evaluate (Cond, This.GPR (Insn.B_RS1), This.GPR (Insn.B_RS2)) then
         Actual_Offset := Sign_Extend (Insn.B_Imm);

         This.Next_PC.U := This.PC.U + Actual_Offset.U;
      end if;
   end Exec_Branch;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception (This : in out Instance;
                              E    : Except.Kind;
                              tval : U_Register := 0)
   is
   begin
      if Sim.Log.Except then
         Sim.Log.Put_Line ("Raise exception: " & E'Img &
                         " tval: 0x" & Hex (tval));
         Sim.Log.Put_Line ("   MTVEC: : 0x" &
                         Hex (This.CSRs (CSR.mtvec).U));
      end if;

      This.Mcause := E;

      This.CSRs (CSR.mepc) := This.PC;
      This.Next_PC.U := This.CSRs (CSR.mtvec).U and (not 2#11#);

      This.CSRs (CSR.mtval).U := tval;
   end Raise_Exception;

   --------------
   -- Read_CSR --
   --------------

   procedure Read_CSR (This : in out Instance;
                       Data :    out Register;
                       Id   :        CSR.Id)
   is
      Name : constant CSR.Name := CSR.To_Name (Id);
   begin
      case Name is

         --  CSR implemted as simple read
         when CSR.mhartid |
              CSR.mtvec | CSR.stvec |
              CSR.satp |
              CSR.pmpaddr0 | CSR.pmpaddr1 |
              CSR.pmpcfg0 | CSR.pmpcfg1 | CSR.pmpcfg2 | CSR.pmpcfg3 |
              CSR.medeleg | CSR.mideleg | CSR.mie |
              CSR.mstatus | CSR.mepc    | CSR.mtval |
              CSR.mscratch
            =>
            Data := This.CSRs (Name);

         when CSR.misa =>
            Data.U := 2#01_0000_00000000000000000100000000#;

         when CSR.mcause =>
            Data.U := U_Register (This.Mcause'Enum_Rep);

         when others =>
            raise Program_Error with "CSR read not implemented: " & Name'Img;
      end case;

      if Sim.Log.CSRs then
         Sim.Log.Put ("Read CSR: " & CSR.Img (Id) & " (0x" & CSR.Hex (Id));
         Sim.Log.Put_Line (") Data: 0x" & Hex (Data.U));
      end if;
   end Read_CSR;

   ---------------
   -- Write_CSR --
   ---------------

   procedure Write_CSR (This : in out Instance;
                        Data :        Register;
                        Id   :        CSR.Id)
   is
      Name : constant CSR.Name := CSR.To_Name (Id);
   begin
      if Sim.Log.CSRs then
         Sim.Log.Put ("Write CSR: " & CSR.Img (Id) & " (0x" & CSR.Hex (Id));
         Sim.Log.Put_Line (") Data: 0x" & Hex (Data.U));
      end if;

      case Name is

         --  CSR implemted as simple write, no checks on the data
         when CSR.mtvec | CSR.stvec |
              CSR.satp |
              CSR.pmpaddr0 | CSR.pmpaddr1 |
              CSR.pmpcfg0 | CSR.pmpcfg1 | CSR.pmpcfg2 | CSR.pmpcfg3 |
              CSR.medeleg | CSR.mideleg | CSR.mie |
              CSR.mstatus | CSR.mepc    | CSR.mtval |
              CSR.mscratch
            =>
            This.CSRs (Name) := Data;

         --  Ignore write
         when CSR.misa => null;

         when others =>
            raise Program_Error with "CSR write not implemented: " & Name'Img;
      end case;
   end Write_CSR;

   ------------------
   -- Exec_Invalid --
   ------------------

   procedure Exec_Invalid (This : in out Instance;
                           Insn :        Instructions.Instruction)
   is
   begin
      This.Raise_Exception (Except.Illegal_Instruction,
                            U_Register (Insn.Raw));
   end Exec_Invalid;

   ---------------
   -- Exec_SLLI --
   ---------------

   procedure Exec_SLLI (This : in out Instance;
                        Insn :        Instructions.Instruction)
   is
   begin
      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD).U :=
           Shift_Left (This.GPR (Insn.I_RS1).U,
                       Natural (Insn.I_Imm and 2#1_1111#));
      end if;
   end Exec_SLLI;

   ---------------
   -- Exec_SRLI --
   ---------------

   procedure Exec_SRLI (This : in out Instance;
                        Insn :        Instructions.Instruction)
   is
   begin
      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD).U :=
           Shift_Right (This.GPR (Insn.I_RS1).U,
                        Natural (Insn.I_Imm and 2#1_1111#));
      end if;
   end Exec_SRLI;

   ---------------
   -- Exec_SRAI --
   ---------------

   procedure Exec_SRAI (This : in out Instance;
                        Insn :        Instructions.Instruction)
   is
   begin
      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD).U :=
           Shift_Right_Arithmetic (This.GPR (Insn.I_RS1).U,
                                   Natural (Insn.I_Imm and 2#1_1111#));
      end if;
   end Exec_SRAI;

   --------------
   -- Exec_ADD --
   --------------

   procedure Exec_ADD (This : in out Instance;
                       Insn :        Instruction)
   is
   begin
      if Insn.R_RD /= 0 then
         This.GPR (Insn.R_RD).U :=
           This.GPR (Insn.R_RS1).U + This.GPR (Insn.R_RS2).U;
      end if;
   end Exec_ADD;

   --------------
   -- Exec_SUB --
   --------------

   procedure Exec_SUB (This : in out Instance;
                       Insn :        Instruction)
   is
   begin
      if Insn.R_RD /= 0 then
         This.GPR (Insn.R_RD).U :=
           This.GPR (Insn.R_RS1).U - This.GPR (Insn.R_RS2).U;
      end if;
   end Exec_SUB;

   --------------
   -- Exec_SLL --
   --------------

   procedure Exec_SLL (This : in out Instance;
                       Insn :        Instruction)
   is
   begin
      if Insn.R_RD /= 0 then
         This.GPR (Insn.R_RD).U :=
           Shift_Left (This.GPR (Insn.R_RS1).U,
                       Natural (This.GPR (Insn.R_RS2).U and 2#1_1111#));
      end if;
   end Exec_SLL;

   --------------
   -- Exec_SLT --
   --------------

   procedure Exec_SLT (This : in out Instance;
                       Insn :        Instruction)
   is
   begin
      if Insn.R_RD /= 0 then
         This.GPR (Insn.R_RD).S :=
           (if This.GPR (Insn.R_RS1).S < This.GPR (Insn.R_RS2).S
            then 1 else 0);

      end if;
   end Exec_SLT;

   ---------------
   -- Exec_SLTU --
   ---------------

   procedure Exec_SLTU (This : in out Instance;
                        Insn :        Instruction)
   is
   begin
      if Insn.R_RD /= 0 then
         This.GPR (Insn.R_RD).U :=
           (if This.GPR (Insn.R_RS1).U < This.GPR (Insn.R_RS2).U
            then 1 else 0);

      end if;
   end Exec_SLTU;

   --------------
   -- Exec_XOR --
   --------------

   procedure Exec_XOR
     (This : in out Instance;
      Insn :        Instructions.Instruction)
   is
   begin
      if Insn.R_RD /= 0 then
         This.GPR (Insn.R_RD).U :=
           This.GPR (Insn.R_RS1).U xor This.GPR (Insn.R_RS2).U;
      end if;
   end Exec_XOR;

   --------------
   -- Exec_SRL --
   --------------

   procedure Exec_SRL (This : in out Instance;
                       Insn :        Instruction)
   is
   begin
      if Insn.R_RD /= 0 then
         This.GPR (Insn.R_RD).U :=
           Shift_Right (This.GPR (Insn.R_RS1).U,
                        Natural (This.GPR (Insn.R_RS2).U and 2#1_1111#));
      end if;
   end Exec_SRL;

   --------------
   -- Exec_SRA --
   --------------

   procedure Exec_SRA (This : in out Instance;
                       Insn :        Instruction)
   is
   begin
      if Insn.R_RD /= 0 then
         This.GPR (Insn.R_RD).U :=
           Shift_Right_Arithmetic
             (This.GPR (Insn.R_RS1).U,
              Natural (This.GPR (Insn.R_RS2).U and 2#1_1111#));
      end if;
   end Exec_SRA;

   -------------
   -- Exec_OR --
   -------------

   procedure Exec_OR
     (This : in out Instance;
      Insn : Instructions.Instruction)
   is
   begin
      if Insn.R_RD /= 0 then
         This.GPR (Insn.R_RD).U :=
           This.GPR (Insn.R_RS1).U or This.GPR (Insn.R_RS2).U;
      end if;
   end Exec_OR;

   --------------
   -- Exec_AND --
   --------------

   procedure Exec_AND
     (This : in out Instance;
      Insn : Instructions.Instruction)
   is
   begin
      if Insn.R_RD /= 0 then
         This.GPR (Insn.R_RD).U :=
           This.GPR (Insn.R_RS1).U and This.GPR (Insn.R_RS2).U;
      end if;
   end Exec_AND;

   -------------
   -- Exec_SB --
   -------------

   procedure Exec_SB
     (This : in out Instance;
      Insn :        Instructions.Instruction;
      Bus  : in out Sim.Memory_Bus.Class)
   is
      Data : Byte;
      Res  : Sim.Memory_Bus.Access_Result;
      Addr : Register;
   begin
      Data := Byte (This.GPR (Insn.S_RS2).U and 16#FF#);
      Addr.U := This.GPR (Insn.S_RS1).U + Sign_Extend (Insn.S_Imm).U;
      Bus.Store_B (Addr.U, Data, Res);

      if Res /= Sim.Memory_Bus.Success then
         This.Raise_Exception (Except.Store_AMO_Access_Fault, Addr.U);
      end if;
   end Exec_SB;

   -------------
   -- Exec_SH --
   -------------

   procedure Exec_SH
     (This : in out Instance;
      Insn :        Instructions.Instruction;
      Bus  : in out Sim.Memory_Bus.Class)
   is
      Data : Halfword;
      Res  : Sim.Memory_Bus.Access_Result;
      Addr : Register;
   begin

      Data := Halfword (This.GPR (Insn.S_RS2).U and 16#FF_FF#);
      Addr.U := This.GPR (Insn.S_RS1).U + Sign_Extend (Insn.S_Imm).U;

      if (Addr.U and 2#1#) /= 0 then
         This.Raise_Exception (Except.Store_AMO_Address_Misaligned, Addr.U);
         return;
      end if;

      Bus.Store_H (Addr.U, Data, Res);

      if Res /= Sim.Memory_Bus.Success then
         This.Raise_Exception (Except.Store_AMO_Access_Fault, Addr.U);
      end if;
   end Exec_SH;

   -------------
   -- Exec_SW --
   -------------

   procedure Exec_SW
     (This : in out Instance;
      Insn :        Instructions.Instruction;
      Bus  : in out Sim.Memory_Bus.Class)
   is
      Data : Word;
      Res  : Sim.Memory_Bus.Access_Result;
      Addr : Register;
   begin
      Data := Word (This.GPR (Insn.S_RS2).U and 16#FF_FF_FF_FF#);
      Addr.U := This.GPR (Insn.S_RS1).U + Sign_Extend (Insn.S_Imm).U;

      if (Addr.U and 2#11#) /= 0 then
         This.Raise_Exception (Except.Store_AMO_Address_Misaligned, Addr.U);
         return;
      end if;

      Bus.Store_W (Addr.U, Data, Res);

      if Res /= Sim.Memory_Bus.Success then
         This.Raise_Exception (Except.Store_AMO_Access_Fault, Addr.U);
      end if;
   end Exec_SW;

   -------------
   -- Exec_LB --
   -------------

   procedure Exec_LB
     (This : in out Instance;
      Insn :        Instructions.Instruction;
      Bus  :        Sim.Memory_Bus.Class)
   is
      Data : Byte;
      Res  : Sim.Memory_Bus.Access_Result;
      Addr : Register;
   begin
      Addr.U := This.GPR (Insn.I_RS1).U + Sign_Extend (Insn.I_Imm).U;
      Bus.Load_B (Addr.U, Data, Res);

      if Res /= Sim.Memory_Bus.Success then
         This.Raise_Exception (Except.Load_Access_Fault, Addr.U);
         return;
      end if;

      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD).U := Sign_Extend (Data);
      end if;
   end Exec_LB;

   -------------
   -- Exec_LH --
   -------------

   procedure Exec_LH
     (This : in out Instance;
      Insn :        Instructions.Instruction;
      Bus  :        Sim.Memory_Bus.Class)
   is
      Data : Halfword;
      Res  : Sim.Memory_Bus.Access_Result;
      Addr : Register;
   begin
      Addr.U := This.GPR (Insn.I_RS1).U + Sign_Extend (Insn.I_Imm).U;

      if (Addr.U and 2#1#) /= 0 then
         This.Raise_Exception (Except.Load_Address_Misaligned, Addr.U);
         return;
      end if;

      Bus.Load_H (Addr.U, Data, Res);

      if Res /= Sim.Memory_Bus.Success then
         This.Raise_Exception (Except.Load_Access_Fault, Addr.U);
         return;
      end if;

      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD).U := Sign_Extend (Data);
      end if;
   end Exec_LH;

   -------------
   -- Exec_LW --
   -------------

   procedure Exec_LW
     (This : in out Instance;
      Insn :        Instructions.Instruction;
      Bus  :        Sim.Memory_Bus.Class)
   is
      Data : Word;
      Res  : Sim.Memory_Bus.Access_Result;
      Addr : Register;
   begin
      Addr.U := This.GPR (Insn.I_RS1).U + Sign_Extend (Insn.I_Imm).U;

      if (Addr.U and 2#11#) /= 0 then
         This.Raise_Exception (Except.Load_Address_Misaligned, Addr.U);
         return;
      end if;

      Bus.Load_W (Addr.U, Data, Res);

      if Res /= Sim.Memory_Bus.Success then
         This.Raise_Exception (Except.Load_Access_Fault, Addr.U);
         return;
      end if;

      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD).U := Sign_Extend (Data);
      end if;
   end Exec_LW;

   --------------
   -- Exec_LBU --
   --------------

   procedure Exec_LBU
     (This : in out Instance;
      Insn :        Instructions.Instruction;
      Bus  :        Sim.Memory_Bus.Class)
   is
      Data : Byte;
      Res  : Sim.Memory_Bus.Access_Result;
      Addr : Register;
   begin
      Addr.U := This.GPR (Insn.I_RS1).U + Sign_Extend (Insn.I_Imm).U;
      Bus.Load_B (Addr.U, Data, Res);

      if Res /= Sim.Memory_Bus.Success then
         This.Raise_Exception (Except.Load_Access_Fault, Addr.U);
         return;
      end if;

      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD).U := U_Register (Data);
      end if;
   end Exec_LBU;

   --------------
   -- Exec_LHU --
   --------------

   procedure Exec_LHU
     (This : in out Instance;
      Insn :        Instructions.Instruction;
      Bus  :        Sim.Memory_Bus.Class)
   is
      Data : Halfword;
      Res  : Sim.Memory_Bus.Access_Result;
      Addr : Register;
   begin
      Addr.U := This.GPR (Insn.I_RS1).U + Sign_Extend (Insn.I_Imm).U;

      if (Addr.U and 2#1#) /= 0 then
         This.Raise_Exception (Except.Load_Address_Misaligned, Addr.U);
         return;
      end if;

      Bus.Load_H (Addr.U, Data, Res);

      if Res /= Sim.Memory_Bus.Success then
         This.Raise_Exception (Except.Load_Access_Fault, Addr.U);
      end if;

      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD).U := U_Register (Data);
      end if;
   end Exec_LHU;

   ---------------
   -- Exec_ADDI --
   ---------------

   procedure Exec_ADDI (This : in out Instance;
                        Insn : Instructions.Instruction)
   is
   begin
      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD).U :=
           This.GPR (Insn.I_RS1).U + Sign_Extend (Insn.I_Imm).U;
      end if;
   end Exec_ADDI;

   ---------------
   -- Exec_SLTI --
   ---------------

   procedure Exec_SLTI (This : in out Instance;
                        Insn :        Instruction)
   is
   begin
      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD).U :=
           (if This.GPR (Insn.I_RS1).S < Sign_Extend (Insn.I_Imm).S
            then 1 else 0);
      end if;
   end Exec_SLTI;

   ----------------
   -- Exec_SLTIU --
   ----------------

   procedure Exec_SLTIU (This : in out Instance;
                         Insn :        Instruction)
   is
   begin
      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD).U :=
           (if This.GPR (Insn.I_RS1).U < Sign_Extend (Insn.I_Imm).U
            then 1 else 0);
      end if;
   end Exec_SLTIU;

   ---------------
   -- Exec_XORI --
   ---------------

   procedure Exec_XORI
     (This : in out Instance;
      Insn : Instructions.Instruction)
   is
   begin
      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD).U :=
           This.GPR (Insn.I_RS1).U xor Sign_Extend (Insn.I_Imm).U;
      end if;
   end Exec_XORI;

   --------------
   -- Exec_ORI --
   --------------

   procedure Exec_ORI
     (This : in out Instance;
      Insn : Instructions.Instruction)
   is
   begin
      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD).U :=
           This.GPR (Insn.I_RS1).U or Sign_Extend (Insn.I_Imm).U;
      end if;
   end Exec_ORI;

   ---------------
   -- Exec_ANDI --
   ---------------

   procedure Exec_ANDI
     (This : in out Instance;
      Insn : Instructions.Instruction)
   is
   begin
      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD).U :=
           This.GPR (Insn.I_RS1).U and Sign_Extend (Insn.I_Imm).U;
      end if;
   end Exec_ANDI;

   ----------------
   -- Exec_FENCE --
   ----------------

   procedure Exec_FENCE (This : in out Instance) is
   begin
      null; -- Nothing to do here for our simple simulator
   end Exec_FENCE;

   ------------------
   -- Exec_FENCE_I --
   ------------------

   procedure Exec_FENCE_I (This : in out Instance) is
   begin
      null; -- Nothing to do here for our simple simulator
   end Exec_FENCE_I;

   ----------------
   -- Exec_ECALL --
   ----------------

   procedure Exec_ECALL (This : in out Instance) is
   begin
      This.Raise_Exception
        ((case This.Privilege is
            when User    => Except.Environment_Call_From_U_mode,
            when Machine => Except.Environment_Call_From_M_mode
        ));
   end Exec_ECALL;

   -----------------
   -- Exec_EBREAK --
   -----------------


   procedure Exec_EBREAK (This : in out Instance) is
   begin
      if This.Debugger_Attached then
         This.State := Debug_Halt;
         This.Halt_Src := Breakpoint;
         This.Next_PC := This.PC;
      else
         This.Raise_Exception (Except.Breakpoint);
      end if;
   end Exec_EBREAK;

   ----------------
   -- Exec_CSRRW --
   ----------------

   procedure Exec_CSRRW (This : in out Instance;
                         Insn :        Instructions.Instruction)
   is
      Id   : constant CSR.Id := CSR.Id (Insn.I_Imm);
      Data : Register;
   begin

      if Insn.I_RD /= 0 then
         --  Read but not store in the GPR yet, as this GPR can also be the
         --  source.
         This.Read_CSR (Data, Id);
      end if;

      This.Write_CSR (This.GPR (Insn.I_RS1), Id);

      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD) := Data;
      end if;

   end Exec_CSRRW;

   ----------------
   -- Exec_CSRRS --
   ----------------

   procedure Exec_CSRRS (This : in out Instance;
                         Insn :        Instructions.Instruction)
   is
      Id   : constant CSR.Id := CSR.Id (Insn.I_Imm);
      R_Data : Register;
      W_Data : Register;
   begin

      This.Read_CSR (R_Data, Id);

      if Insn.I_RS1 /= 0 then
         W_Data.U := R_Data.U or This.GPR (Insn.I_RS1).U;
         This.Write_CSR (W_Data, Id);
      end if;

      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD) := R_Data;
      end if;
   end Exec_CSRRS;

   ----------------
   -- Exec_CSRRC --
   ----------------

   procedure Exec_CSRRC (This : in out Instance;
                         Insn :        Instructions.Instruction)
   is
      Id   : constant CSR.Id := CSR.Id (Insn.I_Imm);
      R_Data : Register;
      W_Data : Register;
   begin

      This.Read_CSR (R_Data, Id);

      if Insn.I_RS1 /= 0 then
         W_Data.U := R_Data.U and (not This.GPR (Insn.I_RS1).U);
         This.Write_CSR (W_Data, Id);
      end if;

      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD) := R_Data;
      end if;
   end Exec_CSRRC;

   -----------------
   -- Exec_CSRRWI --
   -----------------

   procedure Exec_CSRRWI (This : in out Instance;
                          Insn :        Instructions.Instruction)
   is
      Id     : constant CSR.Id := CSR.Id (Insn.I_Imm);
      Data : Register;
   begin

      if Insn.I_RD /= 0 then
         This.Read_CSR (Data, Id);
         This.GPR (Insn.I_RD) := Data;
      end if;

      Data.U := U_Register (Insn.I_RS1);
      This.Write_CSR (Data, Id);
   end Exec_CSRRWI;

   -----------------
   -- Exec_CSRRSI --
   -----------------

   procedure Exec_CSRRSI (This : in out Instance;
                          Insn :        Instructions.Instruction)
   is
      Id   : constant CSR.Id := CSR.Id (Insn.I_Imm);
      R_Data : Register;
      W_Data : Register;
   begin

      This.Read_CSR (R_Data, Id);

      if Insn.I_RS1 /= 0 then
         W_Data.U := R_Data.U or U_Register (Insn.I_RS1);
         This.Write_CSR (W_Data, Id);
      end if;

      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD) := R_Data;
      end if;
   end Exec_CSRRSI;

   -----------------
   -- Exec_CSRRCI --
   -----------------

   procedure Exec_CSRRCI (This : in out Instance;
                          Insn :        Instructions.Instruction)
   is
      Id   : constant CSR.Id := CSR.Id (Insn.I_Imm);
      R_Data : Register;
      W_Data : Register;
   begin

      This.Read_CSR (R_Data, Id);

      if Insn.I_RS1 /= 0 then
         W_Data.U := R_Data.U and (not U_Register (Insn.I_RS1));
         This.Write_CSR (W_Data, Id);
      end if;

      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD) := R_Data;
      end if;
   end Exec_CSRRCI;

   --------------
   -- Exec_LUI --
   --------------

   procedure Exec_LUI (This : in out Instance;
                       Insn :        Instructions.Instruction)
   is
   begin
      if Insn.U_RD /= 0 then
         This.GPR (Insn.U_RD).U := U_Register (Insn.U_Imm);
      end if;
   end Exec_LUI;

   ----------------
   -- Exec_AUIPC --
   ----------------

   procedure Exec_AUIPC (This : in out Instance;
                         Insn :        Instructions.Instruction)
   is
   begin
      if Insn.U_RD /= 0 then
         This.GPR (Insn.U_RD).U := This.PC.U + U_Register (Insn.U_Imm);
      end if;
   end Exec_AUIPC;

   --------------
   -- Exec_JAL --
   --------------

   procedure Exec_JAL (This : in out Instance;
                       Insn :        Instructions.Instruction)
   is
      Imm : constant Register := Sign_Extend (Insn.J_Imm);
   begin
      if Insn.J_RD /= 0 then
         This.GPR (Insn.J_RD).U := This.PC.U + 4;
      end if;

      This.Next_PC.U := This.PC.U + Imm.U;
   end Exec_JAL;

   ---------------
   -- Exec_JALR --
   ---------------

   procedure Exec_JALR (This : in out Instance;
                        Insn :        Instructions.Instruction)
   is
      Imm : constant Register := Sign_Extend (Insn.I_Imm);

      LR  : constant U_Register := This.PC.U + 4;
   begin

      This.Next_PC.U := This.GPR (Insn.I_RS1).U + Imm.U;
      This.Next_PC.U := This.Next_PC.U and (not 1);

      if Insn.I_RD /= 0 then
         This.GPR (Insn.I_RD).U := LR;
      end if;
   end Exec_JALR;

   ---------------
   -- Exec_XRET --
   ---------------

   procedure Exec_XRET (This : in out Instance;
                        Insn :        Instructions.Instruction)
   is
      pragma Unreferenced (Insn);
   begin
      --  TODO: change priviledge mode
      This.Next_PC.U := This.CSRs (CSR.mepc).U;
   end Exec_XRET;

end LibRISCV.Sim.Hart;

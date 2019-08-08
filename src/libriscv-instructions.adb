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

with LibRISCV.CSR;

with ESF;

package body LibRISCV.Instructions
with SPARK_Mode => On
is

   function Shift_Right (Value  : Word; Amount : Natural) return Word;
   pragma Import (Intrinsic, Shift_Right);

   ------------
   -- Decode --
   ------------

   function Decode (Raw : Word) return Instruction is

      function Opcode return Raw_Opcode
      is (Raw_Opcode (Raw and 2#111_1111#));

      function RD return GPR_Id
      is (GPR_Id (Shift_Right (Raw, 7) and 2#11111#));

      function RS1 return GPR_Id
      is (GPR_Id (Shift_Right (Raw, 15) and 2#11111#));

      function RS2 return GPR_Id
      is (GPR_Id (Shift_Right (Raw, 20) and 2#11111#));

      function Funct3 return Raw_Funct3
      is (Raw_Funct3 (Shift_Right (Raw, 12) and 2#111#));

      function Imm12 return Raw_Imm12
      is (Raw_Imm12 (Shift_Right (Raw, 20) and 2#1111_1111_1111#));

      function Imm12_S return Raw_Imm12 is
         I_11_5, I_4_0 : Word;
      begin
         I_11_5 := Shift_Right (Raw, 25) and 2#111_1111#;
         I_4_0  := Shift_Right (Raw, 7) and 2#1_1111#;

         return Raw_Imm12 (Shift_Left (I_11_5, 5)
                           or I_4_0);
      end Imm12_S;

      function Imm13_B return Raw_Imm13 is
         I_12, I_10_5, I_4_1, I_11 : Word;
      begin
         I_12   := Shift_Right (Raw, 31) and 2#1#;
         I_10_5 := Shift_Right (Raw, 25) and 2#11_1111#;
         I_4_1  := Shift_Right (Raw,  8) and 2#1111#;
         I_11   := Shift_Right (Raw,  7) and 2#1#;

         return Raw_Imm13 (Shift_Left (I_12, 12)
                           or Shift_Left (I_10_5, 5)
                           or Shift_Left (I_4_1, 1)
                           or Shift_Left (I_11, 11));
      end Imm13_B;

      function Funct7 return Raw_Funct7
      is (Raw_Funct7 (Shift_Right (Raw, 25) and 2#111_1111#));

      function Imm32_U return Word
      is (Raw and 16#FF_FF_F0_00#);

      function Imm21_J return Raw_Imm21 is
         I_20, I_10_1, I_11, I_19_12 : Word;

         Res : Raw_Imm21;
      begin
         I_20    := Shift_Right (Raw, 31) and 2#1#;
         I_10_1  := Shift_Right (Raw, 21) and 2#11_1111_1111#;
         I_11    := Shift_Right (Raw, 20) and 2#1#;
         I_19_12 := Shift_Right (Raw, 12) and 2#1111_1111#;

         Res := Raw_Imm21 (Shift_Left (I_20, 20)
                           or Shift_Left (I_19_12, 12)
                           or Shift_Left (I_11, 11)
                           or Shift_Left (I_10_1, 1));
         return Res;
      end Imm21_J;

      Invalid_Insn : constant Instruction := (Kind => Invalid, Raw => Raw);
   begin

      case Opcode is

         when OP_LUI      =>
            return (Insn_LUI, RD, Imm32_U);

         when OP_AUIPC    =>
            return (Insn_AUIPC, RD, Imm32_U);

         when OP_JAL    =>
            return (Insn_JAL, RD, Imm21_J);

         when OP_JALR     =>
            if Funct3 /= Funct3_JALR then
               return Invalid_Insn;
            else
               return (Insn_JALR, RD, RS1, Imm12);
            end if;

         when OP_Branch   =>
            return (case Funct3 is
                       when Funct3_BEQ  => (Insn_BEQ,  RS1, RS2, Imm13_B),
                       when Funct3_BNE  => (Insn_BNE,  RS1, RS2, Imm13_B),
                       when Funct3_BLT  => (Insn_BLT,  RS1, RS2, Imm13_B),
                       when Funct3_BGE  => (Insn_BGE,  RS1, RS2, Imm13_B),
                       when Funct3_BLTU => (Insn_BLTU, RS1, RS2, Imm13_B),
                       when Funct3_BGEU => (Insn_BGEU, RS1, RS2, Imm13_B),
                       when others      => Invalid_Insn);

         when OP_Load     =>
            return (case Funct3 is
                       when Funct3_LB  => (Insn_LB,  RD, RS1, Imm12),
                       when Funct3_LH  => (Insn_LH,  RD, RS1, Imm12),
                       when Funct3_LW  => (Insn_LW,  RD, RS1, Imm12),
                       when Funct3_LBU => (Insn_LBU, RD, RS1, Imm12),
                       when Funct3_LHU => (Insn_LHU, RD, RS1, Imm12),
                       when others     => Invalid_Insn);

         when OP_Store    =>
            return (case Funct3 is
                       when Funct3_SB => (Insn_SB, RS1, RS2, Imm12_S),
                       when Funct3_SH => (Insn_SH, RS1, RS2, Imm12_S),
                       when Funct3_SW => (Insn_SW, RS1, RS2, Imm12_S),
                       when others    => Invalid_Insn);

         when OP_OP_IMM   =>
            return (case Funct3 is
                       when Funct3_ADDI  => (Insn_ADDI,  RD, RS1, Imm12),
                       when Funct3_SLTI  => (Insn_SLTI,  RD, RS1, Imm12),
                       when Funct3_SLTIU => (Insn_SLTIU, RD, RS1, Imm12),
                       when Funct3_XORI  => (Insn_XORI,  RD, RS1, Imm12),
                       when Funct3_ORI   => (Insn_ORI,   RD, RS1, Imm12),
                       when Funct3_ANDI  => (Insn_ANDI,  RD, RS1, Imm12),

                       when Funct3_SLLI  =>
                         (case Funct7 is
                             when Funct7_SLLI => (Insn_SLLI,  RD, RS1, Imm12),
                             when others     => Invalid_Insn),

                       when Funct3_SRLI  =>
                         (case Funct7 is
                             when Funct7_SRLI => (Insn_SRLI,  RD, RS1, Imm12),
                             when Funct7_SRAI => (Insn_SRAI,  RD, RS1, Imm12),
                             when others     => Invalid_Insn));

         when OP_OP       =>
            return (case Funct3 is
                       when Funct3_ADD_SUB =>
                         (case Funct7 is
                             when Funct7_ADD => (Insn_ADD, RD, RS1, RS2),
                             when Funct7_SUB => (Insn_SUB, RD, RS1, RS2),
                             when others     => Invalid_Insn),

                       when Funct3_SLL =>
                         (case Funct7 is
                             when Funct7_SLL => (Insn_SLL, RD, RS1, RS2),
                             when others     => Invalid_Insn),

                       when Funct3_SLT =>
                         (case Funct7 is
                             when Funct7_SLT => (Insn_SLT, RD, RS1, RS2),
                             when others      => Invalid_Insn),

                       when Funct3_SLTU =>
                         (case Funct7 is
                             when Funct7_SLTU => (Insn_SLTU, RD, RS1, RS2),
                             when others      => Invalid_Insn),

                       when Funct3_XOR =>
                         (case Funct7 is
                             when Funct7_XOR => (Insn_XOR, RD, RS1, RS2),
                             when others     => Invalid_Insn),

                       when Funct3_OR =>
                         (case Funct7 is
                             when Funct7_OR => (Insn_OR, RD, RS1, RS2),
                             when others    => Invalid_Insn),

                       when Funct3_AND =>
                         (case Funct7 is
                             when Funct7_AND => (Insn_AND, RD, RS1, RS2),
                             when others     => Invalid_Insn),

                       when Funct3_SR =>
                         (case Funct7 is
                             when Funct7_SRL => (Insn_SRL, RD, RS1, RS2),
                             when Funct7_SRA => (Insn_SRA, RD, RS1, RS2),
                             when others     => Invalid_Insn));

         when OP_Misc_Mem =>
            if RD = 0 and then RS1 = 0 then
               return
                 (case Funct3 is
                     when Funct3_FENCE   => (Insn_FENCE, RD, RS1, Imm12),
                     when Funct3_FENCE_I => (Insn_FENCE_I, RD, RS1, Imm12),
                     when others         => Invalid_Insn);
            else
               return Invalid_Insn;
            end if;

         when OP_System =>
            if Funct3 = Funct3_PRIV then
               if RD = 0 and then RS1 = 0 then
                  return
                    (case Imm12 is
                        when 2#0000000_00000# => (Insn_ECALL, RD, RS1, Imm12),
                        when 2#0000000_00001# => (Insn_EBREAK, RD, RS1, Imm12),
                        when 2#0000000_00010# => (Insn_URET, RD, RS1, Imm12),
                        when 2#0001000_00010# => (Insn_SRET, RD, RS1, Imm12),
                        when 2#0011000_00010# => (Insn_MRET, RD, RS1, Imm12),
                        when others           => Invalid_Insn);

               else
                  return Invalid_Insn;
               end if;

            else

               return (case Funct3 is
                       when Funct3_CSRRW  => (Insn_CSRRW, RD, RS1, Imm12),
                       when Funct3_CSRRS  => (Insn_CSRRS, RD, RS1, Imm12),
                       when Funct3_CSRRC  => (Insn_CSRRC, RD, RS1, Imm12),
                       when Funct3_CSRRWI => (Insn_CSRRWI, RD, RS1, Imm12),
                       when Funct3_CSRRSI => (Insn_CSRRSI, RD, RS1, Imm12),
                       when Funct3_CSRRCI => (Insn_CSRRCI, RD, RS1, Imm12),
                       when others        => Invalid_Insn);
            end if;

         when others =>
            return Invalid_Insn;
      end case;
   end Decode;

   ------------
   -- Encode --
   ------------

   function Encode (Insn : Instruction) return Word is
   begin
      case Insn.Kind is
         when Invalid =>
            return Insn.Raw;
         when R_Insn_Kind =>
            return Encode_R (Insn);
         when I_Insn_Kind =>
            return Encode_I (Insn);
         when S_Insn_Kind =>
            return Encode_S (Insn);
         when B_Insn_Kind =>
            return Encode_B (Insn);
         when U_Insn_Kind =>
            return Encode_U (Insn);
         when J_Insn_Kind =>
            return Encode_J (Insn);
      end case;
   end Encode;

   ----------------
   -- Add_Opcode --
   ----------------

   procedure Add_Opcode (Raw : in out Word; Op : Raw_Opcode) is
   begin
      --  Clear
      Raw := Raw and not (2#111_1111#);

      --  Set
      Raw := Raw or Word (Op);
   end Add_Opcode;

   ----------------
   -- Add_Funct3 --
   ----------------

   procedure Add_Funct3 (Raw : in out Word; Funct : Raw_Funct3) is
   begin
      --  Clear
      Raw := Raw and not (2#111_0000_0000_0000#);

      --  Set
      Raw := Raw or Shift_Left (Word (Funct), 12);
   end Add_Funct3;

   ----------------
   -- Add_Funct7 --
   ----------------

   procedure Add_Funct7 (Raw : in out Word; Funct : Raw_Funct7) is
   begin
      --  Clear
      Raw := Raw and not (2#1111_1110_0000_0000_0000_0000_0000_0000#);

      --  Set
      Raw := Raw or Shift_Left (Word (Funct), 25);
   end Add_Funct7;

   ------------
   -- Add_RD --
   ------------

   procedure Add_RD (Raw : in out Word; Id : GPR_Id) is
   begin
      --  Clear
      Raw := Raw and not (2#1111_1000_0000#);

      --  Set
      Raw := Raw or Shift_Left (Word (Id), 7);
   end Add_RD;

   -------------
   -- Add_RS1 --
   -------------

   procedure Add_RS1 (Raw : in out Word; Id : GPR_Id) is
   begin
      --  Clear
      Raw := Raw and not (2#1111_1000_0000_0000_0000#);

      --  Set
      Raw := Raw or Shift_Left (Word (Id), 15);
   end Add_RS1;

   -------------
   -- Add_RS2 --
   -------------

   procedure Add_RS2 (Raw : in out Word; Id : GPR_Id) is
   begin
      --  Clear
      Raw := Raw and not (2#1_1111_0000_0000_0000_0000_0000#);

      --  Set
      Raw := Raw or Shift_Left (Word (Id), 20);
   end Add_RS2;

   -----------------
   -- Add_Imm13_B --
   -----------------

   procedure Add_Imm13_B (Raw : in out Word; Imm : Raw_Imm13) is
      I_12, I_10_5, I_4_1, I_11 : Word;
   begin
      --  Clear
      Raw := Raw and not (2#1111_1110_0000_0000_0001_1111_1000_0000#);

      I_12   := Shift_Right (Word (Imm), 12) and 2#1#;
      I_10_5 := Shift_Right (Word (Imm),  5) and 2#11_1111#;
      I_4_1  := Shift_Right (Word (Imm),  1) and 2#1111#;
      I_11   := Shift_Right (Word (Imm), 11) and 2#1#;

      --  Set
      Raw := Raw or ((Shift_Left (I_12, 31)
                      or Shift_Left (I_10_5, 25)
                      or Shift_Left (I_4_1, 8)
                      or Shift_Left (I_11, 7)));
   end Add_Imm13_B;

   -----------------
   -- Add_Imm12_I --
   -----------------

   procedure Add_Imm12_I (Raw : in out Word; Imm : Raw_Imm12) is
   begin
      --  Clear
      Raw := Raw and not (2#1111_1111_1111_0000_0000_0000_0000_0000#);

      --  Set
      Raw := Raw or (Shift_Left (Word (Imm), 20));
   end Add_Imm12_I;

   -----------------
   -- Add_Imm12_S --
   -----------------

   procedure Add_Imm12_S (Raw : in out Word; Imm : Raw_Imm12) is
      I_4_0, I_11_5 : Word;
   begin
      --  Clear
      Raw := Raw and not (2#1111_1110_0000_0000_0000_1111_1000_0000#);

      I_4_0  := Word (Imm) and 2#1_1111#;
      I_11_5 := Shift_Right (Word (Imm),  5) and 2#11_1111#;

      --  Set
      Raw := Raw or ((Shift_Left (I_11_5, 25)
                      or Shift_Left (I_4_0, 7)));
   end Add_Imm12_S;

   -----------------
   -- Add_Imm21_J --
   -----------------

   procedure Add_Imm21_J (Raw : in out Word; Imm : Raw_Imm21) is
      I_20, I_10_1, I_19_12, I_11 : Word;
   begin

      --  Clear
      Raw := Raw and not (2#1111_1110_0000_0000_0001_1111_1000_0000#);

      I_20   := Shift_Right (Word (Imm), 20) and 2#1#;
      I_10_1 := Shift_Right (Word (Imm),  1) and 2#11_1111_1111#;
      I_19_12  := Shift_Right (Word (Imm), 12) and 2#1111_1111#;
      I_11   := Shift_Right (Word (Imm), 11) and 2#1#;

      --  Set
      Raw := Raw or ((Shift_Left (I_20, 31)
                      or Shift_Left (I_10_1, 21)
                      or Shift_Left (I_19_12, 12)
                      or Shift_Left (I_11, 20)));
   end Add_Imm21_J;

   --------------
   -- Encode_R --
   --------------

   function Encode_R (Insn : Instruction) return Word is
      Raw : Word := 0;
   begin
      Add_RD (Raw, Insn.R_RD);
      Add_RS1 (Raw, Insn.R_RS1);
      Add_RS2 (Raw, Insn.R_RS2);

      Add_Opcode (Raw, OP_OP);

      Add_Funct3 (Raw, (case Insn.Kind is
                     when Insn_ADD | Insn_SUB => Funct3_ADD_SUB,
                     when Insn_SLL            => Funct3_SLL,
                     when Insn_SLT            => Funct3_SLT,
                     when Insn_SLTU           => Funct3_SLTU,
                     when Insn_XOR            => Funct3_XOR,
                     when Insn_SRL | Insn_SRA => Funct3_SR,
                     when Insn_OR             => Funct3_OR,
                     when Insn_AND            => Funct3_AND,
                     when others              => raise Program_Error));

      Add_Funct7 (Raw, (case Insn.Kind is
                     when Insn_ADD            => Funct7_ADD,
                     when Insn_SUB            => Funct7_SUB,
                     when Insn_SLL            => Funct7_SLL,
                     when Insn_SLT            => Funct7_SLT,
                     when Insn_SLTU           => Funct7_SLTU,
                     when Insn_XOR            => Funct7_XOR,
                     when Insn_SRL            => Funct7_SRL,
                     when Insn_SRA            => Funct7_SRA,
                     when Insn_OR             => Funct7_OR,
                     when Insn_AND            => Funct7_AND,
                     when others              => raise Program_Error));

      return Raw;
   end Encode_R;

   --------------
   -- Encode_I --
   --------------

   function Encode_I (Insn : Instruction) return Word is
      Raw : Word := 0;
   begin
      Add_RD (Raw, Insn.I_RD);
      Add_RS1 (Raw, Insn.I_RS1);

      --  Ensure correct Imm some special instructions
      case Insn.Kind is
         when Insn_FENCE_I | Insn_ECALL =>
            Add_Imm12_I (Raw, 0);
         when Insn_EBREAK =>
            Add_Imm12_I (Raw, 1);
         when others =>
            Add_Imm12_I (Raw, Insn.I_Imm);
      end case;

      Add_Opcode (Raw, (case Insn.Kind is
                     when Insn_SLLI    => OP_OP_IMM,
                     when Insn_SRLI    => OP_OP_IMM,
                     when Insn_SRAI    => OP_OP_IMM,
                     when Insn_ADDI    => OP_OP_IMM,
                     when Insn_SLTI    => OP_OP_IMM,
                     when Insn_SLTIU   => OP_OP_IMM,
                     when Insn_XORI    => OP_OP_IMM,
                     when Insn_ORI     => OP_OP_IMM,
                     when Insn_ANDI    => OP_OP_IMM,
                     when Insn_JALR    => OP_JALR,
                     when Insn_LB      => OP_Load,
                     when Insn_LH      => OP_Load,
                     when Insn_LW      => OP_Load,
                     when Insn_LBU     => OP_Load,
                     when Insn_LHU     => OP_Load,
                     when Insn_FENCE   => OP_Misc_Mem,
                     when Insn_FENCE_I => OP_Misc_Mem,
                     when Insn_ECALL   => OP_System,
                     when Insn_EBREAK  => OP_System,
                     when Insn_CSRRW   => OP_System,
                     when Insn_CSRRS   => OP_System,
                     when Insn_CSRRC   => OP_System,
                     when Insn_CSRRWI  => OP_System,
                     when Insn_CSRRSI  => OP_System,
                     when Insn_CSRRCI  => OP_System,
                     when Insn_URET    => OP_System,
                     when Insn_SRET    => OP_System,
                     when Insn_MRET    => OP_System,
                     when others       => raise Program_Error));

      Add_Funct3 (Raw, (case Insn.Kind is
                     when Insn_SLLI    => Funct3_SLLI,
                     when Insn_SRLI    => Funct3_SRLI,
                     when Insn_SRAI    => Funct3_SR,
                     when Insn_ADDI    => Funct3_ADDI,
                     when Insn_SLTI    => Funct3_SLTI,
                     when Insn_SLTIU   => Funct3_SLTIU,
                     when Insn_XORI    => Funct3_XORI,
                     when Insn_ORI     => Funct3_ORI,
                     when Insn_ANDI    => Funct3_ANDI,
                     when Insn_JALR    => Funct3_JALR,
                     when Insn_LB      => Funct3_LB,
                     when Insn_LH      => Funct3_LH,
                     when Insn_LW      => Funct3_LW,
                     when Insn_LBU     => Funct3_LBU,
                     when Insn_LHU     => Funct3_LHU,
                     when Insn_FENCE   => Funct3_FENCE,
                     when Insn_FENCE_I => Funct3_FENCE_I,
                     when Insn_ECALL   => Funct3_ECALL,
                     when Insn_EBREAK  => Funct3_EBREAK,
                     when Insn_CSRRW   => Funct3_CSRRW,
                     when Insn_CSRRS   => Funct3_CSRRS,
                     when Insn_CSRRC   => Funct3_CSRRC,
                     when Insn_CSRRWI  => Funct3_CSRRWI,
                     when Insn_CSRRSI  => Funct3_CSRRSI,
                     when Insn_CSRRCI  => Funct3_CSRRCI,
                     when Insn_URET    => Funct3_PRIV,
                     when Insn_SRET    => Funct3_PRIV,
                     when Insn_MRET    => Funct3_PRIV,
                     when others       => raise Program_Error));

      return Raw;
   end Encode_I;

   --------------
   -- Encode_S --
   --------------

   function Encode_S (Insn : Instruction) return Word is
      Raw : Word := 0;
   begin
      Add_RS1 (Raw, Insn.S_RS1);
      Add_RS2 (Raw, Insn.S_RS2);
      Add_Imm12_S (Raw, Insn.S_Imm);
      Add_Opcode (Raw, OP_Store);

      Add_Funct3 (Raw, (case Insn.Kind is
                     when Insn_SB    => Funct3_SB,
                     when Insn_SH    => Funct3_SH,
                     when Insn_SW    => Funct3_SW,
                     when others     => raise Program_Error));

      return Raw;
   end Encode_S;

   --------------
   -- Encode_B --
   --------------

   function Encode_B (Insn : Instruction) return Word is
      Raw : Word := 0;
   begin
      Add_RS1 (Raw, Insn.B_RS1);
      Add_RS2 (Raw, Insn.B_RS2);
      Add_Opcode (Raw, OP_Branch);
      Add_Imm13_B (Raw, Insn.B_Imm);

      Add_Funct3 (Raw, (case Insn.Kind is
                     when Insn_BEQ  => Funct3_BEQ,
                     when Insn_BNE  => Funct3_BNE,
                     when Insn_BLT  => Funct3_BLT,
                     when Insn_BGE  => Funct3_BGE,
                     when Insn_BLTU => Funct3_BLTU,
                     when Insn_BGEU => Funct3_BGEU,
                     when others => raise Program_Error));
      return Raw;
   end Encode_B;

   --------------
   -- Encode_U --
   --------------

   function Encode_U (Insn : Instruction) return Word is
      Raw : Word := Insn.U_Imm;
   begin
      Add_RD (Raw, Insn.U_RD);
      Add_Opcode (Raw, (case Insn.Kind is
                     when Insn_LUI   => OP_LUI,
                     when Insn_AUIPC => OP_AUIPC,
                     when others     => raise Program_Error));

      return Raw;
   end Encode_U;

   --------------
   -- Encode_J --
   --------------

   function Encode_J (Insn : Instruction) return Word is
      Raw : Word := 0;
   begin
      Add_RD (Raw, Insn.J_RD);
      Add_Imm21_J (Raw, Insn.J_Imm);

      Add_Opcode (Raw, (case Insn.Kind is
                     when Insn_JAL   => OP_JAL,
                     when others     => raise Program_Error));

      return Raw;
   end Encode_J;

   ---------
   -- Img --
   ---------

   function Img (Insn : Instruction; Addr : Address) return String is
      use ESF;
   begin

      if Insn.Kind in Insn_CSRRW .. Insn_CSRRC then
         return Fmt ("0x\s\t\s\t\s, \s, \s",
                     Hex (Addr),
                     Img (Insn.Kind),
                     Img (Insn.I_RD),
                     CSR.Img (CSR.Id (Insn.I_Imm)),
                     Img (Insn.I_RS1));

      elsif Insn.Kind in Insn_CSRRWI .. Insn_CSRRCI then
         return Fmt ("0x\s\t\s\t\s, \s, \s",
                     Hex (Addr),
                     Img (Insn.Kind),
                     Img (Insn.I_RD),
                     CSR.Img (CSR.Id (Insn.I_Imm)),
                     Insn.I_RS1'Img);
      else

         case Insn.Kind is

         when Invalid =>
            return Fmt ("0x\s\tinvalid 0x\s",
                        Hex (Addr),
                        Hex (Insn.Raw));

         when R_Insn_Kind =>
            return Fmt ("0x\s\t\s\t\s, \s, \s",
                        Hex (Addr),
                        Img (Insn.Kind),
                        Img (Insn.R_RD),
                        Img (Insn.R_RS1),
                        Img (Insn.R_RS2));

         when I_Insn_Kind =>
            if Insn.Kind in
              Insn_FENCE | Insn_FENCE_I | Insn_ECALL |
              Insn_EBREAK | Insn_URET | Insn_MRET | Insn_SRET
            then
               return Fmt ("0x\s\t\s",
                           Hex (Addr),
                           Img (Insn.Kind));
            else
               return Fmt ("0x\s\t\s\t\s, \s, \s",
                           Hex (Addr),
                           Img (Insn.Kind),
                           Img (Insn.I_RD),
                           Img (Insn.I_RS1),
                           Insn.I_Imm'Img);
            end if;

         when S_Insn_Kind =>
            return Fmt ("0x\s\t\s\t\s, \s(\s)",
                        Hex (Addr),
                        Img (Insn.Kind),
                        Img (Insn.S_RS2),
                        Sign_Extend (Insn.S_Imm).S'Img,
                        Img (Insn.S_RS1));

         when B_Insn_Kind =>
            return Fmt ("0x\s\t\s\t\s, \s, \s",
                        Hex (Addr),
                        Img (Insn.Kind),
                        Img (Insn.B_RS1),
                        Img (Insn.B_RS2),
                        Sign_Extend (Insn.B_Imm).S'Img);

         when U_Insn_Kind =>
            return Fmt ("0x\s\t\s\t\s, \s",
                        Hex (Addr),
                        Img (Insn.Kind),
                        Img (Insn.U_RD),
                        Insn.U_Imm'Img);

         when J_Insn_Kind =>
            return Fmt ("0x\s\t\s\t\s, \s",
                        Hex (Addr),
                        Img (Insn.Kind),
                        Img (Insn.J_RD),
                        Sign_Extend (Insn.J_Imm).S'Img);
         end case;
      end if;
   end Img;

   ---------
   -- Img --
   ---------

   function Img (Kind : Insn_Kind) return String
   is (case Kind is
          when Invalid      => "invalid",
          when Insn_ADD     => "add",
          when Insn_SUB     => "sub",
          when Insn_SLL     => "sll",
          when Insn_SLT     => "slt",
          when Insn_SLTU    => "sltu",
          when Insn_XOR     => "xor",
          when Insn_SRL     => "srl",
          when Insn_SRA     => "sra",
          when Insn_OR      => "or",
          when Insn_AND     => "and",
          when Insn_BEQ     => "beq",
          when Insn_BNE     => "bne",
          when Insn_BLT     => "blt",
          when Insn_BGE     => "bge",
          when Insn_BLTU    => "bltu",
          when Insn_BGEU    => "bgeu",
          when Insn_SB      => "sb",
          when Insn_SH      => "sh",
          when Insn_SW      => "sw",
          when Insn_SLLI    => "slli",
          when Insn_SRLI    => "srli",
          when Insn_SRAI    => "srai",
          when Insn_JALR    => "jalr",
          when Insn_LB      => "lb",
          when Insn_LH      => "lh",
          when Insn_LW      => "lw",
          when Insn_LBU     => "lbu",
          when Insn_LHU     => "lhu",
          when Insn_ADDI    => "addi",
          when Insn_SLTI    => "slti",
          when Insn_SLTIU   => "sltiu",
          when Insn_XORI    => "xori",
          when Insn_ORI     => "ori",
          when Insn_ANDI    => "andi",
          when Insn_FENCE   => "fence",
          when Insn_FENCE_I => "fence.i",
          when Insn_ECALL   => "ecall",
          when Insn_EBREAK  => "ebreak",
          when Insn_CSRRW   => "csrrw",
          when Insn_CSRRS   => "csrrs",
          when Insn_CSRRC   => "csrrc",
          when Insn_CSRRWI  => "csrrwi",
          when Insn_CSRRSI  => "csrrsi",
          when Insn_CSRRCI  => "csrrci",
          when Insn_URET    => "uret",
          when Insn_SRET    => "sret",
          when Insn_MRET    => "mret",
          when Insn_LUI     => "lui",
          when Insn_AUIPC   => "auipc",
          when Insn_JAL     => "jal"
      );

   -----------------
   -- Sign_Extend --
   -----------------

   function Sign_Extend (Imm : Raw_Imm12) return Register is
      U : U_Register;
   begin
      if (Imm and 2#1000_0000_0000#) /= 0 then
         U := 16#FF_FF_F8_00# or U_Register (Imm);
      else
         U := U_Register (Imm);
      end if;

      return (Signed => False, U => U);
   end Sign_Extend;

   -----------------
   -- Sign_Extend --
   -----------------

   function Sign_Extend (Imm : Raw_Imm13) return Register is
      U : U_Register;
   begin
      if (Imm and 2#1_0000_0000_0000#) /= 0 then
         U := 16#FF_FF_E0_00# or U_Register (Imm);
      else
         U := U_Register (Imm);
      end if;

      return (Signed => False, U => U);
   end Sign_Extend;

   -----------------
   -- Sign_Extend --
   -----------------

   function Sign_Extend (Imm : Raw_Imm21) return Register is
      U : U_Register;
   begin
      if (Imm and 2#1_0000_0000_0000_0000_0000#) /= 0 then
         U := 16#FF_E0_00_00# or U_Register (Imm);
      else
         U := U_Register (Imm);
      end if;

      return (Signed => False, U => U);
   end Sign_Extend;

end LibRISCV.Instructions;

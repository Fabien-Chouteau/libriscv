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

package LibRISCV.Instructions
with SPARK_Mode => On
is

   type Insn_Kind is
     (
      Invalid,

      --  R-type
      Insn_ADD,
      Insn_SUB, Insn_SLL, Insn_SLT, Insn_SLTU,
      Insn_XOR, Insn_SRL, Insn_SRA, Insn_OR,
      Insn_AND,

      --  B-Type
      Insn_BEQ, Insn_BNE, Insn_BLT, Insn_BGE,
      Insn_BLTU, Insn_BGEU,

      --  S-Type
      Insn_SB, Insn_SH, Insn_SW,

      --  I-Type
      Insn_SLLI, Insn_SRLI, Insn_SRAI,
      Insn_JALR, Insn_LB, Insn_LH, Insn_LW,
      Insn_LBU, Insn_LHU, Insn_ADDI, Insn_SLTI,
      Insn_SLTIU, Insn_XORI, Insn_ORI, Insn_ANDI,
      Insn_FENCE, Insn_FENCE_I,
      Insn_ECALL, Insn_EBREAK, Insn_CSRRW, Insn_CSRRS,
      Insn_CSRRC, Insn_CSRRWI, Insn_CSRRSI, Insn_CSRRCI,
      Insn_URET, Insn_SRET, Insn_MRET,

      --  U-type
      Insn_LUI, Insn_AUIPC,

      --  J-Type
      Insn_JAL
     );

   subtype R_Insn_Kind is Insn_Kind range Insn_ADD .. Insn_AND;
   subtype I_Insn_Kind is Insn_Kind range Insn_SLLI .. Insn_MRET;
   subtype S_Insn_Kind is Insn_Kind range Insn_SB   .. Insn_SW;
   subtype B_Insn_Kind is Insn_Kind range Insn_BEQ  .. Insn_BGEU;
   subtype U_Insn_Kind is Insn_Kind range Insn_LUI  .. Insn_AUIPC;
   subtype J_Insn_Kind is Insn_Kind range Insn_JAL  .. Insn_JAL;

   type Raw_Opcode is mod 2 ** 7  with Size => 7;
   type Raw_Imm8   is mod 2 ** 8  with Size => 8;
   type Raw_Imm12  is mod 2 ** 12 with Size => 12;
   type Raw_Imm13  is mod 2 ** 13 with Size => 13;
   type Raw_Imm16  is mod 2 ** 16 with Size => 16;
   type Raw_Imm19  is mod 2 ** 19 with Size => 19;
   type Raw_Imm21  is mod 2 ** 21 with Size => 21;

   type Instruction (Kind : Insn_Kind := Invalid) is record
      case Kind is
         when Invalid     =>
            Raw : Word;

         when R_Insn_Kind =>
            R_RD, R_RS1, R_RS2 : GPR_Id;

         when I_Insn_Kind =>
            I_RD, I_RS1 : GPR_Id;
            I_Imm : Raw_Imm12;

         when S_Insn_Kind =>
            S_RS1, S_RS2 : GPR_Id;
            S_Imm : Raw_Imm12;

         when B_Insn_Kind =>
            B_RS1, B_RS2 : GPR_Id;
            B_Imm : Raw_Imm13;

         when U_Insn_Kind =>
            U_RD : GPR_Id;
            U_Imm : Word;

         when J_Insn_Kind =>
            J_RD : GPR_Id;
            J_Imm : Raw_Imm21;
      end case;
   end record;

   function Decode (Raw  : Word) return Instruction;
   function Encode (Insn : Instruction) return Word;

   function Img (Insn : Instruction; Addr : Address) return String;

   function Img (Kind : Insn_Kind) return String;

   function Sign_Extend (Imm : Raw_Imm13) return Register;
   function Sign_Extend (Imm : Raw_Imm12) return Register;
   function Sign_Extend (Imm : Raw_Imm21) return Register;

   type Raw_Funct7 is mod 2 ** 7  with Size => 7;
   type Raw_Funct3 is mod 2 ** 3  with Size => 3;

   OP_LUI      : constant Raw_Opcode := 2#0110111#;
   OP_AUIPC    : constant Raw_Opcode := 2#0010111#;
   OP_JAL      : constant Raw_Opcode := 2#1101111#;
   OP_JALR     : constant Raw_Opcode := 2#1100111#;
   OP_Branch   : constant Raw_Opcode := 2#1100011#;
   OP_Load     : constant Raw_Opcode := 2#0000011#;
   OP_Store    : constant Raw_Opcode := 2#0100011#;
   OP_OP_IMM   : constant Raw_Opcode := 2#0010011#;
   OP_OP       : constant Raw_Opcode := 2#0110011#;
   OP_Misc_Mem : constant Raw_Opcode := 2#0001111#;
   OP_System   : constant Raw_Opcode := 2#1110011#;

   Funct3_JALR  : constant Raw_Funct3 := 2#000#;

   Funct3_BEQ  : constant Raw_Funct3 := 2#000#;
   Funct3_BNE  : constant Raw_Funct3 := 2#001#;
   Funct3_BLT  : constant Raw_Funct3 := 2#100#;
   Funct3_BGE  : constant Raw_Funct3 := 2#101#;
   Funct3_BLTU : constant Raw_Funct3 := 2#110#;
   Funct3_BGEU : constant Raw_Funct3 := 2#111#;

   Funct3_LB   : constant Raw_Funct3 := 2#000#;
   Funct3_LH   : constant Raw_Funct3 := 2#001#;
   Funct3_LW   : constant Raw_Funct3 := 2#010#;
   Funct3_LBU  : constant Raw_Funct3 := 2#100#;
   Funct3_LHU  : constant Raw_Funct3 := 2#101#;

   Funct3_SB    : constant Raw_Funct3 := 2#000#;
   Funct3_SH    : constant Raw_Funct3 := 2#001#;
   Funct3_SW    : constant Raw_Funct3 := 2#010#;

   Funct3_ADDI  : constant Raw_Funct3 := 2#000#;
   Funct3_SLLI  : constant Raw_Funct3 := 2#001#;
   Funct3_SLTI  : constant Raw_Funct3 := 2#010#;
   Funct3_SLTIU : constant Raw_Funct3 := 2#011#;
   Funct3_XORI  : constant Raw_Funct3 := 2#100#;
   Funct3_SRLI  : constant Raw_Funct3 := 2#101#;
   Funct3_ORI   : constant Raw_Funct3 := 2#110#;
   Funct3_ANDI  : constant Raw_Funct3 := 2#111#;

   Funct3_FENCE   : constant Raw_Funct3 := 2#000#;
   Funct3_FENCE_I : constant Raw_Funct3 := 2#001#;
   Funct3_ECALL   : constant Raw_Funct3 := 2#000#;
   Funct3_EBREAK  : constant Raw_Funct3 := 2#000#;

   Funct3_ADD_SUB : constant Raw_Funct3 := 2#000#;
   Funct3_SLL     : constant Raw_Funct3 := 2#001#;
   Funct3_SLT     : constant Raw_Funct3 := 2#010#;
   Funct3_SLTU    : constant Raw_Funct3 := 2#011#;
   Funct3_XOR     : constant Raw_Funct3 := 2#100#;
   Funct3_SR      : constant Raw_Funct3 := 2#101#;
   Funct3_OR      : constant Raw_Funct3 := 2#110#;
   Funct3_AND     : constant Raw_Funct3 := 2#111#;

   Funct3_PRIV    : constant Raw_Funct3 := 2#000#;
   Funct3_CSRRW   : constant Raw_Funct3 := 2#001#;
   Funct3_CSRRS   : constant Raw_Funct3 := 2#010#;
   Funct3_CSRRC   : constant Raw_Funct3 := 2#011#;
   Funct3_CSRRWI  : constant Raw_Funct3 := 2#101#;
   Funct3_CSRRSI  : constant Raw_Funct3 := 2#110#;
   Funct3_CSRRCI  : constant Raw_Funct3 := 2#111#;

   Funct7_ADD  : constant Raw_Funct7 := 2#0000000#;
   Funct7_SUB  : constant Raw_Funct7 := 2#0100000#;
   Funct7_SLL  : constant Raw_Funct7 := 2#0000000#;
   Funct7_SLT  : constant Raw_Funct7 := 2#0000000#;
   Funct7_SLTU : constant Raw_Funct7 := 2#0000000#;
   Funct7_XOR  : constant Raw_Funct7 := 2#0000000#;
   Funct7_OR   : constant Raw_Funct7 := 2#0000000#;
   Funct7_AND  : constant Raw_Funct7 := 2#0000000#;
   Funct7_SRL  : constant Raw_Funct7 := 2#0000000#;
   Funct7_SRA  : constant Raw_Funct7 := 2#0100000#;

   Funct7_SLLI  : constant Raw_Funct7 := 2#0000000#;
   Funct7_SRLI  : constant Raw_Funct7 := 2#0000000#;
   Funct7_SRAI  : constant Raw_Funct7 := 2#0100000#;

private

   procedure Add_Opcode (Raw : in out Word; Op : Raw_Opcode);
   procedure Add_Funct3 (Raw : in out Word; Funct : Raw_Funct3);
   procedure Add_Funct7 (Raw : in out Word; Funct : Raw_Funct7);
   procedure Add_RD (Raw : in out Word; Id : GPR_Id);
   procedure Add_RS1 (Raw : in out Word; Id : GPR_Id);
   procedure Add_RS2 (Raw : in out Word; Id : GPR_Id);
   procedure Add_Imm13_B (Raw : in out Word; Imm : Raw_Imm13);
   procedure Add_Imm12_I (Raw : in out Word; Imm : Raw_Imm12);
   procedure Add_Imm12_S (Raw : in out Word; Imm : Raw_Imm12);
   procedure Add_Imm21_J (Raw : in out Word; Imm : Raw_Imm21);

   function Encode_R (Insn : Instruction) return Word
     with Pre => Insn.Kind in R_Insn_Kind;
   function Encode_I (Insn : Instruction) return Word
     with Pre => Insn.Kind in I_Insn_Kind;
   function Encode_S (Insn : Instruction) return Word
     with Pre => Insn.Kind in S_Insn_Kind;
   function Encode_B (Insn : Instruction) return Word
     with Pre => Insn.Kind in B_Insn_Kind;
   function Encode_U (Insn : Instruction) return Word
     with Pre => Insn.Kind in U_Insn_Kind;
   function Encode_J (Insn : Instruction) return Word
     with Pre => Insn.Kind in J_Insn_Kind;

end LibRISCV.Instructions;

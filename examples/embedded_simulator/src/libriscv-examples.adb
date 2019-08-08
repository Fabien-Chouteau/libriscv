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

with ESF;

package body LibRISCV.Examples is

   procedure Load_Hello_World (Bus  : in out Sim.Memory_Bus.Class;
                               HTIF :        Address) is
      Program : constant array (Natural range <>) of Instruction :=
        ((Insn_AUIPC, 5, 0),           -- 0x00: Load message address
         (Insn_ADDI, 5, 5, 48),        -- 0x04: cont.
         (Insn_LUI, 6, 16#F000_0000#), -- 0x08: Load console address
         (Insn_LBU, 7, 5, 0),          -- 0x0C: Load a character
         (Insn_BEQ, 7, 0, 16),         -- 0x10: Jump to exit loop if char is 0
         (Insn_SB, 6, 7, 0),           -- 0x14: Write character to the console
         (Insn_ADDI, 5, 5, 1),         -- 0x18: Increase pointer
         (Insn_JAL, 0, -16),           -- 0x1C: Go back to load a character
         (Insn_LUI, 6, Word (HTIF)),   -- 0x20: Load HTIF address
         (Insn_ADDI, 5, 0, 1),         -- 0x24: Set x5 to 1
         (Insn_SW, 6, 5, 0),           -- 0x28: Write in the HTIF to exit
         (Insn_JAL, 0, 0)              -- 0x2C: Infinite loop
        );
      Res  : Sim.Memory_Bus.Access_Result;
      Addr : Address := 16#8000_0000#;
      Raw  : Word;

      Message : constant String :=
        "Hello from a program inside a RISC-V simulator " &
        "inside a RISC-V Simulator :)" & ASCII.NUL;

   begin
      --  Load a simple hello world program
      for Insn of Program loop
         Raw := Encode (Insn);

         if Sim.Log.Info then
            Sim.Log.Put_Line
              (ESF.Fmt ("Load instruction: \s (0x\s)",
                        Img (Insn, Addr), Hex (Raw)));
         end if;

         Bus.Store_W (Addr, Encode (Insn), Res);
         Addr := Addr + 4;
      end loop;

      for C of Message loop
         Bus.Store_B (Addr, Byte (C'Enum_Rep), Res);
         Addr := Addr + 1;
      end loop;
   end Load_Hello_World;

end LibRISCV.Examples;

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

with Ada.Text_IO; use Ada.Text_IO;

with LibRISCV.Sim.Memory_Bus; use LibRISCV.Sim.Memory_Bus;
with LibRISCV.Sim.Log;

with ESF;

package body LibRISCV.Signature is

   Begin_Signature : Address := 0;
   End_Signature   : Address := 0;
   File            : File_Type;

   -------------------------
   -- Set_Begin_Signature --
   -------------------------

   procedure Set_Begin_Signature (Addr : Address) is
   begin
      Begin_Signature := Addr;
   end Set_Begin_Signature;

   -----------------------
   -- Set_End_Signature --
   -----------------------

   procedure Set_End_Signature (Addr : Address) is
   begin
      End_Signature := Addr;
   end Set_End_Signature;

   ------------------------
   -- Signature_Filepath --
   ------------------------

   procedure Signature_Filepath (Path : String) is
   begin
      Ada.Text_IO.Put_Line ("Creating signature output file: '" & Path & "'");
      Create (File, Ada.Text_IO.Out_File, Path);
   end Signature_Filepath;

   --------------------
   -- Dump_Signature --
   --------------------

   procedure Dump_Signature (Bus : in out Sim.Memory_Bus.Class) is
      Res  : Access_Result;
      Hexa : constant array (Byte range 0 .. 15) of Character :=
        "0123456789abcdef";

      procedure Put (Nible : Byte) is
      begin
         Ada.Text_IO.Put (File, (1 => Hexa (Nible and 16#F#)));
      end Put;

      Current : Address := Begin_Signature;
      Data    : Word;
   begin
      if Ada.Text_IO.Is_Open (File) then
         if Begin_Signature /= 0 and then End_Signature /= 0 then

            if Sim.Log.Info then
               Sim.Log.Put_Line
                 (ESF.Fmt ("Dumping signature from 0x\s to 0x\s",
                  Hex (Begin_Signature),
                  Hex (End_Signature)));
            end if;

            while Current < End_Signature loop

               Bus.Load_W (Current, Data, Res);

               if Res /= Success then
                  raise Program_Error with "Signature bounds out of RAM";
               else
                  for X in 1 .. 8 loop
                     Put (Byte (Shift_Right (Data and 16#F000_0000#, 28)));
                     Data := Shift_Left (Data, 4);
                  end loop;
               end if;

               New_Line (File);

               Current := Current + 4;
            end loop;
         end if;
         Close (File);
      end if;
   end Dump_Signature;

end LibRISCV.Signature;

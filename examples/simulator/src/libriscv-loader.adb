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

with Ada.Streams; use Ada.Streams;

with Bfd;
with Bfd.Files;
with Bfd.Sections;
with Bfd.Symbols;

with LibRISCV.Signature;
with LibRISCV.Sim.Log;
with LibRISCV.Sim.OS_Interface;

package body LibRISCV.Loader is

   use type Bfd.Section_Flags;
   use type Sim.Memory_Bus.Access_Result;

   ------------------
   -- Load_Symbols --
   ------------------

   procedure Load_Symbols (File :        Bfd.Files.File_Type;
                           Bus  : in out Sim.Memory_Bus.Class)
   is

      Symbols : Bfd.Symbols.Symbol_Table;
      It      : Bfd.Symbols.Symbol_Iterator;
   begin
      Bfd.Symbols.Read_Symbols (File, Symbols);
      It := Bfd.Symbols.Get_Iterator (Symbols);
      while Bfd.Symbols.Has_Element (It) loop
         declare
            Sym   : constant Bfd.Symbols.Symbol := Bfd.Symbols.Element (It);
            Addr  : constant Address := Address (Bfd.Symbols.Get_Value (Sym));
            Name  : constant String := Bfd.Symbols.Get_Name (Sym);
         begin
            if Name = "begin_signature" then
               Signature.Set_Begin_Signature (Addr);
            elsif Name = "end_signature" then
               Signature.Set_End_Signature (Addr);
            elsif Name = "tohost" then
               Bus.Set_Tohost (Addr);
            end if;
         end;
         Bfd.Symbols.Next (It);
      end loop;
   end Load_Symbols;

   --------------
   -- Load_Elf --
   --------------

   procedure Load_Elf (Bus : in out Sim.Memory_Bus.Class; Path : String) is
      File : Bfd.Files.File_Type;
      Iter : Bfd.Sections.Section_Iterator;
   begin

      begin

         Bfd.Files.Open (File, Path, "");

         if Bfd.Files.Check_Format (File, Bfd.Files.OBJECT) then
            Iter := Bfd.Sections.Get_Sections (File);
            while Bfd.Sections.Has_Element (Iter) loop
               declare
                  Sec  : constant Bfd.Sections.Section := Bfd.Sections.Element (Iter);
                  Data : Stream_Element_Array (1 .. Stream_Element_Offset (Sec.Size));
                  Last : Stream_Element_Offset;
                  Res  : Sim.Memory_Bus.Access_Result;
               begin
                  if (Sec.Flags and Bfd.Sections.SEC_LOAD) /= 0
                  then
                     if Sim.Log.Elf then
                        Sim.Log.Put_Line ("Loading section '" &
                                        Bfd.Sections.Get_Name (Sec) & "'");
                        Sim.Log.Put_Line ("   Vma :" & Sec.Vma'Img);
                        Sim.Log.Put_Line ("   Lma :" & Sec.Lma'Img);
                        Sim.Log.Put_Line ("   Size:" & Sec.Size'Img);

                     end if;

                     Bfd.Sections.Get_Section_Contents
                       (File, Sec, 0, Data, Last);

                     for Index in Data'First .. Last loop
                        Bus.Store_B (Address (Sec.Lma) + Address (Index) - 1,
                                     Byte (Data (Index)),
                                     Res);

                        if Res /= Sim.Memory_Bus.Success and then Sim.Log.Elf then
                           Sim.Log.Put_Line ("Elf loader: invalid mem write");
                        end if;
                     end loop;
                  end if;
               end;
               Bfd.Sections.Next (Iter);
            end loop;
         end if;

         Load_Symbols (File, Bus);

         Bfd.Files.Close (File);

      exception
         when Bfd.OPEN_ERROR =>
            Sim.OS_Interface.Put_Line ("Cannot open file " & Path);
            Sim.OS_Interface.Put_Line (Bfd.Get_Error_Message (Bfd.Get_Error));
      end;
   end Load_Elf;

end LibRISCV.Loader;

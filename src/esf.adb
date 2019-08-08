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

package body ESF is

   procedure Append (Dst    : in out String;
                     Cursor : in out Natural;
                     Str    :        String)
     with Inline_Always;
   --  Append a string to the result string

   ------------
   -- Append --
   ------------

   procedure Append (Dst    : in out String;
                     Cursor : in out Natural;
                     Str    :        String)
   is
   begin
      Dst (Cursor .. Cursor + Str'Length - 1) := Str;
      Cursor := Cursor + Str'Length;
   end Append;

   procedure Append (Dst    : in out String;
                     Cursor : in out Natural;
                     C      :        Character)
     with Inline_Always;
   --  Append a character to the result string

   ------------
   -- Append --
   ------------

   procedure Append (Dst    : in out String;
                     Cursor : in out Natural;
                     C      :        Character)
   is
   begin
      Dst (Cursor) := C;
      Cursor := Cursor + 1;
   end Append;

   ---------
   -- Fmt --
   ---------

   function Fmt
     (Format : String; S1, S2, S3, S4, S5 : String := "") return String
   is
      Max_Len : constant Natural := Format'Length +
        S1'Length + S2'Length + S3'Length + S4'Length + S5'Length;

      Result : String (1 .. Max_Len);
      Cursor : Integer := Format'First;

      R_Cursor : Natural := Result'First;
      --  Result cursor


      Insert_Count : Natural := 0;
      --  Number of string inserted so far, this is used to know which string
      --  to insert next.
   begin

      while Cursor <= Format'Last loop

         if Format (Cursor) = '\'
           and then
            Cursor < Format'Last
           and then
            Format (Cursor + 1) /= '\'
         then

            case Format (Cursor + 1) is
               when 'a' => Append (Result, R_Cursor, ASCII.BEL);
               when 'b' => Append (Result, R_Cursor, ASCII.BS);
               when 'e' => Append (Result, R_Cursor, ASCII.ESC);
               when 'f' => Append (Result, R_Cursor, ASCII.FF);
               when 'n' => Append (Result, R_Cursor, ASCII.LF);
               when 'r' => Append (Result, R_Cursor, ASCII.CR);
               when 't' => Append (Result, R_Cursor, ASCII.HT);
               when 'v' => Append (Result, R_Cursor, ASCII.VT);
               when '?' => Append (Result, R_Cursor, ASCII.Query);
               when '\' => Append (Result, R_Cursor, '\');
               when 's' =>
                  case Insert_Count is
                  when 0 => Append (Result, R_Cursor, S1);
                  when 1 => Append (Result, R_Cursor, S2);
                  when 2 => Append (Result, R_Cursor, S3);
                  when 3 => Append (Result, R_Cursor, S4);
                  when 4 => Append (Result, R_Cursor, S5);
                  when others => null;
                  end case;
                  Insert_Count := Insert_Count + 1;
               when others =>
                  Append (Result, R_Cursor, '\');
                  Append (Result, R_Cursor, Format (Cursor + 1));
            end case;
            Cursor := Cursor + 2;
         else
            Append (Result, R_Cursor, Format (Cursor));
            Cursor := Cursor + 1;
         end if;

      end loop;
      return Result (Result'First .. R_Cursor - 1);
   end Fmt;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image (Value  : T) return String
   is
      Hex : constant array (T range 0 .. 15) of Character :=
        "0123456789ABCDEF";

      V : T := Value;

      Result : String (1 .. T'Size / 4);
   begin
      for C of reverse Result loop
         C := Hex (V and 16#F#);
         V := V / 2**4;
      end loop;
      return Result;
   end Hex_Image;

   ---------------
   -- Bin_Image --
   ---------------

   function Bin_Image (Value  : T) return String
   is
      V : T := Value;

      Result : String (1 .. T'Size);
   begin
      for C of reverse Result loop
         C := (if (V and 16#1#) = 0 then '0' else '1');
         V := V / 2;
      end loop;
      return Result;
   end Bin_Image;

end ESF;

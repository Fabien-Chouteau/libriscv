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

package body LibRISCV is

   ---------
   -- Img --
   ---------

   function Img (Id : GPR_Id) return String
   is (case Id is
          when 0 => "zero",
          when 1 => "ra",
          when 2 => "sp",
          when 3 => "gp",
          when 4 => "tp",
          when 5 => "t0",
          when 6 => "t1",
          when 7 => "t2",
          when 8 => "s0",
          when 9 => "s1",
          when 10 => "a0",
          when 11 => "a1",
          when 12 => "a2",
          when 13 => "a3",
          when 14 => "a4",
          when 15 => "a5",
          when 16 => "a6",
          when 17 => "a7",
          when 18 => "s2",
          when 19 => "s3",
          when 20 => "s4",
          when 21 => "s5",
          when 22 => "s6",
          when 23 => "s7",
          when 24 => "s8",
          when 25 => "s9",
          when 26 => "s10",
          when 27 => "s11",
          when 28 => "t3",
          when 29 => "t4",
          when 30 => "t5",
          when 31 => "t6");

   -----------------
   -- Sign_Extend --
   -----------------

   function Sign_Extend (Value : Byte) return U_Register is
   begin
      if (Value and 2#1000_0000#) /= 0 then
         return 16#FF_FF_FF_00# or U_Register (Value);
      else
         return 16#00_00_00_FF# and U_Register (Value);
      end if;
   end Sign_Extend;

   -----------------
   -- Sign_Extend --
   -----------------

   function Sign_Extend (Value : Halfword) return U_Register is
   begin
      if (Value and 2#1000_0000_0000_0000#) /= 0 then
         return 16#FF_FF_00_00# or U_Register (Value);
      else
         return 16#00_00_FF_FF# and U_Register (Value);
      end if;
   end Sign_Extend;

   -----------------
   -- Sign_Extend --
   -----------------

   function Sign_Extend (Value : Word) return U_Register is
   begin
      return U_Register (Value);
   end Sign_Extend;

end LibRISCV;

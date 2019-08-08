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

package LibRISCV is

   XLEN : constant := 32;

   type Byte is mod 2 ** 8
     with Size => 8;

   type Halfword is mod 2 ** 16
     with Size => 16;

   type Word is mod 2 ** 32
     with Size => 32;

   type Dword is mod 2 ** 64
     with Size => 64;

   type U_Register is mod 2 ** XLEN
     with Size => XLEN;
   --  Unsigned representation of an XLEN register

   type S_Register is range -(2 ** (XLEN - 1)) .. (2 ** (XLEN - 1)) - 1
     with Size => XLEN;
   --  Signed representation of an XLEN register

   type Register (Signed : Boolean := False) is record
      case Signed is
         when True  => S : S_Register;
         when False => U : U_Register;
      end case;
   end record
     with
       Pack, Size => XLEN,
       Unchecked_Union;

   subtype Address is U_Register;

   type GPR_Id is mod 2 ** 5
     with Size => 5;

   function Img (Id : GPR_Id) return String;

   function Sign_Extend (Value : Byte) return U_Register;
   function Sign_Extend (Value : Halfword) return U_Register;
   function Sign_Extend (Value : Word) return U_Register;

   function Shift_Right (Value  : Byte; Amount : Natural) return Byte;
   function Shift_Right (Value  : Halfword; Amount : Natural) return Halfword;
   function Shift_Right (Value  : Word; Amount : Natural) return Word;
   function Shift_Right (Value  : U_Register; Amount : Natural)
                         return U_Register;

   function Shift_Right_Arithmetic (Value  : U_Register; Amount : Natural)
                                    return U_Register;

   function Shift_Left (Value  : Byte; Amount : Natural) return Byte;
   function Shift_Left (Value  : Halfword; Amount : Natural) return Halfword;
   function Shift_Left (Value  : Word; Amount : Natural) return Word;
   function Shift_Left (Value  : U_Register; Amount : Natural)
                        return U_Register;

   pragma Import (Intrinsic, Shift_Right);
   pragma Import (Intrinsic, Shift_Right_Arithmetic);
   pragma Import (Intrinsic, Shift_Left);

   function Hex is new ESF.Hex_Image (Byte);
   function Hex is new ESF.Hex_Image (Halfword);
   function Hex is new ESF.Hex_Image (Word);
   function Hex is new ESF.Hex_Image (U_Register);

end LibRISCV;

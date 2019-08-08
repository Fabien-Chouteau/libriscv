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

package LibRISCV.Sim.Memory_Bus
with SPARK_Mode => On
is

   type Instance (RAM_Base : Address;
                  RAM_Size : Positive)
   is tagged private;

   subtype Class is Instance'Class;

   type Access_Result is (Success, Failure);

   procedure Load_B (This : Instance;
                     Addr : Address;
                     Data : out Byte;
                     Res  : out Access_Result);

   procedure Load_H (This : Instance;
                     Addr : Address;
                     Data : out Halfword;
                     Res  : out Access_Result);

   procedure Load_W (This : Instance;
                     Addr : Address;
                     Data : out Word;
                     Res  : out Access_Result);

   procedure Store_B (This : in out Instance;
                      Addr : Address;
                      Data : Byte;
                      Res  : out Access_Result);

   procedure Store_H (This : in out Instance;
                      Addr : Address;
                      Data : Halfword;
                      Res  : out Access_Result);

   procedure Store_W (This : in out Instance;
                      Addr : Address;
                      Data : Word;
                      Res  : out Access_Result);

   -- Host Target Interface --

   procedure Enable_HTIF (This : in out Instance);
   --  Enable the HTIF device. If this procedure is not called, the HTIF will
   --  not be simulated, even if tohost address is set.

   procedure Set_Tohost (This : in out Instance;
                         Addr :        Address);
   --  Define the address the HTIF tohost register. If this procedure is not
   --  called, the register is not simulated.

private

   type RAM_Array is array (Positive range <>) of Byte;

   type Instance (RAM_Base : Address;
                  RAM_Size : Positive)
   is tagged record
      RAM : RAM_Array (1 .. RAM_Size) := (others => 0);

      HTIF_Enabled : Boolean := False;

      Tohost_Set   : Boolean := False;
      Tohost_Addr  : Address := 0;
   end record;

end LibRISCV.Sim.Memory_Bus;

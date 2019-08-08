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

with LibRISCV.Sim.Log;
with LibRISCV.Sim.Shutdown;

package body LibRISCV.Sim.Memory_Bus is

   ------------
   -- Load_B --
   ------------

   procedure Load_B
     (This :     Instance;
      Addr :     Address;
      Data : out Byte;
      Res  : out Access_Result)
   is
   begin
      if Addr in This.RAM_Base .. This.RAM_Base + This.RAM'Length - 1 then
         Data := This.RAM (Positive (Addr - This.RAM_Base + 1));
         Res := Success;
      else
         if Sim.Log.Mem_Access then
            Sim.Log.Put_Line ("Load_B invalid access at 0x" & Hex (Addr));
         end if;
         Res := Failure;
      end if;

      if Sim.Log.Mem_Access then
         Sim.Log.Put_Line (ESF.Fmt ("Load_B Addr: 0x\s Data: 0x\s",
                       Hex (Addr), Hex (Data)));
      end if;
   end Load_B;

   ------------
   -- Load_H --
   ------------

   procedure Load_H
     (This :     Instance;
      Addr :     Address;
      Data : out Halfword;
      Res  : out Access_Result)
   is
      Input : array (Address range 0 .. 1) of Byte;
   begin
      for Offset in Input'Range loop
         This.Load_B (Addr + Offset, Input (Offset), Res);
         if Res /= Success then
            Data := 0;
            return;
         end if;
      end loop;

      Data := Halfword (Shift_Left (Word (Input (1)), 8)
                        or Word (Input (0)));
      Res := Success;

      if Sim.Log.Mem_Access then
         Sim.Log.Put_Line (ESF.Fmt ("Load_H Addr: 0x\s Data: 0x\s",
                       Hex (Addr), Hex (Data)));
      end if;
   end Load_H;

   ------------
   -- Load_W --
   ------------

   procedure Load_W
     (This :     Instance;
      Addr :     Address;
      Data : out Word;
      Res  : out Access_Result)
   is
      Input : array (Address range 0 .. 3) of Byte;
   begin
      for Offset in Input'Range loop
         This.Load_B (Addr + Offset, Input (Offset), Res);
         if Res /= Success then
            Data := 0;
            return;
         end if;
      end loop;

      Data := Shift_Left (Word (Input (3)), 24)
        or Shift_Left (Word (Input (2)), 16)
        or Shift_Left (Word (Input (1)), 8)
        or Word (Input (0));

      Res := Success;

      if Sim.Log.Mem_Access then
         Sim.Log.Put_Line (ESF.Fmt ("Load_W Addr: 0x\s Data: 0x\s",
                       Hex (Addr), Hex (Data)));
      end if;
   end Load_W;

   -------------
   -- Store_B --
   -------------

   procedure Store_B
     (This : in out Instance;
      Addr :        Address;
      Data :        Byte;
      Res  :    out Access_Result)
   is
   begin
      if Sim.Log.Mem_Access then
         Sim.Log.Put_Line (ESF.Fmt ("Store_B Addr: 0x\s Data: 0x\s",
                       Hex (Addr), Hex (Data)));
      end if;

      if Addr in This.RAM_Base .. This.RAM_Base + This.RAM'Length - 1 then
         This.RAM (Positive (Addr - This.RAM_Base + 1)) := Data;
         Res := Success;
      elsif Addr = 16#F0000000# then

         if Sim.Log.Console then
            Sim.Log.Put ((1 => Character'Val (Data)));
         end if;

         Res := Success;
      else
         if Sim.Log.Mem_Access then
            Sim.Log.Put_Line ("Store_B invalid access at 0x" & Hex (Addr));
         end if;
         Res := Failure;
      end if;
   end Store_B;

   -------------
   -- Store_H --
   -------------

   procedure Store_H
     (This : in out Instance;
      Addr :        Address;
      Data :        Halfword;
      Res  :    out Access_Result)
   is
      B : Byte;
      Tmp_Data : Halfword := Data;
   begin
      if Sim.Log.Mem_Access then
         Sim.Log.Put_Line (ESF.Fmt ("Store_H Addr: 0x\s Data: 0x\s",
                         Hex (Addr), Hex (Data)));
      end if;

      for Offset in Address range 0 .. 1 loop

         B := Byte (Tmp_Data and 16#FF#);
         This.Store_B (Addr + Offset, B, Res);

         if Res /= Success then
            return;
         end if;
         Tmp_Data := Shift_Right (Tmp_Data, 8);
      end loop;
   end Store_H;

   -------------
   -- Store_W --
   -------------

   procedure Store_W
     (This : in out Instance;
      Addr :        Address;
      Data :        Word;
      Res  :    out Access_Result)
   is
      B : Byte;
      Tmp_Data : Word := Data;
   begin
      if Sim.Log.Mem_Access then
         Sim.Log.Put_Line (ESF.Fmt ("Store_W Addr: 0x\s Data: 0x\s",
                       Hex (Addr), Hex (Data)));
      end if;

      if This.HTIF_Enabled
        and then
         This.Tohost_Set
        and then
         Addr = This.Tohost_Addr
        and then
         Data = 1
      then
         Sim.Shutdown.Request;
         Res := Success;
      else

         for Offset in Address range 0 .. 3 loop

            B := Byte (Tmp_Data and 16#FF#);
            This.Store_B (Addr + Offset, B, Res);

            if Res /= Success then
               return;
            end if;
            Tmp_Data := Shift_Right (Tmp_Data, 8);
         end loop;
      end if;
   end Store_W;

   -----------------
   -- Enable_HTIF --
   -----------------

   procedure Enable_HTIF (This : in out Instance) is
   begin
      This.HTIF_Enabled := True;
   end Enable_HTIF;

   ----------------
   -- Set_Tohost --
   ----------------

   procedure Set_Tohost (This : in out Instance;
                         Addr :        Address)
   is
   begin
      This.Tohost_Addr := Addr;
      This.Tohost_Set := True;
   end Set_Tohost;

end LibRISCV.Sim.Memory_Bus;

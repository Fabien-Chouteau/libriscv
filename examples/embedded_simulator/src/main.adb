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

with LibRISCV.Sim.Platform;
with LibRISCV.Sim.Memory_Bus;
with LibRISCV.Sim.Shutdown;
with LibRISCV.Examples;

use LibRISCV;
use LibRISCV.Sim;


with Last_Chance_Handler;
pragma Unreferenced (Last_Chance_Handler);

procedure Main is

   SOC : Platform.Instance (Hart_Count => 1);
   Bus : Memory_Bus.Instance (RAM_Base => 16#8000_0000#,
                              RAM_Size => 1024);

   use type Platform.State_Kind;

   pragma Warnings (Off, "*has been made static*");
   Tohost : Word with Volatile;
   pragma Export (C, Tohost, "tohost");
   pragma Warnings (On, "*has been made static*");
begin

   SOC.Reset;

   Bus.Set_Tohost (16#E000_0000#);
   Bus.Enable_HTIF;

   Examples.Load_Hello_World (Bus, 16#E000_0000#);
   loop
      SOC.Cycle (Bus);
      exit when SOC.State = Platform.Reset or else Shutdown.Requested;
   end loop;

   Tohost := 1;
end Main;

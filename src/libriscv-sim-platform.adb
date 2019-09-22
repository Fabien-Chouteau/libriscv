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

package body LibRISCV.Sim.Platform is

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Instance) is
   begin
      for H of This.Harts loop
         H.Reset;
      end loop;
   end Reset;

   -----------
   -- Cycle --
   -----------

   procedure Cycle (This : in out Instance;
                    Bus  : in out Sim.Memory_Bus.Class)
   is
   begin
      for H of This.Harts loop
         H.Cycle (Bus);
      end loop;
   end Cycle;

   -----------
   -- State --
   -----------

   function State (This : Instance) return State_Kind is
      pragma Unreferenced (This);
   begin
      return Running;
   end State;

   ------------
   -- Resume --
   ------------

   procedure Resume (This : in out Instance) is
   begin
      for H of This.Harts loop
         H.Resume;
      end loop;
   end Resume;

   --------------
   -- Get_Hart --
   --------------

   function Get_Hart (This : in out Instance;
                      Id   : Hart_Id)
                      return not null Hart.Ptr
   is (This.Harts (Id)'Unchecked_Access);

   ----------
   -- Dump --
   ----------

   procedure Dump (This : Instance) is
   begin
      for H of This.Harts loop
         H.Dump;
      end loop;
   end Dump;

end LibRISCV.Sim.Platform;

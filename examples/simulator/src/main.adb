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

with Ada.Text_IO;
with Ada.Command_Line;
with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Strings;              use GNAT.Strings;

with LibRISCV.Sim.Hart;
with LibRISCV.Sim.Platform;
with LibRISCV.Sim.Memory_Bus;
with LibRISCV.Loader;
with LibRISCV.Signature;
with LibRISCV.Sim.Shutdown;
with LibRISCV.Sim.Log;
with LibRISCV.Sim.GDB_Remote_Target;
use LibRISCV;

procedure Main is

   Config         : Command_Line_Configuration;
   Signature_Path : aliased String_Access := null;
   Log_Enable     : aliased String_Access := null;
   HTIF_Enable    : aliased Boolean := False;
   GDB_Enable     : aliased Boolean := False;

   SOC : aliased Sim.Platform.Instance (Hart_Count => 1);
   Bus : aliased Sim.Memory_Bus.Instance (RAM_Base => 16#8000_0000#,
                                          RAM_Size => 256 * 1024);

   GDB_Target : Sim.GDB_Remote_Target.Instance (SOC'Unchecked_Access,
                                                Bus'Unchecked_Access,
                                                256);

   use type Sim.Platform.State_Kind;
   use type Sim.Hart.State_Kind;

begin

   declare
   begin
      Define_Switch
        (Config, Signature_Path'Access, "-s:",
         Long_Switch => "--signature=",
         Help        => "Pathname for the test signature output");

      Define_Switch
        (Config, Log_Enable'Access, "-l:",
         Long_Switch => "--log=",
         Help        =>
           "Comma separated list of log topics to enable/disable");

      Define_Switch
        (Config, HTIF_Enable'Access, "-t",
         Long_Switch => "--htif",
         Help        => "Enable the Host Target interface");

      Define_Switch
        (Config, GDB_Enable'Access, "-g",
         Long_Switch => "--gdb",
         Help        => "Enable the GDB remote interface (localhost:1234)");

      Set_Usage
        (Config,
         "[switches] binary.elf",
         "RISC-V simulator");

      Getopt (Config);
   exception
      when GNAT.Command_Line.Invalid_Switch =>
         Ada.Command_Line.Set_Exit_Status (1);
         return;
      when GNAT.Command_Line.Exit_From_Command_Line =>
         return;
   end;

   if Signature_Path /= null and then Signature_Path.all /= "" then
      Signature.Signature_Filepath (Signature_Path.all);
   end if;

   if Log_Enable /= null and then Log_Enable.all /= "" then
      if not LibRISCV.Sim.Log.Set_Arg (Log_Enable.all) then
         Ada.Text_IO.Put_Line ("Invalid log argument: '" &
                                 Log_Enable.all & "'");
         Ada.Command_Line.Set_Exit_Status (1);
         return;
      end if;
   end if;

   if HTIF_Enable then
      Bus.Enable_HTIF;
   end if;

   loop
      declare
         Arg : constant String := Get_Argument (Do_Expansion => True);
      begin
         exit when Arg'Length = 0;

         Loader.Load_Elf (Bus, Arg);
      end;
   end loop;


   SOC.Reset;

   if GDB_Enable then
      GDB_Target.Start_Server;
   else
      SOC.Resume;
   end if;

   loop

      if GDB_Enable then
         GDB_Target.Poll;
      end if;

      SOC.Cycle (Bus);

      if SOC.Get_Hart (1).State = Sim.Hart.Debug_Halt then
         case SOC.Get_Hart (1).Halt_Source is
            when Sim.Hart.None => null;
            when Sim.Hart.Single_Step => GDB_Target.Halted_On_Single_Step;
            when Sim.Hart.Breakpoint => GDB_Target.Halted_On_Breakpoint;
            when Sim.Hart.Watchpoint => null;
         end case;
      end if;

      exit when SOC.State = Sim.Platform.Reset or else Sim.Shutdown.Requested;
   end loop;

   Signature.Dump_Signature (Bus);
end Main;

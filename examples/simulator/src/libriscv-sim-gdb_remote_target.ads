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

with Interfaces; use Interfaces;

with GNAT.Sockets;

with GDB_Remote.Target;

with LibRISCV.Sim.Platform;
with LibRISCV.Sim.Memory_Bus;

package LibRISCV.Sim.GDB_Remote_Target is

   package Parent renames GDB_Remote.Target;

   type Instance (P           : not null Platform.Ptr;
                  Bus         : not null Memory_Bus.Ptr;
                  Buffer_Size : GDB_Remote.Buffer_Lenght_Type)
   is new Parent.Instance
   with private;

   subtype Class is Instance'Class;
   type Ptr is access all Class;

   procedure Start_Server (This : in out Instance);

   procedure Poll (This : in out Instance);

   overriding
   procedure Detach (This : in out Instance);

   overriding
   procedure Send_To_Host (This : in out Instance;
                           C    :        Character);

   overriding
   procedure Set_Thread (This : in out Instance;
                         Id   :        Integer);

   overriding
   procedure Read_Memory (This    : in out Instance;
                          Addr    :        GDB_Remote.Target.Address;
                          Data    :    out Unsigned_8;
                          Success :    out Boolean);

   overriding
   procedure Write_Memory (This    : in out Instance;
                           Addr    :        GDB_Remote.Target.Address;
                           Data    :        Unsigned_8;
                           Success :    out Boolean);

   overriding
   function Last_General_Register (This : Instance) return Natural
   is (31);

   overriding
   procedure Read_Register (This    : in out Instance;
                            Id      :        Natural;
                            Data    :    out GDB_Remote.Target.Register;
                            Success :    out Boolean);

   overriding
   procedure Write_Register (This    : in out Instance;
                             Id      :        Natural;
                             Data    :        GDB_Remote.Target.Register;
                             Success :    out Boolean);

   overriding
   procedure Continue (This        : in out Instance;
                       Single_Step :        Boolean := False);

   overriding
   procedure Continue_At (This        : in out Instance;
                          Addr        :        GDB_Remote.Target.Address;
                          Single_Step :        Boolean := False);

   overriding
   procedure Halt (This : in out Instance);

   procedure Halted_On_Single_Step (This : in out Instance);
   procedure Halted_On_Breakpoint (This : in out Instance);

   overriding
   function Supported (This   : Instance;
                       B_Type : GDB_Remote.Breakpoint_Type)
                       return Boolean;

   overriding
   procedure Insert_Breakpoint (This   : in out Instance;
                                B_Type :        GDB_Remote.Breakpoint_Type;
                                Addr   :        GDB_Remote.Target.Address;
                                Kind   :        Natural);

   overriding
   procedure Remove_Breakpoint (This   : in out Instance;
                                B_Type :        GDB_Remote.Breakpoint_Type;
                                Addr   :        GDB_Remote.Target.Address;
                                Kind   :        Natural);

private

   type Instance (P           : not null Platform.Ptr;
                  Bus         : not null Memory_Bus.Ptr;
                  Buffer_Size : GDB_Remote.Buffer_Lenght_Type)
   is new Parent.Instance (Buffer_Size)
     with record
      Receiver     : GNAT.Sockets.Socket_Type;
      Connection   : GNAT.Sockets.Socket_Type;
      Client       : GNAT.Sockets.Sock_Addr_Type;
      Channel      : GNAT.Sockets.Stream_Access;

      Current_Hart : Platform.Hart_Id := 1;
   end record;

   procedure Wait_For_Connection (This : in out Instance);

end LibRISCV.Sim.GDB_Remote_Target;

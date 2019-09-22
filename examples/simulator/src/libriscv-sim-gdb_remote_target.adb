with Ada.Text_IO;
with Ada.IO_Exceptions;

with LibRISCV.Sim.Hart;
with LibRISCV.CSR;

package body LibRISCV.Sim.GDB_Remote_Target is

   use type GDB_Remote.Breakpoint_Type;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server (This : in out Instance) is
   begin
      GNAT.Sockets.Create_Socket (Socket => This.Receiver);
      GNAT.Sockets.Set_Socket_Option
        (Socket => This.Receiver,
         Level  => GNAT.Sockets.Socket_Level,
         Option => (Name    => GNAT.Sockets.Reuse_Address, Enabled => True));
      GNAT.Sockets.Bind_Socket
        (Socket  => This.Receiver,
         Address => (Family => GNAT.Sockets.Family_Inet,
                     Addr   => GNAT.Sockets.Inet_Addr ("127.0.0.1"),
                     Port   => 1234));

      This.Wait_For_Connection;
   end Start_Server;

   -------------------------
   -- Wait_For_Connection --
   -------------------------

   procedure Wait_For_Connection (This : in out Instance) is
      Req : GNAT.Sockets.Request_Type;
   begin
      GNAT.Sockets.Listen_Socket (Socket => This.Receiver);

      GNAT.Sockets.Accept_Socket
        (Server  => This.Receiver,
         Socket  => This.Connection,
         Address => This.Client);
      Ada.Text_IO.Put_Line
        ("Client connected from " & GNAT.Sockets.Image (This.Client));

      Req := (GNAT.Sockets.Non_Blocking_IO, True);
      GNAT.Sockets.Control_Socket (This.Connection, Req);

      This.Channel := GNAT.Sockets.Stream (This.Connection);

      for Id in 1 .. This.P.Hart_Count loop
         This.P.Get_Hart (Id).Set_Debugger_Attached;
      end loop;
   end Wait_For_Connection;

   ----------
   -- Poll --
   ----------

   procedure Poll (This : in out Instance) is
      C : Character;
   begin
      C := Character'Input (This.Channel);
      This.From_Host (C);
   exception
      when Ada.IO_Exceptions.End_Error | GNAT.Sockets.Socket_Error =>
         null;
   end Poll;

   ------------
   -- Detach --
   ------------

   overriding
   procedure Detach (This : in out Instance) is
   begin
      GNAT.Sockets.Close_Socket (This.Connection);

      for Id in 1 .. This.P.Hart_Count loop
         This.P.Get_Hart (Id).Set_Debugger_Attached (False);
      end loop;

      This.Wait_For_Connection;
   end Detach;

   ------------------
   -- Send_To_Host --
   ------------------

   overriding procedure Send_To_Host (This : in out Instance; C : Character) is
   begin
      Character'Output (This.Channel, C);
   end Send_To_Host;

   ----------------
   -- Set_Thread --
   ----------------

   overriding
   procedure Set_Thread (This : in out Instance;
                         Id   :        Integer)
   is
   begin
      if Id in -1 | 0 then
         This.Current_Hart := 1;
      else
         This.Current_Hart := Platform.Hart_Id (Id);
      end if;
   end Set_Thread;

   -----------------
   -- Read_Memory --
   -----------------

   overriding procedure Read_Memory
     (This : in out Instance; Addr : GDB_Remote.Target.Address;
      Data :    out Unsigned_8; Success : out Boolean)
   is
      use type Memory_Bus.Access_Result;

      Res : Memory_Bus.Access_Result;
   begin
      This.Bus.Load_B (Address (Addr), Byte (Data), Res);

      Success := Res = Memory_Bus.Success;
   end Read_Memory;

   ------------------
   -- Write_Memory --
   ------------------

   overriding procedure Write_Memory
     (This : in out Instance; Addr : GDB_Remote.Target.Address;
      Data :        Unsigned_8; Success : out Boolean)
   is
      use type Memory_Bus.Access_Result;

      Res : Memory_Bus.Access_Result;
   begin
      This.Bus.Store_B (Address (Addr), Byte (Data), Res);

      Success := Res = Memory_Bus.Success;
   end Write_Memory;

   -------------------
   -- Read_Register --
   -------------------

   overriding
   procedure Read_Register (This    : in out Instance;
                            Id      :        Natural;
                            Data    :    out GDB_Remote.Target.Register;
                            Success :    out Boolean)
   is
      H : constant not null Hart.Ptr := This.P.Get_Hart (This.Current_Hart);
   begin
      if Id in Natural (GPR_Id'First) .. Natural (GPR_Id'Last) then
         Data := GDB_Remote.Target.Register (H.Read_GPR (GPR_Id (Id)).U);
         Success := True;

      elsif Id = 32 then
         Data := GDB_Remote.Target.Register (H.Read_PC.U);
         Success := True;

      elsif Id < Natural (CSR.Id'Last) then
         Data := GDB_Remote.Target.Register (H.Debug_Read_CSR (CSR.Id (Id)).U);
         Success := True;

      else
         Success := False;
      end if;

      Data := (Shift_Right (Data, 24) and 16#FF#)
        or (Shift_Right (Data, 8) and 16#FF00#)
        or (Shift_Left (Data, 8) and 16#FF0000#)
        or (Shift_Left (Data, 24) and 16#FF000000#);
   end Read_Register;

   --------------------
   -- Write_Register --
   --------------------

   overriding
   procedure Write_Register (This    : in out Instance;
                             Id      :        Natural;
                             Data    :        GDB_Remote.Target.Register;
                             Success :    out Boolean)
   is
      H : constant not null Hart.Ptr := This.P.Get_Hart (This.Current_Hart);
   begin
      if Id in Natural (GPR_Id'First) .. Natural (GPR_Id'Last) then
         H.Write_GPR (GPR_Id (Id), (Signed => False,
                                    U => U_Register ((Shift_Right (Data, 24) and 16#FF#)
                                      or (Shift_Right (Data, 8) and 16#FF00#)
                                      or (Shift_Left (Data, 8) and 16#FF0000#)
                                      or (Shift_Left (Data, 24) and 16#FF000000#))));
         Success := True;
      else
         Success := False;
      end if;
   end Write_Register;

   --------------
   -- Continue --
   --------------

   overriding
   procedure Continue (This        : in out Instance;
                       Single_Step :        Boolean := False)
   is
      H : constant not null Hart.Ptr := This.P.Get_Hart (This.Current_Hart);
   begin
      if Single_Step then
         H.Single_Step;
      else
         H.Resume;
      end if;
   end Continue;

   -----------------
   -- Continue_At --
   -----------------

   overriding
   procedure Continue_At (This        : in out Instance;
                          Addr        :        GDB_Remote.Target.Address;
                          Single_Step :        Boolean := False)
   is
      H : constant not null Hart.Ptr := This.P.Get_Hart (This.Current_Hart);
   begin

      H.Write_PC ((Signed => False, U => U_Register (Addr)));
      if Single_Step then
         H.Single_Step;
      else
         H.Resume;
      end if;
   end Continue_At;

   ----------
   -- Halt --
   ----------

   overriding
   procedure Halt (This : in out Instance) is
   begin
      This.P.Get_Hart (This.Current_Hart).Halt;
   end Halt;

   ---------------
   -- Supported --
   ---------------

   overriding
   function Supported (This   : Instance;
                       B_Type : GDB_Remote.Breakpoint_Type)
                       return Boolean
   is (B_Type /= GDB_Remote.Software_Breakpoint);

   ---------------------------
   -- Halted_On_Single_Step --
   ---------------------------

   procedure Halted_On_Single_Step (This : in out Instance) is
   begin
      This.Send_Packet ("T05thread:01;");
   end Halted_On_Single_Step;

   --------------------------
   -- Halted_On_Breakpoint --
   --------------------------

   procedure Halted_On_Breakpoint (This : in out Instance) is
   begin
      This.Send_Packet ("T05thread:01;");
   end Halted_On_Breakpoint;

   -----------------------
   -- Insert_Breakpoint --
   -----------------------

   overriding
   procedure Insert_Breakpoint (This   : in out Instance;
                                B_Type :        GDB_Remote.Breakpoint_Type;
                                Addr   :        GDB_Remote.Target.Address;
                                Kind   :        Natural)
   is
      pragma Unreferenced (Kind);
      H       : constant not null Hart.Ptr := This.P.Get_Hart (This.Current_Hart);
      Success : Boolean;
   begin
      case B_Type is
         when GDB_Remote.Hardware_Breakpoint =>
            H.Add_Breakpoint (U_Register (Addr), Success);
         when others => raise Program_Error;
      end case;
   end Insert_Breakpoint;

   -----------------------
   -- Remove_Breakpoint --
   -----------------------

   overriding
   procedure Remove_Breakpoint (This   : in out Instance;
                                B_Type :        GDB_Remote.Breakpoint_Type;
                                Addr   :        GDB_Remote.Target.Address;
                                Kind   :        Natural)
   is
      pragma Unreferenced (Kind);
      H       : constant not null Hart.Ptr := This.P.Get_Hart (This.Current_Hart);
      Success : Boolean;
   begin
      case B_Type is
         when GDB_Remote.Hardware_Breakpoint =>
            H.Remove_Breakpoint (U_Register (Addr), Success);
         when others => raise Program_Error;
      end case;
   end Remove_Breakpoint;

end LibRISCV.Sim.GDB_Remote_Target;

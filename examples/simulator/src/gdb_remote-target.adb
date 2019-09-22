with GDB_Remote.Agent; use GDB_Remote.Agent;

package body GDB_Remote.Target is

   subtype Dispatch is Class;

   ---------------
   -- From_Host --
   ---------------

   procedure From_Host (This : in out Instance; C : Character) is
      Evt : constant Agent.Event := This.Received (C);
   begin

      case Evt.Kind is
         when Agent.None | Agent.Got_Ack | Agent.Got_Nack =>
            null;

         when Agent.Got_Packet =>
            case Evt.P.Kind is
               when Bad_Packet =>
                  This.Nack;

               when Unknown_Packet =>
                  This.Send_Packet ("");

               when Detach =>
                  This.Ack;
                  This.Send_Packet ("OK");
                  Dispatch (This).Detach;

               when General_Query => null;
                  case Evt.P.Q_Topic is
                     when Supported =>
                        This.Ack;
                        This.Send_Packet ("PacketSize=1024");

                     when TStatus =>
                        --  Send empty packet because we don't support Traces...
                        This.Ack;
                        This.Send_Packet ("");

                     when Attached =>
                        This.Ack;
                        This.Send_Packet ("1");

                     when Thread_Info_Start =>
                        This.Ack;
                        This.Send_Packet ("m1");
                     when Thread_Info_Cont =>
                        This.Ack;
                        This.Send_Packet ("l");

                     when Unknown_General_Query =>
                        --  Send empty packet when we don't know the query
                        This.Ack;
                        This.Send_Packet ("");
                  end case;

               when Query_Supported_Continue =>
                  This.Ack;
                  This.Send_Packet ("vCont;c;s;t");

               when Must_Reply_Empty =>
                  This.Ack;
                  This.Send_Packet ("");

               when Question_Halt =>
                  This.Ack;
                  This.Send_Packet ("T05thread:01;");

               when Set_Thread =>
                  This.Ack;
                  This.Send_Packet ("OK");

               when Read_General_Registers =>
                  This.Ack;
                  This.Start_Packet;
                  declare
                     Success : Boolean := False;
                     Data    : Register;
                  begin
                     for Id in 0 .. Dispatch (This).Last_General_Register loop
                        Dispatch (This).Read_Register (Id, Data, Success);
                        Push_Register (Parent.Instance (This), Data);
                     end loop;
                  end;
                  This.End_Packet;

               when Read_Register =>
                  This.Ack;
                  declare
                     Success : Boolean := False;
                     Data    : Register;
                  begin
                     Dispatch (This).Read_Register (Evt.P.Register_Id, Data, Success);

                     if Success then
                        This.Start_Packet;
                        Push_Register (Parent.Instance (This), Data);
                        This.End_Packet;
                     else
                        This.Send_Packet ("");
                     end if;
                  end;

               when Read_Memory =>
                  This.Ack;
                  This.Start_Packet;

                  declare
                     Data    : Unsigned_8;
                     Success : Boolean;
                  begin
                     for X in Evt.P.M_Address .. Evt.P.M_Address + Evt.P.M_Size - 1 loop

                        Dispatch (This).Read_Memory (X, Data, Success);

                        if Success then
                           This.Push_Data (Hex (Data));
                        else
                           --  In theory GDB is able to handle a shorter answer
                           --  than expected if the target "can read only part of
                           --  the region of memory", in practice this makes GDB
                           --  crash. So we send zero instead.
                           This.Push_Data ("00");
                        end if;
                     end loop;
                  end;
                  This.End_Packet;

               when Write_Memory =>
                  This.Ack;

                  declare
                     Data    : Unsigned_8;
                     Success : Boolean;
                  begin
                     for X in Evt.P.M_Address .. Evt.P.M_Address + Evt.P.M_Size - 1 loop

                        Data := Dispatch (This).Next_Data (Success);
                        if not Success then
                           This.Send_Packet ("E ");
                           return;
                        end if;

                        Dispatch (This).Write_Memory (X, Data, Success);
                        if not Success then
                           This.Send_Packet ("E ");
                           return;
                        end if;
                     end loop;
                  end;
                  This.Send_Packet ("OK");

               when Continue =>
                  This.Ack;
                  Dispatch (This).Continue;

               when Continue_Addr =>
                  This.Ack;
                  Dispatch (This).Continue_At (Evt.P.C_Address);

               when Single_Step =>
                  This.Ack;
                  Dispatch (This).Continue (Single_Step => True);

               when Single_Step_Addr =>
                  This.Ack;
                  Dispatch (This).Continue_At (Evt.P.C_Address,
                                              Single_Step => True);

               when Insert_Breakpoint | Remove_Breakpoint =>
                  This.Ack;
                  if Dispatch (This).Supported (Evt.P.B_Type) then
                     if Evt.P.Kind = Insert_Breakpoint then
                        Dispatch (This).Insert_Breakpoint (Evt.P.B_Type,
                                                           Evt.P.B_Addr,
                                                           Evt.P.B_Kind);
                     else
                        Dispatch (This).Remove_Breakpoint (Evt.P.B_Type,
                                                           Evt.P.B_Addr,
                                                           Evt.P.B_Kind);
                     end if;
                  else
                     Dispatch (This).Send_Packet ("");
                  end if;

            end case;


         when Agent.Got_An_Interrupt =>
            Dispatch (This).Halt;
            This.Send_Packet ("T02Thread:01;");

      end case;
   end From_Host;

end GDB_Remote.Target;

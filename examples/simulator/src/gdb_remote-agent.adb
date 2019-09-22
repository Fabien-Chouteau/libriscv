package body GDB_Remote.Agent is

   subtype Dispatch is Class;

   --------------
   -- Received --
   --------------

   function Received (This : in out Instance; C : Character) return Event is
   begin

      case This.State is
         when Waiting_For_Ack =>
            case C is
               when '+'    =>
                  This.State := Waiting_For_Start;
                  return (Kind => Got_Ack);
               when '-'    =>
                  This.State := Waiting_For_Start;
                  return (Kind => Got_Nack);
               when others =>
                  raise Program_Error
                    with "Unexpected char when waiting for ack/nack";
            end case;
      when Waiting_For_Start =>
            case C is
               when '$' =>
                  This.State := Receiving_Packet;
                  This.Checksum := 0;
                  This.Checksum_Str_Count := 0;
                  This.Char_Count := 0;

               when '+' =>
                  null; -- Potentially an ack from a reconnction

               when ASCII.ETX =>
                  return (Kind => Got_An_Interrupt);

               when others =>
                  raise Program_Error
                    with "Unexpected char when waiting for start";
            end case;

         when Receiving_Packet =>
            case C is
               when '#' =>
                  This.State := Checksum;
               when others =>
                  --  Add the new character to the buffer and compute checksum

                  This.Char_Count := This.Char_Count + 1;
                  This.Buffer (This.Char_Count) := C;

                  This.Checksum := This.Checksum + C'Enum_Rep;
            end case;

         when Checksum =>
            This.Checksum_Str_Count := This.Checksum_Str_Count + 1;
            This.Checksum_Str (This.Checksum_Str_Count) := C;

            if This.Checksum_Str_Count = 2 then

               if This.Checksum_OK then

                  --  We have a packet

                  This.State := Waiting_For_Start;

                  return Res : Event := (Kind => Got_Packet, others => <>) do
                     Res.P := Parse_Packet
                       (This.Buffer (1 .. This.Char_Count),
                        This.Field_Cursor);

                     if Res.P.Kind in Write_Memory then
                        This.Data_Fmt := Res.P.M_Data_Fmt;
                     end if;
                  end return;
               else
                  raise Program_Error
                    with "Bad checksum: expected: " & Hex (This.Checksum) &
                    " got: '" & This.Checksum_Str & "'";
               end if;
            end if;
      end case;

      return (Kind => None);
   end Received;

   ----------------
   -- Next_Field --
   ----------------

   function Next_Field (This : in out Instance) return String
   is (Next_Field (This.Buffer (This.Buffer'First .. This.Char_Count),
                   This.Field_Cursor));

   ---------------
   -- Next_Data --
   ---------------

   function Next_Data (This    : in out Instance;
                       Success :    out Boolean)
                       return Unsigned_8
   is (Next_Data (This.Buffer (This.Buffer'First .. This.Char_Count),
                  This.Field_Cursor,
                  This.Data_Fmt, Success));

   ---------
   -- Ack --
   ---------

   procedure Ack (This : in out Instance) is
   begin
      Dispatch (This).Send_To_Host ('+');
   end Ack;

   ----------
   -- Nack --
   ----------

   procedure Nack (This : in out Instance) is
   begin
      Dispatch (This).Send_To_Host ('-');
   end Nack;

   -----------------
   -- Send_Packet --
   -----------------

   procedure Send_Packet (This : in out Instance;
                          Data :        String)
   is
      Checksum : Unsigned_8 := 0;
   begin
      Dispatch (This).Send_To_Host ('$');

      for C of Data loop
         Checksum := Checksum + C'Enum_Rep;
         Dispatch (This).Send_To_Host  (C);
      end loop;

      Dispatch (This).Send_To_Host ('#');

      declare
         Cs : constant String := Hex (Checksum);
      begin
         Dispatch (This).Send_To_Host (Cs (Cs'First));
         Dispatch (This).Send_To_Host (Cs (Cs'Last));
      end;
   end Send_Packet;

   ------------------
   -- Start_Packet --
   ------------------

   procedure Start_Packet (This : in out Instance)
   is
   begin
      Dispatch (This).Send_To_Host ('$');
      This.Tx_Checksum := 0;
   end Start_Packet;

   ---------------
   -- Push_Data --
   ---------------

   procedure Push_Data (This : in out Instance;
                        Data : String)
   is
   begin
      for C of Data loop
         This.Tx_Checksum := This.Tx_Checksum + C'Enum_Rep;
         Dispatch (This).Send_To_Host (C);
      end loop;
   end Push_Data;

   --------------
   -- Push_Mod --
   --------------

   procedure Push_Mod (This : in out Instance;
                       Data : T)
   is
      function Hex_Image is new ESF.Hex_Image (T);
   begin
      This.Push_Data (Hex_Image (Data));
   end Push_Mod;

   ----------------
   -- End_Packet --
   ----------------

   procedure End_Packet (This : in out Instance)
   is
   begin
      Dispatch (This).Send_To_Host ('#');

      declare
         Cs : constant String := Hex (This.Tx_Checksum);
      begin
         Dispatch (This).Send_To_Host (Cs (Cs'First));
         Dispatch (This).Send_To_Host (Cs (Cs'Last));
      end;
   end End_Packet;

   -----------------
   -- Checksum_OK --
   -----------------

   function Checksum_OK (This : Instance) return Boolean is
      H : constant Character := This.Checksum_Str (1);
      L : constant Character := This.Checksum_Str (2);
   begin
      if This.Checksum_Str_Count /= 2
        or else
          not Is_Hex (H)
        or else
          not Is_Hex (L)
      then
         return False;
      else
         return
           ((From_Hex (H) = (Shift_Right (This.Checksum, 4) and 16#F#))
            and then
            (From_Hex (L) = (This.Checksum and 16#F#)));
      end if;
   end Checksum_OK;

end GDB_Remote.Agent;

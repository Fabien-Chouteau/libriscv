
package GDB_Remote.Agent is

   subtype Address is Unsigned_64;

   type Instance (Buffer_Size : Buffer_Lenght_Type := 256)
   is abstract tagged limited private;

   subtype Class is Instance'Class;
   type Ptr is access all Class;

   procedure Send_To_Host (This : in out Instance;
                           C    :        Character)
   is abstract;


   type Event_Kind is (None, Got_Packet, Got_Ack, Got_Nack, Got_An_Interrupt);

   type Event (Kind : Event_Kind := None) is record
      case Kind is
         when Got_Packet =>
            P : Packet;
         when None | Got_Ack | Got_Nack | Got_An_Interrupt =>
            null;
      end case;
   end record;

   function Received (This : in out Instance;
                      C    :        Character)
                      return Event;

   function Next_Field (This : in out Instance) return String;

   function Next_Data (This    : in out Instance;
                       Success :    out Boolean)
                       return Unsigned_8;
   --  Return the next octet of the binary data in a packet

   procedure Ack (This : in out Instance);

   procedure Nack (This : in out Instance);

   procedure Send_Packet (This : in out Instance;
                          Data :        String);

   procedure Start_Packet (This : in out Instance);

   procedure Push_Data (This : in out Instance;
                        Data : String);

   generic
      type T is mod <>;
   procedure Push_Mod (This : in out Instance;
                       Data : T);

   procedure End_Packet (This : in out Instance);

private

   type State_Kind is (Waiting_For_Ack,
                       Waiting_For_Start,
                       Receiving_Packet,
                       Checksum);

   type Instance (Buffer_Size : Buffer_Lenght_Type := 256)
   is abstract tagged limited record
      Buffer : String (1 .. Buffer_Size);
      Char_Count : Natural := 0;

      Field_Cursor : Natural := 0;

      Checksum : Unsigned_8;
      Checksum_Str : String (1 .. 2);
      Checksum_Str_Count : Natural := 0;

      State : State_Kind := Waiting_For_Ack;

      Data_Fmt : Data_Format := Binary;

      Tx_Checksum : Unsigned_8;
   end record;

   function Checksum_OK (This : Instance) return Boolean;

end GDB_Remote.Agent;

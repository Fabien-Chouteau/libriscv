with Interfaces; use Interfaces;

private with ESF;

package GDB_Remote is

   type Packet_Kind is (Bad_Packet, Unknown_Packet,
                        General_Query,
                        Must_Reply_Empty,
                        Set_Thread,
                        Question_Halt,
                        Read_General_Registers,
                        Read_Register,
                        Read_Memory,
                        Write_Memory,
                        Query_Supported_Continue,
                        Continue,
                        Continue_Addr,
                        Single_Step,
                        Single_Step_Addr,
                        Insert_Breakpoint,
                        Remove_Breakpoint,
                        Detach);

   type General_Query_Topic is (Unknown_General_Query, Supported, TStatus,
                                Attached, Thread_Info_Start, Thread_Info_Cont);

   type Breakpoint_Type is (Software_Breakpoint,
                            Hardware_Breakpoint,
                            Read_Watchpoint,
                            Write_Watchpoint,
                            Access_Watchpoint);

   type Data_Format is (Hexadecimal, Binary);

   type Packet (Kind : Packet_Kind := Bad_Packet) is record
      case Kind is
         when Bad_Packet | Unknown_Packet | Must_Reply_Empty | Question_Halt |
              Read_General_Registers | Query_Supported_Continue | Continue |
              Single_Step | Detach
            => null;
         when General_Query =>
            Q_Topic : General_Query_Topic;
         when Set_Thread =>
            Op        : Character;
            Thread_ID : Integer;
         when Read_Register =>
            Register_Id : Integer;
         when Read_Memory | Write_Memory =>
            M_Address  : Unsigned_64;
            M_Size     : Unsigned_64;
            M_Data_Fmt : Data_Format;
         when Continue_Addr | Single_Step_Addr =>
            C_Address : Unsigned_64;
         when Insert_Breakpoint | Remove_Breakpoint =>
            B_Type : Breakpoint_Type;
            B_Addr : Unsigned_64;
            B_Kind : Natural;
      end case;
   end record;

   function Parse_Packet (Str : String; Cursor : out Natural) return Packet;

   function Next_Field (P      :        String;
                        Cursor : in out Natural)
                        return String;

   function Next_Data (P       :        String;
                       Cursor  : in out Natural;
                       Fmt     :        Data_Format;
                       Success :    out Boolean)
                       return Unsigned_8;
   --  Return the next octet of data part of a packet

   subtype Buffer_Lenght_Type is Positive range 1 .. 4096;

private

   function Parse_General_Query (Str : String; Cursor : in out Natural) return Packet;
   function Parse_v_Packet (Str : String; Cursor : in out Natural) return Packet;
   function Parse_Set_Thread (Str : String; Cursor : in out Natural) return Packet;
   function Parse_Read_Register (Str : String; Cursor : in out Natural) return Packet;
   function Parse_Read_Memory (Str : String; Cursor : in out Natural) return Packet;
   function Parse_Write_Memory (Str : String; Cursor : in out Natural; Fmt : Data_Format) return Packet;
   function Parse_Continue (Str : String; Cursor : in out Natural) return Packet;
   function Parse_Single_Step (Str : String; Cursor : in out Natural) return Packet;
   function Parse_Breakpoint (Str : String; Cursor : in out Natural; Insert : Boolean) return Packet;

   function Hex is new ESF.Hex_Image (Unsigned_8);

   function Is_Hex (C : Character) return Boolean
   is (C in '0' .. '9' | 'A' .. 'F' | 'a' .. 'f')
     with Inline;

   function From_Hex (C : Character) return Unsigned_8
   is (case C is
          when '0' .. '9' => Character'Pos (C) - Character'Pos ('0'),
          when 'A' .. 'F' => Character'Pos (C) - Character'Pos ('A') + 10,
          when 'a' .. 'f' => Character'Pos (C) - Character'Pos ('a') + 10,
          when others => raise Program_Error)
        with Pre => Is_Hex (C);

   function From_Hex (Str : String) return Unsigned_64
     with Pre => (for all C of Str => Is_Hex (C));

   function To_Int (Str : String) return Integer;

end GDB_Remote;

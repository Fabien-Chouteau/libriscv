package body GDB_Remote is

   --------------
   -- From_Hex --
   --------------

   function From_Hex (Str : String) return Unsigned_64
   is
      Res : Unsigned_64 := 0;
   begin
      for C of Str loop
         Res := Shift_Left (Res, 4);
         Res := Res + Unsigned_64 (From_Hex (C));
      end loop;

      return Res;
   end From_Hex;

   ------------------
   -- Parse_Packet --
   ------------------

   function Parse_Packet (Str : String; Cursor : out Natural) return Packet is
   begin
      Cursor := Str'First + 1;

      case Str (Str'First) is

      when 'c' =>
         return Parse_Continue (Str, Cursor);

      when 'D' =>
         return (Kind => Detach);

      when 's' =>
         return Parse_Single_Step (Str, Cursor);

      when 'q' =>
         return Parse_General_Query (Str, Cursor);

      when 'v' =>
         return Parse_v_Packet (Str, Cursor);

      when 'H' =>
         return Parse_Set_Thread (Str, Cursor);

      when '?' =>
         return (Kind => Question_Halt);

      when 'g' =>
         return (Kind => Read_General_Registers);

      when 'p' =>
         return Parse_Read_Register (Str, Cursor);

      when 'm' =>
         return Parse_Read_Memory (Str, Cursor);

      when 'M' =>
         return Parse_Write_Memory (Str, Cursor, Hexadecimal);

      when 'X' =>
         return Parse_Write_Memory (Str, Cursor, Binary);

      when 'z' =>
         return Parse_Breakpoint (Str, Cursor, Insert => False);

      when 'Z' =>
         return Parse_Breakpoint (Str, Cursor, Insert => True);

      when others => return (Kind => Unknown_Packet);
      end case;
   end Parse_Packet;

   -------------------------
   -- Parse_General_Query --
   -------------------------

   function Parse_General_Query (Str    :        String;
                                 Cursor : in out Natural)
                                 return Packet
   is
      Topic_Str : constant String := Next_Field (Str, Cursor);
      Topic     : General_Query_Topic := Unknown_General_Query;
   begin
      if Topic_Str = "Supported" then
         Topic := Supported;
      elsif Topic_Str = "TStatus" then
         Topic := TStatus;
      elsif Topic_Str = "Attached" then
         Topic := Attached;
      elsif Topic_Str = "fThreadInfo" then
         Topic := Thread_Info_Start;
      elsif Topic_Str = "sThreadInfo" then
         Topic := Thread_Info_Cont;
      end if;

      return (Kind    => General_Query,
              Q_Topic => Topic);
   end Parse_General_Query;

   -------------------
   -- Parse_v_Query --
   -------------------

   function Parse_v_Packet (Str    :        String;
                            Cursor : in out Natural)
                            return Packet
   is
      Id_Str : constant String := Next_Field (Str, Cursor);
   begin
      Cursor := Str'Last + 1;

      if Id_Str = "MustReplyEmpty" then
         return (Kind => Must_Reply_Empty);
      elsif Id_Str = "Cont?" then
         return (Kind => Query_Supported_Continue);
      else
         return (Kind => Unknown_Packet);
      end if;
   end Parse_v_Packet;

   ----------------------
   -- Parse_Set_Thread --
   ----------------------

   function Parse_Set_Thread (Str    :        String;
                              Cursor : in out Natural)
                              return Packet
   is
      Res : Packet := (Kind => Set_Thread, others => <>);
   begin
      if Str'Length < 3 then
         return (Kind => Bad_Packet);
      end if;

      Res.Op := Str (Cursor);

      Res.Thread_ID := To_Int (Str (Cursor + 1 .. Str'Last));

      Cursor := Str'Last + 1;
      return Res;
   end Parse_Set_Thread;

   -------------------------
   -- Parse_Read_Register --
   -------------------------

   function Parse_Read_Register (Str    :        String;
                                 Cursor : in out Natural)
                                 return Packet
   is
      Res : Packet := (Kind => Read_Register, others => <>);
   begin
      if Str'Length < 2 then
         return (Kind => Bad_Packet);
      end if;

      Res.Register_Id := To_Int (Str (Cursor .. Str'Last));

      Cursor := Str'Last + 1;
      return Res;
   end Parse_Read_Register;

   -----------------------
   -- Parse_Read_Memory --
   -----------------------

   function Parse_Read_Memory (Str    :        String;
                               Cursor : in out Natural)
                               return Packet
   is
      Res : Packet := (Kind => Read_Memory, others => <>);
      Addr_Str : constant String := Next_Field (Str, Cursor);
      Size_Str : constant String := Next_Field (Str, Cursor);
   begin

      Res.M_Address := From_Hex (Addr_Str);
      Res.M_Size := From_Hex (Size_Str);
      return Res;
   end Parse_Read_Memory;

   ------------------------
   -- Parse_Write_Memory --
   ------------------------

   function Parse_Write_Memory (Str    :        String;
                                Cursor : in out Natural;
                                Fmt    : Data_Format)
                               return Packet
   is
      Res : Packet := (Kind => Write_Memory, others => <>);
      Addr_Str : constant String := Next_Field (Str, Cursor);
      Size_Str : constant String := Next_Field (Str, Cursor);
   begin

      Res.M_Address := From_Hex (Addr_Str);
      Res.M_Size := From_Hex (Size_Str);
      Res.M_Data_Fmt := Fmt;
      return Res;
   end Parse_Write_Memory;

   --------------------
   -- Parse_Continue --
   --------------------

   function Parse_Continue (Str : String;
                            Cursor : in out Natural)
                            return Packet
   is
      Addr : Integer;
   begin
      if Str'Length > 1 then
         Addr := To_Int (Str (Cursor .. Str'Last));
         Cursor := Str'Last + 1;
         return (Kind => Continue_Addr, C_Address => Unsigned_64 (Addr));
      else
         return (Kind => Continue);
      end if;
   end Parse_Continue;

   -----------------------
   -- Parse_Single_Step --
   -----------------------

   function Parse_Single_Step (Str : String;
                               Cursor : in out Natural)
                               return Packet
   is
      Addr : Integer;
   begin
      if Str'Length > 1 then
         Addr := To_Int (Str (Cursor .. Str'Last));
         Cursor := Str'Last + 1;
         return (Kind => Single_Step_Addr, C_Address => Unsigned_64 (Addr));
      else
         return (Kind => Single_Step);
      end if;
   end Parse_Single_Step;

   ----------------------
   -- Parse_Breakpoint --
   ----------------------

   function Parse_Breakpoint (Str    :        String;
                              Cursor : in out Natural;
                              Insert : Boolean)
                              return Packet
   is
      Type_Str : constant String := Next_Field (Str, Cursor);
      Addr_Str : constant String := Next_Field (Str, Cursor);
      Kind_Str : constant String := Next_Field (Str, Cursor);

      B_Type : Breakpoint_Type;
      B_Kind : Natural;
      B_Addr : Unsigned_64;
   begin
      if Type_Str'Length /= 1 then
         return (Kind => Bad_Packet);
      end if;

      case Type_Str (Type_Str'First) is
         when '0' => B_Type := Software_Breakpoint;
         when '1' => B_Type := Hardware_Breakpoint;
         when '2' => B_Type := Write_Watchpoint;
         when '3' => B_Type := Read_Watchpoint;
         when '4' => B_Type := Access_Watchpoint;
         when others => return (Kind => Bad_Packet);
      end case;

      B_Addr := From_Hex (Addr_Str);
      B_Kind := Natural (From_Hex (Kind_Str));

      if Insert then
         return (Kind   => Insert_Breakpoint,
                 B_Type => B_Type,
                 B_Addr => B_Addr,
                 B_Kind => B_Kind);
      else
         return (Kind   => Remove_Breakpoint,
                 B_Type => B_Type,
                 B_Addr => B_Addr,
                 B_Kind => B_Kind);
      end if;
   end Parse_Breakpoint;

   ----------------
   -- Next_Field --
   ----------------

   function Next_Field (P      :        String;
                        Cursor : in out Natural)
                        return String
   is
      From   : constant Natural := Cursor;
      To     :          Natural;
   begin
      while Cursor in P'First .. P'Last
        and then
          P (Cursor) not in ':' | ';' | ','
      loop
         Cursor := Cursor + 1;
      end loop;

      To := Cursor - 1;

      Cursor := Cursor + 1;

      return P (From .. To);
   end Next_Field;

   ---------------
   -- Next_Data --
   ---------------

   function Next_Data (P       :        String;
                       Cursor  : in out Natural;
                       Fmt     :        Data_Format;
                       Success :    out Boolean)
                       return Unsigned_8
   is
      Res : Unsigned_8;
   begin
      case Fmt is
         when Hexadecimal =>
            if Cursor in P'First .. P'Last - 1 then
               Success := True;
               return Unsigned_8 (From_Hex (P (Cursor .. Cursor + 1)));
            end if;
         when Binary =>
            if Cursor in P'First .. P'Last then
               if P (Cursor) = '}' then
                  --  Escape character

                  Cursor := Cursor + 1;

                  if Cursor <= P'Last then

                     Res := Unsigned_8 (P (Cursor)'Enum_Rep) xor 16#20#;
                     Cursor := Cursor + 1;
                     Success := True;
                     return Res;
                  end if;
               else
                  Res := Unsigned_8 (P (Cursor)'Enum_Rep);
                  Cursor := Cursor + 1;
                  Success := True;
                  return Res;
               end if;
            end if;
      end case;
      Success := False;
      return 0;
   end Next_Data;

   ------------
   -- To_Int --
   ------------

   function To_Int (Str : String) return Integer is
      Res    : Integer := 0;
      Cursor : Natural;
   begin
      if Str (Str'First) = '-' then
         Cursor := Str'First + 1;
      else
         Cursor := Str'First;
      end if;

      while Cursor <= Str'Last loop
         Res := Res * 16 + Integer (From_Hex (Str (Cursor)));
         Cursor := Cursor + 1;
      end loop;


      if Str (Str'First) = '-' then
         return -Res;
      else
         return Res;
      end if;
   end To_Int;

end GDB_Remote;

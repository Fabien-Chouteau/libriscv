with GDB_Remote.Agent;

package GDB_Remote.Target is

   subtype Address is Unsigned_64;
   subtype Register is Unsigned_32;

   package Parent renames Agent;

   type Instance (Buffer_Size : Buffer_Lenght_Type := 256)
   is abstract limited new Parent.Instance
   with private;

   subtype Class is Instance'Class;
   type Ptr is access all Class;

   procedure From_Host (This : in out Instance;
                        C    :        Character);

   procedure Detach (This : in out Instance)
   is abstract;

   procedure Set_Thread (This : in out Instance;
                         Id   :        Integer)
   is abstract;

   procedure Read_Memory (This    : in out Instance;
                          Addr    :        Address;
                          Data    :    out Unsigned_8;
                          Success :    out Boolean)
   is abstract;

   procedure Write_Memory (This    : in out Instance;
                           Addr    :        Address;
                           Data    :        Unsigned_8;
                           Success :    out Boolean)
   is abstract;

   function Last_General_Register (This : Instance) return Natural
   is abstract;

   procedure Read_Register (This    : in out Instance;
                            Id      :        Natural;
                            Data    :    out Register;
                            Success :    out Boolean)
   is abstract;

   procedure Write_Register (This    : in out Instance;
                             Id      :        Natural;
                             Data    :        Register;
                             Success :    out Boolean)
   is abstract;

   procedure Continue (This        : in out Instance;
                       Single_Step :        Boolean := False)
   is abstract;

   procedure Continue_At (This        : in out Instance;
                          Addr        :        Address;
                          Single_Step :        Boolean := False)
   is abstract;

   procedure Halt (This : in out Instance)
   is abstract;

   function Supported (This   : Instance;
                       B_Type : Breakpoint_Type)
                       return Boolean
   is abstract;

   procedure Insert_Breakpoint (This   : in out Instance;
                                B_Type :        Breakpoint_Type;
                                Addr   :        Address;
                                Kind   :        Natural)
   is abstract;

   procedure Remove_Breakpoint (This   : in out Instance;
                                B_Type :        Breakpoint_Type;
                                Addr   :        Address;
                                Kind   :        Natural)
   is abstract;

private

   type Instance (Buffer_Size : Buffer_Lenght_Type := 256)
   is abstract limited new Parent.Instance (Buffer_Size)
   with null record;

   procedure Push_Register is new Agent.Push_Mod (Register);

end GDB_Remote.Target;

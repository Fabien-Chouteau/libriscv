------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   S Y S T E M .  M A C H I N E _ R E S E T               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2011-2019, Free Software Foundation, Inc.       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package body System.Machine_Reset is
   procedure OS_Exit;
   pragma Import (Ada, OS_Exit, "__gnat_exit");
   pragma No_Return (OS_Exit);
   --  Reset the board

   procedure OS_Abort;
   pragma Export (Ada, OS_Abort, "abort");
   pragma No_Return (OS_Abort);
   --  Same as OS_Exit (rename in body to allow multiple pragma Export)

   --------------
   -- OS_Abort --
   --------------

   procedure OS_Abort renames OS_Exit;

   ----------
   -- Stop --
   ----------

   procedure Stop renames OS_Exit;
end System.Machine_Reset;
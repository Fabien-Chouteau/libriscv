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

package body LibRISCV.Sim.Log is

   -------------
   -- Set_Arg --
   -------------

   function Set_Arg (S : String) return Boolean is
      function Handle_Item (Item : String) return Boolean is
         Enable : constant Boolean := Item (Item'First) /= '-';

         Substr : constant String :=
           Item ((if not Enable then
                      Item'First + 1
                 else Item'First) .. Item'Last);
      begin

         if Substr = "console" then
            Console := Enable;
         elsif Substr = "info" then
            Info := Enable;
         elsif Substr = "decode" then
            Decode := Enable;
         elsif Substr = "exec" then
            Exec := Enable;
         elsif Substr = "except" then
            Except := Enable;
         elsif Substr = "mem" then
            Mem_Access := Enable;
         elsif Substr = "csr" then
            CSRs := Enable;
         elsif Substr = "elf" then
            Elf := Enable;
         else
            return False;
         end if;
         return True;
      end Handle_Item;

      From, To : Integer;
   begin
      From := S'First;
      To   := S'First - 1;
      loop

         To := To + 1;

         if To > S'Last or else S (To) = ',' then

            if To > From then
               if not Handle_Item (S (From .. To - 1)) then
                  return False;
               end if;
            end if;
            To   := To + 1;
            From := To;
         end if;

         exit when To > S'Last;
      end loop;
      return True;
   end Set_Arg;

end LibRISCV.Sim.Log;

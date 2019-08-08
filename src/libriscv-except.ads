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

package LibRISCV.Except is

   type Kind is
     (
      Instruction_Address_Misaligned,
      Instruction_Access_Fault,
      Illegal_Instruction,
      Breakpoint,
      Load_Address_Misaligned,
      Load_Access_Fault,
      Store_AMO_Address_Misaligned,
      Store_AMO_Access_Fault,
      Environment_Call_From_U_mode,
      Environment_Call_From_S_mode,
      Environment_Call_From_M_mode,
      Instruction_Page_Fault,
      Load_Page_Fault,
      Store_AMO_Page_Fault,
      User_Soft_Int,
      Supervisor_Soft_Int,
      Hypervisor_Soft_Int,
      Machine_Soft_Int,
      User_Timer_Int,
      Supervisor_Timer_Int,
      Hypervisor_Timer_Int,
      Machine_Timer_Int,
      User_External_Int,
      Supervisor_External_Int,
      Hypervisor_External_Int,
      Machine_External_Int,
      Reserved);

   Interrupt_Flag : constant := 16#80_00_00_00#;

   for Kind use
     (
      Instruction_Address_Misaligned => 16#00_00_00_00#,
      Instruction_Access_Fault       => 16#00_00_00_01#,
      Illegal_Instruction            => 16#00_00_00_02#,
      Breakpoint                     => 16#00_00_00_03#,
      Load_Address_Misaligned        => 16#00_00_00_04#,
      Load_Access_Fault              => 16#00_00_00_05#,
      Store_AMO_Address_Misaligned   => 16#00_00_00_06#,
      Store_AMO_Access_Fault         => 16#00_00_00_07#,
      Environment_Call_From_U_mode   => 16#00_00_00_08#,
      Environment_Call_From_S_mode   => 16#00_00_00_09#,
      Environment_Call_From_M_mode   => 16#00_00_00_0B#,
      Instruction_Page_Fault         => 16#00_00_00_0C#,
      Load_Page_Fault                => 16#00_00_00_0D#,
      Store_AMO_Page_Fault           => 16#00_00_00_0F#,
      User_Soft_Int                  => 16#80_00_00_00#,
      Supervisor_Soft_Int            => 16#80_00_00_01#,
      Hypervisor_Soft_Int            => 16#80_00_00_02#,
      Machine_Soft_Int               => 16#80_00_00_03#,
      User_Timer_Int                 => 16#80_00_00_04#,
      Supervisor_Timer_Int           => 16#80_00_00_05#,
      Hypervisor_Timer_Int           => 16#80_00_00_06#,
      Machine_Timer_Int              => 16#80_00_00_07#,
      User_External_Int              => 16#80_00_00_08#,
      Supervisor_External_Int        => 16#80_00_00_09#,
      Hypervisor_External_Int        => 16#80_00_00_0A#,
      Machine_External_Int           => 16#80_00_00_0B#,
      Reserved                       => 16#FF_FF_FF_FF#);

end LibRISCV.Except;

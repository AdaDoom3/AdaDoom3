------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--              P O S I X . T E R M I N A L _ F U N C T I O N S             --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--             Copyright (C) 1996-1997 Florida State University             --
--                     Copyright (C) 1998-2014, AdaCore                     --
--                                                                          --
--  This file is a component of FLORIST, an  implementation of an  Ada API  --
--  for the POSIX OS services, for use with  the  GNAT  Ada  compiler  and  --
--  the FSU Gnu Ada Runtime Library (GNARL).   The  interface  is intended  --
--  to be close to that specified in  IEEE STD  1003.5: 1990  and IEEE STD  --
--  1003.5b: 1996.                                                          --
--                                                                          --
--  FLORIST is free software;  you can  redistribute  it and/or  modify it  --
--  under terms of the  GNU  General  Public  License as  published by the  --
--  Free Software Foundation;  either version  2, or (at  your option) any  --
--  later version.  FLORIST is distributed  in  the hope  that  it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without  even the implied  warranty  --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE.  See  the GNU  --
--  General Public License for more details.  You  should have  received a  --
--  copy of the GNU General Public License  distributed  with  GNARL;  see  --
--  file  COPYING.  If not,  write to  the  Free  Software  Foundation, 59  --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.                   --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
------------------------------------------------------------------------------

with POSIX.Implementation,
     Unchecked_Conversion;

package body POSIX.Terminal_Functions is

   use POSIX.C,
       POSIX.Implementation;

   -------------------------
   --  Local Subprograms  --
   -------------------------

   procedure Validate (Characteristics : Terminal_Characteristics);
   function To_Ada_Baud (Val : speed_t) return Baud_Rate;

   procedure Validate (Characteristics : Terminal_Characteristics) is
   begin
      Check (Characteristics.Valid, Invalid_Argument);
   end Validate;
   pragma Inline (Validate);

   function To_Ada_Baud (Val : speed_t) return Baud_Rate is
   begin
      if Val = POSIX.C.B0 then
         return B0;
      end if;
      if Val = POSIX.C.B50 then
         return B50;
      end if;
      if Val = POSIX.C.B75 then
         return B75;
      end if;
      if Val = POSIX.C.B110 then
         return B110;
      end if;
      if Val = POSIX.C.B134 then
         return B134;
      end if;
      if Val = POSIX.C.B150 then
         return B150;
      end if;
      if Val = POSIX.C.B200 then
         return B200;
      end if;
      if Val = POSIX.C.B300 then
         return B300;
      end if;
      if Val = POSIX.C.B600 then
         return B600;
      end if;
      if Val = POSIX.C.B1200 then
         return B1200;
      end if;
      if Val = POSIX.C.B1800 then
         return B1800;
      end if;
      if Val = POSIX.C.B2400 then
         return B2400;
      end if;
      if Val = POSIX.C.B4800 then
         return B4800;
      end if;
      if Val = POSIX.C.B9600 then
         return B9600;
      end if;
      if Val = POSIX.C.B19200 then
         return B19200;
      end if;
      if Val = POSIX.C.B38400 then
         return B38400;
      end if;
      if Val = POSIX.C.B57600 then
         return B57600;
      end if;
      if Val = POSIX.C.B115200 then
         return B115200;
      end if;
      if Val = POSIX.C.B230400 then
         return B230400;
      end if;
      if Val = POSIX.C.B460800 then
         return B460800;
      end if;
      Raise_POSIX_Error (Invalid_Argument);
      --  fake return to avoid compiler warning message
      return B38400;
   end To_Ada_Baud;

   ----------------------------------
   -- Get_Terminal_Characteristics --
   ----------------------------------

   function tcgetattr (fd : int; pt : access struct_termios) return int;
   pragma Import (C, tcgetattr, tcgetattr_LINKNAME);

   function Get_Terminal_Characteristics (File : POSIX.IO.File_Descriptor)
      return Terminal_Characteristics is
      Pt : Terminal_Characteristics;
   begin
      Pt.Valid := True;
      Check (tcgetattr (int (File), Pt.termios'Unchecked_Access));
      return Pt;
   end Get_Terminal_Characteristics;

   ----------------------------------
   -- Set_Terminal_Characteristics --
   ----------------------------------

   To_C_Times : constant array (Terminal_Action_Times) of int :=
     (Immediately => TCSANOW,
      After_Output => TCSADRAIN,
      After_Output_And_Input => TCSAFLUSH);

   function tcsetattr (fd : int; action : int; pt : termios_ptr) return int;
   pragma Import (C, tcsetattr, tcsetattr_LINKNAME);

   procedure Set_Terminal_Characteristics
      (File            : POSIX.IO.File_Descriptor;
       Characteristics : Terminal_Characteristics;
       Apply           : Terminal_Action_Times := Immediately;
       Masked_Signals  : POSIX.Signal_Masking := POSIX.RTS_Signals) is
      Old_Mask : aliased Signal_Mask;
      Result : int;
   begin
      Validate (Characteristics);
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := tcsetattr (int (File),
        To_C_Times (Apply), Characteristics.termios'Unchecked_Access);
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
   end Set_Terminal_Characteristics;

   -----------------------
   -- Terminal_Modes_Of --
   -----------------------

   To_C_Terminal_Mode : constant array (Terminal_Modes) of tcflag_t :=
     (
      --  Input_Modes
      Interrupt_On_Break => BRKINT,
      Map_CR_To_LF => ICRNL,
      Ignore_Break => IGNBRK,
      Ignore_CR => IGNCR,
      Ignore_Parity_Errors => IGNPAR,
      Map_LF_To_CR => INLCR,
      Enable_Parity_Check => INPCK,
      Strip_Character => ISTRIP,
      Enable_Start_Stop_Input => IXOFF,
      Enable_Start_Stop_Output => IXON,
      Mark_Parity_Errors => PARMRK,
      --  Output_Modes
      Perform_Output_Processing => OPOST,
      --  Control_Modes
      Ignore_Modem_Status => CLOCAL,
      Enable_Receiver => CREAD,
      Send_Two_Stop_Bits => CSTOPB,
      Hang_Up_On_Last_Close => HUPCL,
      Parity_Enable => PARENB,
      Odd_Parity => PARODD,
      --  Local_Modes
      Echo => POSIX.C.ECHO,
      Echo_Erase => ECHOE,
      Echo_Kill => ECHOK,
      Echo_LF => ECHONL,
      Canonical_Input => ICANON,
      Extended_Functions => IEXTEN,
      Enable_Signals => ISIG,
      No_Flush => NOFLSH,
      Send_Signal_For_BG_Output => TOSTOP);

   i_mask : constant tcflag_t := BRKINT or ICRNL or IGNBRK
     or IGNCR or IGNPAR or INLCR or INPCK or ISTRIP
     or IXOFF or IXON or PARMRK;
   o_mask : constant tcflag_t := OPOST;
   c_mask : constant tcflag_t := CLOCAL or CREAD or CSTOPB
     or HUPCL or PARENB or PARODD;
   l_mask : constant tcflag_t := POSIX.C.ECHO or ECHOE
     or ECHOK or ECHONL or ICANON or IEXTEN or ISIG or
     NOFLSH or TOSTOP;

   function Terminal_Modes_Of (Characteristics : Terminal_Characteristics)
     return Terminal_Modes_Set is
      Modes : Terminal_Modes_Set := (others => False);
   begin
      Validate (Characteristics);
      for I in Input_Modes loop
         if (Characteristics.termios.c_iflag and To_C_Terminal_Mode (I))
            /= 0
         then
            Modes (I) := True;
         end if;
      end loop;
      for I in Output_Modes loop
         if (Characteristics.termios.c_oflag and To_C_Terminal_Mode (I))
            /= 0
         then
            Modes (I) := True;
         end if;
      end loop;
      for I in Control_Modes loop
         if (Characteristics.termios.c_cflag and To_C_Terminal_Mode (I))
            /= 0
         then
            Modes (I) := True;
         end if;
      end loop;
      for I in Local_Modes loop
         if (Characteristics.termios.c_lflag and To_C_Terminal_Mode (I))
            /= 0
         then
            Modes (I) := True;
         end if;
      end loop;
      return Modes;
   end Terminal_Modes_Of;

   ---------------------------
   -- Define_Terminal_Modes --
   ---------------------------

   procedure Define_Terminal_Modes
     (Characteristics : in out Terminal_Characteristics;
      Modes           : Terminal_Modes_Set) is
      Tmp : tcflag_t;
   begin
      Validate (Characteristics);
      Tmp := 0;
      for I in Input_Modes loop
         if Modes (I) then
            Tmp := Tmp or To_C_Terminal_Mode (I);
         end if;
      end loop;
      Characteristics.termios.c_iflag :=
        (Characteristics.termios.c_iflag and not i_mask) or Tmp;
      Tmp := 0;
      for I in Output_Modes loop
         if Modes (I) then
            Tmp := Tmp or To_C_Terminal_Mode (I);
         end if;
      end loop;
      Characteristics.termios.c_oflag :=
        (Characteristics.termios.c_oflag and not o_mask) or Tmp;
      Tmp := 0;
      for I in Control_Modes loop
         if Modes (I) then
            Tmp := Tmp or To_C_Terminal_Mode (I);
         end if;
      end loop;
      Characteristics.termios.c_cflag :=
        (Characteristics.termios.c_cflag and not c_mask) or Tmp;
      Tmp := 0;
      for I in Local_Modes loop
         if Modes (I) then
            Tmp := Tmp or To_C_Terminal_Mode (I);
         end if;
      end loop;
      Characteristics.termios.c_lflag :=
        (Characteristics.termios.c_lflag and not l_mask) or Tmp;
   end Define_Terminal_Modes;

   ---------------------------
   -- Bits_Per_Character_Of --
   ---------------------------

   function Bits_Per_Character_Of (Characteristics : Terminal_Characteristics)
     return Bits_Per_Character is
      csize_bits : constant tcflag_t :=
        Characteristics.termios.c_cflag and CSIZE;
   begin
      Validate (Characteristics);
      if csize_bits = CS5 then
         return 5;
      end if;
      if csize_bits = CS6 then
         return 6;
      end if;
      if csize_bits = CS7 then
         return 7;
      end if;
      if csize_bits = CS8 then
         return 8;
      end if;
      Raise_POSIX_Error (Invalid_Argument);
      --  fake return to avoid compiler warning message
      return 8;
   end Bits_Per_Character_Of;

   -------------------------------
   -- Define_Bits_Per_Character --
   -------------------------------

   To_C_Bits : constant array (Bits_Per_Character) of tcflag_t :=
     (5 => CS5, 6 => CS6, 7 => CS7, 8 => CS8);

   procedure Define_Bits_Per_Character
     (Characteristics : in out Terminal_Characteristics;
      Bits            : Bits_Per_Character) is
   begin
      Validate (Characteristics);
      Characteristics.termios.c_cflag :=
        (Characteristics.termios.c_cflag and not CSIZE) or To_C_Bits (Bits);
   end Define_Bits_Per_Character;

   ------------------------
   -- Input_Baud_Rate_Of --
   ------------------------

   function cfgetispeed (termios_p : termios_ptr) return speed_t;
   pragma Import (C, cfgetispeed, cfgetispeed_LINKNAME);

   function Input_Baud_Rate_Of (Characteristics : Terminal_Characteristics)
     return Baud_Rate is
   begin
      Validate (Characteristics);
      return To_Ada_Baud
        (cfgetispeed (Characteristics.termios'Unchecked_Access));
   end Input_Baud_Rate_Of;

   ----------------------------
   -- Define_Input_Baud_Rate --
   ----------------------------

   To_C_Baud : constant array (Baud_Rate) of speed_t :=
     (B0 => POSIX.C.B0,
      B50 => POSIX.C.B50,
      B75 => POSIX.C.B75,
      B110 => POSIX.C.B110,
      B134 => POSIX.C.B134,
      B150 => POSIX.C.B150,
      B200 => POSIX.C.B200,
      B300 => POSIX.C.B300,
      B600 => POSIX.C.B600,
      B1200 => POSIX.C.B1200,
      B1800 => POSIX.C.B1800,
      B2400 => POSIX.C.B2400,
      B4800 => POSIX.C.B4800,
      B9600 => POSIX.C.B9600,
      B19200 => POSIX.C.B19200,
      B38400 => POSIX.C.B38400,
      B57600 => POSIX.C.B57600,
      B115200 => POSIX.C.B115200,
      B230400 => POSIX.C.B230400,
      B460800 => POSIX.C.B460800);

   function cfsetispeed
     (termios_p : termios_ptr;
      speed : speed_t)
     return int;
   pragma Import (C, cfsetispeed, cfsetispeed_LINKNAME);

   procedure Define_Input_Baud_Rate
     (Characteristics : in out Terminal_Characteristics;
      Input_Baud_Rate : Baud_Rate) is
   begin
      Validate (Characteristics);
      Check (cfsetispeed (Characteristics.termios'Unchecked_Access,
             To_C_Baud (Input_Baud_Rate)));
   end Define_Input_Baud_Rate;

   -------------------------
   -- Output_Baud_Rate_Of --
   -------------------------

   function cfgetospeed (termios_p : termios_ptr) return speed_t;
   pragma Import (C, cfgetospeed, cfgetospeed_LINKNAME);

   function Output_Baud_Rate_Of (Characteristics : Terminal_Characteristics)
     return Baud_Rate is
   begin
      Validate (Characteristics);
      return To_Ada_Baud
        (cfgetospeed (Characteristics.termios'Unchecked_Access));
   end Output_Baud_Rate_Of;

   -----------------------------
   -- Define_Output_Baud_Rate --
   -----------------------------

   function cfsetospeed
     (termios_p : termios_ptr;
      speed : speed_t)
     return int;
   pragma Import (C, cfsetospeed, cfsetospeed_LINKNAME);

   procedure Define_Output_Baud_Rate
     (Characteristics  : in out Terminal_Characteristics;
      Output_Baud_Rate : Baud_Rate) is
   begin
      Validate (Characteristics);
      Check (cfsetospeed (Characteristics.termios'Unchecked_Access,
             To_C_Baud (Output_Baud_Rate)));
   end Define_Output_Baud_Rate;

   ----------------------------------
   -- Special_Control_Character_Of --
   ----------------------------------

   To_Integer : constant
     array (Control_Character_Selector) of Integer :=
     (EOF_Char => VEOF,
      EOL_Char => VEOL,
      Erase_Char => VERASE,
      Interrupt_Char => VINTR,
      Kill_Char => VKILL,
      Quit_Char => VQUIT,
      Suspend_Char => VSUSP,
      Start_Char => VSTART,
      Stop_Char => VSTOP);

   function Special_Control_Character_Of
     (Characteristics : Terminal_Characteristics;
      Selector        : Control_Character_Selector)
      return POSIX.POSIX_Character is
   begin
      return POSIX.POSIX_Character'Val
        (Characteristics.termios.c_cc (To_Integer (Selector)));
   end Special_Control_Character_Of;

   --------------------------------------
   -- Define_Special_Control_Character --

   --------------------------------------
   procedure Define_Special_Control_Character
      (Characteristics : in out Terminal_Characteristics;
       Selector        : Control_Character_Selector;
       Char            : POSIX.POSIX_Character) is
   begin
      Validate (Characteristics);
      Characteristics.termios.c_cc (To_Integer (Selector)) :=
            cc_t (POSIX.POSIX_Character'Pos (Char));
   end Define_Special_Control_Character;

   -------------------------------
   -- Disable_Control_Character --
   -------------------------------

   procedure Disable_Control_Character
      (Characteristics : in out Terminal_Characteristics;
       Selector        : Control_Character_Selector) is
   begin
      Characteristics.termios.c_cc (To_Integer (Selector)) := 0;
   end Disable_Control_Character;

   -------------------
   -- Input_Time_Of --
   -------------------

   function Input_Time_Of (Characteristics : Terminal_Characteristics)
      return Duration is
   begin
      Validate (Characteristics);
      return Duration (Characteristics.termios.c_cc (VTIME)) / 10.0;
   end Input_Time_Of;

   -----------------------
   -- Define_Input_Time --
   -----------------------

   procedure Define_Input_Time
      (Characteristics : in out Terminal_Characteristics;
       Input_Time      : Duration) is
   begin
      Validate (Characteristics);
      if Input_Time < 0.0
        or else Input_Time > Duration (cc_t'Last) / 10.0
      then
         Raise_POSIX_Error (Invalid_Argument);
      end if;

      Characteristics.termios.c_cc (VTIME) := cc_t (Input_Time * 10);
   end Define_Input_Time;

   ----------------------------
   -- Minimum_Input_Count_Of --
   ----------------------------

   function Minimum_Input_Count_Of (Characteristics : Terminal_Characteristics)
     return Natural is
   begin
      Validate (Characteristics);
      return Natural (Characteristics.termios.c_cc (VMIN));
   end Minimum_Input_Count_Of;

   --------------------------------
   -- Define_Minimum_Input_Count --
   --------------------------------

   procedure Define_Minimum_Input_Count
      (Characteristics     : in out Terminal_Characteristics;
       Minimum_Input_Count : Natural) is
   begin
      Validate (Characteristics);
      Check
        (Minimum_Input_Count <= Natural (cc_t'Last), Invalid_Argument);
      Characteristics.termios.c_cc (VMIN) := cc_t (Minimum_Input_Count);
   end Define_Minimum_Input_Count;

   ----------------
   -- Send_Break --
   ----------------

   function tcsendbreak (fd : int; dur : int) return int;
   pragma Import (C, tcsendbreak, tcsendbreak_LINKNAME);

   procedure Send_Break
      (File         : POSIX.IO.File_Descriptor;
       The_Duration : Duration  := 0.0) is
      Num : Float;
   begin
      Num := Float (The_Duration);
      Check (tcsendbreak (int (File), int (Num / 0.25)));
   end Send_Break;

   -----------
   -- Drain --
   -----------

   function tcdrain (fd : int) return int;
   pragma Import (C, tcdrain, tcdrain_LINKNAME);

   procedure Drain
      (File           : POSIX.IO.File_Descriptor;
       Masked_Signals : POSIX.Signal_Masking
           := POSIX.RTS_Signals) is
      Old_Mask : aliased Signal_Mask;
      Result : int;
   begin
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := tcdrain (int (File));
      Restore_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Check (Result);
   end Drain;

   ------------------
   -- Discard_Data --
   ------------------

   To_C_Queue : constant array (Queue_Selector) of int :=
     (Received_But_Not_Read => TCIFLUSH,
      Written_But_Not_Transmitted => TCOFLUSH,
      Both => TCIOFLUSH);

   function tcflush (fd : int; action : int) return int;
   pragma Import (C, tcflush, tcflush_LINKNAME);

   procedure Discard_Data
      (File     : POSIX.IO.File_Descriptor;
       Selector : Queue_Selector) is
   begin
      Check (tcflush (int (File), To_C_Queue (Selector)));
   end Discard_Data;

   ----------
   -- Flow --
   ----------

   To_C_Flow_Action : constant array (Flow_Action) of int :=
     (Suspend_Output => TCOOFF,
      Restart_Output => TCOON,
      Transmit_Stop => TCIOFF,
      Transmit_Start => TCION);

   function tcflow (fd : int; action : int) return int;
   pragma Import (C, tcflow, tcflow_LINKNAME);

   procedure Flow
     (File   : POSIX.IO.File_Descriptor;
      Action : Flow_Action) is
   begin
      Check (tcflow (int (File), To_C_Flow_Action (Action)));
   end Flow;

   --------------------------
   -- Get_Process_Group_ID --
   --------------------------

   function tcgetpgrp (fd : int) return pid_t;
   pragma Import (C, tcgetpgrp, tcgetpgrp_LINKNAME);

   function To_Process_Group_ID is new Unchecked_Conversion
     (pid_t, POSIX.Process_Identification.Process_Group_ID);

   function Get_Process_Group_ID
     (File : POSIX.IO.File_Descriptor)
     return POSIX.Process_Identification.Process_Group_ID is
      Result : pid_t;
   begin
      Result := tcgetpgrp (int (File));
      if Result = -1 then
         Raise_POSIX_Error;
      end if;
      return To_Process_Group_ID (Result);
   end Get_Process_Group_ID;

   --------------------------
   -- Set_Process_Group_ID --
   --------------------------

   function tcsetpgrp (fd : int; pgrp : pid_t) return int;
   pragma Import (C, tcsetpgrp, tcsetpgrp_LINKNAME);

   function To_pid_t is new Unchecked_Conversion
     (POSIX.Process_Identification.Process_Group_ID, pid_t);

   procedure Set_Process_Group_ID
      (File : POSIX.IO.File_Descriptor;
       Group_ID : POSIX.Process_Identification.Process_Group_ID) is
   begin
      Check (tcsetpgrp (int (File), To_pid_t (Group_ID)));
   end Set_Process_Group_ID;

   -----------------------------------
   -- Get_Controlling_Terminal_Name --
   -----------------------------------

   function ctermid (s : char_ptr) return char_ptr;
   pragma Import (C, ctermid, ctermid_LINKNAME);

   function Get_Controlling_Terminal_Name return POSIX.Pathname is
      Result : POSIX_String (1 .. L_ctermid);
   begin
      return Form_POSIX_String (ctermid (Result (1)'Unchecked_Access));
   end Get_Controlling_Terminal_Name;

end POSIX.Terminal_Functions;

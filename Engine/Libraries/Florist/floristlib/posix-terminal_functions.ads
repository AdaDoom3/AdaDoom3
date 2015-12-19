------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--              P O S I X . T E R M I N A L _ F U N C T I O N S             --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                                                                          --
--  This  file is a component  of FLORIST,  an implementation of the POSIX  --
--  Ada  bindings  for  use with the GNAT Ada compiler and the FSU Gnu Ada  --
--  Runtime Library (GNARL).                                                --
--                                                                          --
--  This package specification contains some text extracted from  IEEE STD  --
--  1003.5: 1990, Information Technology -- POSIX Ada Language  Interfaces  --
--  Part 1: Binding  for  System Application Program Interface, as amended  --
--  by IEEE STD 1003.5b: 1996, Amendment 1: Realtime Extensions, copyright  --
--  1996 by the Institute of Electrical and Electronics Engineers, Inc.     --
--                                                                          --
--  The package specifications in the IEEE standards cited above represent  --
--  only a  portion  of  the  documents  and  are  not to be interpreteted  --
--  outside the context  of  the documents.  The standards must be used in  --
--  conjunction  with  the  package   specifications  in  order  to  claim  --
--  conformance.   The IEEE takes no responsibility for and will assume no  --
--  liability for damages resulting from the reader's misinterpretation of  --
--  said  information resulting from its out-of-context nature.   To order  --
--  copies of the IEEE standards,  please contact the  IEEE Service Center  --
--  at 445 Hoes Lane, PO Box 1331, Piscataway, NJ 08855-1331; via phone at  --
--  1-800-678-IEEE, 908-981-1393; or via fax at 908-981-9667.               --
--                                                                          --
--  These  package  specifications are  distributed in  the hope that they  --
--  will  be useful, but  WITHOUT  ANY  WARRANTY; without even the implied  --
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.        --
--                                                                          --
------------------------------------------------------------------------------

with POSIX.C,
     POSIX.IO,
     POSIX.Process_Identification;
package POSIX.Terminal_Functions is

   --  Special Characters used in terminal input

   Null_POSIX_Character : constant POSIX.POSIX_Character
                        := POSIX.POSIX_Character'Val (0);
   Flag_POSIX_Character : constant POSIX.POSIX_Character
                        := POSIX.POSIX_Character'Val (0);

   --  Get and Define Terminal operating characteristics

   type Terminal_Characteristics is private;
   Invalid_Terminal_Characteristics : constant Terminal_Characteristics;
   function Get_Terminal_Characteristics (File : POSIX.IO.File_Descriptor)
      return Terminal_Characteristics;
   type Terminal_Action_Times is
         (Immediately, After_Output, After_Output_And_Input);
   procedure Set_Terminal_Characteristics
     (File            : POSIX.IO.File_Descriptor;
      Characteristics : Terminal_Characteristics;
      Apply           : Terminal_Action_Times := Immediately;
      Masked_Signals  : POSIX.Signal_Masking
                      := POSIX.RTS_Signals);
   type Terminal_Modes is
      --  Subtype Input_Modes:
      (Interrupt_On_Break, Map_CR_To_LF, Ignore_Break,
       Ignore_CR, Ignore_Parity_Errors, Map_LF_To_CR,
       Enable_Parity_Check, Strip_Character, Enable_Start_Stop_Input,
       Enable_Start_Stop_Output, Mark_Parity_Errors,
      --  Subtype Output_Modes :
       Perform_Output_Processing,
      --  Subtype Control_Modes :
       Ignore_Modem_Status, Enable_Receiver, Send_Two_Stop_Bits,
       Hang_Up_On_Last_Close, Parity_Enable, Odd_Parity,
      --  Subtype Local_Modes:
       Echo, Echo_Erase, Echo_Kill, Echo_LF, Canonical_Input,
       Extended_Functions, Enable_Signals, No_Flush,
       Send_Signal_For_BG_Output);
   subtype Input_Modes is Terminal_Modes
       range Interrupt_On_Break .. Mark_Parity_Errors;
   subtype Output_Modes is Terminal_Modes
       range Perform_Output_Processing .. Perform_Output_Processing;
   subtype Control_Modes is Terminal_Modes
      range Ignore_Modem_Status .. Odd_Parity;
   subtype Local_Modes is Terminal_Modes
      range Echo .. Send_Signal_For_BG_Output;
   type Terminal_Modes_Set is array (Terminal_Modes) of Boolean;
   subtype Bits_Per_Character is Positive range 5 .. 8;
   type Baud_Rate is
      (B0, B50, B75, B110, B134, B150, B200, B300, B600,
       B1200, B1800, B2400, B4800, B9600, B19200, B38400,
       B57600, B115200, B230400, B460800);
   function Terminal_Modes_Of (Characteristics : Terminal_Characteristics)
      return Terminal_Modes_Set;
   procedure Define_Terminal_Modes
     (Characteristics : in out Terminal_Characteristics;
      Modes           : Terminal_Modes_Set);
   function Bits_Per_Character_Of
     (Characteristics : Terminal_Characteristics)
     return Bits_Per_Character;
   procedure Define_Bits_Per_Character
     (Characteristics : in out Terminal_Characteristics;
      Bits            : Bits_Per_Character);
   function Input_Baud_Rate_Of
     (Characteristics : Terminal_Characteristics)
     return Baud_Rate;
   procedure Define_Input_Baud_Rate
     (Characteristics : in out Terminal_Characteristics;
      Input_Baud_Rate : Baud_Rate);
   function Output_Baud_Rate_Of
     (Characteristics : Terminal_Characteristics)
     return Baud_Rate;
   procedure Define_Output_Baud_Rate
     (Characteristics  : in out Terminal_Characteristics;
      Output_Baud_Rate : Baud_Rate);
   type Control_Character_Selector is
      (EOF_Char, EOL_Char, Erase_Char, Interrupt_Char,
       Kill_Char, Quit_Char, Suspend_Char, Start_Char, Stop_Char);
   function Special_Control_Character_Of
     (Characteristics : Terminal_Characteristics;
      Selector        : Control_Character_Selector)
     return POSIX.POSIX_Character;
   procedure Define_Special_Control_Character
     (Characteristics : in out Terminal_Characteristics;
      Selector        : Control_Character_Selector;
      Char            : POSIX.POSIX_Character);
   procedure Disable_Control_Character
     (Characteristics : in out Terminal_Characteristics;
      Selector        : Control_Character_Selector);
   function Input_Time_Of (Characteristics : Terminal_Characteristics)
      return Duration;
   procedure Define_Input_Time
     (Characteristics : in out Terminal_Characteristics;
      Input_Time      : Duration);
   function Minimum_Input_Count_Of
     (Characteristics : Terminal_Characteristics)
     return Natural;
   procedure Define_Minimum_Input_Count
     (Characteristics     : in out Terminal_Characteristics;
      Minimum_Input_Count : Natural);

   --  Line Control Operations

   procedure Send_Break
     (File         : POSIX.IO.File_Descriptor;
      The_Duration : Duration := 0.0);
   procedure Drain
     (File           : POSIX.IO.File_Descriptor;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals);
   type Queue_Selector is
      (Received_But_Not_Read, Written_But_Not_Transmitted, Both);
   procedure Discard_Data
     (File     : POSIX.IO.File_Descriptor;
      Selector : Queue_Selector);
   type Flow_Action is
      (Suspend_Output, Restart_Output, Transmit_Stop, Transmit_Start);
   procedure Flow
     (File   : POSIX.IO.File_Descriptor;
      Action : Flow_Action);

   --  Foreground Process Group ID

   function Get_Process_Group_ID
     (File : POSIX.IO.File_Descriptor)
     return POSIX.Process_Identification.Process_Group_ID;
   procedure Set_Process_Group_ID
     (File     : POSIX.IO.File_Descriptor;
      Group_ID : POSIX.Process_Identification.Process_Group_ID);

   --  Get pathname of current controlling terminal for the current process

   function Get_Controlling_Terminal_Name return POSIX.Pathname;

private

   --  .... Change POSIX.5?
   --  This is a terrible choice of interface.
   --  The type Terminal_Characteristics should have been a
   --  limited private type, but is is declared private here!
   --  We are forced to do strange contortions to provide:
   --  (1) implicit initialization of objects of this type to
   --      a recognizable "undefined" value
   --  (2) a constant to stand for this undefined value
   --  (3) no use of "access types" -- i.e. heap allocation
   --  Were it not for the latter, we could easily map the type
   --  Terminal_Characteristics to the C type struct termios *.

   type Terminal_Characteristics is
      record
         Valid : Boolean := False;
         termios : aliased POSIX.C.struct_termios;
      end record;

   Dummy : Terminal_Characteristics;
   --  provides a default initial value, without depending on
   --  internal structure of type POSIX.C.struct_termios

   Invalid_Terminal_Characteristics : constant Terminal_Characteristics
                                    := Dummy;
end POSIX.Terminal_Functions;

------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--              P O S I X . P R O C E S S _ P R I M I T I V E S             --
--                                                                          --
--                                  S p e c                                 --
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

with POSIX,
     POSIX.C,
     POSIX.IO,
     POSIX.Process_Environment,
     POSIX.Process_Identification,
     POSIX.Signals;

package POSIX.Process_Primitives is

   --  Process Template

   type Process_Template is limited private;

   procedure Open_Template (Template : in out Process_Template);

   procedure Close_Template (Template : in out Process_Template);

   procedure Set_Keep_Effective_IDs
     (Template : in out Process_Template);

   procedure Set_Signal_Mask
     (Template : in out Process_Template;
      Mask : POSIX.Signals.Signal_Set);

   procedure Set_Creation_Signal_Masking
     (Template : in out Process_Template;
      Masked_Signals : POSIX.Signal_Masking
                     := POSIX.RTS_Signals);

   procedure Set_File_Action_To_Close
     (Template : in out Process_Template;
      File : POSIX.IO.File_Descriptor);

   procedure Set_File_Action_To_Open
     (Template : in out Process_Template;
      File : POSIX.IO.File_Descriptor;
      Name : POSIX.Pathname;
      Mode : POSIX.IO.File_Mode := POSIX.IO.Read_Only;
      Options : POSIX.IO.Open_Option_Set := POSIX.IO.Empty_Set);

   procedure Set_File_Action_To_Duplicate
     (Template : in out Process_Template;
      File : POSIX.IO.File_Descriptor;
      From_File : POSIX.IO.File_Descriptor);

   --  Process Creation

   procedure Start_Process
     (Child : out POSIX.Process_Identification.Process_ID;
      Pathname : POSIX.Pathname;
      Template : Process_Template;
      Arg_List : POSIX.POSIX_String_List
               := POSIX.Empty_String_List);

   procedure Start_Process
     (Child : out POSIX.Process_Identification.Process_ID;
      Pathname : POSIX.Pathname;
      Template : Process_Template;
      Env_List : POSIX.Process_Environment.Environment;
      Arg_List : POSIX.POSIX_String_List
               := POSIX.Empty_String_List);

   procedure Start_Process_Search
     (Child : out POSIX.Process_Identification.Process_ID;
      Filename : POSIX.Filename;
      Template : Process_Template;
      Arg_List : POSIX.POSIX_String_List
               := POSIX.Empty_String_List);

   procedure Start_Process_Search
     (Child : out POSIX.Process_Identification.Process_ID;
      Filename : POSIX.Filename;
      Template : Process_Template;
      Env_List : POSIX.Process_Environment.Environment;
      Arg_List : POSIX.POSIX_String_List
               := POSIX.Empty_String_List);

   --  Process Exit

   type Exit_Status is range 0 .. 2 ** 8 - 1;

   Normal_Exit              : constant Exit_Status := 0;
   Failed_Creation_Exit     : constant Exit_Status := 41;
   Unhandled_Exception_Exit : constant Exit_Status := 42;

   procedure Exit_Process (Status : Exit_Status := Normal_Exit);

   --  Termination Status

   type Termination_Status is private;
   type Termination_Cause  is
     (Exited, Terminated_By_Signal, Stopped_By_Signal);

   function Status_Available (Status : Termination_Status)
      return Boolean;

   function Process_ID_Of (Status : Termination_Status)
      return POSIX.Process_Identification.Process_ID;

   function Termination_Cause_Of (Status : Termination_Status)
      return Termination_Cause;

   function Exit_Status_Of (Status : Termination_Status)
      return Exit_Status;

   function Termination_Signal_Of (Status : Termination_Status)
      return POSIX.Signals.Signal;

   function Stopping_Signal_Of (Status : Termination_Status)
      return POSIX.Signals.Signal;

   --  Wait for Process Termination

   procedure Wait_For_Child_Process
     (Status : out Termination_Status;
      Child : POSIX.Process_Identification.Process_ID;
      Block : Boolean := True;
      Trace_Stopped : Boolean := True;
      Masked_Signals : POSIX.Signal_Masking
                     := POSIX.RTS_Signals);

   procedure Wait_For_Child_Process
     (Status : out Termination_Status;
      Group : POSIX.Process_Identification.Process_Group_ID;
      Block : Boolean := True;
      Trace_Stopped : Boolean := True;
      Masked_Signals : POSIX.Signal_Masking
                     := POSIX.RTS_Signals);

   procedure Wait_For_Child_Process
     (Status : out Termination_Status;
      Block : Boolean := True;
      Trace_Stopped : Boolean := True;
      Masked_Signals : POSIX.Signal_Masking
                     := POSIX.RTS_Signals);

private

   type Termination_Status is
      record
         pid : POSIX.C.pid_t := POSIX.C.pid_t (Integer'(-1));
         stat_val : aliased POSIX.C.int := 0;
      end record;

   type FD_Action_Type is (Open, Close, Duplicate);

   type FD_Set_Element
     (FD_Action : FD_Action_Type;
      File_Name_Size : Positive);
   type FD_Set_Ptr is access FD_Set_Element;
   type FD_Set_Element
     (FD_Action : FD_Action_Type;
      File_Name_Size : Positive) is
      record
         FD : POSIX.IO.File_Descriptor;
         Next : FD_Set_Ptr;
         Action : FD_Action_Type;
         case FD_Action is
            when Close =>
               null;
            when Open  =>
               File_Name : POSIX.Pathname (1 .. File_Name_Size);
               File_Mode : POSIX.IO.File_Mode;
               File_Options : POSIX.IO.Open_Option_Set;
            when Duplicate =>
               Dup_From : POSIX.IO.File_Descriptor;
         end case;
      end record;

   type Process_Template is
      record
         Is_Closed : Boolean := True;
         Keep_Effective_IDs : Boolean;
         Sig_Set : POSIX.Signals.Signal_Set;
         --  Implicitly initialized to no signal by POSIX_Signals.
         Masked_Sig : POSIX.Signal_Masking := POSIX.RTS_Signals;
         FD_Set : FD_Set_Ptr;
      end record;

end POSIX.Process_Primitives;

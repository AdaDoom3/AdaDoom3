------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                         P O S I X . S I G N A L S                        --
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

with Ada.Task_Identification,
     Ada.Finalization,
     POSIX,
     POSIX.C,
     POSIX.Process_Identification,
     System,
     System.Interrupt_Management,
     System.Storage_Elements;

--  To ensure that this file does not get compiled when thread support is
--  disabled
pragma Warnings (Off);
with POSIX.Implementation.OK_Signals;
pragma Warnings (On);

package POSIX.Signals is

   --  Signal Type

   type Signal is
     new System.Interrupt_Management.Interrupt_ID'Base
     range 0 .. POSIX.C.NSIGS;
   for Signal'Size use POSIX.C.int'Size;

   function Image (Sig : Signal) return String;
   function Value (Str : String) return Signal;

   --  Standard Signals (required)

   Signal_Null,
   SIGNULL                    : constant Signal := 0;
   Signal_Abort,
   SIGABRT                    : constant Signal := POSIX.C.SIGABRT;
   Signal_Alarm,
   SIGALRM                    : constant Signal := POSIX.C.SIGALRM;
   Signal_Bus_Error,
   SIGBUS                     : constant Signal := POSIX.C.SIGBUS;
   Signal_Floating_Point_Error,
   SIGFPE                     : constant Signal := POSIX.C.SIGFPE;
   Signal_Hangup,
   SIGHUP                     : constant Signal := POSIX.C.SIGHUP;
   Signal_Illegal_Instruction,
   SIGILL                     : constant Signal := POSIX.C.SIGILL;
   Signal_Interrupt,
   SIGINT                     : constant Signal := POSIX.C.SIGINT;
   Signal_Kill,
   SIGKILL                    : constant Signal := POSIX.C.SIGKILL;
   Signal_Pipe_Write,
   SIGPIPE                    : constant Signal := POSIX.C.SIGPIPE;
   Signal_Quit,
   SIGQUIT                    : constant Signal := POSIX.C.SIGQUIT;
   Signal_Segmentation_Violation,
   SIGSEGV                    : constant Signal := POSIX.C.SIGSEGV;
   Signal_Terminate,
   SIGTERM                    : constant Signal := POSIX.C.SIGTERM;
   Signal_User_1,
   SIGUSR1                    : constant Signal := POSIX.C.SIGUSR1;
   Signal_User_2,
   SIGUSR2                    : constant Signal := POSIX.C.SIGUSR2;

   --  Standard Signals (job control)

   Signal_Child,
   SIGCHLD                    : constant Signal := POSIX.C.SIGCHLD;
   Signal_Continue,
   SIGCONT                    : constant Signal := POSIX.C.SIGCONT;
   Signal_Stop,
   SIGSTOP                    : constant Signal := POSIX.C.SIGSTOP;
   Signal_Terminal_Stop,
   SIGTSTP                    : constant Signal := POSIX.C.SIGTSTP;
   Signal_Terminal_Input,
   SIGTTIN                    : constant Signal := POSIX.C.SIGTTIN;
   Signal_Terminal_Output,
   SIGTTOU                    : constant Signal := POSIX.C.SIGTTOU;

   --  Signals from P1003.5c

   Signal_IO,
   SIGIO                      : constant Signal := POSIX.C.SIGIO;
   Signal_Out_Of_Band_Data,
   SIGURG                     : constant Signal := POSIX.C.SIGURG;

   subtype Realtime_Signal is Signal range
     POSIX.C.SIGRTMIN .. POSIX.C.SIGRTMAX;

   --  Signal sets

   type Signal_Set is private;

   procedure Add_Signal
     (Set : in out Signal_Set;
      Sig : Signal);
   procedure Add_All_Signals (Set : in out Signal_Set);
   procedure Delete_Signal
     (Set : in out Signal_Set;
      Sig : Signal);
   procedure Delete_All_Signals (Set : in out Signal_Set);
   function Is_Member
     (Set : Signal_Set;
      Sig : Signal)
     return Boolean;

   --  Blocking and Unblocking Signals

   procedure Set_Blocked_Signals
     (New_Mask : Signal_Set;
      Old_Mask : out Signal_Set);
   procedure Block_Signals
     (Mask_to_Add : Signal_Set;
      Old_Mask    : out Signal_Set);
   procedure Unblock_Signals
     (Mask_to_Subtract : Signal_Set;
      Old_Mask         : out Signal_Set);
   function Blocked_Signals return Signal_Set;

   --  Ignoring Signals

   procedure Ignore_Signal (Sig : Signal);
   procedure Unignore_Signal (Sig : Signal);
   function Is_Ignored (Sig : Signal) return Boolean;
   procedure Install_Empty_Handler (Sig : Signal);

   --  Controlling Delivery of Signal_Child Signal

   procedure Set_Stopped_Child_Signal (Enable : Boolean := True);
   function Stopped_Child_Signal_Enabled return Boolean;

   --  Examining Pending Signals

   function Pending_Signals return Signal_Set;
   type Signal_Event is private;
   type Signal_Data  is private;

   type Notification is range Integer'First .. Integer'Last;
   No_Notification     : constant Notification := POSIX.C.SIGEV_NONE;
   Signal_Notification : constant Notification := POSIX.C.SIGEV_SIGNAL;

   function Get_Signal (Event : Signal_Event) return Signal;
   procedure Set_Signal
     (Event : in out Signal_Event;
      Sig   : Signal);
   function Get_Notification (Event : Signal_Event) return Notification;
   procedure Set_Notification
     (Event  : in out Signal_Event;
      Notify : Notification);
   function Get_Data (Event : Signal_Event) return Signal_Data;
   procedure Set_Data
     (Event : in out Signal_Event;
      Data  : Signal_Data);

   type Signal_Source is range Integer'First .. Integer'Last;
   From_Send_Signal   : constant Signal_Source := POSIX.C.SI_USER;
   From_Queue_Signal  : constant Signal_Source := POSIX.C.SI_QUEUE;
   From_Timer         : constant Signal_Source := POSIX.C.SI_TIMER;
   From_Async_IO      : constant Signal_Source := POSIX.C.SI_ASYNCIO;
   From_Message_Queue : constant Signal_Source := POSIX.C.SI_MESGQ;

   type Signal_Info is private;
   function Get_Signal (Info : Signal_Info) return Signal;
   procedure Set_Signal
     (Info : in out Signal_Info;
      Sig  : Signal);
   function Get_Source (Info : Signal_Info) return Signal_Source;
   procedure Set_Source
     (Info   : in out Signal_Info;
      Source : Signal_Source);
   function Has_Data (Source : Signal_Source) return Boolean;
   function Get_Data (Info : Signal_Info) return Signal_Data;
   procedure Set_Data
     (Info : in out Signal_Info;
      Data : Signal_Data);

   procedure Enable_Queueing (Sig : Signal);
   procedure Disable_Queueing (Sig : Signal);

   function Await_Signal (Set : Signal_Set) return Signal;
   function Await_Signal_Or_Timeout
     (Set     : Signal_Set;
      Timeout : POSIX.Timespec)
     return Signal;
   function Await_Signal (Set : Signal_Set) return Signal_Info;
   function Await_Signal_Or_Timeout
     (Set     : Signal_Set;
      Timeout : POSIX.Timespec)
     return Signal_Info;

   Signal_Abort_Ref           : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGABRT));
   Signal_Hangup_Ref          : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGHUP));
   Signal_Interrupt_Ref       : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGINT));
   Signal_Pipe_Write_Ref      : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGPIPE));
   Signal_Quit_Ref            : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGQUIT));
   Signal_Terminate_Ref       : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGTERM));
   Signal_User_1_Ref          : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGUSR1));
   Signal_User_2_Ref          : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGUSR2));
   Signal_Child_Ref           : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGCHLD));
   Signal_Continue_Ref        : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGCONT));
   Signal_Terminal_Stop_Ref   : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGTSTP));
   Signal_Terminal_Input_Ref  : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGTTIN));
   Signal_Terminal_Output_Ref : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGTTOU));

   function Signal_Reference (Sig : Signal) return System.Address;

   procedure Send_Signal
     (Process : POSIX.Process_Identification.Process_ID;
      Sig     : Signal);
   procedure Send_Signal
     (Group : POSIX.Process_Identification.Process_Group_ID;
      Sig   : Signal);
   procedure Send_Signal (Sig : Signal);

   procedure Queue_Signal
     (Process : POSIX.Process_Identification.Process_ID;
      Sig     : Signal;
      Data    : Signal_Data);

   procedure Interrupt_Task
     (T : Ada.Task_Identification.Task_Id);

private

   type Signal_Set is new Ada.Finalization.Controlled with record
      C : aliased POSIX.C.sigset_t;
   end record;

   procedure Initialize (Set : in out Signal_Set);
   procedure Finalize (Set : in out Signal_Set);

   --  We formerly used an explicit array, rather than the C type
   --  sigset_t, because:
   --  1. C provides no operation to enumerate the
   --     members of a sigset_t, other than calling sigismember() for
   --     every value in the range of valid Signals.
   --  2. We would have to use a controlled type to do the initialization,
   --     since making a sigset_t object empty requires calling sigemptyset.
   --     We should have put the array inside a record, to get
   --     default initialization, but did not -- a mistake that needed
   --     correcting in any case.
   --  3. We thought objects of type sigset_t might involve implicitly
   --     allocated dynamic storage, which could lead to storage leakage,
   --     and would not support private-type (assignment) semantics.
   --  Unfortunately, using this different representation meant quite a
   --  bit of extra computation, to translate between the two forms, and
   --  that ends up with iteration over the range of valid Signals anyway.
   --  The current solution does assume that the sigset_t representation
   --  supports meaningful equality testing.

   type Signal_Info is new POSIX.C.siginfo_t;
   type Signal_Event is new POSIX.C.struct_sigevent;
   type Signal_Data is record
      Data : System.Storage_Elements.Storage_Array
        (1 .. POSIX.C.sigval_byte_size);
   end record;
   for Signal_Data'Alignment use POSIX.C.sigval_alignment;

end POSIX.Signals;

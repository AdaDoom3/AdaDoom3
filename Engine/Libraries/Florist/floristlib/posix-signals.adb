------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                         P O S I X . S I G N A L S                        --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--             Copyright (C) 1996-1997 Florida State University             --
--                     Copyright (C) 1998-2013, AdaCore                     --
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

--  Please take care in future maintenance updates to avoid adding
--  direct system calls that modify the signal action or signal
--  masking, and to coordinate changes with the GNAT runtime.

--  The implementation of this package is closely dependent on the
--  GNAT packages System.Interrupts, and
--  System.Interrupt_Management.  See comments in those packages
--  for related explanation of the design for signal handling.

--  Unfortunately, this means maintenance changes to Florist and
--  GNAT need to be synchronized.  A person with an older version of
--  GNAT will have problems using the current version of Florist.

--  The present design is a compromise.  If it were not for the
--  backward compatibility issue, all of the necessary POSIX
--  signal management support would be implemented directly in
--  the package System.Interrupts.  (That was the original design.)
--  We have tried to avoid changing the GNARL runtime system package
--  interfaces, in order that it would be possible to compile Florist
--  using earlier versions of GNAT.  This has meant in some cases
--  putting the implementation of new functionality (e.g., the
--  POSIX.5b Interrupt_Task and the POSIX.5c Install_Empty_Handler)
--  directly into the body of POSIX.Signals.  As a result, the
--  functionality is now divided between the two packages, in a
--  way that may not make much sense to a new reader.

--  With luck, it should be possible to compile this version of Florist
--  with earlier versions of GNAT.  There will a variable degree of
--  effect on the functioning of the signal management interfaces.
--  Since there were significant defects in this part of earlier releases
--  of Florist (detected by the POSIX.5b validation tests), we hope
--  no earlier Florist users are dependent on the way these operations
--  "worked" before.  We had to make the changes.

--  Ideally, there should be no operations in here that directly modify the
--  signal state of the process or thread.  For safety, all such operations
--  should be implemented by calls to operations in System.Interrupts.
--  Otherwise, we could break invariants upon which the Ada tasking
--  runtime system depends.  However, to allow this version of Florist
--  to be used with earlier versions of GNAT, there are some places where
--  direct system calls are done.  People doing maintenance should beware
--  of adding other direct calls without careful analysis of how they
--  might interact with what the GNAT runtime system is doing.

with POSIX.Implementation,
     System.Tasking,
     System.Interrupts,
     System.Task_Primitives.Operations,
     Unchecked_Conversion;

package body POSIX.Signals is

   use POSIX.C,
       POSIX.Implementation,
       System,
       System.Storage_Elements,
       System.Tasking;

   package SI renames System.Interrupts;
   subtype SIID is SI.Interrupt_ID;

   package Bogus_Signal_Enum is

      package PS renames POSIX.Signals;
      type Signal_Name_Enum is
        (Signal_Null,
         SIGNULL,
         Signal_Abort,
         SIGABRT,
         Signal_Alarm,
         SIGALRM,
         Signal_Bus_Error,
         SIGBUS,
         Signal_Floating_Point_Error,
         SIGFPE,
         Signal_Hangup,
         SIGHUP,
         Signal_Illegal_Instruction,
         SIGILL,
         Signal_Interrupt,
         SIGINT,
         Signal_Kill,
         SIGKILL,
         Signal_Pipe_Write,
         SIGPIPE,
         Signal_Quit,
         SIGQUIT,
         Signal_Segmentation_Violation,
         SIGSEGV,
         Signal_Terminate,
         SIGTERM,
         Signal_User_1,
         SIGUSR1,
         Signal_User_2,
         SIGUSR2,
         Signal_Child,
         SIGCHLD,
         Signal_Continue,
         SIGCONT,
         Signal_Stop,
         SIGSTOP,
         Signal_Terminal_Stop,
         SIGTSTP,
         Signal_Terminal_Input,
         SIGTTIN,
         Signal_Terminal_Output,
         SIGTTOU,
         Signal_IO,
         SIGIO,
         Signal_Out_Of_Band_Data,
         SIGURG);

      Enum_To_Signal : array (Signal_Name_Enum'Range) of Signal :=
        (Signal_Null                   => 0,
         SIGNULL                       => 0,
         Signal_Abort                  => PS.SIGABRT,
         SIGABRT                       => PS.SIGABRT,
         Signal_Alarm                  => PS.SIGALRM,
         SIGALRM                       => PS.SIGALRM,
         Signal_Bus_Error              => PS.SIGBUS,
         SIGBUS                        => PS.SIGBUS,
         Signal_Floating_Point_Error   => PS.SIGFPE,
         SIGFPE                        => PS.SIGFPE,
         Signal_Hangup                 => PS.SIGHUP,
         SIGHUP                        => PS.SIGHUP,
         Signal_Illegal_Instruction    => PS.SIGILL,
         SIGILL                        => PS.SIGILL,
         Signal_Interrupt              => PS.SIGINT,
         SIGINT                        => PS.SIGINT,
         Signal_Kill                   => PS.SIGKILL,
         SIGKILL                       => PS.SIGKILL,
         Signal_Pipe_Write             => PS.SIGPIPE,
         SIGPIPE                       => PS.SIGPIPE,
         Signal_Quit                   => PS.SIGQUIT,
         SIGQUIT                       => PS.SIGQUIT,
         Signal_Segmentation_Violation => PS.SIGSEGV,
         SIGSEGV                       => PS.SIGSEGV,
         Signal_Terminate              => PS.SIGTERM,
         SIGTERM                       => PS.SIGTERM,
         Signal_User_1                 => PS.SIGUSR1,
         SIGUSR1                       => PS.SIGUSR1,
         Signal_User_2                 => PS.SIGUSR2,
         SIGUSR2                       => PS.SIGUSR2,
         Signal_Child                  => PS.SIGCHLD,
         SIGCHLD                       => PS.SIGCHLD,
         Signal_Continue               => PS.SIGCONT,
         SIGCONT                       => PS.SIGCONT,
         Signal_Stop                   => PS.SIGSTOP,
         SIGSTOP                       => PS.SIGSTOP,
         Signal_Terminal_Stop          => PS.SIGTSTP,
         SIGTSTP                       => PS.SIGTSTP,
         Signal_Terminal_Input         => PS.SIGTTIN,
         SIGTTIN                       => PS.SIGTTIN,
         Signal_Terminal_Output        => PS.SIGTTOU,
         SIGTTOU                       => PS.SIGTTOU,
         Signal_IO                     => PS.SIGIO,
         SIGIO                         => PS.SIGIO,
         Signal_Out_Of_Band_Data       => PS.SIGURG,
         SIGURG                        => PS.SIGURG);

      Signal_To_Enum : array (Signal'Range) of Signal_Name_Enum :=
        (0 => Signal_Null,
         PS.SIGABRT => Signal_Abort,
         PS.SIGALRM => Signal_Alarm,
         PS.SIGBUS  => Signal_Bus_Error,
         PS.SIGFPE  => Signal_Floating_Point_Error,
         PS.SIGHUP  => Signal_Hangup,
         PS.SIGILL  => Signal_Illegal_Instruction,
         PS.SIGINT  => Signal_Interrupt,
         PS.SIGKILL => Signal_Kill,
         PS.SIGPIPE => Signal_Pipe_Write,
         PS.SIGQUIT => Signal_Quit,
         PS.SIGSEGV => Signal_Segmentation_Violation,
         PS.SIGTERM => Signal_Terminate,
         PS.SIGUSR1 => Signal_User_1,
         PS.SIGUSR2 => Signal_User_2,
         PS.SIGCHLD => Signal_Child,
         PS.SIGCONT => Signal_Continue,
         PS.SIGSTOP => Signal_Stop,
         PS.SIGTSTP => Signal_Terminal_Stop,
         PS.SIGTTIN => Signal_Terminal_Input,
         PS.SIGTTOU => Signal_Terminal_Output,
         PS.SIGIO   => Signal_IO,
         PS.SIGURG  => Signal_Out_Of_Band_Data,
         others  => Signal_Null);
   end Bogus_Signal_Enum;
   use Bogus_Signal_Enum;

   ------------------
   --  Global Data --
   ------------------

   type Signal_Bit_Vector is array (Signal) of Boolean;

   --  Reserved_Signal is the union of the following sets of
   --  signals:

   --  (1) The reserved signals, as defined
   --  by the POSIX.5 standard.  The reserved signals
   --  include the named required reserved signals, plus any other
   --  signals that are reserved by the implementation.

   --  (2)  The signals for which the
   --  implementation does not allow us to set the action.

   --  (3) The signals for which sigwait is not safe.

   --  (4) The set of signals, as defined by
   --  the Ada runtime system, for which it is unsafe to call
   --  System.Interrupt_Management.Ignore_Signal.

   --  (5) The set of signals, as defined by
   --  the Ada runtime system, for which user-defined signal entries
   --  are not supported.

   --  (6) The set of signals, as defined by
   --  the Ada runtime system, for which it is unsafe to call
   --  System.Interrupt_Management.Block_Signals.

   --  This constant is initialized
   --  in the begin-end block of the package body, below, because
   --  it depends on values in POSIX.Implementation.OK_Signals.

   Reserved_Signal : Signal_Bit_Vector;

   --  Signal_Disposition is use by Set_Blocked_Signals, to decide who
   --  should mask or unmask a given signal.

   type Signal_Disposition is
     (No_Change,
      SI_To_Mask,
      SI_To_Unmask);

   ------------------------
   --  Local Subprograms --
   ------------------------

   function To_pid_t is new Unchecked_Conversion
     (POSIX.Process_Identification.Process_ID, pid_t);
   function To_pid_t is new Unchecked_Conversion
     (POSIX.Process_Identification.Process_Group_ID, pid_t);

   function Convert_Ids is new Unchecked_Conversion
     (Ada.Task_Identification.Task_Id, System.Tasking.Task_Id);

   function To_Signal_Data is new Unchecked_Conversion (sigval, Signal_Data);
   function To_sigval is new Unchecked_Conversion (Signal_Data, sigval);

   function sigismember (set : sigset_t_ptr; sig : int) return int;
   pragma Import (C, sigismember, sigismember_LINKNAME);
   function sigaddset (set : access sigset_t; sig : int) return int;
   pragma Import (C, sigaddset, sigaddset_LINKNAME);
   function sigfillset (set : access sigset_t) return int;
   pragma Import (C, sigfillset, sigfillset_LINKNAME);
   function sigemptyset (set : access sigset_t) return int;
   pragma Import (C, sigemptyset, sigemptyset_LINKNAME);
   function sigdelset (set : access sigset_t; sig : int) return int;
   pragma Import (C, sigdelset, sigdelset_LINKNAME);
   function sigpending (set : sigset_t_ptr) return int;
   pragma Import (C, sigpending, sigpending_LINKNAME);
   function sigaction
     (sig  : int;
      act  : sigaction_ptr;
      oact : sigaction_ptr)
     return int;
   pragma Import (C, sigaction, sigaction_LINKNAME);
   function pthread_sigmask
     (how : int;
      set : sigset_t_ptr;
      oset : sigset_t_ptr) return int;
   pragma Import (C, pthread_sigmask, pthread_sigmask_LINKNAME);
   function sigwait
     (set : sigset_t_ptr;
      sig : int_ptr) return int;
   pragma Import (C, sigwait, sigwait_LINKNAME);
   function sigwaitinfo
     (set : sigset_t_ptr; info : siginfo_t_ptr) return int;
   pragma Import (C, sigwaitinfo, sigwaitinfo_LINKNAME);
   function sigtimedwait
     (set     : sigset_t_ptr;
      info    : siginfo_t_ptr;
      timeout : timespec_ptr) return int;
   pragma Import (C, sigtimedwait, sigtimedwait_LINKNAME);

   procedure Check_Awaitable (Set : Signal_Set);
   pragma Inline (Check_Awaitable);

   procedure Null_Handler;
   pragma Convention (C, Null_Handler);

   procedure Void (Ignore : int);
   pragma Inline (Void);

   --  The Await_Signal operations report Invalid_Argument for
   --  the reserved signals and for signals that are attached to
   --  a task entry.  By extension, we treat signals that are
   --  attached to protected procedures as if they were attached
   --  to a task entry.

   procedure Check_Awaitable
     (Set : Signal_Set) is
   begin
      for Sig in Signal loop
         if Reserved_Signal (Sig) then
            if Sig /= SIGKILL and then Sig /= SIGSTOP and then
              sigismember (Set.C'Unchecked_Access, int (Sig)) = 1
            then
               Raise_POSIX_Error (Invalid_Argument);
            end if;
         else
            --  This signal might be attached to a
            --  task entry or protected procedure
            if sigismember (Set.C'Unchecked_Access, int (Sig)) = 1
              and then (SI.Is_Entry_Attached (SIID (Sig))
                or else SI.Is_Handler_Attached (SIID (Sig)))
            then
               Raise_POSIX_Error (Invalid_Argument);
            end if;
         end if;
      end loop;
   end Check_Awaitable;

   procedure Null_Handler is
   begin
      null;
   end Null_Handler;

   procedure Void (Ignore : int) is
      pragma Warnings (Off, Ignore);
   begin
      null;
   end Void;

   ----------------------------------------
   -- Signal_Set Initialize and Finalize --
   ----------------------------------------

   procedure Initialize (Set : in out Signal_Set) is
   begin
      Void (sigemptyset (Set.C'Unchecked_Access));
   end Initialize;

   procedure Finalize (Set : in out Signal_Set) is
   begin
      Void (sigemptyset (Set.C'Unchecked_Access));
   end Finalize;

   -----------
   -- Image --
   -----------

   function Image (Sig : Signal) return String is
      Tmp : constant Signal_Name_Enum := Signal_To_Enum (Sig);
   begin
      if Tmp = Bogus_Signal_Enum.Signal_Null and then Sig /= 0 then
         declare
            Img : constant String := Signal'Image (Sig);
         begin
            return "SIGNAL_" & Img (Img'First + 1 .. Img'Last);
         end;
      else
         return Signal_Name_Enum'Image (Tmp);
      end if;
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Str : String) return Signal is
      A : constant Positive := Str'First;
   begin
      if Str'Length > 7 and then Str (A .. A + 6) = "SIGNAL_"
        and then Str (A + 7) in '0' .. '9'
      then
         return Signal'Value (Str (A + 7 .. Str'Last));
      else
         return Enum_To_Signal (Signal_Name_Enum'Value (Str));
      end if;
   end Value;

   ----------------
   -- Add_Signal --
   ----------------

   procedure Add_Signal (Set : in out Signal_Set; Sig : Signal) is
   begin
      if Sig /= Signal_Null then
         Void (sigaddset (Set.C'Unchecked_Access, int (Sig)));
      end if;
      --  Signal_Null (i.e., zero) is implicitly a member of every set.
   end Add_Signal;

   --------------------
   -- Add_All_Signal --
   --------------------

   procedure Add_All_Signals (Set : in out Signal_Set) is
   begin
      Void (sigfillset (Set.C'Unchecked_Access));
   end Add_All_Signals;

   -------------------
   -- Delete_Signal --
   -------------------

   procedure Delete_Signal (Set : in out Signal_Set; Sig : Signal) is
   begin
      if Sig /= Signal_Null then
         Void (sigdelset (Set.C'Unchecked_Access, int (Sig)));
      end if;
   end Delete_Signal;

   ------------------------
   -- Delete_All_Signals --
   ------------------------

   procedure Delete_All_Signals (Set : in out Signal_Set) is
   begin
      if sigemptyset (Set.C'Unchecked_Access) = 0 then
         null;
      end if;
   end Delete_All_Signals;

   ---------------
   -- Is_Member --
   ---------------

   function Is_Member
     (Set : Signal_Set; Sig : Signal) return Boolean is
   begin
      if Sig = Signal_Null
        or else sigismember (Set.C'Unchecked_Access, int (Sig)) = 1
      then
         return True;
      end if;
      return False;
   end Is_Member;

   -----------------------------------
   --  Set_Blocked_Signals   --
   -----------------------------------

   --  The operations that block/unblock signals do not raise an
   --  exception for any reserved or uncatchable signals, but
   --  quietly have no effect on the masking of SIGKILL, SIGSTOP,
   --  and the reserved signals.

   procedure Set_Blocked_Signals
     (New_Mask : Signal_Set;
      Old_Mask : out Signal_Set) is
      os_new_mask : aliased sigset_t;
      Prev_Mask : Signal_Set;
      Disposition : array (Signal) of Signal_Disposition :=
        (others => No_Change);
   begin
      Begin_Critical_Section;
      Prev_Mask := Blocked_Signals;
      Void (pthread_sigmask
        (SIG_SETMASK, null, os_new_mask'Unchecked_Access));
      --  Partition the signals between those that
      --  are managed by System.Interrupts and those that we manage
      --  directly here.
      for Sig in Signal loop
         if not Reserved_Signal (Sig) then
            --  It is OK to modify this signal's masking, using the
            --  interfaces of System.Interrupts.
            if sigismember
              (New_Mask.C'Unchecked_Access, int (Sig)) = 1
            then
               if not SI.Is_Blocked (SIID (Sig)) then
                  Disposition (Sig) := SI_To_Mask;
               end if;
            else
               if SI.Is_Blocked (SIID (Sig)) then
                  Disposition (Sig) := SI_To_Unmask;
               end if;
            end if;
         end if;
      end loop;
      --  Update the record of which task has which signal unblocked.
      for Sig in Signal loop
         case Disposition (Sig) is
         when No_Change => null;
         when SI_To_Mask =>
            SI.Block_Interrupt (SIID (Sig));
            --  ???? Rely that no exception can be raised, due to previous
            --  checks?  Otherwise, we need to provide a handler to end the
            --  critical section.
         when SI_To_Unmask =>
            SI.Unblock_Interrupt (SIID (Sig));
            --  ???? Rely that no exception can be raised, due to previous
            --  checks?  Otherwise, we need to provide a handler to end the
            --  critical section.
         end case;
      end loop;
      End_Critical_Section;
      Old_Mask := Prev_Mask;
   end Set_Blocked_Signals;

   ---------------------
   --  Block_Signals  --
   ---------------------

   procedure Block_Signals
     (Mask_to_Add : Signal_Set;
      Old_Mask    : out Signal_Set) is
      os_new_mask : aliased sigset_t;
      Prev_Mask : Signal_Set;
      Disposition : array (Signal) of Signal_Disposition :=
        (others => No_Change);
   begin
      Begin_Critical_Section;
      Prev_Mask := Blocked_Signals;
      Void (sigemptyset (os_new_mask'Unchecked_Access));
      for Sig in Signal loop
         if not Reserved_Signal (Sig) then
            --  It is OK to modify this signal's masking, using the
            --  interfaces of System.Interrupts.
            if sigismember
              (Mask_to_Add.C'Unchecked_Access, int (Sig)) = 1
            then
               if not SI.Is_Blocked (SIID (Sig)) then
                  Disposition (Sig) := SI_To_Mask;
               end if;
            else
               null;
            end if;
         end if;
      end loop;
      --  Update the record of which task has which signal unblocked.
      for Sig in Signal loop
         case Disposition (Sig) is
         when No_Change => null;
         when SI_To_Mask =>
            SI.Block_Interrupt (SIID (Sig));
            --  ???? Rely that no exception can be raised, due to previous
            --  checks?  Otherwise, we need to provide a handler to end the
            --  critical section.
         when SI_To_Unmask =>
            --  Should never get here!
            raise Program_Error;
         end case;
      end loop;
      End_Critical_Section;
      Old_Mask := Prev_Mask;
   end Block_Signals;

   -----------------------
   --  Unblock_Signals  --
   -----------------------

   procedure Unblock_Signals
     (Mask_to_Subtract : Signal_Set;
      Old_Mask         : out Signal_Set) is
      os_new_mask : aliased sigset_t;
      Prev_Mask : Signal_Set;
      Disposition : array (Signal) of Signal_Disposition :=
        (others => No_Change);
   begin
      Begin_Critical_Section;
      Prev_Mask := Blocked_Signals;
      Void (sigemptyset (os_new_mask'Unchecked_Access));
      --  Partition the signals between those that
      --  are managed by System.Interrupts and those that we manage
      --  directly here.
      for Sig in Signal loop
         if not Reserved_Signal (Sig) then
            --  It is OK to modify this signal's masking, using the
            --  interfaces of System.Interrupts.
            if sigismember
              (Mask_to_Subtract.C'Unchecked_Access, int (Sig)) = 1
            then
               if SI.Is_Blocked (SIID (Sig)) then
                  Disposition (Sig) := SI_To_Unmask;
               end if;
            end if;
         end if;
      end loop;
      --  Update the record of which task has which signal unblocked.
      for Sig in Signal loop
         case Disposition (Sig) is
         when No_Change => null;
         when SI_To_Mask =>
            raise Program_Error;
            --   Should never get here!
         when SI_To_Unmask =>
            SI.Unblock_Interrupt (SIID (Sig));
            --  ???? Rely that no exception can be raised, due to previous
            --  checks?  Otherwise, we need to provide a handler to end the
            --  critical section.
         end case;
      end loop;
      End_Critical_Section;
      Old_Mask := Prev_Mask;
   end Unblock_Signals;

   -----------------------
   --  Blocked_Signals  --
   -----------------------

   function Blocked_Signals return Signal_Set is
      Old_Mask : Signal_Set;
   begin
      --  Get thread-level signal mask, directly from OS, since
      --  for a badly matched GNARL and operating system, there
      --  may be more values in POSIX.Signal
      --  than System.Interrupts.Interrupt_ID
      if pthread_sigmask
        (SIG_BLOCK, null, Old_Mask.C'Unchecked_Access) = 0
      then
         null;
      end if;
      --  Delete any ublocked signals from System.Interrupts.
      for Sig in Signal loop
         if not Reserved_Signal (Sig) then
            if SI.Is_Blocked (SIID (Sig)) then
               null;
               --  Void (sigaddset (Old_Mask.C'Unchecked_Access, int (Sig)));
               --  Rely that we cannot have a signal that is unmasked
               --  in the current thread and is also logically
               --  blocked by the signal manager.
            else
               Void (sigdelset (Old_Mask.C'Unchecked_Access, int (Sig)));
            end if;
         end if;
      end loop;

      return Old_Mask;
   end Blocked_Signals;

   -------------------
   -- Ignore_Signal --
   -------------------

   --  The signal ignoring/unignoring operations report
   --  Invalid_Operation for SIGKILL, SIGSTOP, the reserved signals,
   --  Signal_Null, or any other signals for which the signal action
   --  is not permitted to be set by an application.

   procedure Ignore_Signal (Sig : Signal) is
   begin
      if Reserved_Signal (Sig) then
         Raise_POSIX_Error (Invalid_Argument);
      else
         SI.Ignore_Interrupt (SIID (Sig));
      end if;
   end Ignore_Signal;

   ---------------------
   -- Unignore_Signal --
   ---------------------

   procedure Unignore_Signal (Sig : Signal) is
   begin
      if Reserved_Signal (Sig) then
         Raise_POSIX_Error (Invalid_Argument);
      else
         SI.Unignore_Interrupt (SIID (Sig));
      end if;
   end Unignore_Signal;

   ----------------
   -- Is_Ignored --
   ----------------

   function Is_Ignored (Sig : Signal) return Boolean is
      act : aliased struct_sigaction;
   begin
      if Reserved_Signal (Sig) then
         Raise_POSIX_Error (Invalid_Argument);
         return False;
      else
         Check (sigaction (int (Sig), null, act'Unchecked_Access));
         return act.sa_handler = To_Address (SIG_IGN);
      end if;
   end Is_Ignored;

   ---------------------------
   -- Install_Empty_Handler --
   ---------------------------

   --  This is a POSIX.5c addition.

   --  .... This functionality needs to be merged into the
   --  Ada runtime system (s-interr.adb) so as to ensure mutual
   --  exclusion between these changes to signal handler state
   --  and changes that are done there.
   --  The best solution may be to export operations for
   --  locking/unlocking, rather than to add new entries to the
   --  signal manager task.

   procedure Install_Empty_Handler (Sig : Signal) is
      act, oact : aliased struct_sigaction;
      Result : int;
   begin
      if Reserved_Signal (Sig) then
         Raise_POSIX_Error (Invalid_Argument);
      end if;
      Begin_Critical_Section;
      act.sa_flags := 0;
      act.sa_handler := Null_Handler'Address;
      Check (sigemptyset (act.sa_mask'Unrestricted_Access));
      Result := sigaction (int (Sig),
        act'Unchecked_Access, oact'Unchecked_Access);
      End_Critical_Section;
      Check (Result);
   end Install_Empty_Handler;

   ------------------------------
   -- Set_Stopped_Child_Signal --
   ------------------------------

   --  .... This functionality needs to be merged into the
   --  Ada runtime system (s-interr.adb) so as to ensure mutual
   --  exclusion between these changes to signal handler state
   --  and changes that are done there.
   --  The best solution may be to export operations for
   --  locking/unlocking, rather than to add new entries to the
   --  signal manager task.

   procedure Set_Stopped_Child_Signal (Enable : Boolean := True) is
      Action, Oact : aliased struct_sigaction;
      Result : int;
   begin
      Begin_Critical_Section;
      --  ...  Need to coordinate with System.Interrupts
      --  to enforce mutual exclusion on signal state changes
      Result := sigaction (POSIX.C.SIGCHLD, null, Oact'Unchecked_Access);
      if Result /= -1 then
         Action := Oact;
         --  .... need to check that this feature is really supported
         --  and raise POSIX_Error, if it is not, else we will have some
         --  strange effects from the default values of these constants!!
         --  In general, should look at various systems to see which features
         --  are not supported, and make sure we are fail-safe if those
         --  features are missing.
         if Enable then
            Action.sa_flags :=
              int (Bits (Action.sa_flags) and not SA_NOCLDSTOP);
         else
            Action.sa_flags :=
              int (Bits (Action.sa_flags) or SA_NOCLDSTOP);
         end if;
         Result := sigaction
            (POSIX.C.SIGCHLD, Action'Unchecked_Access, Oact'Unchecked_Access);
      end if;
      End_Critical_Section;
      Check (Result);
   end Set_Stopped_Child_Signal;

   ----------------------------------
   -- Stopped_Child_Signal_Enabled --
   ----------------------------------

   function Stopped_Child_Signal_Enabled return Boolean is
      Action : aliased struct_sigaction;
      Result : int;
   begin
      Begin_Critical_Section;
      Result := sigaction (POSIX.C.SIGCHLD, null, Action'Unchecked_Access);
      End_Critical_Section;
      Check (Result);
      return ((Bits (Action.sa_flags) and SA_NOCLDSTOP) = 0);
   end Stopped_Child_Signal_Enabled;

   ---------------------
   -- Pending_Signals --
   ---------------------

   function Pending_Signals return Signal_Set is
      Set : Signal_Set;
      Result : int;
   begin
      Begin_Critical_Section;
      Result := sigpending (Set.C'Unchecked_Access);
      End_Critical_Section;
      Check (Result);
      return Set;
   end Pending_Signals;

   ------------------
   --  Get_Signal  --
   ------------------

   function Get_Signal (Event : Signal_Event) return Signal is
   begin
      return Signal (Event.sigev_signo);
   end Get_Signal;

   ------------------
   --  Set_Signal  --
   ------------------

   procedure Set_Signal
     (Event : in out Signal_Event;
      Sig   : Signal) is
   begin
      Event.sigev_signo := int (Sig);
   end Set_Signal;

   ------------------------
   --  Get_Notification  --
   ------------------------

   function Get_Notification (Event : Signal_Event) return Notification is
   begin
      return Notification (Event.sigev_notify);
   end Get_Notification;

   ------------------------
   --  Set_Notification  --
   ------------------------

   procedure Set_Notification
     (Event  : in out Signal_Event;
      Notify : Notification) is
   begin
      Event.sigev_notify := int (Notify);
   end Set_Notification;

   ----------------
   --  Get_Data  --
   ----------------

   function Get_Data (Event : Signal_Event) return Signal_Data is
   begin
      return To_Signal_Data (Event.sigev_value);
   end Get_Data;

   ----------------
   --  Set_Data  --
   ----------------

   procedure Set_Data
     (Event : in out Signal_Event;
      Data  : Signal_Data) is
   begin
      Event.sigev_value := To_sigval (Data);
   end Set_Data;

   ------------------
   --  Get_Signal  --
   ------------------

   function Get_Signal (Info : Signal_Info) return Signal is
   begin
      return Signal (Info.si_signo);
   end Get_Signal;

   ------------------
   --  Set_Signal  --
   ------------------

   procedure Set_Signal
     (Info : in out Signal_Info;
      Sig  : Signal) is
   begin
      Info.si_signo := int (Sig);
   end Set_Signal;

   ------------------
   --  Get_Source  --
   ------------------

   function Get_Source (Info : Signal_Info) return Signal_Source is
   begin
      return Signal_Source (Info.si_code);
   end Get_Source;

   ------------------
   --  Set_Source  --
   ------------------

   procedure Set_Source
     (Info   : in out Signal_Info;
      Source : Signal_Source) is
   begin
      Info.si_code := int (Source);
   end Set_Source;

   ----------------
   --  Has_Data  --
   ----------------

   function Has_Data (Source : Signal_Source) return Boolean is
   begin
      return (Source = From_Queue_Signal) or (Source = From_Async_IO)
         or (Source = From_Message_Queue) or (Source = From_Timer);
   end Has_Data;

   ----------------
   --  Get_Data  --
   ----------------

   function Get_Data (Info : Signal_Info) return Signal_Data is
   begin
      return To_Signal_Data (Info.si_value);
   end Get_Data;

   ----------------
   --  Set_Data  --
   ----------------

   procedure Set_Data
     (Info : in out Signal_Info;
      Data : Signal_Data) is
   begin
      Info.si_value := To_sigval (Data);
   end Set_Data;

   -----------------------
   --  Enable_Queueing  --
   -----------------------

   --  .... POSIX.5 needs fixing here, to reflect the fact that
   --  Enabling/Disabling queueing on a signal might not have
   --  any effect unless there is a handler (even null) installed,
   --  or to require that this operation install a null handler,
   --  as a side-effect.

   --  .... This functionality needs to be merged into the
   --  Ada runtime system (s-interr.adb) so as to ensure mutual
   --  exclusion between these changes to signal handler state
   --  and changes that are done there.
   --  The best solution may be to export operations for
   --  locking/unlocking, rather than to add new entries to the
   --  signal manager task.

   procedure Enable_Queueing
     (Sig : Signal) is
      Action : aliased struct_sigaction;
      Result : int;
   begin
      if not HAVE_sigqueue then
         Raise_POSIX_Error (Operation_Not_Supported);
      end if;
      Begin_Critical_Section;
      Result := sigaction (int (Sig), null, Action'Unchecked_Access);
      if Result /= -1 then
         Action.sa_flags := int (Bits (Action.sa_flags) or SA_SIGINFO);
         Result := sigaction (int (Sig), Action'Unchecked_Access, null);
      end if;
      End_Critical_Section;
      Check (Result);
   end Enable_Queueing;

   ------------------------
   --  Disable_Queueing  --
   ------------------------

   procedure Disable_Queueing (Sig : Signal) is
      Action : aliased struct_sigaction;
      Result : int;
   begin
      if not HAVE_sigqueue then
         Raise_POSIX_Error (Operation_Not_Supported);
      end if;
      Begin_Critical_Section;
      Result := sigaction (int (Sig), null, Action'Unchecked_Access);
      if Result /= -1 then
         Action.sa_flags := int (Bits (Action.sa_flags) and not SA_SIGINFO);
         Result := sigaction (int (Sig), Action'Unchecked_Access, null);
      end if;
      End_Critical_Section;
   end Disable_Queueing;

   --------------------
   --  Await_Signal  --
   --------------------

   function Await_Signal (Set : Signal_Set) return Signal is
      Result   : aliased int;
   begin
      Check_Awaitable (Set);
      if sigwait
        (Set.C'Unchecked_Access, Result'Unchecked_Access) = -1
      then
         Raise_POSIX_Error (Fetch_Errno);
      end if;
      return Signal (Result);
   end Await_Signal;

   -------------------------------
   --  Await_Signal_Or_Timeout  --
   -------------------------------

   function Await_Signal_Or_Timeout
     (Set     : Signal_Set;
      Timeout : POSIX.Timespec) return Signal is
   begin
      return Signal (Await_Signal_Or_Timeout (Set, Timeout).si_signo);
   end Await_Signal_Or_Timeout;

   --------------------
   --  Await_Signal  --
   --------------------

   function Await_Signal (Set : Signal_Set) return Signal_Info is
      Info   : aliased siginfo_t;
   begin
      Check_Awaitable (Set);
      Check (sigwaitinfo (Set.C'Unchecked_Access, Info'Unchecked_Access));
      return Signal_Info (Info);
   end Await_Signal;

   -------------------------------
   --  Await_Signal_Or_Timeout  --
   -------------------------------

   function Await_Signal_Or_Timeout
     (Set : Signal_Set; Timeout : POSIX.Timespec) return Signal_Info
   is
      c_timeout : aliased struct_timespec;
      Info : aliased siginfo_t;
      S  : Seconds;
      NS : Nanoseconds;
   begin
      Check_Awaitable (Set);
      Split (Timeout, S, NS);
      c_timeout.tv_sec := time_t (S);
      c_timeout.tv_nsec := long (NS);
      Check (sigtimedwait
        (Set.C'Unchecked_Access,
         Info'Unchecked_Access,
         c_timeout'Unchecked_Access));
      return Signal_Info (Info);
   end Await_Signal_Or_Timeout;

   ------------------------
   --  Signal_Reference  --
   ------------------------

   function Signal_Reference (Sig : Signal) return System.Address is
   begin
      --  Signal_Reference reports Invalid_Argument if signal entries
      --  are not supported for the specified signal.
      if Reserved_Signal (Sig) then
         Raise_POSIX_Error (Invalid_Argument);
      end if;
      return To_Address (Integer_Address (Sig));
   end Signal_Reference;

   -----------------
   -- Send_Signal --
   -----------------

   function kill (pid : pid_t; sig : C.int) return int;
   pragma Import (C, kill, kill_LINKNAME);

   procedure Send_Signal
     (Process : POSIX.Process_Identification.Process_ID;
      Sig     : Signal) is
   begin
      Check (kill (To_pid_t (Process), int (Sig)));
   end Send_Signal;

   -----------------
   -- Send_Signal --
   -----------------

   procedure Send_Signal
     (Group : POSIX.Process_Identification.Process_Group_ID;
      Sig   : Signal) is
   begin
      Check (kill (-To_pid_t (Group), int (Sig)));
   end Send_Signal;

   -----------------
   -- Send_Signal --
   -----------------

   procedure Send_Signal (Sig : Signal) is
   begin
      Check (kill (0, int (Sig)));
   end Send_Signal;

   --------------------
   --  Queue_Signal  --
   --------------------

   function sigqueue
     (pid   : pid_t;
      signo : int;
      value : sigval) return int;
   pragma Import (C, sigqueue, sigqueue_LINKNAME);

   procedure Queue_Signal
     (Process : POSIX.Process_Identification.Process_ID;
      Sig     : Signal;
      Data    : Signal_Data) is
   begin
      Check (sigqueue (To_pid_t (Process), int (Sig), To_sigval (Data)));
   end Queue_Signal;

   ----------------------
   --  Interrupt_Task  --
   ----------------------

   procedure Interrupt_Task (T : Ada.Task_Identification.Task_Id) is
   begin
      System.Task_Primitives.Operations.Abort_Task (Convert_Ids (T));
   end Interrupt_Task;

begin
   Reserved_Signal := (others => False);

   for Sig in Signal loop
      case Sig is
      when SIGALRM | SIGBUS | SIGILL | SIGSEGV | SIGFPE | SIGABRT =>
         Reserved_Signal (Sig) := True;
      when SIGKILL | SIGSTOP =>
         Reserved_Signal (Sig) := True;
      when others =>
         Reserved_Signal (Sig) :=
           not POSIX.Implementation.OK_Signals.OK (Integer (Sig));
      end case;
   end loop;

   --  Merge in signals that are reserved by the Ada runtime system.
   for Sig in Signal loop
      pragma Warnings (Off);
      --  Kill warning about condition being always true generated
      --  on some platforms, since this code is meant to be compiled
      --  on several platforms.

      if Integer (Sig) <= Integer (SIID'Last) then
         if SI.Is_Reserved (SIID (Sig)) and then (Sig /= SIGKILL
           and Sig /= SIGSTOP)
         then
            Reserved_Signal (Sig) := True;
         end if;
      else
         Reserved_Signal (Sig) := True;
      end if;

      pragma Warnings (On);
   end loop;
end POSIX.Signals;

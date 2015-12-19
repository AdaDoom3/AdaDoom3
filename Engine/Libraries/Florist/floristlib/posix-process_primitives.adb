------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--              P O S I X . P R O C E S S _ P R I M I T I V E S             --
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
     POSIX.Unsafe_Process_Primitives,
     Unchecked_Conversion,
     Unchecked_Deallocation;

package body POSIX.Process_Primitives is

   use POSIX.C,
       POSIX.Implementation,
       POSIX.Process_Identification,
       POSIX.Process_Environment;

   C_File_Mode : constant array (POSIX.IO.File_Mode) of Bits :=
     (POSIX.IO.Read_Only  => O_RDONLY,
      POSIX.IO.Write_Only => O_WRONLY,
      POSIX.IO.Read_Write => O_RDWR);

   -----------------------------
   --  Unchecked Conversions  --
   -----------------------------

   function To_int is new Unchecked_Conversion (Bits, int);

   function To_String_List_Ptr is
     new Unchecked_Conversion (POSIX_String_List, String_List_Ptr);
   function To_String_List_Ptr is
     new Unchecked_Conversion
     (POSIX.Process_Environment.Environment, String_List_Ptr);

   function To_Process_ID is new
      Unchecked_Conversion (pid_t, Process_ID);
   function To_pid_t is new
      Unchecked_Conversion (Process_Group_ID, pid_t);
   function To_pid_t is new
      Unchecked_Conversion (Process_ID, pid_t);

   procedure Free is new
      Unchecked_Deallocation (FD_Set_Element, FD_Set_Ptr);

   -------------------------
   --  Local Subprograms  --
   -------------------------

   function Make_Path_Name
     (Directory : POSIX_String;
      File : POSIX_String) return POSIX_String;
   pragma Inline (Make_Path_Name);
   --  Concatenate a directory name and a file name.

   function Make_Path_Name
     (Directory : POSIX_String;
      File : POSIX_String) return POSIX_String is
   begin
      if Directory = "" then
         return File & NUL;
      end if;
      if Directory (Directory'Last) = '/' then
         return Directory & File & NUL;
      end if;
      return Directory & '/' & File & NUL;
   end Make_Path_Name;

   procedure Delete_Head (Pointer : in out FD_Set_Ptr);
   procedure Delete_Head (Pointer : in out FD_Set_Ptr) is
      Head : FD_Set_Ptr := Pointer;
   begin
      Pointer := Head.Next;
      Free (Head);
   end Delete_Head;

   procedure Execute_Template (Template : Process_Template);

   procedure Void (Ignore : int);
   pragma Inline (Void);
   procedure Void (Ignore : int) is
      pragma Unreferenced (Ignore);
   begin
      null;
   end Void;

   function sigemptyset (set : sigset_t_ptr) return int;
   pragma Import (C, sigemptyset, sigemptyset_LINKNAME);
   function sigaddset (set : sigset_t_ptr; sig : POSIX.Signals.Signal)
     return int;
   pragma Import (C, sigaddset, sigaddset_LINKNAME);
   function pthread_sigmask
     (how : int;
      set : sigset_t_ptr;
      oset : sigset_t_ptr) return int;
   pragma Import (C, pthread_sigmask, pthread_sigmask_LINKNAME);

   procedure Check_Fatal (Result : int);
   --  See comments in Execute_Template, below.
   procedure Check_Fatal (Result : int) is
   begin
      if Result = -1 then
         Exit_Process (Failed_Creation_Exit);
      end if;
   end Check_Fatal;

   function getuid return uid_t;
   pragma Import (C, getuid, getuid_LINKNAME);
   function setuid (uid : uid_t) return int;
   pragma Import (C, setuid, setuid_LINKNAME);

   function getgid return gid_t;
   pragma Import (C, getgid, getgid_LINKNAME);
   function setgid (gid : gid_t) return int;
   pragma Import (C, setgid, setgid_LINKNAME);

   function close (fildes : int) return int;
   pragma Import (C, close, close_LINKNAME);

   function open (path : char_ptr; oflag : int) return int;
   pragma Import (C, open, open_LINKNAME);

   function dup2 (fildes, fildes2 : int) return int;
   pragma Import (C, dup2, dup2_LINKNAME);

   procedure Execute_Template (Template : Process_Template) is
      FD1, FD2 : int;
      Cur : FD_Set_Ptr := Template.FD_Set;
      New_Mask, Old_Mask : aliased sigset_t;

   begin
      if not Template.Keep_Effective_IDs then
         --  See note below why we do not call operations from
         --  POSIX_Process_Identification, since they may raise
         --  exceptions, and we worry about our ability to handle
         --  them.
         Check_Fatal (setuid (getuid));
         Check_Fatal (setgid (getgid));
      end if;

      --  We cannot use signal masking operations from
      --  POSIX.Signals, since they are implemented as
      --  virtual operations, relative to the Ada task's
      --  view of the signal interface.  We normally keep
      --  most signals masked in all tasks except the designated
      --  signal handler threads, so that we can safely use
      --  sigwait.  During this situation, we have just forked
      --  and we hope|expect there are no other threads active
      --  in the new (child) process.  Under these conditions
      --  (only) it should be safe to use the raw signal masking
      --  operations.  In earlier versions, we used the almost-raw
      --  versions, from System.Interrupt_Management.Operations.
      --  These had the advantage that the Ada RTS has already
      --  taken care of mapping to any nonstandard functions,
      --  such as the Solaris 2.x thr_sigmask, versus the
      --  POSIX.1c pthread_sigmask.  However, more recent versions
      --  of Unix operating systems do support the standard,
      --  and in posi-signals.gpb we have already used some of
      --  the raw C interfaces.  In the current version, we have
      --  gone over to completely avoiding calls to the Ada tasking
      --  runtime system.

      --  If an exception is raised during this time, the tasking
      --  runtime system's data structures may "lie" about there
      --  being other tasks active.  This could prevent
      --  orderly shutdown of the process.  Hence, we use
      --  Check_Fatal instead of the usual Check, and generally
      --  try to avoid calling anything that could raise an
      --  exception.

      --  .... ????
      --  The code below may not be robust against exceptions
      --  that occur between fork and exec calls.  There may be
      --  a possibility of deadlock, if the fork occurred while some
      --  other task is holding an RTS-internal lock that we need to
      --  process exceptions.
      --  The present approach is to avoid exceptions, by calling the
      --  "raw" C interfaces below, and to replace the soft-links that are
      --  used to set up exception-handling frames to use the nontasking
      --  versions, since we may not be able to avoid those routines being
      --  called.  The soft links are switched inside the version of Fork
      --  that we import from POSIX.Unsafe_Process_Primitives.

      while Cur /= null loop
         case Cur.Action is
         when Close =>
            Check_Fatal (close (int (Cur.FD)));
         when Open  =>
            FD1 := open (Cur.File_Name (Cur.File_Name'First)'Unchecked_Access,
              To_int (Option_Set (Cur.File_Options).Option
                or C_File_Mode (Cur.File_Mode)));
            if FD1 = -1 then
               Exit_Process (Failed_Creation_Exit);
            end if;
            --  FD2 := dup2 (FD1, int (Cur.FD)); should be enough for the
            --  following if/else statement. However, we have a mulfunction
            --  under Linux when the two arguments are the same. The following
            --  code is a workaround.
            if FD1 /= int (Cur.FD) then
               FD2 := dup2 (FD1, int (Cur.FD));
            else
               FD2 := FD1;
            end if;
            if FD2 = -1 then
               Exit_Process (Failed_Creation_Exit);
            end if;
         when Duplicate =>
            FD2 := dup2 (int (Cur.Dup_From), int (Cur.FD));
            if FD2 = -1 then
               Exit_Process (Failed_Creation_Exit);
            end if;
         end case;
         Cur := Cur.Next;
      end loop;
      Void (sigemptyset (New_Mask'Unchecked_Access));
      for Sig in 1 .. POSIX.Signals.Signal'Last loop
         if POSIX.Signals.Is_Member (Template.Sig_Set, Sig) then
            Void (sigaddset (New_Mask'Unchecked_Access, Sig));
         end if;
      end loop;
      Void (pthread_sigmask (SIG_SETMASK, New_Mask'Unchecked_Access,
        Old_Mask'Unchecked_Access));
      --  ???? is pthread_sigmask OK after a fork?
      --  sigprocmask is not safe in a multithreaded process, but after
      --  the fork() call we are effectively in a single-threaded process,
      --  so it might be better to use sigprocmask?
      --  Void (sigprocmask (SIG_SETMASK, New_Mask'Unchecked_Access, null));
   exception when others =>
      Exit_Process (Failed_Creation_Exit);
   --  Since this may not work, we have tried to avoid raising
   --  any exceptions.  However, in case we have missed something
   --  and an exception is raised, we leave the handler here,
   --  on the off-chance it might work.
   end Execute_Template;

   procedure Validate (Template : Process_Template);

   procedure Validate (Template : Process_Template) is
   begin
      if Template.Is_Closed then
         Raise_POSIX_Error (Invalid_Argument);
      end if;
   end Validate;

   ---------------------
   --  Open_Template  --
   ---------------------

   procedure Open_Template
    (Template : in out Process_Template) is
   begin
      Template.Is_Closed := False;
      Template.Keep_Effective_IDs := False;
      Template.Masked_Sig := No_Signals;
      Template.FD_Set := null;
   end Open_Template;

   ----------------------
   --  Close_Template  --
   ----------------------

   procedure Close_Template
     (Template : in out Process_Template) is
   begin
      Validate (Template);
      while Template.FD_Set /= null loop
         Delete_Head (Template.FD_Set);
      end loop;
      Template.Is_Closed := True;
   end Close_Template;

   ------------------------------
   --  Set_Keep_Effective_IDs  --
   ------------------------------

   procedure Set_Keep_Effective_IDs
     (Template : in out Process_Template) is
   begin
      Validate (Template);
      Template.Keep_Effective_IDs := True;
   end Set_Keep_Effective_IDs;

   -----------------------
   --  Set_Signal_Mask  --
   -----------------------

   procedure Set_Signal_Mask
     (Template : in out Process_Template;
      Mask : POSIX.Signals.Signal_Set) is
   begin
      Validate (Template);
      Template.Sig_Set := Mask;
   end Set_Signal_Mask;

   -----------------------------------
   --  Set_Creation_Signal_Masking  --
   -----------------------------------

   procedure Set_Creation_Signal_Masking
     (Template : in out Process_Template;
      Masked_Signals : POSIX.Signal_Masking
                     := POSIX.RTS_Signals) is
   begin
      Validate (Template);
      Template.Masked_Sig := Masked_Signals;
   end Set_Creation_Signal_Masking;

   --------------------------------
   --  Set_File_Action_To_Close  --
   --------------------------------

   procedure Set_File_Action_To_Close
     (Template : in out Process_Template;
      File : POSIX.IO.File_Descriptor)
   is
      Tmp : FD_Set_Ptr := Template.FD_Set;
   begin
      Validate (Template);

      if Tmp = null then
         Template.FD_Set := new FD_Set_Element (Close, 1);
         Tmp := Template.FD_Set;
      else
         while Tmp.Next /= null loop
            Tmp := Tmp.Next;
         end loop;
         Tmp.Next := new FD_Set_Element (Close, 1);
         Tmp := Tmp.Next;
      end if;

      Tmp.FD := File;
      Tmp.Next := null;
      Tmp.Action := Close;
   end Set_File_Action_To_Close;

   -------------------------------
   --  Set_File_Action_To_Open  --
   -------------------------------

   procedure Set_File_Action_To_Open
     (Template : in out Process_Template;
      File : POSIX.IO.File_Descriptor;
      Name : POSIX.Pathname;
      Mode : POSIX.IO.File_Mode := POSIX.IO.Read_Only;
      Options : POSIX.IO.Open_Option_Set := POSIX.IO.Empty_Set)
   is
      Name_With_NUL : constant POSIX_String := Name & NUL;
      Tmp : FD_Set_Ptr := Template.FD_Set;
   begin
      Validate (Template);

      if Tmp = null then
         Template.FD_Set := new FD_Set_Element (Open, Name_With_NUL'Length);
         Tmp := Template.FD_Set;
      else
         while Tmp.Next /= null loop
            Tmp := Tmp.Next;
         end loop;

         Tmp.Next := new FD_Set_Element (Open, Name_With_NUL'Length);
         Tmp := Tmp.Next;
      end if;

      Tmp.FD := File;
      Tmp.Next := null;
      Tmp.Action := Open;
      Tmp.File_Name := Name_With_NUL;
      Tmp.File_Mode := Mode;
      Tmp.File_Options := Options;
   end Set_File_Action_To_Open;

   ------------------------------------
   --  Set_File_Action_To_Duplicate  --
   ------------------------------------

   procedure Set_File_Action_To_Duplicate
     (Template : in out Process_Template;
      File : POSIX.IO.File_Descriptor;
      From_File : POSIX.IO.File_Descriptor)
   is
      Tmp : FD_Set_Ptr := Template.FD_Set;
   begin
      Validate (Template);

      if Tmp = null then
         Template.FD_Set := new FD_Set_Element (Duplicate, 1);
         Tmp := Template.FD_Set;
      else
         while Tmp.Next /= null loop
            Tmp := Tmp.Next;
         end loop;

         Tmp.Next := new FD_Set_Element (Duplicate, 1);
         Tmp := Tmp.Next;
      end if;

      Tmp.FD := File;
      Tmp.Next := null;
      Tmp.Action := Duplicate;
      Tmp.Dup_From := From_File;
   end Set_File_Action_To_Duplicate;

   ---------------------
   --  Start_Process  --
   ---------------------

   function execv
     (path : char_ptr;
      argv : char_ptr_ptr) return int;
   pragma Import (C, execv, execv_LINKNAME);
   function execve
     (path : char_ptr;
      argv : char_ptr_ptr;
      envp : char_ptr_ptr) return int;
   pragma Import (C, execve, execve_LINKNAME);
   function execvp
     (file : char_ptr;
      argv : char_ptr_ptr) return int;
   pragma Import (C, execvp, execvp_LINKNAME);

   function UFork return POSIX.Process_Identification.Process_ID
     renames POSIX.Unsafe_Process_Primitives.Fork;

   procedure Start_Process
     (Child : out POSIX.Process_Identification.Process_ID;
      Pathname : POSIX.Pathname;
      Template : Process_Template;
      Arg_List : POSIX_String_List := Empty_String_List)
   is
      pid : pid_t;
      Result : int;
      pragma Unreferenced (Result);

      Pathname_With_NUL : POSIX_String := Pathname & NUL;
      Arg : String_List_Ptr := To_String_List_Ptr (Arg_List);
      Default_Arg : POSIX_String_List;
      Old_Mask : aliased Signal_Mask;

   begin
      --  Construct a default argument list with the executable name (argv[0])

      if Arg_List = null or else Length (Arg_List) = 0 then
         Append (Default_Arg, Pathname_With_NUL);
         Arg := To_String_List_Ptr (Default_Arg);
      end if;

      Validate (Template);
      --  .... Consider trying to "quiesce" the tasking system
      --  before doing the fork.  It is probably not feasible.
      Mask_Signals
        (Template.Masked_Sig, Old_Mask'Unchecked_Access);
      pid := To_pid_t (UFork); Check (int (pid));
      if pid = 0 then  --  child process
         Execute_Template (Template);
         Result := execv (Pathname_With_NUL
           (Pathname_With_NUL'First)'Unchecked_Access,
            Arg.Char (1)'Unchecked_Access);
         Exit_Process (Failed_Creation_Exit);
      else
         Child := To_Process_ID (pid);
         Make_Empty (Default_Arg);
         Restore_Signals
           (Template.Masked_Sig, Old_Mask'Unchecked_Access);
      end if;
   end Start_Process;

   procedure Start_Process
     (Child : out POSIX.Process_Identification.Process_ID;
      Pathname : POSIX.Pathname;
      Template : Process_Template;
      Env_List : POSIX.Process_Environment.Environment;
      Arg_List : POSIX_String_List := Empty_String_List)
   is
      pid : pid_t;
      Result : int;
      pragma Unreferenced (Result);

      Pathname_With_NUL : POSIX_String := Pathname & NUL;
      Arg : String_List_Ptr := To_String_List_Ptr (Arg_List);
      Env : String_List_Ptr := To_String_List_Ptr (Env_List);

   begin
      if Arg = null then
         Arg := Null_String_List_Ptr;
      end if;
      if Env = null then
         Env := Null_String_List_Ptr;
      end if;
      Validate (Template);
      pid := To_pid_t (UFork); Check (int (pid));

      if pid = 0 then    --  child process
         Execute_Template (Template);
         Result := execve (Pathname_With_NUL
           (Pathname_With_NUL'First)'Unchecked_Access,
            Arg.Char (1)'Access, Env.Char (1)'Access);
         Exit_Process (Failed_Creation_Exit);
      else
         Child := To_Process_ID (pid);
      end if;
   end Start_Process;

   ----------------------------
   --  Start_Process_Search  --
   ----------------------------

   procedure Start_Process_Search
     (Child : out POSIX.Process_Identification.Process_ID;
      Filename : POSIX.Filename;
      Template : Process_Template;
      Arg_List : POSIX_String_List := Empty_String_List)
   is
      pid : pid_t;
      Result : int;
      pragma Unreferenced (Result);

      Filename_With_NUL : POSIX_String := Filename & NUL;
      Arg : String_List_Ptr := To_String_List_Ptr (Arg_List);

   begin
      if Arg = null then
         Arg := Null_String_List_Ptr;
      end if;
      Validate (Template);
      pid := To_pid_t (UFork); Check (int (pid));
      if pid = 0 then    --  child process
         Execute_Template (Template);
         Result := execvp (Filename_With_NUL
           (Filename_With_NUL'First)'Unchecked_Access, Arg.Char (1)'Access);
         Exit_Process (Failed_Creation_Exit);
      else
         Child := To_Process_ID (pid);
      end if;
   end Start_Process_Search;

   ----------------------------
   --  Start_Process_Search  --
   ----------------------------

   procedure Start_Process_Search
     (Child : out POSIX.Process_Identification.Process_ID;
      Filename : POSIX.Filename;
      Template : Process_Template;
      Env_List : POSIX.Process_Environment.Environment;
      Arg_List : POSIX_String_List := Empty_String_List)
   is
      pid : pid_t;
      Filename_With_NUL : POSIX_String := Filename & NUL;
      Arg : String_List_Ptr := To_String_List_Ptr (Arg_List);
      Env : String_List_Ptr := To_String_List_Ptr (Env_List);

   begin
      if Arg = null then
         Arg := Null_String_List_Ptr;
      end if;
      if Env = null then
         Env := Null_String_List_Ptr;
      end if;
      Validate (Template);
      pid := To_pid_t (UFork); Check (int (pid));

      if pid = 0 then    --  child process
         Execute_Template (Template);
         --  See comments in POSIX.Unsafe_Process_Primitives.Exec_Search.
         --  We duplicate the code here, since we don't want to raise
         --  any exceptions in the child process.
         for I in Filename'Range loop
            if Filename (I) = '/' then
               Check_Fatal (execve
                 (Filename_With_NUL (Filename_With_NUL'First)'Unchecked_Access,
                  Arg.Char (1)'Access, Env.Char (1)'Access));
               return;
            end if;
         end loop;
         --  filename does not contain "/"
         declare
            Path : constant POSIX_String
                 := POSIX.Process_Environment.Environment_Value_Of
                    ("PATH", "/bin:/usr/bin");
            Start : Positive;
            P : Positive;
            Result : int;
            pragma Unreferenced (Result);

         begin
            P := Path'First;
            loop
               Start := P;
               while P <= Path'Last and then Path (P) /= ':' loop
                  P := P + 1;
               end loop;
               declare
                  Pathname : POSIX_String
                    := Make_Path_Name (Path (Start .. P - 1), Filename);
               begin
                  Result := execve (Pathname
                    (Pathname'First)'Unchecked_Access,
                     Arg.Char (1)'Access, Env.Char (1)'Access);
                  if Fetch_Errno /= ENOENT then
                     Exit_Process (Failed_Creation_Exit);
                  end if;
               end;
               exit when P > Path'Last;
               P := P + 1; -- skip colon
            end loop;
         end;
         Exit_Process (Failed_Creation_Exit);
      else
         Child := To_Process_ID (pid);
      end if;
   end Start_Process_Search;

   --------------------
   --  Exit_Process  --
   --------------------

   procedure sys_exit (status : int);
   pragma Import (C, sys_exit, "_exit");

   procedure Exit_Process
     (Status : Exit_Status := Normal_Exit) is
   begin
      sys_exit (int (Status));
   end Exit_Process;

   ------------------------
   --  Status_Available  --
   ------------------------

   function Status_Available
     (Status : Termination_Status) return Boolean is
   begin
      return Status.pid /= 0 and Status.pid /= -1;
   end Status_Available;

   ---------------------
   --  Process_ID_Of  --
   ---------------------

   function Process_ID_Of (Status : Termination_Status)
      return POSIX.Process_Identification.Process_ID is
   begin
      if not Status_Available (Status) then
         Raise_POSIX_Error (Invalid_Argument);
      end if;
      return To_Process_ID (Status.pid);
   end Process_ID_Of;

   ----------------------
   --  Exit_Status_Of  --
   ----------------------

   function wifexited (stat_val : int) return int;
   pragma Import (C, wifexited, "wifexited");

   function wexitstatus (stat_val : int) return int;
   pragma Import (C, wexitstatus, "wexitstatus");

   function Exit_Status_Of (Status : Termination_Status)
      return Exit_Status is
   begin
      if not Status_Available (Status)
        or else wifexited (Status.stat_val) = 0
      then
         Raise_POSIX_Error (Invalid_Argument);
      end if;
      return Exit_Status (wexitstatus (Status.stat_val));
   end Exit_Status_Of;

   -----------------------------
   --  Termination_Signal_Of  --
   -----------------------------

   function wifsignaled (stat_val : int) return int;
   pragma Import (C, wifsignaled, "wifsignaled");

   function wtermsig (stat_val : int) return int;
   pragma Import (C, wtermsig, "wtermsig");

   function Termination_Signal_Of (Status : Termination_Status)
      return POSIX.Signals.Signal is
   begin
      if not Status_Available (Status)
        or else wifsignaled (Status.stat_val) = 0
      then
         Raise_POSIX_Error (Invalid_Argument);
      end if;
      return POSIX.Signals.Signal (wtermsig (Status.stat_val));
   end Termination_Signal_Of;

   --------------------------
   --  Stopping_Signal_Of  --
   --------------------------

   function wifstopped (stat_val : int) return int;
   pragma Import (C, wifstopped, "wifstopped");

   function wstopsig (stat_val : int) return int;
   pragma Import (C, wstopsig, "wstopsig");

   function Stopping_Signal_Of (Status : Termination_Status)
      return POSIX.Signals.Signal is
   begin
      if not Status_Available (Status)
        or else wifstopped (Status.stat_val) = 0
      then
         Raise_POSIX_Error (Invalid_Argument);
      end if;
      return POSIX.Signals.Signal (wstopsig (Status.stat_val));
   end Stopping_Signal_Of;

   ----------------------------
   --  Termination_Cause_Of  --
   ----------------------------

   function Termination_Cause_Of (Status : Termination_Status)
      return Termination_Cause is
   begin
      if not Status_Available (Status) then
         Raise_POSIX_Error (Invalid_Argument);
      end if;
      if wifexited (Status.stat_val) /= 0 then
         return Exited;
      end if;
      if wifsignaled (Status.stat_val) /= 0 then
         return Terminated_By_Signal;
      end if;
      if wifstopped (Status.stat_val) /= 0 then
         return Stopped_By_Signal;
      end if;
      --  should never get here, unles system is broken
      --  .... so we punt
      Raise_POSIX_Error (ENOSYS);
      return Stopped_By_Signal;  --  to suppress compiler warning;
   end Termination_Cause_Of;

   ------------------------------
   --  Wait_For_Child_Process  --
   ------------------------------

   function waitpid
     (pid : pid_t;
      stat_loc : access int;
      options : int) return pid_t;
   pragma Import (C, waitpid, waitpid_LINKNAME);

   procedure Wait_For_Child_Process
     (Status : out Termination_Status;
      Child : POSIX.Process_Identification.Process_ID;
      Block : Boolean := True;
      Trace_Stopped  : Boolean := True;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
   is
      pragma Unreferenced (Masked_Signals);
      Options : Bits := 0;
   begin
      if Trace_Stopped then
         Options := Options or WUNTRACED;
      end if;
      if not Block then
         Options := Options or WNOHANG;
      end if;
      Defer_Abortion;
      --  .... Change P1003.5?
      --  We ignore the signal masking parameter, since we keep
      --  most signals masked all the time except in the special
      --  handler threads.  Thus, effectively, this operation
      --  cannot be interrupted, except by somebody asynchronously
      --  sending the thread or process one of the signals that
      --  are: mapped to exceptions (e.g. SIGSEGV); used by the
      --  threads library (which we dare not mask); or used for
      --  Ada abortion (e.g. SIGABRT).  We think it is unsafe to
      --  mask these, and so intentionally do not implement the
      --  exact POSIX.5 semantics here.
      Status.pid := waitpid (To_pid_t (Child),
        Status.stat_val'Unchecked_Access, To_int (Options));
      Undefer_Abortion;
      Check (int (Status.pid));
   end Wait_For_Child_Process;

   ------------------------------
   --  Wait_For_Child_Process  --
   ------------------------------

   procedure Wait_For_Child_Process
     (Status : out Termination_Status;
      Group : POSIX.Process_Identification.Process_Group_ID;
      Block : Boolean := True;
      Trace_Stopped : Boolean := True;
      Masked_Signals : POSIX.Signal_Masking
                     := POSIX.RTS_Signals) is
   begin
      Wait_For_Child_Process
        (Status, To_Process_ID (-To_pid_t (Group)),
         Block, Trace_Stopped, Masked_Signals);
   end Wait_For_Child_Process;

   ------------------------------
   --  Wait_For_Child_Process  --
   ------------------------------

   procedure Wait_For_Child_Process
     (Status : out Termination_Status;
      Block : Boolean := True;
      Trace_Stopped : Boolean := True;
      Masked_Signals : POSIX.Signal_Masking
                     := POSIX.RTS_Signals) is
   begin
      Wait_For_Child_Process
        (Status, To_Process_ID (-1), Block, Trace_Stopped, Masked_Signals);
   end Wait_For_Child_Process;

end POSIX.Process_Primitives;

------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                              P O S I X . I O                             --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--             Copyright (C) 1996-1997 Florida State University             --
--                     Copyright (C) 1998-2010, AdaCore                     --
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

--  ?????
--  Perhaps we should put exception handlers around the critical
--  sections in this code, in case Storage_Error is raised by one
--  of the system calls within them?  This would be a lot more overhead.

with Ada.IO_Exceptions,
     System.Storage_Elements,
     POSIX.Implementation,
     POSIX.Permissions.Implementation,
     Unchecked_Conversion;
package body POSIX.IO is

   use POSIX.C,
       POSIX.Implementation,
       POSIX.Permissions.Implementation;

   function To_int is new Unchecked_Conversion (Bits, int);
   function To_Bits is new Unchecked_Conversion (int, Bits);

   C_File_Mode : constant array (File_Mode) of Bits :=
     (Read_Only  => O_RDONLY,
      Write_Only => O_WRONLY,
      Read_Write => O_RDWR);

   C_Whence : constant array (Position) of int :=
     (From_Beginning => SEEK_SET,
      From_End_Of_File => SEEK_END,
      From_Current_Position => SEEK_CUR);

   procedure Check_NNeg_And_Restore_Signals
     (Result : int;
      Masked_Signals : Signal_Masking;
      Old_Mask : Signal_Mask_Access);
   procedure Check_NNeg_And_Restore_Signals
     (Result : ssize_t;
      Masked_Signals : Signal_Masking;
      Old_Mask : Signal_Mask_Access);
   pragma Inline (Check_NNeg_And_Restore_Signals);

   procedure Check_NNeg_And_Restore_Signals
     (Result : int;
      Masked_Signals : Signal_Masking;
      Old_Mask : Signal_Mask_Access) is
   begin
      if Result < 0 then
         Restore_Signals_And_Raise_POSIX_Error
           (Masked_Signals, Old_Mask);
      else
         Restore_Signals (Masked_Signals, Old_Mask);
      end if;
   end Check_NNeg_And_Restore_Signals;

   procedure Check_NNeg_And_Restore_Signals
     (Result : ssize_t;
      Masked_Signals : Signal_Masking;
      Old_Mask : Signal_Mask_Access) is
   begin
      if Result < 0 then
         Restore_Signals_And_Raise_POSIX_Error
           (Masked_Signals, Old_Mask);
      else
         Restore_Signals (Masked_Signals, Old_Mask);
      end if;
   end Check_NNeg_And_Restore_Signals;

   ------------
   --  Open  --
   ------------

   function open (path : char_ptr; oflag : int) return int;
   function open (path : char_ptr; oflag : int; mode : mode_t) return int;
   pragma Import (C, open, open_LINKNAME);

   function Open
     (Name           : Pathname;
      Mode           : File_Mode;
      Options        : Open_Option_Set := Empty_Set;
      Masked_Signals : Signal_Masking := RTS_Signals) return File_Descriptor
   is
      Result : int;
      Name_With_NUL : POSIX_String := Name & NUL;
      Old_Mask : aliased Signal_Mask;
   begin
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := open
       (path => Name_With_NUL (Name_With_NUL'First)'Unchecked_Access,
        oflag => To_int (Option_Set (Options).Option or C_File_Mode (Mode)));
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
      return File_Descriptor (Result);
   end Open;

   ----------------------
   --  Open_Or_Create  --
   ----------------------

   function Open_Or_Create
     (Name             : Pathname;
      Mode             : File_Mode;
      Permissions      : POSIX.Permissions.Permission_Set;
      Options          : Open_Option_Set := Empty_Set;
      Masked_Signals   : POSIX.Signal_Masking := RTS_Signals)
     return File_Descriptor is
      Result : int;
      Name_With_NUL : POSIX_String := Name & NUL;
      Old_Mask : aliased Signal_Mask;
   begin
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := open
       (path => Name_With_NUL (Name_With_NUL'First)'Unchecked_Access,
        oflag => To_int (Option_Set (Options).Option
                 or C_File_Mode (Mode) or O_CREAT),
        mode => Form_C_Permission (Permissions));
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
      return File_Descriptor (Result);
   end Open_Or_Create;

   ---------------
   --  Is_Open  --
   ---------------

   function fcntl (fildes : int; cmd : int) return int;
   function fcntl (fildes : int; cmd : int; arg : int) return int;
   pragma Import (C, fcntl, fcntl_LINKNAME);

   function Is_Open (File : File_Descriptor) return Boolean is
   begin
      return fcntl (int (File), F_GETFL) /= -1;
   end Is_Open;

   -------------
   --  Close  --
   -------------

   function close (fildes : int) return int;
   pragma Import (C, close, close_LINKNAME);

   procedure Close
     (File          : File_Descriptor;
      Masked_Signals : Signal_Masking := RTS_Signals) is
      Old_Mask : aliased Signal_Mask;
      Result : int;
   begin
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := close (int (File));
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
   end Close;

   -----------------
   --  Duplicate  --
   -----------------

   function dup (fildes : int) return int;
   pragma Import (C, dup, dup_LINKNAME);

   function Duplicate
     (File   : File_Descriptor;
      Target : File_Descriptor := 0)
     return File_Descriptor is
      pragma Warnings (Off, Target);
   begin
      return File_Descriptor (Check (dup (int (File))));
   end Duplicate;

   ---------------------------
   --  Duplicate_and_Close  --
   ---------------------------

   function dup2 (fildes, fildes2 : int) return int;
   --  fildes = old fd, fildes2 = new fd
   pragma Import (C, dup2, dup2_LINKNAME);

   function Duplicate_and_Close
     (File           : File_Descriptor;
      Target         : File_Descriptor := 0;
      Masked_Signals : Signal_Masking := RTS_Signals)
     return File_Descriptor is
      Old_Mask : aliased Signal_Mask;
      Result : int;
   begin
      if File = Target then
         return Target;
      end if;
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := dup2 (int (File), int (Target));
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
      return File_Descriptor (Result);
   end Duplicate_and_Close;

   -------------------
   --  Create_Pipe  --
   -------------------

   type fildes_pair is array (1 .. 2) of File_Descriptor;

   function pipe (fildes : access fildes_pair) return int;
   pragma Import (C, pipe, pipe_LINKNAME);

   procedure Create_Pipe
     (Read_End  : out File_Descriptor;
      Write_End : out File_Descriptor) is
      Fildes : aliased fildes_pair;
   begin
      Check_NZ (pipe (Fildes'Unchecked_Access));
      Read_End := Fildes (1);
      Write_End := Fildes (2);
   end Create_Pipe;

   ------------
   --  Read  --
   ------------

   --  .... Change P1003.5?
   --  We have trouble getting a pointer to the Buffer argument,
   --  which we need in order to pass it through to the OS.
   --  1) The type Ada.Streams.Stream_Element_Array
   --  is not declared with aliased components.  This prevents us
   --  from using Buffer (Buffer'First)'Unchecked_Access.
   --  2) The parameter Buffer is not aliased, so we can't use
   --  Buffer'Unchecked_Access.
   --  3) The parameter Buffer is not itself an access parameter.
   --  Therefore, we use Buffer (Buffer'First)'Address.
   --  The compiler should always
   --  accept this, but some day it may quietly stop working, as it relies
   --  on assumptions about the meaning of 'Address and how the compiler
   --  chooses to pass the parameter Buffer.
   --  If this breaks here, then it will also break in several other
   --  places, where we use the same technique.

   function read (fildes : int; buf : System.Address; nbyte : size_t)
     return ssize_t;
   pragma Import (C, read, read_LINKNAME);

   procedure Read
     (File           : File_Descriptor;
      Buffer         : out IO_Buffer;
      Last           : out IO_Count;
      Masked_Signals : Signal_Masking := RTS_Signals) is
      Result : ssize_t;
      Old_Mask : aliased Signal_Mask;
   begin
      if Buffer'Length = 0 then
         Last := IO_Count (Buffer'First) - 1;
         return;
      end if;
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := read (int (File), Buffer (Buffer'First)'Address,
        size_t (Buffer'Last - Buffer'First + 1));
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
      Last := IO_Count (Buffer'First) + IO_Count (Result) - 1;
      if Result = 0 then
         raise Ada.IO_Exceptions.End_Error;
      end if;
   end Read;

   procedure NONSTANDARD_Read
     (File           : File_Descriptor;
      Buffer         : out IO_Buffer;
      Last           : out Natural;
      Masked_Signals : Signal_Masking := RTS_Signals) is
      Result : ssize_t;
      Old_Mask : aliased Signal_Mask;
   begin
      if Buffer'Length = 0 then
         Last := Buffer'First - 1;
         return;
      end if;
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := read (int (File), Buffer (Buffer'First)'Address,
        size_t (Buffer'Last - Buffer'First + 1));
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
      Last := Buffer'First + Integer (Result) - 1;
      if Result = 0 then
         raise Ada.IO_Exceptions.End_Error;
      end if;
   end NONSTANDARD_Read;

   procedure Read
     (File           :  File_Descriptor;
      Buffer         : out Ada.Streams.Stream_Element_Array;
      Last           : out Ada.Streams.Stream_Element_Offset;
      Masked_Signals : Signal_Masking := RTS_Signals) is
      Result : ssize_t;
      Old_Mask : aliased Signal_Mask;
      use Ada.Streams;
   begin
      if Buffer'Length = 0 then
         Last := Buffer'First - 1;
         return;
      end if;
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := read (int (File), Buffer (Buffer'First)'Address,
        size_t (Buffer'Last - Buffer'First + 1));
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
      Last := Buffer'First
        + Ada.Streams.Stream_Element_Offset (Result) - 1;
      if Result = 0 then
         raise Ada.IO_Exceptions.End_Error;
      end if;
   end Read;

   --  .... Consider writing one lower-level subprogram for Read and
   --  having both versions call it.  Similarly for Write.

   -------------
   --  Write  --
   -------------

   function write (fildes : int; buf : System.Address; nbyte : size_t)
     return ssize_t;
   pragma Import (C, write, write_LINKNAME);

   --  ....Change POSIX.5????
   --  Something is inconsistent here.
   --  If Last is the last position, then for a null array
   --  we don't want to set it to zero!

   procedure Write
     (File           : File_Descriptor;
      Buffer         : IO_Buffer;
      Last           : out IO_Count;
      Masked_Signals : Signal_Masking := RTS_Signals) is
      Result : ssize_t;
      Old_Mask : aliased Signal_Mask;
   begin
      if Buffer'Length = 0 then
         Last := IO_Count (Buffer'First - 1);
         return;
      end if;
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := write (int (File), Buffer (Buffer'First)'Address,
        size_t (Buffer'Last - Buffer'First + 1));
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
      Last := IO_Count (Buffer'First) + IO_Count (Result) - 1;
   end Write;

   --  .... Change POSIX.5?????
   --  The type of Last really should be Natural, since it is
   --  an index in a POSIX_String array.

   procedure NONSTANDARD_Write
     (File           : File_Descriptor;
      Buffer         : IO_Buffer;
      Last           : out Natural;
      Masked_Signals : Signal_Masking := RTS_Signals) is
      Result : ssize_t;
      Old_Mask : aliased Signal_Mask;
   begin
      if Buffer'Length = 0 then
         Last := Buffer'First - 1;
         return;
      end if;
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := write (int (File), Buffer (Buffer'First)'Address,
        size_t (Buffer'Last - Buffer'First + 1));
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
      Last := Buffer'First + Integer (Result) - 1;
   end NONSTANDARD_Write;

   procedure Write
     (File           : File_Descriptor;
      Buffer         : Ada.Streams.Stream_Element_Array;
      Last           : out Ada.Streams.Stream_Element_Offset;
      Masked_Signals : Signal_Masking := RTS_Signals) is
      Result : ssize_t;
      Old_Mask : aliased Signal_Mask;
      use Ada.Streams;
   begin
      if Buffer'Length = 0 then
         Last := Buffer'First - 1;
         return;
      end if;
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := write (int (File), Buffer (Buffer'First)'Address,
        size_t (Buffer'Last - Buffer'First + 1));
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
      Last := Buffer'First
           + Ada.Streams.Stream_Element_Offset (Result) - 1;
   end Write;

   --------------------
   --  Generic_Read  --
   --------------------

   procedure Generic_Read
     (File           : File_Descriptor;
      Item           : out T;
      Masked_Signals : Signal_Masking := RTS_Signals) is
      Result : ssize_t;
      Old_Mask : aliased Signal_Mask;
   begin
      if Item'Size rem char'Size /= 0 then
         Raise_POSIX_Error (Operation_Not_Implemented);
      end if;
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := read (int (File), Item'Address,
         size_t (Item'Size / char'Size));
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
      if Result < Item'Size / char'Size then
         raise Ada.IO_Exceptions.End_Error;
      end if;
   end Generic_Read;

   ---------------------
   --  Generic_Write  --
   ---------------------

   procedure Generic_Write
     (File           : File_Descriptor;
      Item           : T;
      Masked_Signals : Signal_Masking := RTS_Signals) is
      Result : ssize_t;
      Written : System.Storage_Elements.Storage_Offset := 0;
      To_Write : System.Storage_Elements.Storage_Offset :=
        System.Storage_Elements.Storage_Offset (Item'Size / char'Size);
      Old_Mask : aliased Signal_Mask;
      use System.Storage_Elements;
   begin
      if Item'Size rem char'Size /= 0 then
         Raise_POSIX_Error (Operation_Not_Implemented);
      end if;

      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);

      --  Write is called iteratively because it may only perform a
      --  partial write, for example in the case the filesystem is
      --  full. If fewer bytes are written than expected then try
      --  again to write the remaining portion of the object.

      loop
         Result := write
           (int (File), Item'Address + Written, size_t (To_Write - Written));
         --  Exit if write fails or zero-length write succeeds.
         exit when Result <= 0;
         Written := Written + Storage_Offset (Result);
         To_Write := To_Write - Storage_Offset (Result);
         --  Exit if done writing.
         exit when To_Write = 0;
      end loop;

      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
   end Generic_Write;

   ------------
   --  Seek  --
   ------------

   function lseek (fildes : int; offset : off_t; whence : int) return off_t;
   pragma Import (C, lseek, lseek_LINKNAME);

   procedure Seek
     (File           : File_Descriptor;
      Offset         : IO_Offset;
      Result         : out IO_Offset;
      Starting_Point : Position  := From_Beginning) is
   begin
      Result := IO_Offset
        (lseek (int (File), off_t (Offset), C_Whence (Starting_Point)));
      Check (int (Result));
   end Seek;

   -----------------
   --  File_Size  --
   -----------------

   function File_Size (File : File_Descriptor) return IO_Count is
      Prevoff, Endoff : off_t;
   begin
      Begin_Critical_Section;
      Prevoff := lseek (int (File), 0, SEEK_CUR);
      if Prevoff < 0 then
         End_Critical_Section;
         Raise_POSIX_Error;
      end if;
      Endoff := lseek (int (File), 0, SEEK_END);
      if Endoff < 0 then
         End_Critical_Section;
         Raise_POSIX_Error;
      end if;
      Prevoff := lseek (int (File), Prevoff, SEEK_SET);
      if Prevoff < 0 then
         End_Critical_Section;
         Raise_POSIX_Error;
      end if;
      End_Critical_Section;
      return (IO_Count (Endoff));
   end File_Size;

   ---------------------
   --  File_Position  --
   ---------------------

   function File_Position (File : File_Descriptor) return IO_Offset is
   begin
      return IO_Offset (Check (int (lseek (int (File), 0, SEEK_CUR))));
   end File_Position;

   ---------------------
   --  Is_A_Terminal  --
   ---------------------

   function isatty (fildes : int) return int;
   pragma Import (C, isatty, isatty_LINKNAME);

   function Is_A_Terminal (File : File_Descriptor) return Boolean is
   begin return isatty (int (File)) = 1;
   end Is_A_Terminal;

   -------------------------
   --  Get_Terminal_Name  --
   -------------------------

   function ttyname (fildes : int) return char_ptr;
   pragma Import (C, ttyname, ttyname_LINKNAME);

   function Get_Terminal_Name (File : File_Descriptor) return Pathname is
      Result : char_ptr;
   begin
      Result := ttyname (int (File));
      if Result = null then
         Raise_POSIX_Error;
      end if;
      return Form_POSIX_String (Result);
   end Get_Terminal_Name;

   ------------------------
   --  Get_File_Control  --
   ------------------------

   procedure Get_File_Control
     (File    : File_Descriptor;
      Mode    : out File_Mode;
      Options : out Open_Option_Set) is
      Result : Bits;
      Access_Mode : Bits;
   begin
      Defer_Abortion;
      Result := To_Bits (Check (fcntl (int (File), F_GETFL)));
      Undefer_Abortion;
      Access_Mode := Result and O_ACCMODE;
      if Access_Mode = O_RDONLY then
         Mode := Read_Only;
      elsif Access_Mode = O_WRONLY then
         Mode := Write_Only;
      elsif Access_Mode = O_RDWR then
         Mode := Read_Write;
      else
         Raise_POSIX_Error (ENOSYS);  --  should never be reached
      end if;
      Options := Open_Option_Set (Option_Set'
        (Option => Result and not O_ACCMODE));
   end Get_File_Control;

   ------------------------
   --  Set_File_Control  --
   ------------------------

   C_Other_Open_Options : constant Bits :=
     O_TRUNC or O_EXCL or O_NOCTTY or
     O_SYNC or O_DSYNC or O_RSYNC or O_RDONLY or O_RDWR or O_WRONLY;

   procedure Set_File_Control
     (File    : File_Descriptor;
      Options : Open_Option_Set) is
      Old_Values : int;
      New_Values : Bits;
   begin
      Begin_Critical_Section;
      Old_Values := fcntl (int (File), F_GETFL);
      if Old_Values = -1 then
         End_Critical_Section;
         Raise_POSIX_Error;
      end if;
      New_Values := (Option_Set (Options).Option and not C_Other_Open_Options)
        or (To_Bits (Old_Values) and C_Other_Open_Options);
      if fcntl (int (File), F_SETFL, To_int (New_Values)) = -1 then
         End_Critical_Section;
         Raise_POSIX_Error;
      end if;
      End_Critical_Section;
   end Set_File_Control;

   -------------------------
   --  Get_Close_On_Exec  --
   -------------------------

   function Get_Close_On_Exec (File : File_Descriptor) return Boolean is
      Result : int;
   begin
      Result := fcntl (int (File), F_GETFD);
      if Result = -1 then
         Raise_POSIX_Error;
      end if;
      return (To_Bits (Result) and FD_CLOEXEC) /= 0;
   end Get_Close_On_Exec;

   -------------------------
   --  Set_Close_On_Exec  --
   -------------------------

   procedure Set_Close_On_Exec
     (File : File_Descriptor;
      To   : Boolean := True) is
      Flags : Bits;
      pragma Warnings (Off);
      Result : int;
   begin
      Begin_Critical_Section;
      Flags := To_Bits (fcntl (int (File), F_GETFD));
      if Flags = -1 then
         End_Critical_Section;
         Raise_POSIX_Error;
      end if;
      if To then
         Flags := Flags or FD_CLOEXEC;
      else
         Flags := Flags and not FD_CLOEXEC;
      end if;
      if fcntl (int (File), F_SETFD, To_int (Flags)) = -1 then
         End_Critical_Section;
         Raise_POSIX_Error;
      end if;
      Result := fcntl (int (File), F_GETFD);
      --  should not fail since previous call did not fail
      --  ??? Is it the case that the value of Result should not be checked
      pragma Warnings (Off);
      End_Critical_Section;
   end Set_Close_On_Exec;

   -------------------------
   --  Change_Permission  --
   -------------------------

   function fchmod (fildes : int; mode : mode_t) return int;
   pragma Import (C, fchmod, fchmod_LINKNAME);

   procedure Change_Permissions
     (File :        File_Descriptor;
      Permission :  POSIX.Permissions.Permission_Set) is
   begin
      Check (fchmod (int (File), Form_C_Permission (Permission)));
   end Change_Permissions;

   ---------------------
   --  Truncate_File  --
   ---------------------

   function ftruncate (fildes : int; length : off_t) return int;
   pragma Import (C, ftruncate, ftruncate_LINKNAME);

   procedure Truncate_File
     (File   : File_Descriptor;
      Length : IO_Count) is
   begin
      Check (ftruncate (int (File), off_t (Length)));
   end Truncate_File;

   ------------------------
   --  Synchronize_File  --
   ------------------------

   function fsync (fildes : int) return int;
   pragma Import (C, fsync, fsync_LINKNAME);

   procedure Synchronize_File (File : File_Descriptor) is
   begin
      Check (fsync (int (File)));
   end Synchronize_File;

   ------------------------
   --  Synchronize_Data  --
   ------------------------

   function fdatasync (fildes : int) return int;
   pragma Import (C, fdatasync, fdatasync_LINKNAME);

   procedure Synchronize_Data (File : File_Descriptor) is
   begin
      Check (fdatasync (int (File)));
   end Synchronize_Data;

   --  6.1.12 Sockets File Ownership procedures from P1003.5c

   pragma Warnings (Off);
   procedure Get_Owner
      (File    :  File_Descriptor;
       Process : out POSIX.Process_Identification.Process_ID;
       Group   : out POSIX.Process_Identification.Process_Group_ID) is
   begin
      Raise_POSIX_Error (Operation_Not_Implemented);
   end Get_Owner;
   pragma Warnings (On);

   procedure Set_Socket_Process_Owner
      (File    : File_Descriptor;
       Process : POSIX.Process_Identification.Process_ID) is
      pragma Unreferenced (File);
      pragma Unreferenced (Process);
   begin
      Raise_POSIX_Error (Operation_Not_Implemented);
   end Set_Socket_Process_Owner;

   procedure Set_Socket_Group_Owner
      (File    : File_Descriptor;
       Group   : POSIX.Process_Identification.Process_Group_ID) is
      pragma Unreferenced (File);
      pragma Unreferenced (Group);
   begin
      Raise_POSIX_Error (Operation_Not_Implemented);
   end Set_Socket_Group_Owner;

   procedure Set_Buffer
      (Vector : in out IO_Vector;
       Buffer : System.Address;
       Length : Positive) is
   begin
      Vector.C.iov_base := To_char_ptr (Buffer);
      Vector.C.iov_len  := size_t (Length);
   end Set_Buffer;

   procedure Get_Buffer
      (Vector : IO_Vector;
       Buffer : out System.Address;
       Length : out POSIX.IO_Count) is
   begin
      Buffer := To_Address (Vector.C.iov_base);
      Length := POSIX.IO_Count (Vector.C.iov_len);
   end Get_Buffer;

end POSIX.IO;

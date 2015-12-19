------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                  P O S I X . I M P L E M E N T A T I O N                 --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                                                                          --
--  Copyright (c) 1996-1999 Florida State University (FSU).     All Rights  --
--  Reserved.                                                               --
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

with POSIX.C,
     System.Interrupt_Management,
     Unchecked_Conversion;
package POSIX.Implementation is
   pragma Elaborate_Body;

   --  =========  --
   --   WARNING   --
   --  =========  --

   --  This package should NOT be used directly by an application.
   --  It is internal to the FLORIST implementation of the POSIX.5 API,
   --  and may be changed or replaced in future versions of FLORIST.

   -------------------------
   --  Critical Sections  --
   -------------------------

   --  NEVER raise an exception within a critical section
   --  or abort-deferred section!
   --  Not even indirectly, by calling a subprogram
   --  that might raise an exception.
   --  Always exit the section, then raise the exception.

   --  ALWAYS enclose critical sections in a block with an
   --  exception handler that will call End_Critical_Section
   --  before allowing the exception to propagate, unless you
   --  can prove that no exception will be raised in the code.
   --  (How about Storage_Error, due to stack overflow?)

   --  Try to avoid nesting critical sections,
   --  as it means extra overhead.

   procedure Defer_Abortion;
   procedure Undefer_Abortion;

   --  The following two also defer/undefer abort, as side-effects.

   procedure Begin_Critical_Section;
   procedure End_Critical_Section;

   --------------
   --  Checks  --
   --------------

   --  Don't ever call any of these within a critical section,
   --  or within an abort-deferred section!

   subtype Signal_Mask is System.Interrupt_Management.Interrupt_Mask;
   type Signal_Mask_Access is access all Signal_Mask;

   procedure Raise_POSIX_Error (Error : Error_Code := No_Error);
   pragma No_Return (Raise_POSIX_Error);

   procedure Check (Condition : Boolean;
                    Error : Error_Code;
                    Old_Mask : Signal_Mask_Access := null);

   --  if Condition is false, raise POSIX_Error with
   --  specified error code, else just return
   --
   --  If Old_Mask /= null, then on failure call Restore_Signals with
   --  that mask before raising POSIX_Error.

   procedure Check
     (Result : POSIX.C.int; Old_Mask : Signal_Mask_Access := null);
   function Check
     (Result : POSIX.C.int; Old_Mask : Signal_Mask_Access := null)
     return POSIX.C.int;

   --  if Result is -1 raise POSIX_Error with current error code
   --  otherwise just return Result.
   --
   --  If Old_Mask /= null, then call Restore_Signals with that mask
   --  before raising POSIX_Error.

   procedure Check_NNeg (Result : POSIX.C.int);
   function Check_NNeg (Result : POSIX.C.int) return POSIX.C.int;

   --  same as Check, except any negative value is treated
   --  as a failure
   --  pragma Inline (Check);

   procedure Check_NZ (Result : POSIX.C.int);

   --  same as Check, except any nonzero value is an error code
   --  pragma Inline (Check);

   function Not_Implemented_Neg_One return POSIX.C.int;
   --  return -1 with error code ENOSYS
   pragma Export (C, Not_Implemented_Neg_One, "nosys_neg_one");

   function Not_Implemented_Direct return POSIX.C.int;
   --  return error code ENOSYS
   pragma Export (C, Not_Implemented_Direct, "nosys_direct");

   function Not_Supported_Neg_One return POSIX.C.int;
   --  return -1 with error code ENOTSUP
   pragma Export (C, Not_Supported_Neg_One, "notsup_neg_one");

   function Not_Supported_Direct return POSIX.C.int;
   --  return ENOTSUP
   pragma Export (C, Not_Supported_Direct, "notsup_direct");

   --  These are used as stub link-names for C interface subprograms
   --  which are missing from the OS include-files.

   --  .... We still need to analyze all these functions, one by one,
   --  so that the code in c-posix.c initialized the corresponding ..._LINKNAME
   --  variable to the right value.
   --  If we have any calls to functions that may legitimately return
   --  a value of -1 for a non-error condition, we may need to add some
   --  special stubs for those functions.

   ---------------
   --  Strings  --
   ---------------

   NUL_String : POSIX_String := (1 => NUL);
   function Form_String (Str : POSIX.C.char_ptr) return String;
   function Trim_Leading_Blank (S : String) return String;
   --  pragma Inline (Trim_Leading_Blank);
   procedure Nulterminate
     (To : out POSIX_String;
      From :  String);

   --------------------
   --  String Lists  --
   --------------------

   type POSIX_String_Ptr is access all POSIX_String;

   type PSP_Array is array (Positive range <>) of POSIX_String_Ptr;
   type String_List (Length : Natural) is record
      List : PSP_Array (1 .. Length);
      Char : POSIX.C.char_ptr_array (1 .. Length);
      --  X.Char(i) = X.List(i)(1)'Unchecked_access
   end record;
   type String_List_Ptr is access all String_List;
   --  No_Strict_Aliasing is necessary here to avoid potential
   --  optimization issues when making unchecked conversions to
   --  String_List_Ptr.
   pragma No_Strict_Aliasing (String_List_Ptr);
   Null_String_List : aliased String_List :=
     (Length => 1, List => (1 => null), Char => (1 => null));
   Null_String_List_Ptr : constant String_List_Ptr :=
      Null_String_List'Access;

   --  We try to represent String_List in a form that does not
   --  require further conversion to pass it to the C interface.
   --  The main problem is that Ada strings carry along "dope"
   --  (including index range info) which will confuse a C subprogram,
   --  but which is needed for proper storage deallocation.
   --  We'd like to simply use char_ptr_ptr, but that does not
   --  give us the length information we need to do storage
   --  deallocation.  Likewise, for the component strings, we
   --  can't just use char_ptr, since that does not carry along
   --  the length information we will need later.  In principle,
   --  we could take advantage of compiler-dependent information
   --  about how arrays are laid out, including the location of
   --  dope, but then we'd have to change this code every time
   --  the compiler changes.  Instead, we create a redundant
   --  data structure, that contains its own dope.

   --  Each element string must be null-terminated, as is
   --  the array of pointers Char.  Thus,
   --  X.Length is not the virtual "length" of the list;
   --  that must be calculated, C-style, by counting positions
   --  until a null element is reached.

   --  We address the problem of predicting the length of
   --  array needed by blocking and recopying if necessary
   --  for the Append operation.
   --  For now, we guess the string length is 16,
   --  and double the length each time it overflows.
   --  On the average, this should result in fewer calls
   --  to malloc() than if we were to use a linked list.

   Min_String_List_Length : constant := 16;

   ----------------------
   --  Signal Masking  --
   ----------------------

   --  The following two also defer/undefer abortion, as side-effects.

   procedure Mask_Signals
     (Masking  : Signal_Masking;
      Old_Mask : Signal_Mask_Access);

   procedure Restore_Signals
     (Masking  : Signal_Masking;
      Old_Mask : Signal_Mask_Access);

   procedure Restore_Signals
     (Old_Mask : Signal_Mask_Access);

   --  The following are provided for exit from a critical
   --  section where error checking needs to be done.  The issue
   --  here is that Restore_Signals may change the value of errno,
   --  so we need to combine the actions into one operation,
   --  saving the errno value over the call to Restore_Signals.

   procedure Restore_Signals_And_Raise_POSIX_Error
     (Masked_Signals : Signal_Masking;
      Old_Mask : Signal_Mask_Access);

   procedure Check_NNeg_And_Restore_Signals
     (Result : POSIX.C.int;
      Masked_Signals : Signal_Masking;
      Old_Mask : Signal_Mask_Access);

   -------------------
   --  Error Codes  --
   -------------------

   --  The following operate on the raw Pthread errno value,
   --  and must be written in C since errno may be accessed via
   --  a macro.

   function Fetch_Errno return Error_Code;
   pragma Import (C, Fetch_Errno, "fetch_errno");

   procedure Store_Errno (value : Error_Code);
   pragma Import (C, Store_Errno, "store_errno");

   --  The following operate on the Ada per-task errno value.
   --  The difference is that this value is not affected by any
   --  implicit OS calls that might occur during the implementation
   --  of exception propagation.

   function Get_Ada_Error_Code return Error_Code;

   procedure Set_Ada_Error_Code (Error : Error_Code);

   package Bogus_Error_Codes is

      --  These names are enclosed in this inner
      --  package to avoid name conflicts
      --  with the real error code constants, which are
      --  exported by this package.

      type Error_Name_Enum is
        (No_Error,
         Argument_List_Too_Long,
         Bad_Address,
         Bad_File_Descriptor,
         Bad_Message,
         Broken_Pipe,
         Directory_Not_Empty,
         Exec_Format_Error,
         File_Exists,
         File_Too_Large,
         Filename_Too_Long,
         Improper_Link,
         Inappropriate_IO_Control_Operation,
         Input_Output_Error,
         Interrupted_Operation,
         Invalid_Argument,
         Invalid_Seek,
         Is_A_Directory,
         Message_Too_Long,
         No_Child_Process,
         No_Locks_Available,
         No_Space_Left_On_Device,
         No_Such_Operation_On_Device,
         No_Such_Device_Or_Address,
         No_Such_File_Or_Directory,
         No_Such_Process,
         Not_A_Directory,
         Not_Enough_Space,
         Operation_Canceled,
         Operation_In_Progress,
         Operation_Not_Implemented,
         Operation_Not_Permitted,
         Operation_Not_Supported,
         Permission_Denied,
         Read_Only_File_System,
         Resource_Busy,
         Resource_Deadlock_Avoided,
         Resource_Temporarily_Unavailable,
         Timed_Out,
         Too_Many_Links,
         Too_Many_Open_Files,
         Too_Many_Open_Files_In_System,
         --  2.4.6 Socket Error Codes from P1003.5c
         Address_In_Use,
         Address_Not_Available,
         Already_Awaiting_Connection,
         Connection_Aborted,
         Connection_Refused,
         Connection_Reset,
         Domain_Error,
         Host_Down,
         Host_Unreachable,
         Inappropriate_Family,
         Is_Already_Connected,
         Network_Down,
         Network_Reset,
         Network_Unreachable,
         No_Buffer_Space,
         Not_A_Socket,
         Not_Connected,
         Option_Not_Supported,
         Protocol_Not_Supported,
         Socket_Not_Supported,
         Unknown_Protocol_Option,
         Would_Block,
         Wrong_Protocol_Type);

      type Error_Array_Type is array (Error_Name_Enum) of Error_Code;
   end Bogus_Error_Codes;

   Error_Array : constant Bogus_Error_Codes.Error_Array_Type :=
     (No_Error, E2BIG, EFAULT, EBADF, EBADMSG, EPIPE, ENOTEMPTY, ENOEXEC,
      EEXIST, EFBIG, ENAMETOOLONG, EXDEV, ENOTTY, EIO, EINTR, EINVAL,
      ESPIPE, EISDIR, EMSGSIZE, ECHILD, ENOLCK, ENOSPC, ENODEV, ENXIO,
      ENOENT, ESRCH, ENOTDIR, ENOMEM, ECANCELED, EINPROGRESS, ENOSYS,
      EPERM, ENOTSUP, EACCES, EROFS, EBUSY, EDEADLK, EAGAIN, ETIMEDOUT,
      EMLINK, EMFILE, ENFILE,
      --  2.4.6 Socket Error Codes from P1003.5c
      EADDRINUSE, EADDRNOTAVAIL, EALREADY, ECONNABORTED, ECONNREFUSED,
      ECONNRESET, EDOM, EHOSTDOWN, EHOSTUNREACH, EAFNOSUPPORT, EISCONN,
      ENETDOWN, ENETRESET, ENETUNREACH, ENOBUFS, ENOTSOCK, ENOTCONN,
      EOPNOTSUPP, EPROTONOSUPPORT, ESOCKTNOSUPPORT, ENOPROTOOPT,
      EWOULDBLOCK, EPROTOTYPE);

   ------------------------
   --  Time Conversions  --
   ------------------------

   NS_per_S : constant := 10#1#E9;
   MS_per_S : constant := 10#1#E6;
   type D_Int is mod 2 ** (Duration'Size);
   function To_D_Int is new Unchecked_Conversion (Duration, D_Int);
   function To_Duration is new Unchecked_Conversion (D_Int, Duration);
   Duration_Delta_Assertion : constant :=
     Boolean'Pos (Boolean'Pred (Duration'Small = 0.000_000_001));

   --  We rely that POSIX.Calendar.Time and Calendar.Time are
   --  implemented using the same representation as Duration, and
   --  both are implemented using a UNIX clock.

   function To_Struct_Timespec (D : Duration) return POSIX.C.struct_timespec;
   function To_Struct_Timespec (T : Timespec) return POSIX.C.struct_timespec;
   function To_Duration (TS : POSIX.C.struct_timespec) return Duration;
   function To_Timespec (TS : POSIX.C.struct_timespec) return Timespec;
   function To_Struct_Timeval (D : Duration) return POSIX.C.struct_timeval;
   function To_Duration (TV : POSIX.C.struct_timeval) return Duration;

end POSIX.Implementation;

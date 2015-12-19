------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                 P O S I X . A S Y N C H R O N O U S _ I O                --
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

with Ada.Streams,
     System,
     POSIX.Implementation,
     Unchecked_Conversion,
     Unchecked_Deallocation;

package body POSIX.Asynchronous_IO is

   use Ada.Streams,
       POSIX.C,
       POSIX.Implementation;

   ---------------------------------
   --  Create_AIO_Control_Block   --
   ---------------------------------

   function Create_AIO_Control_Block return AIO_Descriptor is
   begin
      return new Aiocb_Wrapper;
   end Create_AIO_Control_Block;

   ---------------------------------
   -- Destroy_AIO_Control_Block   --
   ---------------------------------

   function aio_error (AD : AIO_Descriptor) return Error_Code;
   pragma Import (C, aio_error, aio_error_LINKNAME);
   procedure Free is
     new Unchecked_Deallocation (Aiocb_Wrapper, AIO_Descriptor);

   --  ????? Change POSIX.5b?
   --  This operation is very difficult to use correctly, since
   --  it is not idempotent.  That is, if there is an exception and
   --  we want to clean up after it, we cannot safely call Destroy_...
   --  since we don't know whether the AIO_Descriptor is valid.

   procedure Destroy_AIO_Control_Block (AD : in out AIO_Descriptor) is
   begin
      Check (AD /= null, Invalid_Argument);
      if aio_error (AD) = EINPROGRESS then
         Raise_POSIX_Error (Operation_Not_Permitted);
      end if;
      Free (AD);
   end Destroy_AIO_Control_Block;

   ----------------
   --  Get_File  --
   ----------------

   function Get_File (AD : AIO_Descriptor) return POSIX.IO.File_Descriptor is
   begin
      Check (AD /= null, Invalid_Argument);
      return POSIX.IO.File_Descriptor (AD.C.aio_fildes);
   end Get_File;

   ----------------
   --  Set_File  --
   ----------------

   procedure Set_File
     (AD   : AIO_Descriptor;
      File : POSIX.IO.File_Descriptor) is
   begin
      Check (AD /= null, Invalid_Argument);
      AD.C.aio_fildes := int (File);
   end Set_File;

   ------------------
   --  Get_Offset  --
   ------------------

   function Get_Offset (AD : AIO_Descriptor) return POSIX.IO.IO_Offset is
   begin
      Check (AD /= null, Invalid_Argument);
      return POSIX.IO.IO_Offset (AD.C.aio_offset);
   end Get_Offset;

   ------------------
   --  Set_Offset  --
   ------------------

   procedure Set_Offset
     (AD     : AIO_Descriptor;
      Offset : POSIX.IO.IO_Offset) is
   begin
      Check (AD /= null, Invalid_Argument);
      AD.C.aio_offset := off_t (Offset);
   end Set_Offset;

   ------------------
   --  Get_Buffer  --
   ------------------

   --  .... Change POSIX.5?
   --  The component aio_buf is of type volatile void * in C
   --  The Ada buffer should also be required to be declared volatile.

   function Get_Buffer (AD : AIO_Descriptor) return IO_Array_Pointer is
   begin
      Check (AD /= null, Invalid_Argument);
      return AD.P;
   end Get_Buffer;

   ------------------
   --  Set_Buffer  --
   ------------------

   procedure Set_Buffer
     (AD     : AIO_Descriptor;
      Buffer : IO_Array_Pointer) is
   begin
      Check (AD /= null, Invalid_Argument);
      AD.C.aio_nbytes := Buffer'Length;
      AD.C.aio_buf := Buffer (Buffer'First)'Address;
      AD.P := Buffer;
   end Set_Buffer;

   ------------------
   --  Get_Length  --
   ------------------

   function Get_Length (AD : AIO_Descriptor) return IO_Count is
   begin
      Check (AD /= null, Invalid_Argument);
      return IO_Count (AD.C.aio_nbytes);
   end Get_Length;

   ------------------
   --  Set_Length  --
   ------------------

   procedure Set_Length
     (AD     : AIO_Descriptor;
      Length : IO_Count) is
   begin
      Check (AD /= null, Invalid_Argument);
      AD.C.aio_nbytes := size_t (Length);
   end Set_Length;

   ------------------------------
   --  Get_Priority_Reduction  --
   ------------------------------

   function Get_Priority_Reduction (AD : AIO_Descriptor) return Natural is
   begin
      Check (AD /= null, Invalid_Argument);
      return Natural (AD.C.aio_reqprio);
   end Get_Priority_Reduction;

   ------------------------------
   --  Set_Priority_Reduction  --
   ------------------------------

   procedure Set_Priority_Reduction
     (AD                 : AIO_Descriptor;
      Priority_Reduction : Natural) is
   begin
      Check (AD /= null, Invalid_Argument);
      AD.C.aio_reqprio := int (Priority_Reduction);
   end Set_Priority_Reduction;

   -----------------
   --  Get_Event  --
   -----------------

   function To_Signal_Event is
     new Unchecked_Conversion (struct_sigevent, POSIX.Signals.Signal_Event);

   function Get_Event (AD : AIO_Descriptor)
     return POSIX.Signals.Signal_Event is
   begin
      Check (AD /= null, Invalid_Argument);
      return To_Signal_Event (AD.C.aio_sigevent);
   end Get_Event;

   -----------------
   --  Set_Event  --
   -----------------

   function To_struct_sigevent is
     new Unchecked_Conversion (POSIX.Signals.Signal_Event, struct_sigevent);

   procedure Set_Event
     (AD    : AIO_Descriptor;
      Event : POSIX.Signals.Signal_Event) is
   begin
      Check (AD /= null, Invalid_Argument);
      AD.C.aio_sigevent := To_struct_sigevent (Event);
   end Set_Event;

   ---------------------
   --  Get_Operation  --
   ---------------------

   function Get_Operation (AD : AIO_Descriptor) return List_IO_Operations is
      opcode : int;
   begin
      Check (AD /= null, Invalid_Argument);
      opcode := AD.C.aio_lio_opcode;

      pragma Warnings (Off);
      --  Disable warning on some platforms where LIO_NOP=LIO_READ=LIO_WRITE=0

      if opcode = LIO_NOP then
         return No_Op;
      elsif opcode = LIO_READ then
         return Read;
      elsif opcode = LIO_WRITE then
         return Write;
      end if;

      pragma Warnings (On);

      Raise_POSIX_Error (Invalid_Argument);
      --  to suppress compiler warning message:
      return No_Op;
   end Get_Operation;

   ---------------------
   --  Set_Operation  --
   ---------------------

   C_lio_op : constant array (List_IO_Operations) of int :=
     (No_Op => LIO_NOP,
      Read => LIO_READ,
      Write => LIO_WRITE);

   procedure Set_Operation
     (AD        : AIO_Descriptor;
      Operation : List_IO_Operations) is
   begin
      Check (AD /= null, Invalid_Argument);
      AD.C.aio_lio_opcode := C_lio_op (Operation);
   end Set_Operation;

   ------------
   --  Read  --
   ------------

   procedure Read (AD : AIO_Descriptor) is
      function aio_read (AD : AIO_Descriptor) return int;
      pragma Import (C, aio_read, aio_read_LINKNAME);
   begin
      Check (AD /= null, Invalid_Argument);
      Check (aio_read (AD));
   end Read;

   -------------
   --  Write  --
   -------------

   procedure Write (AD : AIO_Descriptor) is
      function aio_write (AD : AIO_Descriptor) return int;
      pragma Import (C, aio_write, aio_write_LINKNAME);
   begin
      Check (AD /= null, Invalid_Argument);
      Check (aio_write (AD));
   end Write;

   -----------------------
   --  List_IO_No_Wait  --
   -----------------------

   function lio_listio
     (mode : int;
      list : access AIO_Descriptor;
      nent : int;
      sig  : sigevent_ptr) return int;
   pragma Import (C, lio_listio, lio_listio_LINKNAME);

   procedure List_IO_No_Wait
     (List  : in out AIO_Descriptor_List;
      Event : POSIX.Signals.Signal_Event) is
      sigevent : aliased struct_sigevent := To_struct_sigevent (Event);
   begin
      for i in List'Range loop
         Check (List (i) /= null, Invalid_Argument);
      end loop;
      Check (lio_listio (LIO_NOWAIT,
        List (List'First)'Unchecked_Access,
        int (List'Length), sigevent'Unchecked_Access));
   end List_IO_No_Wait;

   ---------------------
   --  List_IO_Wait  --
   ---------------------

   procedure List_IO_Wait
     (List           : in out AIO_Descriptor_List;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals) is
      Old_Mask : aliased Signal_Mask;
      Result : int;
   begin
      for i in List'Range loop
         Check (List (i) /= null, Invalid_Argument);
      end loop;
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := lio_listio (LIO_WAIT,
        List (List'First)'Unchecked_Access, int (List'Length), null);
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
   end List_IO_Wait;

   ----------------------
   --  Get_AIO_Status  --
   ----------------------

   function Get_AIO_Status (AD : AIO_Descriptor) return AIO_Status is
      Result : Error_Code;
   begin
      Check (AD /= null, Invalid_Argument);
      Result := aio_error (AD);
      if Result = 0 then
         return Completed_Successfully;
      elsif Result = EINPROGRESS then
         return In_Progress;
      elsif Result = ECANCELED then
         return Canceled;
      end if;
      Raise_POSIX_Error;
      --  to supress compiler warning message
      return Canceled;
   end Get_AIO_Status;

   --------------------------
   --  Get_AIO_Error_Code  --
   --------------------------

   function Get_AIO_Error_Code (AD : AIO_Descriptor) return POSIX.Error_Code is
      Result : Error_Code;
   begin
      Check (AD /= null, Invalid_Argument);
      Result := aio_error (AD);
      if Result = ENOSYS or else Result = EINVAL then
         Raise_POSIX_Error;
      end if;
      return Result;
   end Get_AIO_Error_Code;

   -------------------------------
   --  Get_Bytest_Transferred   --
   -------------------------------

   function Get_Bytes_Transferred
     (AD :    AIO_Descriptor) return IO_Count is
      function aio_return (AD : AIO_Descriptor) return ssize_t;
      pragma Import (C, aio_return, aio_return_LINKNAME);
      Result :  ssize_t;
   begin
      Check (AD /= null, Invalid_Argument);
      Result := aio_return (AD);
      Check (int (Result));
      return IO_Count (Result);
   end Get_Bytes_Transferred;

   --------------
   --  Cancel  --
   --------------

   function aio_cancel
     (fildes : int;
      aiocb : AIO_Descriptor) return int;
   pragma Import (C, aio_cancel, aio_cancel_LINKNAME);

   function Cancel (AD : AIO_Descriptor) return Cancelation_Status is
      Result :  int;
   begin
      Result := aio_cancel (AD.C.aio_fildes, AD);

      pragma Warnings (Off);
      --  Disable warning on some platforms where AIO_*=0

      if Result = AIO_CANCELED then
         return Canceled;
      elsif Result = AIO_NOTCANCELED then
         return Not_Canceled;
      elsif Result = AIO_ALLDONE then
         return All_Done;
      end if;

      pragma Warnings (On);

      Raise_POSIX_Error;
      --  to suppress compiler warning message

      return All_Done;
   end Cancel;

   function Cancel
     (File : POSIX.IO.File_Descriptor) return Cancelation_Status is
      Result : int;
   begin
      Result := aio_cancel (int (File), null);

      pragma Warnings (Off);
      --  Disable warning on some platforms where AIO_*=0

      if Result = AIO_CANCELED then
         return Canceled;
      elsif Result = AIO_NOTCANCELED then
         return Not_Canceled;
      elsif Result = AIO_ALLDONE then
         return All_Done;
      end if;

      pragma Warnings (On);

      Raise_POSIX_Error;
      --  to suppress compiler warning message

      return All_Done;
   end Cancel;

   ---------------------------
   --  Await_IO_Or_Timeout  --
   ---------------------------

   type aiocb_ptr_ptr is access constant AIO_Descriptor;

   function aio_suspend
     (list    : aiocb_ptr_ptr;
      nent    : int;
      timeout : timespec_ptr) return int;
   pragma Import (C, aio_suspend, aio_suspend_LINKNAME);

   procedure Await_IO_Or_Timeout
     (AD             : AIO_Descriptor;
      Timeout        : POSIX.Timespec;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals) is
      TS : aliased struct_timespec;
      Old_Mask : aliased Signal_Mask;
      List : AIO_Descriptor_List (1 .. 1) := (others => AD);
      Result : int;
   begin
      Check (AD /= null, Invalid_Argument);
      TS := To_Struct_Timespec (To_Duration (Timeout));
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := aio_suspend (List (List'First)'Unchecked_Access,
        List'Length, TS'Unchecked_Access);
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
   end Await_IO_Or_Timeout;

   procedure Await_IO_Or_Timeout
     (List           : AIO_Descriptor_List;
      Timeout        : POSIX.Timespec;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals) is
      TS : aliased struct_timespec;
      Old_Mask : aliased Signal_Mask;
      Result : int;
   begin
      for i in List'Range loop
         Check (List (i) /= null, Invalid_Argument);
      end loop;
      TS := To_Struct_Timespec (To_Duration (Timeout));
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := aio_suspend (List (List'First)'Unchecked_Access,
        List'Length, TS'Unchecked_Access);
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
   end Await_IO_Or_Timeout;

   ----------------
   --  Await_IO  --
   ----------------

   procedure Await_IO
     (AD             : AIO_Descriptor;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals) is
      Old_Mask : aliased Signal_Mask;
      List : AIO_Descriptor_List (1 .. 1) := (others => AD);
      Result : int;
   begin
      Check (AD /= null, Invalid_Argument);
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := aio_suspend (List (List'First)'Unchecked_Access,
        List'Length, null);
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
   end Await_IO;

   procedure Await_IO
     (List           : AIO_Descriptor_List;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals) is
      Old_Mask : aliased Signal_Mask;
      Result : int;
   begin
      for i in List'Range loop
         if List (i) = null then
            Raise_POSIX_Error (Invalid_Argument);
         end if;
      end loop;
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := aio_suspend (List (List'First)'Unchecked_Access,
        List'Length, null);
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
   end Await_IO;

   ------------------------
   --  Synchronize_File  --
   ------------------------

   function aio_fsync
     (op : int;
      AD : AIO_Descriptor) return int;
   pragma Import (C, aio_fsync, aio_fsync_LINKNAME);

   procedure Synchronize_File (AD : AIO_Descriptor) is
   begin
      Check (AD /= null, Invalid_Argument);
      Check (aio_fsync (O_SYNC, AD));
   end Synchronize_File;

   ------------------------
   --  Synchronize_Data  --
   ------------------------

   procedure Synchronize_Data (AD : AIO_Descriptor) is
   begin
      Check (AD /= null, Invalid_Argument);
      Check (aio_fsync (O_DSYNC, AD));
   end Synchronize_Data;

begin
   --  Check that struct aiocb component is allocated in first position,
   --  so that we can safely convert pointers.
   declare
      X : aliased Aiocb_Wrapper;
      use System;
   begin
      if X'Address /= X.C'Address then
         raise Program_Error;
      end if;
   end;
end POSIX.Asynchronous_IO;

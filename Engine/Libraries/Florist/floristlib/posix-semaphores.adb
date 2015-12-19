------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                      P O S I X . S E M A P H O R E S                     --
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

with POSIX.Implementation,
     POSIX.Permissions.Implementation,
     Unchecked_Conversion;

package body POSIX.Semaphores is

   use POSIX.C,
       POSIX.Implementation,
       POSIX.Permissions.Implementation;

   function To_int is new Unchecked_Conversion (Bits, int);
   function To_int is
     new Unchecked_Conversion (Semaphore_Descriptor, ptr_as_int);

   procedure Check_And_Restore_Signals
     (Result : Semaphore_Descriptor;
      Masked_Signals : Signal_Masking;
      Old_Mask : Signal_Mask_Access);
   pragma Inline (Check_And_Restore_Signals);

   procedure Check_And_Restore_Signals
     (Result : Semaphore_Descriptor;
      Masked_Signals : Signal_Masking;
      Old_Mask : Signal_Mask_Access) is
   begin
      if To_int (Result) = -1 then
         Restore_Signals_And_Raise_POSIX_Error
           (Masked_Signals, Old_Mask);
      else
         Restore_Signals (Masked_Signals, Old_Mask);
      end if;
   end Check_And_Restore_Signals;

   procedure Validate (Sem : Semaphore_Descriptor);
   pragma Inline (Validate);
   procedure Validate (Sem : Semaphore_Descriptor) is
   begin
      if Sem = null then
         Raise_POSIX_Error (Invalid_Argument);
      end if;
   end Validate;

   ---------------------------------
   --        Initialize           --
   ---------------------------------

   function sem_init
     (s : Semaphore_Descriptor;
      pshared : int;
      value : unsigned) return int;
   pragma Import (C, sem_init, sem_init_LINKNAME);

   procedure Initialize
     (Sem       : in out Semaphore;
      Value     : Natural;
      Is_Shared : Boolean := False) is
   begin
      Check (sem_init (Sem.Sem'Unchecked_Access,
        Boolean'Pos (Is_Shared), unsigned (Value)));
   end Initialize;

   ---------------------------------
   --    Descriptor_Of            --
   ---------------------------------

   function Descriptor_Of (Sem : Semaphore) return Semaphore_Descriptor is
   begin
      return Sem.Sem'Unchecked_Access;
   end Descriptor_Of;

   ---------------------------------
   --         Finalize           --
   ---------------------------------

   function sem_destroy (sem : Semaphore_Descriptor) return int;
   pragma Import (C, sem_destroy, sem_destroy_LINKNAME);

   procedure Finalize (Sem : in out Semaphore) is
   begin
      Check (sem_destroy (Sem.Sem'Unchecked_Access));
   end Finalize;

   ---------------------------------
   --         Open                --
   ---------------------------------

   function sem_open
     (name  : char_ptr;
      oflag : int;
      mode  : mode_t;
      value : unsigned) return Semaphore_Descriptor;
   function sem_open
     (name  : char_ptr;
      oflag : int) return Semaphore_Descriptor;
   pragma Import (C, sem_open, sem_open_LINKNAME);

   function Open
     (Name           : POSIX.POSIX_String;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
      return Semaphore_Descriptor is
      Result : Semaphore_Descriptor;
      Name_With_NUL : POSIX_String := Name & NUL;
      Old_Mask : aliased Signal_Mask;
   begin
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := sem_open
        (Name_With_NUL (Name_With_NUL'First)'Unchecked_Access, 0);
      Check_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
      return Result;
   end Open;

   ---------------------------------
   --      Open_Or_Create         --
   ---------------------------------

   function Open_Or_Create
     (Name : POSIX.POSIX_String;
      Permissions : POSIX.Permissions.Permission_Set;
      Value : Natural;
      Options : POSIX.IO.Open_Option_Set := POSIX.IO.Empty_Set;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
     return Semaphore_Descriptor is
      Result : Semaphore_Descriptor;
      Name_With_NUL : POSIX_String := Name & NUL;
      Old_Mask : aliased Signal_Mask;
   begin
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := sem_open
       (Name_With_NUL (Name_With_NUL'First)'Unchecked_Access,
        To_int (Option_Set (Options).Option or O_CREAT),
        Form_C_Permission (Permissions),
        unsigned (Value));
      Check_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
      return Result;
   end Open_Or_Create;

   ---------------------------------
   --      Close                  --
   ---------------------------------

   function sem_close (sem : Semaphore_Descriptor) return int;
   pragma Import (C, sem_close, sem_close_LINKNAME);

   procedure Close (Sem : in out Semaphore_Descriptor) is
   begin
      Check (sem_close (Sem));
   end Close;

   ---------------------------------
   --     Unlink_Semaphore        --
   ---------------------------------

   function sem_unlink (name : char_ptr) return int;
   pragma Import (C, sem_unlink, sem_unlink_LINKNAME);

   procedure Unlink_Semaphore (Name : POSIX.POSIX_String) is
      Name_With_NUL : POSIX_String := Name & NUL;
   begin
      Check (sem_unlink
        (Name_With_NUL (Name_With_NUL'First)'Unchecked_Access));
   end Unlink_Semaphore;

   ---------------------------------
   --         Wait                --
   ---------------------------------

   function sem_wait (sem : Semaphore_Descriptor) return int;
   pragma Import (C, sem_wait, sem_wait_LINKNAME);

   procedure Wait
     (Sem : Semaphore_Descriptor;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals) is
      Result : int;
      Old_Mask : aliased Signal_Mask;
   begin
      Validate (Sem);
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := sem_wait (Sem);
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
   end Wait;

   ---------------------------------
   --        Try_Wait             --
   ---------------------------------

   function sem_trywait (sem : Semaphore_Descriptor) return int;
   pragma Import (C, sem_trywait, sem_trywait_LINKNAME);

   function Try_Wait (Sem : Semaphore_Descriptor) return Boolean is
      Result : int;
   begin
      Validate (Sem);
      Result := sem_trywait (Sem);
      if Result = 0 then
         return True;
      elsif Fetch_Errno = EAGAIN then
         return False;
      else
         Raise_POSIX_Error;
         --  return statement to suppress compiler warning message
         return False;
      end if;
   end Try_Wait;

   ---------------------------------
   --            Post             --
   ---------------------------------

   function sem_post (sem : Semaphore_Descriptor) return int;
   pragma Import (C, sem_post, sem_post_LINKNAME);

   procedure Post (Sem : Semaphore_Descriptor) is
   begin
      Validate (Sem);
      Check (sem_post (Sem));
   end Post;

   ---------------------------------
   --      Get_Value              --
   ---------------------------------

   function sem_getvalue
     (sem : Semaphore_Descriptor;
      sval : access int) return int;
   pragma Import (C, sem_getvalue, sem_getvalue_LINKNAME);

   function Get_Value (Sem : Semaphore_Descriptor) return Integer is
      Value : aliased int;
   begin
      Validate (Sem);
      Check (sem_getvalue (Sem, Value'Unchecked_Access));
      return Integer (Value);
   end Get_Value;

end POSIX.Semaphores;

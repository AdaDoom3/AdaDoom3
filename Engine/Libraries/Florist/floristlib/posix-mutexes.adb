------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                         P O S I X . M U T E X E S                        --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--             Copyright (C) 1996-1997 Florida State University             --
--                    Copyright (C) 1998-2007, AdaCore                      --
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

with POSIX.Implementation;

package body POSIX.Mutexes is

   use POSIX.C;
   use POSIX.Implementation;

   type Mutexattr_Descriptor is access constant pthread_mutexattr_t;

   ------------------
   --  Initialize  --
   ------------------

   function pthread_mutexattr_init
     (attr : access pthread_mutexattr_t) return int;
   pragma Import (C, pthread_mutexattr_init,
     pthread_mutexattr_init_LINKNAME);

   procedure Initialize (Attr : in out Attributes) is
   begin
      Check_NZ (pthread_mutexattr_init (Attr.Attr'Unchecked_Access));
   end Initialize;

   ----------------
   --  Finalize  --
   ----------------

   function pthread_mutexattr_destroy
     (attr : access pthread_mutexattr_t) return int;
   pragma Import (C, pthread_mutexattr_destroy,
     pthread_mutexattr_destroy_LINKNAME);

   procedure Finalize (Attr : in out Attributes) is
   begin
      Check_NZ (pthread_mutexattr_destroy (Attr.Attr'Unchecked_Access));
   end Finalize;

   --------------------------
   --  Get_Process_Shared  --
   --------------------------

   function pthread_mutexattr_getpshared
     (attr : Mutexattr_Descriptor;
      pshared : access int) return int;
   pragma Import (C, pthread_mutexattr_getpshared,
     pthread_mutexattr_getpshared_LINKNAME);

   function Get_Process_Shared (Attr : Attributes)
      return Boolean is
      Result : aliased int;
   begin
      Check_NZ (pthread_mutexattr_getpshared
        (Attr.Attr'Unchecked_Access, Result'Unchecked_Access));
      return Result = PTHREAD_PROCESS_SHARED;
   end Get_Process_Shared;

   --------------------------
   --  Set_Process_Shared  --
   --------------------------

   function pthread_mutexattr_setpshared
     (attr : access pthread_mutexattr_t;
      pshared : int) return int;
   pragma Import (C, pthread_mutexattr_setpshared,
     pthread_mutexattr_setpshared_LINKNAME);

   To_pshared : constant array (Boolean) of int :=
     (True => PTHREAD_PROCESS_SHARED,
      False => PTHREAD_PROCESS_PRIVATE);

   procedure Set_Process_Shared
     (Attr : in out Attributes;
      Is_Shared : Boolean := False) is
   begin
      Check_NZ (pthread_mutexattr_setpshared
        (Attr.Attr'Unchecked_Access, To_pshared (Is_Shared)));
   end Set_Process_Shared;

   --------------------------
   --  Set_Locking_Policy  --
   --------------------------

   function pthread_mutexattr_setprotocol
     (attr : access pthread_mutexattr_t;
      protocol : int) return int;
   pragma Import (C, pthread_mutexattr_setprotocol,
     pthread_mutexattr_setprotocol_LINKNAME);

   To_C_Policy : constant array (Locking_Policy) of int :=
     (No_Priority_Inheritance => PTHREAD_PRIO_NONE,
      Highest_Blocked_Task => PTHREAD_PRIO_INHERIT,
      Highest_Ceiling_Priority => PTHREAD_PRIO_PROTECT);

   procedure Set_Locking_Policy
      (Attr : in out Attributes;
       Locking : Locking_Policy) is
   begin
      Check_NZ (pthread_mutexattr_setprotocol
        (Attr.Attr'Unchecked_Access, To_C_Policy (Locking)));
   end Set_Locking_Policy;

   --------------------------
   --  Get_Locking_Policy  --
   --------------------------

   function pthread_mutexattr_getprotocol
     (attr : Mutexattr_Descriptor;
      value_ptr : access int) return int;
   pragma Import (C, pthread_mutexattr_getprotocol,
     pthread_mutexattr_getprotocol_LINKNAME);

   function Get_Locking_Policy (Attr : Attributes) return Locking_Policy is
      Result : aliased int;
   begin
      Check_NZ (pthread_mutexattr_getprotocol
       (Attr.Attr'Unchecked_Access, Result'Unchecked_Access));
      if Result = PTHREAD_PRIO_NONE then
         return No_Priority_Inheritance;
      elsif Result = PTHREAD_PRIO_INHERIT then
         return Highest_Blocked_Task;
      elsif Result = PTHREAD_PRIO_PROTECT then
         return Highest_Ceiling_Priority;
      else
         Raise_POSIX_Error (Operation_Not_Supported);
         --  to suppress compiler warning
         return No_Priority_Inheritance;
      end if;
   end Get_Locking_Policy;

   ----------------------------
   --  Set_Ceiling_Priority  --
   ----------------------------

   function pthread_mutexattr_setprioceiling
     (attr : access pthread_mutexattr_t;
      prioceiling : int) return int;
   pragma Import (C, pthread_mutexattr_setprioceiling,
     pthread_mutexattr_setprioceiling_LINKNAME);

   procedure Set_Ceiling_Priority
      (Attr : in out Attributes;
       New_Ceiling : Ceiling_Priority) is
   begin
      Check_NZ (pthread_mutexattr_setprioceiling
        (Attr.Attr'Unchecked_Access, int (New_Ceiling)));
   end Set_Ceiling_Priority;

   ----------------------------
   --  Get_Ceiling_Priority  --
   ----------------------------

   function pthread_mutexattr_getprioceiling
     (attr : Mutexattr_Descriptor;
      prioceiling : access int) return int;
      pragma Import (C, pthread_mutexattr_getprioceiling,
        pthread_mutexattr_getprioceiling_LINKNAME);

   function Get_Ceiling_Priority (Attr : Attributes) return Ceiling_Priority is
      Result : aliased int;
   begin
      Check_NZ (pthread_mutexattr_getprioceiling
        (Attr.Attr'Unchecked_Access, Result'Unchecked_Access));
      return (Ceiling_Priority (Result));
   end Get_Ceiling_Priority;

   ------------------
   --  Initialize  --
   ------------------

   function pthread_mutex_init
     (mutex : access pthread_mutex_t;
      attr  : Mutexattr_Descriptor) return int;
   pragma Import (C, pthread_mutex_init, pthread_mutex_init_LINKNAME);

   procedure Initialize
     (M : in out Mutex;
      Attr : Attributes) is
   begin
      Check_NZ (pthread_mutex_init
        (M.Mutex'Unchecked_Access, Attr.Attr'Unchecked_Access));
   end Initialize;

   procedure Initialize (M : in out Mutex) is
   begin
      Check_NZ (pthread_mutex_init (M.Mutex'Unchecked_Access, null));
   end Initialize;

   ---------------------
   --  Descriptor_Of  --
   ---------------------

   function Descriptor_Of (M : Mutex) return Mutex_Descriptor is
   begin
      return M.Mutex'Unchecked_Access;
   end Descriptor_Of;

   ----------------
   --  Finalize  --
   ----------------

   function pthread_mutex_destroy
     (mutex : access pthread_mutex_t) return int;
   pragma Import (C, pthread_mutex_destroy,
     pthread_mutex_destroy_LINKNAME);

   procedure Finalize (M : in out Mutex) is
   begin
      Check_NZ (pthread_mutex_destroy (M.Mutex'Unchecked_Access));
   end Finalize;

   ----------------------------
   --  Set_Ceiling_Priority  --
   ----------------------------

   type int_ptr is access all int;
   function pthread_mutex_setprioceiling
     (mutex : Mutex_Descriptor;
      prioceiling : int;
      old_ceiling : int_ptr) return int;
   pragma Import (C, pthread_mutex_setprioceiling,
     pthread_mutex_setprioceiling_LINKNAME);

   procedure Set_Ceiling_Priority
     (M           : Mutex_Descriptor;
      New_Ceiling : Ceiling_Priority;
      Old_Ceiling : out Ceiling_Priority) is
      Result : aliased int;
   begin
      Check_NZ (pthread_mutex_setprioceiling
        (M, int (New_Ceiling), Result'Unchecked_Access));
      Old_Ceiling := Ceiling_Priority (Result);
   end Set_Ceiling_Priority;

   ----------------------------
   --  Get_Ceiling_Priority  --
   ----------------------------

   function pthread_mutex_getprioceiling
     (mutex : Mutex_Descriptor;
      prioceiling : access int) return int;
   pragma Import (C, pthread_mutex_getprioceiling,
     pthread_mutex_getprioceiling_LINKNAME);

   function Get_Ceiling_Priority (M : Mutex_Descriptor)
      return Ceiling_Priority is
      Result : aliased int;
   begin
      Check_NZ (pthread_mutex_getprioceiling (M, Result'Unchecked_Access));
      return Ceiling_Priority (Result);
   end Get_Ceiling_Priority;

   ------------
   --  Lock  --
   ------------

   function pthread_mutex_lock
     (mutex : Mutex_Descriptor) return int;
   pragma Import (C, pthread_mutex_lock, pthread_mutex_lock_LINKNAME);

   procedure Lock (M : Mutex_Descriptor) is
   begin
      Check_NZ (pthread_mutex_lock (M));
   end Lock;

   ----------------
   --  Try_Lock  --
   ----------------

   function pthread_mutex_trylock
     (mutex : Mutex_Descriptor) return int;
   pragma Import (C, pthread_mutex_trylock, pthread_mutex_trylock_LINKNAME);

   function Try_Lock (M : Mutex_Descriptor) return Boolean is
      Result : constant int := pthread_mutex_trylock (M);
      --  Note: pthread_mutex_trylock returns an error code in Result, and
      --  does not set errno.

   begin
      case Result is
         when 0 =>
            return True;

         when EBUSY =>
            return False;

         when others =>
            Raise_POSIX_Error (Error_Code (Result));
      end case;
   end Try_Lock;

   --------------
   --  Unlock  --
   --------------

   function pthread_mutex_unlock
     (mutex : Mutex_Descriptor) return int;
   pragma Import (C, pthread_mutex_unlock,
     pthread_mutex_unlock_LINKNAME);

   procedure Unlock (M : Mutex_Descriptor) is
   begin
      Check_NZ (pthread_mutex_unlock (M));
   end Unlock;

end POSIX.Mutexes;

------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--             P O S I X . C O N D I T I O N _ V A R I A B L E S            --
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

with POSIX.Implementation;

with Unchecked_Deallocation;

package body POSIX.Condition_Variables is

   use POSIX.C;
   use POSIX.Implementation;

   procedure Free is new
     Unchecked_Deallocation (pthread_condattr_t, Attributes_Descriptor);

   procedure Free is new
     Unchecked_Deallocation (pthread_cond_t, Condition_Descriptor);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Attr : in out Attributes) is
      function pthread_condattr_init
        (attr : access pthread_condattr_t) return int;
      pragma Import (C, pthread_condattr_init, pthread_condattr_init_LINKNAME);
   begin
      Attr.Attr := new pthread_condattr_t;
      Check_NZ (pthread_condattr_init (Attr.Attr));
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Attr : in out Attributes) is
      function pthread_condattr_destroy
        (attr : access pthread_condattr_t) return int;
      pragma Import (C, pthread_condattr_destroy,
        pthread_condattr_destroy_LINKNAME);
   begin
      Check_NZ (pthread_condattr_destroy (Attr.Attr));
      Free (Attr.Attr);
   end Finalize;

   ------------------------
   -- Get_Process_Shared --
   ------------------------

   function Get_Process_Shared (Attr : Attributes) return Boolean is
      Result : aliased int;
      function pthread_condattr_getpshared
        (attr : Attributes_Descriptor;
         pshared : access int) return int;
      pragma Import (C, pthread_condattr_getpshared,
        pthread_condattr_getpshared_LINKNAME);

   begin
      Check_NZ (pthread_condattr_getpshared
        (Attr.Attr, Result'Unchecked_Access));
      return Result = PTHREAD_PROCESS_SHARED;
   end Get_Process_Shared;

   ------------------------
   -- Set_Process_Shared --
   ------------------------

   To_pshared : constant array (Boolean) of int :=
     (True => PTHREAD_PROCESS_SHARED,
      False => PTHREAD_PROCESS_PRIVATE);

   procedure Set_Process_Shared
     (Attr : in out Attributes;
      Is_Shared : Boolean := False)
   is
      function pthread_condattr_setpshared
        (attr : access pthread_condattr_t;
         pshared : C.int) return int;
      pragma Import (C, pthread_condattr_setpshared,
        pthread_condattr_setpshared_LINKNAME);

   begin
      Check_NZ (pthread_condattr_setpshared
        (Attr.Attr, To_pshared (Is_Shared)));
   end Set_Process_Shared;

   ----------------
   -- Initialize --
   ----------------

   function pthread_cond_init
     (cond : access pthread_cond_t;
      attr : Attributes_Descriptor) return int;
   pragma Import (C, pthread_cond_init, pthread_cond_init_LINKNAME);

   procedure Initialize
      (Cond : in out Condition;
       Attr : Attributes) is
   begin
      Cond.Cond := new pthread_cond_t;
      Check_NZ (pthread_cond_init (Cond.Cond, Attr.Attr));
   end Initialize;

   procedure Initialize (Cond : in out Condition) is
   begin
      Cond.Cond := new pthread_cond_t;
      Check_NZ (pthread_cond_init (Cond.Cond, null));
   end Initialize;

   -------------------
   -- Descriptor_Of --
   -------------------

   function Descriptor_Of (Cond : Condition) return Condition_Descriptor is
   begin
      return Cond.Cond;
   end Descriptor_Of;

   --------------
   -- Finalize --
   --------------

   function pthread_cond_destroy (cond : access pthread_cond_t) return int;
   pragma Import (C, pthread_cond_destroy, pthread_cond_destroy_LINKNAME);

   procedure Finalize (Cond : in out Condition) is
   begin
      Check_NZ (pthread_cond_destroy (Cond.Cond));
      Free (Cond.Cond);
   end Finalize;

   ------------
   -- Signal --
   ------------

   procedure Signal (Cond : Condition_Descriptor) is
      function pthread_cond_signal (cond : Condition_Descriptor) return int;
      pragma Import (C, pthread_cond_signal, pthread_cond_signal_LINKNAME);
   begin
      Check_NZ (pthread_cond_signal (Cond));
   end Signal;

   ---------------
   -- Broadcast --
   ---------------

   procedure Broadcast (Cond : Condition_Descriptor) is
      function pthread_cond_broadcast (cond : Condition_Descriptor) return int;
      pragma Import (C, pthread_cond_broadcast,
        pthread_cond_broadcast_LINKNAME);
   begin
      Check_NZ (pthread_cond_broadcast (Cond));
   end Broadcast;

   ----------
   -- Wait --
   ----------

   procedure Wait
      (Cond : Condition_Descriptor;
       M :    POSIX.Mutexes.Mutex_Descriptor)
   is
      function pthread_cond_wait
        (cond : Condition_Descriptor;
         mutex : POSIX.Mutexes.Mutex_Descriptor) return int;
      pragma Import (C, pthread_cond_wait, pthread_cond_wait_LINKNAME);

   begin
      Check_NZ (pthread_cond_wait (Cond, M));
   end Wait;

   ----------------
   -- Timed_Wait --
   ----------------

   --  .....change POSIX.5b??????
   --  When we tested this operation we found that people tended to
   --  use it incorrectly, not expecting to get an exception if it times
   --  out.  Perhaps there should be an alternate binding closer to the
   --  C-language pthread_cond_timedwait, which does not treat ETIME as
   --  at true error.

   procedure Timed_Wait
     (Cond    : Condition_Descriptor;
      M       : POSIX.Mutexes.Mutex_Descriptor;
      Timeout : POSIX.Timespec)
   is
      function pthread_cond_timedwait
        (cond    : Condition_Descriptor;
         mutex   : POSIX.Mutexes.Mutex_Descriptor;
         abstime : access struct_timespec)  return int;
      pragma Import (C, pthread_cond_timedwait,
        pthread_cond_timedwait_LINKNAME);
      T : aliased struct_timespec := To_Struct_Timespec (Timeout);

   begin
      Check_NZ (pthread_cond_timedwait (Cond, M, T'Unchecked_Access));
   end Timed_Wait;

end POSIX.Condition_Variables;

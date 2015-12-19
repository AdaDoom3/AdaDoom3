------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--            P O S I X . M E M O R Y _ R A N G E _ L O C K I N G           --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--  Copyright (c) 1996-1998                 Florida State University (FSU)  --
--  All Rights Reserved.                                                    --
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
     POSIX.Implementation;
package body POSIX.Memory_Range_Locking is

   use POSIX.C,
       POSIX.Implementation;

   ------------------
   --  Lock_Range  --
   ------------------

   procedure Lock_Range
     (First  : System.Address;
      Length : System.Storage_Elements.Storage_Offset) is
      function mlock (addr : access System.Address; len : size_t) return int;
      pragma Import (C, mlock, mlock_LINKNAME);
      Addr : aliased System.Address := First;
   begin
      --  .... Some OS (eg. Solaris) has non-standard mlock/munlock.
      --  For the reason if "mlock/munlock" fails with EINVAL,
      --  we speculate that the OS has non-standard form of the
      --  functions. So, try it again with a different form of the function.

      --  .... This is not a perfact solution and we feel that this kind of
      --  thing has to be resolved in the configuration management. It does its
      --  work for now....

      if mlock (Addr'Unchecked_Access, size_t (Length)) = -1
        and then Fetch_Errno = Invalid_Argument
      then
         declare
            function mlock (addr : System.Address; len : size_t) return int;
            pragma Import (C, mlock, mlock_LINKNAME);
         begin
            Check (mlock (Addr, size_t (Length)));
         end;
      else
         Check (mlock (Addr'Unchecked_Access, size_t (Length)));
      end if;
   end Lock_Range;

   --------------------
   --  UnLock_Range  --
   --------------------

   procedure Unlock_Range
     (First  : System.Address;
      Length : System.Storage_Elements.Storage_Offset) is
      function munlock (addr : access System.Address; len : size_t) return int;
      pragma Import (C, munlock, munlock_LINKNAME);
      Addr : aliased System.Address := First;
   begin
      if munlock (Addr'Unchecked_Access, size_t (Length)) = -1
        and then Fetch_Errno = Invalid_Argument
      then
         declare
            function munlock (addr : System.Address; len : size_t) return int;
            pragma Import (C, munlock, munlock_LINKNAME);
         begin
            Check (munlock (Addr, size_t (Length)));
         end;
      else
         Check (munlock (Addr'Unchecked_Access, size_t (Length)));
      end if;
   end Unlock_Range;

end POSIX.Memory_Range_Locking;

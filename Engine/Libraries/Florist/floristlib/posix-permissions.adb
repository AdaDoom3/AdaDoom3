------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                     P O S I X . P E R M I S S I O N S                    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--  Copyright (c) 1996, 1997            Florida  State  University  (FSU),  --
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
     POSIX.Implementation,
     POSIX.Permissions.Implementation;
package body POSIX.Permissions is

   use POSIX.C;
   use POSIX.Implementation;
   use POSIX.Permissions.Implementation;

   ------------------------
   -- local declarations --
   ------------------------

   Cached_Umask : mode_t := 0;

   -----------------------
   -- local subprograms --
   -----------------------

   function umask (c_mask : mode_t) return mode_t;
   pragma Import (C, umask, umask_LINKNAME);

   ---------------------------------------
   --  Get_Allowed_Process_Permissions  --
   ---------------------------------------

   function Get_Allowed_Process_Permissions return Permission_Set is
      Mask : mode_t;
   begin
      Begin_Critical_Section;
      Mask := umask (Cached_Umask);
      if Mask /= Cached_Umask then
         Cached_Umask := Mask;
         Mask := umask (Mask);
      end if;
      End_Critical_Section;
      return Form_Ada_Permission
        ((not Cached_Umask) and File_Access_Permission_Bits);

      --  The allowed process permissions are the complement of the
      --  file permission bits in umask.
      --  The Ada interface requires that the other bits be zero.

      --  We cache the old umask, to reduce the number of calls.

   end Get_Allowed_Process_Permissions;

   ---------------------------------------
   --  Set_Allowed_Process_Permissions  --
   ---------------------------------------

   procedure Set_Allowed_Process_Permissions
     (Permissions : Permission_Set) is
      Mask : mode_t :=
        not (Form_C_Permission (Permissions) and File_Access_Permission_Bits);

      --  Mask is assigned to but never referenced when umask is
      --  evaluated for its side effect.
      pragma Warnings (Off, Mask);

   begin
      Cached_Umask := Mask;
      Mask := umask (Mask);
   end Set_Allowed_Process_Permissions;

   ---------------------------------------
   --  Set_Allowed_Process_Permissions  --
   ---------------------------------------

   procedure Set_Allowed_Process_Permissions
     (Permissions : Permission_Set;
      Old_Perms   : out Permission_Set) is
      Mask : constant mode_t :=
        not (Form_C_Permission (Permissions) and File_Access_Permission_Bits);
   begin
      Cached_Umask := Mask;
      Old_Perms := Form_Ada_Permission ((not umask (Mask)) and
        File_Access_Permission_Bits);
   end Set_Allowed_Process_Permissions;

end POSIX.Permissions;

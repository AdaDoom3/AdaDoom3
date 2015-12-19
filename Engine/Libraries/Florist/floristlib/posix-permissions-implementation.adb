------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--      P O S I X . P E R M I S S I O N S . I M P L E M E N T A T I O N     --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--             Copyright (C) 1991-1994 Florida State University             --
--                     Copyright (C) 1995-2014, AdaCore                     --
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

package body POSIX.Permissions.Implementation is

   -------------------
   --  Permissions  --
   -------------------

   function Form_C_Permission (perm : Permission_Set) return mode_t is
      c_perm : mode_t;
   begin
      c_perm := 0;
      if perm (Others_Execute) then
         c_perm := c_perm or S_IXOTH;
      end if;
      if perm (Others_Write) then
         c_perm := c_perm or S_IWOTH;
      end if;
      if perm (Others_Read) then
         c_perm := c_perm or S_IROTH;
      end if;
      if perm (Group_Execute) then
         c_perm := c_perm or S_IXGRP;
      end if;
      if perm (Group_Write) then
         c_perm := c_perm or S_IWGRP;
      end if;
      if perm (Group_Read) then
         c_perm := c_perm or S_IRGRP;
      end if;
      if perm (Owner_Execute) then
         c_perm := c_perm or S_IXUSR;
      end if;
      if perm (Owner_Write) then
         c_perm := c_perm or S_IWUSR;
      end if;
      if perm (Owner_Read) then
         c_perm := c_perm or S_IRUSR;
      end if;
      if perm (Set_User_ID) then
         c_perm := c_perm or S_ISUID;
      end if;
      if perm (Set_Group_ID) then
         c_perm := c_perm or S_ISGID;
      end if;
      return c_perm;
   end Form_C_Permission;

   -----------------------------
   --  form_posix_permission  --
   -----------------------------

   function Form_Ada_Permission (perm : mode_t) return Permission_Set is
      a_perm : Permission_Set := (others => False);
      c_perm : mode_t;
   begin
      c_perm := perm;
      if (c_perm and S_IXOTH) /= 0  then
         a_perm (Others_Execute) := True;
      end if;
      if (c_perm and S_IWOTH) /= 0 then
         a_perm (Others_Write) := True;
      end if;
      if (c_perm and S_IROTH) /= 0 then
         a_perm (Others_Read) := True;
      end if;
      if (c_perm and S_IXGRP) /= 0 then
         a_perm (Group_Execute) := True;
      end if;
      if (c_perm and S_IWGRP) /= 0 then
         a_perm (Group_Write) := True;
      end if;
      if (c_perm and S_IRGRP) /= 0 then
         a_perm (Group_Read) := True;
      end if;
      if (c_perm and S_IXUSR) /= 0 then
         a_perm (Owner_Execute) := True;
      end if;
      if (c_perm and S_IWUSR) /= 0 then
         a_perm (Owner_Write) := True;
      end if;
      if (c_perm and S_IRUSR) /= 0 then
         a_perm (Owner_Read) := True;
      end if;
      if (c_perm and S_ISGID) /= 0 then
         a_perm (Set_Group_ID) := True;
      end if;
      if (c_perm and S_ISUID) /= 0 then
         a_perm (Set_User_ID) := True;
      end if;
      return a_perm;
   end Form_Ada_Permission;

end POSIX.Permissions.Implementation;

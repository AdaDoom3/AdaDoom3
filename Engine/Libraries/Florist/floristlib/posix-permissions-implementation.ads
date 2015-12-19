------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--      P O S I X . P E R M I S S I O N S . I M P L E M E N T A T I O N     --
--                                                                          --
--                                  S p e c                                 --
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

with POSIX.C; use POSIX.C;
package POSIX.Permissions.Implementation is

   --  =========  --
   --   WARNING   --
   --  =========  --

   --  This package should NOT be used directly by an application.
   --  It is internal to the FLORIST implementation of the POSIX.5 API,
   --  and may be changed or replaced in future versions of FLORIST.

   Permission_Bits : constant POSIX.C.mode_t :=
     POSIX.C.S_IRWXU or POSIX.C.S_IRWXG or
     POSIX.C.S_IRWXO or POSIX.C.S_ISUID or POSIX.C.S_ISGID;
   File_Access_Permission_Bits : constant POSIX.C.mode_t :=
     POSIX.C.S_IRWXU or POSIX.C.S_IRWXG or POSIX.C.S_IRWXO;
   function Form_Ada_Permission
     (perm : POSIX.C.mode_t) return Permission_Set;
   function Form_C_Permission
     (perm : Permission_Set) return POSIX.C.mode_t;

end POSIX.Permissions.Implementation;

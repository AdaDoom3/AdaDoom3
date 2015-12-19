------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--         P O S I X . F I L E _ S T A T U S . E X T E N S I O N S          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                       Copyright (C) 2014, AdaCore                        --
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

package body POSIX.File_Status.Extensions is

   ------------------------
   --  IO_Block_Size_Of  --
   ------------------------

   function IO_Block_Size_Of (File_Status : Status) return POSIX.IO_Count is
   begin
      return IO_Count (struct_stat (File_Status).st_blksize);
   end IO_Block_Size_Of;

   ---------------------------
   --  Allocated_Blocks_Of  --
   ---------------------------

   function Allocated_Blocks_Of (File_Status : Status) return POSIX.IO_Count is
   begin
      return IO_Count (struct_stat (File_Status).st_blocks);
   end Allocated_Blocks_Of;

end POSIX.File_Status.Extensions;

------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                  P O S I X . P A G E _ A L I G N M E N T                 --
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

with POSIX.Configurable_System_Limits;
pragma Elaborate_All (POSIX.Configurable_System_Limits);

package body POSIX.Page_Alignment is

   use System.Storage_Elements,
       System;

   Page_Size : constant Storage_Offset :=
     Storage_Offset (POSIX.Configurable_System_Limits.Page_Size);

   ------------------------
   --  Truncate_To_Page  --
   ------------------------

   function Truncate_To_Page
      (Addr : Address) return Address is
   begin
      if Page_Size /= 0  then
         return Addr - (Addr mod Page_Size);
      else
         return Addr;
      end if;
   end Truncate_To_Page;

   function Truncate_To_Page
      (Offset : IO_Count) return IO_Count is
   begin
      if Page_Size /= 0 then
         return Offset - (Offset rem IO_Count (Page_Size));
      else
         return Offset;
      end if;
   end Truncate_To_Page;

   ---------------------
   --  Adjust_Length  --
   ---------------------

   function Adjust_Length
      (Addr :   Address;
       Length : Storage_Offset) return Storage_Offset is
      L : Storage_Offset;
   begin
      L := Length + Addr - Truncate_To_Page (Addr);
      if Page_Size = 0 then
         return L;
      end if;
      if L mod Page_Size = 0 then
         return L;
      end if;
      return Page_Size * (L / Page_Size + 1);
   end Adjust_Length;

   function Adjust_Length
      (Offset : IO_Count;
       Length : Storage_Offset) return Storage_Offset is
      O : IO_Count;
      L : Storage_Offset;
   begin
      O := Offset - Truncate_To_Page (Offset);
      L := Length + Storage_Offset (O);
      if Page_Size = 0 then
         return L;
      end if;
      if L mod Page_Size = 0 then
         return L;
      end if;
      return Page_Size * (L / Page_Size + 1);
   end Adjust_Length;

   --------------
   --  Length  --
   --------------

   function Length
      (Size : Natural) return Storage_Offset is
   begin
      if Size mod System.Storage_Unit = 0 then
         return Storage_Offset (Size);
      else
         return Storage_Offset (Size / System.Storage_Unit + 1);
      end if;
   end Length;

end POSIX.Page_Alignment;

------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--           P O S I X . S H A R E D _ M E M O R Y _ O B J E C T S          --
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

with POSIX.C,
     POSIX.Implementation,
     POSIX.Permissions.Implementation,
     Unchecked_Conversion;

package body POSIX.Shared_Memory_Objects is

   use POSIX.C;
   use POSIX.Implementation;
   use POSIX.Permissions.Implementation;

   function To_int is new Unchecked_Conversion (Bits, int);

   C_File_Mode : constant array (POSIX.IO.File_Mode) of Bits :=
     (POSIX.IO.Read_Only  => O_RDONLY,
      POSIX.IO.Write_Only => O_WRONLY,
      POSIX.IO.Read_Write => O_RDWR);

   --------------------------
   --  Open_Shared_Memory  --
   --------------------------

   function shm_open
     (name  : char_ptr;
      oflag : int;
      mode  : mode_t) return int;
   pragma Import (C, shm_open, shm_open_LINKNAME);

   function Open_Shared_Memory
     (Name           : POSIX.POSIX_String;
      Mode           : POSIX.IO.File_Mode;
      Options        : POSIX.IO.Open_Option_Set := POSIX.IO.Empty_Set;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
      return POSIX.IO.File_Descriptor is
      Old_Mask : aliased Signal_Mask;
      Name_With_NUL : POSIX_String := Name & NUL;
      Result : POSIX.IO.File_Descriptor;
   begin
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := POSIX.IO.File_Descriptor (Check (shm_open
        (Name_With_NUL (Name_With_NUL'First)'Unchecked_Access,
         To_int (Option_Set (Options).Option or C_File_Mode (Mode)),
         0), Old_Mask'Unchecked_Access));
      Check_NNeg_And_Restore_Signals
        (int (Result), Masked_Signals, Old_Mask'Unchecked_Access);
      return Result;
   end Open_Shared_Memory;

   ------------------------------------
   --  Open_Or_Create_Shared_Memory  --
   ------------------------------------

   function Open_Or_Create_Shared_Memory
     (Name           : POSIX.POSIX_String;
      Mode           : POSIX.IO.File_Mode;
      Permissions    : POSIX.Permissions.Permission_Set;
      Options        : POSIX.IO.Open_Option_Set := POSIX.IO.Empty_Set;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
      return POSIX.IO.File_Descriptor is
      Old_Mask : aliased Signal_Mask;
      Name_With_NUL : POSIX_String := Name & NUL;
      Result : POSIX.IO.File_Descriptor;
   begin
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := POSIX.IO.File_Descriptor (Check (shm_open
        (Name_With_NUL (Name_With_NUL'First)'Unchecked_Access,
         To_int (Option_Set (Options).Option or C_File_Mode (Mode) or O_CREAT),
         Form_C_Permission (Permissions)), Old_Mask'Unchecked_Access));
      Check_NNeg_And_Restore_Signals
        (int (Result), Masked_Signals, Old_Mask'Unchecked_Access);
      return Result;
   end Open_Or_Create_Shared_Memory;

   ----------------------------
   --  Unlink_Shared_Memory  --
   ----------------------------

   procedure Unlink_Shared_Memory
     (Name : POSIX.POSIX_String) is
      Name_With_NUL : POSIX_String := Name & NUL;
      function shm_unlink (name : char_ptr) return int;
      pragma Import (C, shm_unlink, shm_unlink_LINKNAME);
   begin
      Check (shm_unlink
        (Name_With_NUL (Name_With_NUL'First)'Unchecked_Access));
   end Unlink_Shared_Memory;

end POSIX.Shared_Memory_Objects;

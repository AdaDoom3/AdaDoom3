------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                    P O S I X . F I L E _ L O C K I N G                   --
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

with POSIX.C,
     POSIX.Implementation,
     Unchecked_Conversion;

package body POSIX.File_Locking is

   use POSIX.C,
       POSIX.Implementation;

   function To_Process_ID is
     new Unchecked_Conversion (pid_t, POSIX.Process_Identification.Process_ID);

   C_Lock_Type : constant array (Lock_Kind) of short :=
     (Read_Lock => F_RDLCK,
      Write_Lock => F_WRLCK,
      Unlock => F_UNLCK);

   C_Whence : constant array (POSIX.IO.Position) of short :=
     (POSIX.IO.From_Beginning => SEEK_SET,
      POSIX.IO.From_End_Of_File => SEEK_END,
      POSIX.IO.From_Current_Position => SEEK_CUR);

   ----------------
   --  Get_Lock  --
   ----------------

   function fcntl
     (fd : int;
      cmd : int;
      arg : flock_ptr) return int;
   pragma Import (C, fcntl, fcntl_LINKNAME);

   procedure Get_Lock
     (File    : POSIX.IO.File_Descriptor;
      Lock    : File_Lock;
      Result  : out File_Lock;
      Process : out POSIX.Process_Identification.Process_ID) is
      T : aliased struct_flock;
      Res : File_Lock (False);
      --  temporary is needed in case Result.Whole_File = True
   begin
      T.l_type := C_Lock_Type (Lock.Lock);
      if Lock.Whole_File then
         T.l_whence := SEEK_SET;
         T.l_start := 0;
         T.l_len := off_t (POSIX.IO.File_Size (File));
      else
         T.l_whence := C_Whence (Lock.Starting_Point);
         T.l_start := off_t (Lock.Start);
         T.l_len := off_t (Lock.Length);
      end if;
      Check (fcntl (int (File), F_GETLK, T'Unchecked_Access));
      if T.l_type = F_UNLCK then
         Process := POSIX.Process_Identification.Null_Process_ID;
         Res.Lock := Unlock;
      else
         Process := To_Process_ID (T.l_pid);
         if T.l_type = F_RDLCK then
            Res.Lock := Read_Lock;
         elsif T.l_type = F_WRLCK then
            Res.Lock := Write_Lock;
         else
            Res.Lock := Unlock;
         end if;
         if T.l_whence = SEEK_SET then
            Res.Starting_Point := POSIX.IO.From_Beginning;
         elsif T.l_whence = SEEK_END then
            Res.Starting_Point := POSIX.IO.From_End_Of_File;
         else
            Res.Starting_Point := POSIX.IO.From_Current_Position;
         end if;
         Res.Start := POSIX.IO.IO_Offset (T.l_start);
         Res.Length := IO_Count (T.l_len);
      end if;
      Result := Res;
   end Get_Lock;

   ----------------
   --  Set_Lock  --
   ----------------

   procedure Set_Lock
     (File : POSIX.IO.File_Descriptor;
      Lock : File_Lock) is
      T : aliased struct_flock;
   begin
      T.l_type := C_Lock_Type (Lock.Lock);
      if Lock.Whole_File then
         T.l_whence := SEEK_SET;
         T.l_start := 0;
         T.l_len := off_t (POSIX.IO.File_Size (File));
      else
         T.l_whence := C_Whence (Lock.Starting_Point);
         T.l_start := off_t (Lock.Start);
         T.l_len := off_t (Lock.Length);
      end if;
      Check (fcntl (int (File), F_SETLK, T'Unchecked_Access));
   end Set_Lock;

   ------------------------
   --  Wait_To_Set_Lock  --
   ------------------------

   procedure Wait_To_Set_Lock
     (File           : POSIX.IO.File_Descriptor;
      Lock           : File_Lock;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals) is
      T : aliased struct_flock;
      Result : int;
      Old_Mask : aliased Signal_Mask;
   begin
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      T.l_type := C_Lock_Type (Lock.Lock);
      if Lock.Whole_File then
         T.l_whence := SEEK_SET;
         T.l_start := 0;
         T.l_len := off_t (POSIX.IO.File_Size (File));
      else
         T.l_whence := C_Whence (Lock.Starting_Point);
         T.l_start := off_t (Lock.Start);
         T.l_len := off_t (Lock.Length);
      end if;
      Result := fcntl (int (File), F_SETLKW, T'Unchecked_Access);
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
   end Wait_To_Set_Lock;

end POSIX.File_Locking;

------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                              P O S I X . I O                             --
--                                                                          --
--                                  S p e c                                 --
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

with Ada.Streams,
     POSIX,
     POSIX.C,
     POSIX.Permissions,
     POSIX.Process_Identification,
     System;
pragma Elaborate_All (POSIX);
package POSIX.IO is

   type File_Descriptor is
     range 0 .. POSIX.Open_Files_Maxima'Last - 1;
   for File_Descriptor'Size use POSIX.C.int'Size;

   Standard_Input  : constant File_Descriptor := 0;
   Standard_Output : constant File_Descriptor := 1;
   Standard_Error  : constant File_Descriptor := 2;

   type IO_Offset is new POSIX.C.off_t;

   --  File Modes and Options

   type File_Mode is (Read_Only, Write_Only, Read_Write);
   type Open_Option_Set is new POSIX.Option_Set;
   --  Empty_Set, "+" and unary and binary "-" are derived operations

   Non_Blocking             : constant Open_Option_Set;
   Append                   : constant Open_Option_Set;
   Truncate                 : constant Open_Option_Set;
   Exclusive                : constant Open_Option_Set;
   Not_Controlling_Terminal : constant Open_Option_Set;
   File_Synchronized        : constant Open_Option_Set;
   Data_Synchronized        : constant Open_Option_Set;
   Read_Synchronized        : constant Open_Option_Set;

   --  Operations to open or close file descriptors

   function Open
     (Name           : POSIX.Pathname;
      Mode           : File_Mode;
      Options        : Open_Option_Set := Empty_Set;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
      return File_Descriptor;

   function Open_Or_Create
     (Name           : POSIX.Pathname;
      Mode           : File_Mode;
      Permissions    : POSIX.Permissions.Permission_Set;
      Options        : Open_Option_Set := Empty_Set;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
      return File_Descriptor;

   function Is_Open (File : File_Descriptor) return Boolean;

   procedure Close
     (File           : File_Descriptor;
      Masked_Signals : POSIX.Signal_Masking
                     := POSIX.RTS_Signals);
   function Duplicate
     (File   : File_Descriptor;
      Target : File_Descriptor := 0)
      return File_Descriptor;
   function Duplicate_and_Close
     (File           : File_Descriptor;
      Target         : File_Descriptor := 0;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
      return File_Descriptor;
   procedure Create_Pipe
     (Read_End  : out File_Descriptor;
      Write_End : out File_Descriptor);

   --  File Input/Output operations

   subtype IO_Buffer is POSIX.POSIX_String;

   procedure Read
     (File           : File_Descriptor;
      Buffer         : out IO_Buffer;
      Last           : out POSIX.IO_Count;
      Masked_Signals : POSIX.Signal_Masking
                     := POSIX.RTS_Signals);
   procedure NONSTANDARD_Read
     (File           : File_Descriptor;
      Buffer         : out IO_Buffer;
      Last           : out Natural;
      Masked_Signals : POSIX.Signal_Masking
                     := POSIX.RTS_Signals);
   procedure Read
     (File           : File_Descriptor;
      Buffer         : out Ada.Streams.Stream_Element_Array;
      Last           : out Ada.Streams.Stream_Element_Offset;
      Masked_Signals : POSIX.Signal_Masking
                     := POSIX.RTS_Signals);
   procedure Write
     (File           : File_Descriptor;
      Buffer         : IO_Buffer;
      Last           : out POSIX.IO_Count;
      Masked_Signals : POSIX.Signal_Masking
                     := POSIX.RTS_Signals);
   procedure NONSTANDARD_Write
     (File           : File_Descriptor;
      Buffer         : IO_Buffer;
      Last           : out Natural;
      Masked_Signals : POSIX.Signal_Masking
                     := POSIX.RTS_Signals);
   procedure Write
     (File           : File_Descriptor;
      Buffer         : Ada.Streams.Stream_Element_Array;
      Last           : out Ada.Streams.Stream_Element_Offset;
      Masked_Signals : POSIX.Signal_Masking
                     := POSIX.RTS_Signals);
   generic
      type T is private;
   procedure Generic_Read
     (File           : File_Descriptor;
      Item           : out T;
      Masked_Signals : POSIX.Signal_Masking
                     := POSIX.RTS_Signals);
   generic
      type T is private;
   procedure Generic_Write
     (File           : File_Descriptor;
      Item           : T;
      Masked_Signals : POSIX.Signal_Masking
                     := POSIX.RTS_Signals);

   --  File position operations

   type Position is
     (From_Beginning, From_Current_Position, From_End_Of_File);
   procedure Seek
     (File           : File_Descriptor;
      Offset         : IO_Offset;
      Result         : out IO_Offset;
      Starting_Point : Position := From_Beginning);
   function File_Size (File : File_Descriptor)
      return POSIX.IO_Count;
   function File_Position (File : File_Descriptor)
      return IO_Offset;

   --  Terminal operations

   function Is_A_Terminal (File : File_Descriptor)
      return Boolean;
   function Get_Terminal_Name (File : File_Descriptor)
      return POSIX.Pathname;

   --  File Control operations

   procedure Get_File_Control
     (File    : File_Descriptor;
      Mode    : out File_Mode;
      Options : out Open_Option_Set);
   procedure Set_File_Control
     (File    : File_Descriptor;
      Options : Open_Option_Set);
   function Get_Close_On_Exec (File : File_Descriptor)
      return Boolean;
   procedure Set_Close_On_Exec
     (File : File_Descriptor;
      To   : Boolean := True);
   procedure Change_Permissions
      (File :        POSIX.IO.File_Descriptor;
       Permission :  POSIX.Permissions.Permission_Set);
   procedure Truncate_File
      (File :        POSIX.IO.File_Descriptor;
       Length :      POSIX.IO_Count);
   procedure Synchronize_File (File : POSIX.IO.File_Descriptor);
   procedure Synchronize_Data (File : POSIX.IO.File_Descriptor);

   --  POSIX.5c/D4 additions

   --  6.1.1 Sockets Option Flags from P1003.5c

   Signal_When_Socket_Ready : constant Open_Option_Set;

   --  6.1.12 Sockets File Ownership procedures from P1003.5c

   procedure Get_Owner
      (File    :  File_Descriptor;
       Process : out POSIX.Process_Identification.Process_ID;
       Group   : out POSIX.Process_Identification.Process_Group_ID);
   procedure Set_Socket_Process_Owner
      (File    : File_Descriptor;
       Process : POSIX.Process_Identification.Process_ID);
   procedure Set_Socket_Group_Owner
      (File    : File_Descriptor;
       Group   : POSIX.Process_Identification.Process_Group_ID);

   type IO_Vector is limited private;
   procedure Set_Buffer
      (Vector : in out IO_Vector;
       Buffer : System.Address;
       Length : Positive);
   procedure Get_Buffer
      (Vector : IO_Vector;
       Buffer : out System.Address;
       Length : out POSIX.IO_Count);

private
   Non_Blocking             : constant Open_Option_Set
     := Open_Option_Set (Option_Set'(Option => POSIX.C.O_NONBLOCK));
   Append                   : constant Open_Option_Set
     := Open_Option_Set (Option_Set'(Option => POSIX.C.O_APPEND));
   --  ....  Change POSIX.5?
   --  This Append hides operation on String_Lists, and vice versa,
   --  if we "use" both this package and POSIX.
   Truncate                 : constant Open_Option_Set
     := Open_Option_Set (Option_Set'(Option => POSIX.C.O_TRUNC));
   Exclusive                : constant Open_Option_Set
     := Open_Option_Set (Option_Set'(Option => POSIX.C.O_EXCL));
   Not_Controlling_Terminal : constant Open_Option_Set
     := Open_Option_Set (Option_Set'(Option => POSIX.C.O_NOCTTY));
   File_Synchronized        : constant Open_Option_Set
     := Open_Option_Set (Option_Set'(Option => POSIX.C.O_SYNC));
   Data_Synchronized        : constant Open_Option_Set
     := Open_Option_Set (Option_Set'(Option => POSIX.C.O_DSYNC));
   Read_Synchronized        : constant Open_Option_Set
     := Open_Option_Set (Option_Set'(Option => POSIX.C.O_RSYNC));

   --  P1003.5c/D4 additions

   Signal_When_Socket_Ready : constant Open_Option_Set :=
     Open_Option_Set (POSIX.Empty_Set);

   type IO_Vector is record
      C : aliased POSIX.C.Sockets.struct_iovec :=
         POSIX.C.Sockets.struct_iovec'(iov_base => null,
                                       iov_len  => 0);
   end record;

end POSIX.IO;

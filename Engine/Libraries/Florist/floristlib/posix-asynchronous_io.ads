------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                 P O S I X . A S Y N C H R O N O U S _ I O                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                                                                          --
--  This  file is a component  of FLORIST,  an implementation of the POSIX  --
--  Ada  bindings  for  use with the GNAT Ada compiler and the FSU Gnu Ada  --
--  Runtime Library (GNARL).                                                --
--                                                                          --
--  This package specification contains some text extracted from  IEEE STD  --
--  1003.5: 1990, Information Technology -- POSIX Ada Language  Interfaces  --
--  Part 1: Binding  for  System Application Program Interface, as amended  --
--  by IEEE STD 1003.5b: 1996, Amendment 1: Realtime Extensions, copyright  --
--  1996 by the Institute of Electrical and Electronics Engineers, Inc.     --
--                                                                          --
--  The package specifications in the IEEE standards cited above represent  --
--  only a  portion  of  the  documents  and  are  not to be interpreteted  --
--  outside the context  of  the documents.  The standards must be used in  --
--  conjunction  with  the  package   specifications  in  order  to  claim  --
--  conformance.   The IEEE takes no responsibility for and will assume no  --
--  liability for damages resulting from the reader's misinterpretation of  --
--  said  information resulting from its out-of-context nature.   To order  --
--  copies of the IEEE standards,  please contact the  IEEE Service Center  --
--  at 445 Hoes Lane, PO Box 1331, Piscataway, NJ 08855-1331; via phone at  --
--  1-800-678-IEEE, 908-981-1393; or via fax at 908-981-9667.               --
--                                                                          --
--  These  package  specifications are  distributed in  the hope that they  --
--  will  be useful, but  WITHOUT  ANY  WARRANTY; without even the implied  --
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.        --
--                                                                          --
------------------------------------------------------------------------------

with POSIX.C,
     POSIX.IO,
     POSIX.Signals;
package POSIX.Asynchronous_IO is

   type AIO_Descriptor is private;
   function Create_AIO_Control_Block return AIO_Descriptor;
   procedure Destroy_AIO_Control_Block (AD : in out AIO_Descriptor);

   type List_IO_Operations is
     (No_Op,
      Read,
      Write);
   type IO_Array_Pointer is access Ada.Streams.Stream_Element_Array;
      for IO_Array_Pointer'Size use Standard'Address_Size;
      --  force this to be a "thin" pointer, like C pointers

   function Get_File (AD : AIO_Descriptor)
     return POSIX.IO.File_Descriptor;
   procedure Set_File
     (AD  : AIO_Descriptor;
      File : POSIX.IO.File_Descriptor);
   function Get_Offset (AD : AIO_Descriptor) return POSIX.IO.IO_Offset;
   procedure Set_Offset
     (AD     : AIO_Descriptor;
      Offset : POSIX.IO.IO_Offset);
   function Get_Buffer (AD : AIO_Descriptor) return IO_Array_Pointer;
   procedure Set_Buffer
     (AD     : AIO_Descriptor;
      Buffer : IO_Array_Pointer);
   function Get_Length (AD : AIO_Descriptor) return POSIX.IO_Count;
   procedure Set_Length
     (AD     : AIO_Descriptor;
      Length : POSIX.IO_Count);
   function Get_Priority_Reduction (AD : AIO_Descriptor) return Natural;
   procedure Set_Priority_Reduction
     (AD                 : AIO_Descriptor;
      Priority_Reduction : Natural);
   function Get_Event (AD : AIO_Descriptor) return POSIX.Signals.Signal_Event;
   procedure Set_Event
     (AD    : AIO_Descriptor;
      Event : POSIX.Signals.Signal_Event);
   function Get_Operation (AD : AIO_Descriptor) return List_IO_Operations;
   procedure Set_Operation
     (AD        : AIO_Descriptor;
      Operation : List_IO_Operations);

   procedure Read (AD : AIO_Descriptor);
   procedure Write (AD : AIO_Descriptor);

   type AIO_Descriptor_List is
      array (Positive range <>) of aliased AIO_Descriptor;

   --  .... "aliased" is not in POSIX.5b

   --  ????? Change POSIX.5b?
   --  This array should probably have been a private (or limited private)
   --  type, so that we could hide a copy of the C-style array of pointers
   --  inside the object.  As things stand, the need to be able to recover
   --  an Ada pointer to the buffer of each AIOCB is forcing us to put a
   --  wrapper around each AIOCB pointer, and so we cannot pass the array
   --  directly as argument to any C system call.  Instead, we have to
   --  dynamically create a C-style array at the point of call.

   procedure List_IO_No_Wait
     (List  : in out AIO_Descriptor_List;
      Event : POSIX.Signals.Signal_Event);
   procedure List_IO_Wait
     (List           : in out AIO_Descriptor_List;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals);

   type AIO_Status is
     (In_Progress,
      Completed_Successfully,
      Canceled);
   function Get_AIO_Status (AD : AIO_Descriptor) return AIO_Status;
   function Get_AIO_Error_Code (AD : AIO_Descriptor) return POSIX.Error_Code;
   function Get_Bytes_Transferred (AD : AIO_Descriptor) return POSIX.IO_Count;

   type Cancelation_Status is
     (Canceled,
      Not_Canceled,
      All_Done);
   function Cancel (AD : AIO_Descriptor) return Cancelation_Status;
   function Cancel (File : POSIX.IO.File_Descriptor) return Cancelation_Status;

   procedure Await_IO_Or_Timeout
     (AD             : AIO_Descriptor;
      Timeout        : POSIX.Timespec;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals);
   procedure Await_IO
     (AD             : AIO_Descriptor;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals);
   procedure Await_IO_Or_Timeout
     (List           : AIO_Descriptor_List;
      Timeout        : POSIX.Timespec;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals);
   procedure Await_IO
     (List           : AIO_Descriptor_List;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals);
   procedure Synchronize_File (AD : AIO_Descriptor);
   procedure Synchronize_Data (AD : AIO_Descriptor);

private

   --  The following wrapper is needed around the C struct aiocb
   --  in order to preserve the Ada "fat" pointer (including constraint info)
   --  that we need in order to be able to recover the range constraint of the
   --  Stream_Element_Array object referenced by the aiocb.
   --  Our code is going to rely on the component C being allocated in first
   --  position.  There will be code in the package body to verify this
   --  assumption.

   type Aiocb_Wrapper is record
      C : aliased POSIX.C.struct_aiocb;
      P : IO_Array_Pointer;
   end record;
   type AIO_Descriptor is access Aiocb_Wrapper;

end POSIX.Asynchronous_IO;

------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                     P O S I X . F I L E _ S T A T U S                    --
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
     POSIX.Calendar,
     POSIX.IO,
     POSIX.Permissions,
     POSIX.Process_Identification;
package POSIX.File_Status is

   type Status is private;

   function Get_File_Status (Pathname : POSIX.Pathname)
      return Status;
   function Get_File_Status (File : POSIX.IO.File_Descriptor)
      return Status;
   --  Get_Link_Status is not in the IEEE standard
   function Get_Link_Status (Pathname : POSIX.Pathname)
      return Status;

   type File_ID is private;
   type Device_ID is private;
   subtype Links is Natural range 0 .. POSIX.Link_Limit_Maxima'Last;
   function Permission_Set_Of (File_Status : Status)
      return POSIX.Permissions.Permission_Set;
   function File_ID_Of (File_Status : Status)
      return File_ID;
   function Device_ID_Of (File_Status : Status)
      return Device_ID;
   function Link_Count_Of (File_Status : Status)
      return Links;
   function Owner_Of (File_Status : Status)
      return POSIX.Process_Identification.User_ID;
   function Group_Of (File_Status : Status)
      return POSIX.Process_Identification.Group_ID;
   function Size_Of (File_Status : Status)
      return POSIX.IO_Count;
   function Last_Access_Time_Of (File_Status : Status)
      return POSIX.Calendar.POSIX_Time;
   function Last_Modification_Time_Of (File_Status : Status)
      return POSIX.Calendar.POSIX_Time;
   function Last_Status_Change_Time_Of (File_Status : Status)
      return POSIX.Calendar.POSIX_Time;
   function Is_Block_Special_File (File_Status : Status)
      return Boolean;
   function Is_Character_Special_File (File_Status : Status)
      return Boolean;
   function Is_Directory (File_Status : Status)
      return Boolean;
   function Is_FIFO (File_Status : Status)
      return Boolean;
   --  Is_Symbolic_Link is not in the IEEE standard
   function Is_Symbolic_Link (File_Status : Status)
      return Boolean;
   function Is_Regular_File (File_Status : Status)
      return Boolean;
   --  Is_Socket is part of the POSIX.5c [D2]
   function Is_Socket (File_Status : Status)
      return Boolean;
   function Is_Shared_Memory (File_Status : Status)
     return Boolean;
   function Is_Message_Queue (File_Status : Status)
      return Boolean;
   function Is_Semaphore (File_Status : Status)
      return Boolean;

private
   type Status is new POSIX.C.struct_stat;
   type File_ID is new POSIX.C.ino_t;
   type Device_ID is new POSIX.C.dev_t;
end POSIX.File_Status;

------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--        P O S I X . C O N F I G U R A B L E _ F I L E _ L I M I T S       --
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

with POSIX,
     POSIX.IO,
     POSIX.Limits,
     POSIX.Options;
package POSIX.Configurable_File_Limits is

   function Link_Is_Limited (Pathname : POSIX.Pathname)  --  obsolescent
      return Boolean;                                    --  obsolescent
   function Link_Is_Limited (File : POSIX.IO.File_Descriptor)
      return Boolean;                                    --  obsolescent
   function Link_Limit (Pathname : POSIX.Pathname)       --  obsolescent
      return POSIX.Link_Limit_Maxima;                    --  obsolescent
   function Link_Limit (File : POSIX.IO.File_Descriptor) --  obsolescent
      return POSIX.Link_Limit_Maxima;                    --  obsolescent
   function Links_Are_Limited (Pathname : POSIX.Pathname)
      return Boolean
      renames Link_Is_Limited;
   function Links_Are_Limited (File : POSIX.IO.File_Descriptor)
      return Boolean
      renames Link_Is_Limited;
   function Links_Maximum (Pathname : POSIX.Pathname)
      return POSIX.Limits.Links_Maxima
      renames Link_Limit;
   function Links_Maximum (File : POSIX.IO.File_Descriptor)
      return POSIX.Limits.Links_Maxima
      renames Link_Limit;

   function Input_Line_Is_Limited (Pathname : POSIX.Pathname)
      return Boolean;
   function Input_Line_Is_Limited (File : POSIX.IO.File_Descriptor)
      return Boolean;
   function Input_Line_Limit (Pathname : POSIX.Pathname) --  obsolescent
      return POSIX.Input_Line_Limit_Maxima;              --  obsolescent
   function Input_Line_Limit (File : POSIX.IO.File_Descriptor)
      return POSIX.Input_Line_Limit_Maxima;              --  obsolescent
   function Input_Line_Maximum (Pathname : POSIX.Pathname)
      return POSIX.Limits.Input_Line_Maxima
      renames Input_Line_Limit;
   function Input_Line_Maximum (File : POSIX.IO.File_Descriptor)
      return POSIX.Limits.Input_Line_Maxima
      renames Input_Line_Limit;

   function Input_Queue_Is_Limited (Pathname : POSIX.Pathname)
      return Boolean;
   function Input_Queue_Is_Limited (File : POSIX.IO.File_Descriptor)
      return Boolean;
   function Input_Queue_Limit (Pathname : POSIX.Pathname) --  obsolescent
      return POSIX.Input_Queue_Limit_Maxima;              --  obsolescent
   function Input_Queue_Limit (File : POSIX.IO.File_Descriptor)
      return POSIX.Input_Queue_Limit_Maxima;              --  obsolescent
   function Input_Queue_Maximum (Pathname : POSIX.Pathname)
      return POSIX.Limits.Input_Queue_Maxima
      renames Input_Queue_Limit;
   function Input_Queue_Maximum (File : POSIX.IO.File_Descriptor)
      return POSIX.Limits.Input_Queue_Maxima
      renames Input_Queue_Limit;

   function Filename_Is_Limited (Pathname : POSIX.Pathname)
      return Boolean;
   function Filename_Is_Limited (File : POSIX.IO.File_Descriptor)
      return Boolean;
   function Filename_Limit (Pathname : POSIX.Pathname)   --  obsolescent
      return POSIX.Filename_Limit_Maxima;                --  obsolescent
   function Filename_Limit (File : POSIX.IO.File_Descriptor)
      return POSIX.Filename_Limit_Maxima;                --  obsolescent
   function Filename_Maximum (Pathname : POSIX.Pathname)
      return POSIX.Limits.Filename_Maxima
      renames Filename_Limit;
   function Filename_Maximum (File : POSIX.IO.File_Descriptor)
      return POSIX.Limits.Filename_Maxima
      renames Filename_Limit;
   function Pathname_Is_Limited (Pathname : POSIX.Pathname)
      return Boolean;
   function Pathname_Is_Limited (File : POSIX.IO.File_Descriptor)
      return Boolean;
   function Pathname_Limit (Pathname : POSIX.Pathname)   --  obsolescent
      return POSIX.Pathname_Limit_Maxima;                --  obsolescent
   function Pathname_Limit (File : POSIX.IO.File_Descriptor)
      return POSIX.Pathname_Limit_Maxima;                --  obsolescent
   function Pathname_Maximum (Pathname : POSIX.Pathname)
      return POSIX.Limits.Pathname_Maxima
      renames Pathname_Limit;
   function Pathname_Maximum (File : POSIX.IO.File_Descriptor)
      return POSIX.Limits.Pathname_Maxima
      renames Pathname_Limit;

   function Pipe_Length_Is_Limited (Pathname : POSIX.Pathname)
      return Boolean;
   function Pipe_Length_Is_Limited (File : POSIX.IO.File_Descriptor)
      return Boolean;
   function Pipe_Length_Limit (Pathname : POSIX.Pathname) --  obsolescent
      return POSIX.Pipe_Limit_Maxima;                     --  obsolescent
   function Pipe_Length_Limit (File : POSIX.IO.File_Descriptor)
      return POSIX.Pipe_Limit_Maxima;
   function Pipe_Length_Maximum (Pathname : POSIX.Pathname)
      return POSIX.Limits.Pipe_Length_Maxima
      renames Pipe_Length_Limit;
   function Pipe_Length_Maximum (File : POSIX.IO.File_Descriptor)
      return POSIX.Limits.Pipe_Length_Maxima
      renames Pipe_Length_Limit;

   function Change_Owner_Is_Restricted (Pathname : POSIX.Pathname)
      return POSIX.Options.Change_Owner_Restriction;
   function Change_Owner_Is_Restricted (File : POSIX.IO.File_Descriptor)
      return POSIX.Options.Change_Owner_Restriction;
   function Filename_Is_Truncated (Pathname : POSIX.Pathname)
      return POSIX.Options.Filename_Truncation;
   function Filename_Is_Truncated (File : POSIX.IO.File_Descriptor)
      return POSIX.Options.Filename_Truncation;

   function Synchronized_IO_Is_Supported (Pathname : POSIX.Pathname)
      return Boolean;
   function Synchronized_IO_Is_Supported (File : POSIX.IO.File_Descriptor)
      return Boolean;
   function Asynchronous_IO_Is_Supported (Pathname : POSIX.Pathname)
      return Boolean;
   function Asynchronous_IO_Is_Supported (File : POSIX.IO.File_Descriptor)
      return Boolean;
   function Prioritized_IO_Is_Supported (Pathname : POSIX.Pathname)
      return Boolean;
   function Prioritized_IO_Is_Supported (File : POSIX.IO.File_Descriptor)
      return Boolean;

   --  Additions from POSIX.5c [Draft 2]

   --  5.4.1 Socket Buffer Limits from P1003.5c

   function Socket_Buffer_Is_Limited (File : POSIX.IO.File_Descriptor)
     return Boolean;
   function Socket_Buffer_Is_Limited (Pathname : POSIX.Pathname)
     return Boolean;

   --  The following deviate from POSIX.5c/D1

   function Socket_Buffer_Limit (Pathname : POSIX.Pathname)
     return POSIX.Limits.Socket_Buffer_Maxima;
   function Socket_Buffer_Limit (File : POSIX.IO.File_Descriptor)
     return POSIX.Limits.Socket_Buffer_Maxima;

   --  Craig Meyers has in D1:

   --  function Socket_Buffer_Maximum
   --     return POSIX.Limits.Socket_Buffer_Maxima;

end POSIX.Configurable_File_Limits;

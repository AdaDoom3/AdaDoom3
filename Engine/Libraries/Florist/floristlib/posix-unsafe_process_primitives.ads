------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--       P O S I X . U N S A F E _ P R O C E S S _ P R I M I T I V E S      --
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
     POSIX.Process_Environment,
     POSIX.Process_Identification;
package POSIX.Unsafe_Process_Primitives is

   function Fork return POSIX.Process_Identification.Process_ID;
   procedure Exec
     (Pathname : POSIX.Pathname;
      Arg_List : POSIX.POSIX_String_List := POSIX.Empty_String_List;
      Env_List : POSIX.Process_Environment.Environment);
   procedure Exec
     (Pathname : POSIX.Pathname;
      Arg_List : POSIX.POSIX_String_List := POSIX.Empty_String_List);
   procedure Exec_Search
     (Filename : POSIX.Filename;
      Arg_List : POSIX.POSIX_String_List := POSIX.Empty_String_List;
      Env_List : POSIX.Process_Environment.Environment);
   procedure Exec_Search
     (Filename : POSIX.Filename;
      Arg_List : POSIX.POSIX_String_List := POSIX.Empty_String_List);

end POSIX.Unsafe_Process_Primitives;

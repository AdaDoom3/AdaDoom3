------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--            P O S I X . M E M O R Y _ R A N G E _ L O C K I N G           --
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

with System,
     System.Storage_Elements;
package POSIX.Memory_Range_Locking is
   procedure Lock_Range
     (First  : System.Address;
      Length : System.Storage_Elements.Storage_Offset);
   procedure Unlock_Range
     (First  : System.Address;
      Length : System.Storage_Elements.Storage_Offset);
end POSIX.Memory_Range_Locking;

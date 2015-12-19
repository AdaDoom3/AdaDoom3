------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                   P O S I X . U S E R _ D A T A B A S E                  --
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
     POSIX.C,
     POSIX_Process_Identification;
package POSIX.User_Database is

   type User_Database_Item is private;

   --  operations to get information From a User_Database_Entry

   function User_Name_Of (DB_Item : User_Database_Item)
      return POSIX.POSIX_String;
   pragma Inline (User_Name_Of);
   function User_ID_Of (DB_Item : User_Database_Item)
      return POSIX_Process_Identification.User_ID;
   pragma Inline (User_ID_Of);
   function Group_ID_Of (DB_Item : User_Database_Item)
      return POSIX_Process_Identification.Group_ID;
   pragma Inline (Group_ID_Of);
   function Initial_Directory_Of (DB_Item : User_Database_Item)
      return POSIX.POSIX_String;
   pragma Inline (Initial_Directory_Of);
   function Initial_Program_Of (DB_Item : User_Database_Item)
      return POSIX.POSIX_String;
   pragma Inline (Initial_Program_Of);

   --  operations to Get User_Database_Item

   function Get_User_Database_Item
         (UID : POSIX_Process_Identification.User_ID)
      return User_Database_Item;
   function Get_User_Database_Item (Name : POSIX.POSIX_String)
      return User_Database_Item;

private
   type User_Database_Item is new POSIX.C.passwd_ptr;
   --  .... Change POSIX.5?
   --  This direct mapping to the C interface means these operations
   --  are not tasking-safe.  However, we see no reasonable alternative.
   --  See comments in POSIX.Group_Database for more detail.
end POSIX.User_Database;

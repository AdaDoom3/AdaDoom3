------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                  P O S I X . G R O U P _ D A T A B A S E                 --
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
     POSIX.Process_Identification;
package POSIX.Group_Database is

   type Group_Database_Item is private;
   type Group_ID_List is private;

   --  operations to get information from a Group_Database_Item
   function Group_Name_Of (DB_Item : Group_Database_Item)
      return POSIX.POSIX_String;
   function Group_ID_Of (DB_Item : Group_Database_Item)
      return POSIX.Process_Identification.Group_ID;
   function Group_ID_List_Of (DB_Item : Group_Database_Item)
      return Group_ID_List;

   --  iterator over the Group_ID_List
   generic
   with procedure Action
     (ID   : POSIX.POSIX_String;
      Quit : in out Boolean);
   procedure For_Every_Member (List : Group_ID_List);
   function Length (Member_List : Group_ID_List) return Natural;

   --  operations to get a Group_Database_Item
   function Get_Group_Database_Item
         (GID : POSIX.Process_Identification.Group_ID)
      return Group_Database_Item;
   function Get_Group_Database_Item (Name : POSIX.POSIX_String)
     return Group_Database_Item;

private
   --  .... Change POSIX.5b?
   --  For correct tasking-safe operation, without storage leakage,
   --  we want to make a copy of the entire group database item
   --  inside each value of type Group_Database_Item.
   --  The problem is that there is no fixed size,
   --  so we would like to make the size depend on a discriminant.
   --  We can't do this, as the interface stands.
   --  This leaves us few options:
   --  (a) impose a fixed size limit, which might overflow;
   --  (b) use dynamic allocation, risking storage leakage;
   --  (c) use the raw C interfaces, risking tasking unsafety
   --      and also storage leakage.
   --  Note that using the new thread-safe operations,
   --  getgrgid_r and getgrnam_r, does not solve our problem,
   --  since we would still have to provide space to hold the strings.
   --  We choose to use the raw C interfaces, since the other
   --  alternatives are not significantly more attractive.
   --  See also POSIX.User_Database.

   --  .... Another modification we want to make in POSIX.5b is to
   --  make types "limited private" where appropriate. For example, we
   --  do not want to compare the whole contents of two lists using "="
   --  operation if we want to follow the POSIX.1 definition of group
   --  structure. We could provide the operation, of course, with some
   --  expensive structures and operations. However, this is not worth
   --  especially when we could perform the same operation using other
   --  operations (For_Every_Member).
   type Group_Database_Item is new POSIX.C.group_ptr;
   type Group_ID_List is new POSIX.C.char_ptr_ptr;
end POSIX.Group_Database;

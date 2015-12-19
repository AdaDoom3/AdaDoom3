------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--          P O S I X . P R O C E S S _ I D E N T I F I C A T I O N         --
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

with POSIX.C;
package POSIX.Process_Identification is

   --  Process Identification

   type Process_ID is private;
   Null_Process_ID : constant Process_ID;
   System_Process_ID : constant Process_ID;
   function Get_Process_ID return Process_ID;
   function Get_Parent_Process_ID return Process_ID;
   function Image (ID : Process_ID)
      return Standard.String;
   function Value (Str : Standard.String)
      return Process_ID;

   --  Process Group Identification

   type Process_Group_ID is private;
   function Get_Process_Group_ID
      return Process_Group_ID;
   procedure Set_Process_Group_ID
     (Process       : Process_ID := Get_Process_ID;
      Process_Group : Process_Group_ID := Get_Process_Group_ID);
   procedure Create_Process_Group
     (Process       :  Process_ID;
      Process_Group : out Process_Group_ID);
   procedure Create_Session (Session_Leader : out Process_Group_ID);
   function Image (ID : Process_Group_ID)
      return Standard.String;
   function Value (Str : Standard.String)
      return Process_Group_ID;

   --  User Identification

   type User_ID is private;
   function Get_Real_User_ID return User_ID;
   function Get_Effective_User_ID return User_ID;
   procedure Set_User_ID (ID : User_ID);
   function Get_Login_Name return POSIX.POSIX_String;
   function Image (ID : User_ID) return Standard.String;
   function Value (Str : Standard.String) return User_ID;

   --  User Group Identification

   type Group_ID is private;
   function Get_Real_Group_ID return Group_ID;
   function Get_Effective_Group_ID return Group_ID;
   procedure Set_Group_ID (ID : Group_ID);
   subtype Group_List_Index is Positive range 1 .. POSIX.Groups_Maxima'Last;
   type Group_List is array (Group_List_Index range <>) of aliased Group_ID;
   --  ... Applications may not rely on "aliased" here being portable.
   --  We have added it to allow for simpler implementation.
   function Get_Groups return Group_List;
   function Image (ID : Group_ID) return Standard.String;
   function Value (Str : Standard.String) return Group_ID;

private
   type Process_ID is new POSIX.C.pid_t;
   Null_Process_ID : constant Process_ID := 0;
   System_Process_ID : constant Process_ID := 1;
   --  The process ID value 1 is reserved for use by the system.
   type Process_Group_ID is new POSIX.C.pid_t;
   type User_ID is new POSIX.C.uid_t;
   type Group_ID is new POSIX.C.gid_t;
end POSIX.Process_Identification;

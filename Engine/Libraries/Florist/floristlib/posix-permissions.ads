------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                     P O S I X . P E R M I S S I O N S                    --
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

with POSIX;
package POSIX.Permissions is

   type Permission is
     (Others_Execute, Others_Write, Others_Read,
      Group_Execute,  Group_Write,  Group_Read,
      Owner_Execute,  Owner_Write,  Owner_Read,
      Set_Group_ID,   Set_User_ID);

   type Permission_Set is array (Permission) of Boolean;

   Owner_Permission_Set : constant Permission_Set := Permission_Set'
     (Owner_Read | Owner_Write | Owner_Execute => True,
      others => False);

   Group_Permission_Set : constant Permission_Set := Permission_Set'
     (Group_Read | Group_Write | Group_Execute => True,
      others => False);

   Others_Permission_Set : constant Permission_Set := Permission_Set'
     (Others_Read | Others_Write | Others_Execute => True,
      others => False);

   Access_Permission_Set : constant Permission_Set := Permission_Set'
     (Owner_Read  | Owner_Write  | Owner_Execute => True,
      Group_Read  | Group_Write  | Group_Execute => True,
      Others_Read | Others_Write | Others_Execute => True,
      others => False);

   Set_Group_ID_Set : constant Permission_Set := Permission_Set'
     (Set_Group_ID => True, others => False);
   Set_User_ID_Set : constant Permission_Set := Permission_Set'
     (Set_User_ID => True, others => False);

   --  POSIX Permission-oriented operations

   function Get_Allowed_Process_Permissions
       return Permission_Set;
   procedure Set_Allowed_Process_Permissions
            (Permissions : Permission_Set);
   procedure Set_Allowed_Process_Permissions
            (Permissions :     Permission_Set;
             Old_Perms   : out Permission_Set);

end POSIX.Permissions;

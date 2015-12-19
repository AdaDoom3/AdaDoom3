------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--             P O S I X . P R O C E S S _ E N V I R O N M E N T            --
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

with Unchecked_Conversion;
package POSIX.Process_Environment is
   pragma Elaborate_Body;

   --  Process Parameters

   function Argument_List return POSIX.POSIX_String_List;

   --  .... Change POSIX.5?
   --  By rights, this function should have been a procedure.
   --  As a function, it is very awkward, since the type POSIX_String_List
   --  is limited (i.e. we cannot assign the value returned by the
   --  function above).  Not only is this inefficient, it forces storage
   --  leakage, unless we implement POSIX_String_List as a controlled type
   --  with automatic storage reclamation --  which is still less efficient,
   --  and which nullifies the original reason for making POSIX_String_List
   --  a limited type!

   --  Environment Variables

   type Environment is limited private;
   procedure Copy_From_Current_Environment
     (Env : in out Environment);
   procedure Copy_To_Current_Environment
     (Env : Environment);
   procedure Copy_Environment
     (Source : Environment;
      Target : in out Environment);
   function Environment_Value_Of
     (Name      : POSIX.POSIX_String;
      Env       : Environment;
      Undefined : POSIX.POSIX_String := "")
     return POSIX.POSIX_String;
   function Environment_Value_Of
     (Name      : POSIX.POSIX_String;
      Undefined : POSIX.POSIX_String := "")
     return POSIX.POSIX_String;
   function Is_Environment_Variable
     (Name : POSIX.POSIX_String;
      Env  : Environment)
     return Boolean;
   function Is_Environment_Variable
     (Name : POSIX.POSIX_String)
     return Boolean;
   procedure Clear_Environment
     (Env : in out Environment);
   procedure Clear_Environment;
   procedure Set_Environment_Variable
     (Name  : POSIX.POSIX_String;
      Value : POSIX.POSIX_String;
      Env   : in out Environment);
   procedure Set_Environment_Variable
     (Name  : POSIX.POSIX_String;
      Value : POSIX.POSIX_String);
   procedure Delete_Environment_Variable
     (Name  : POSIX.POSIX_String;
      Env   : in out Environment);
   procedure Delete_Environment_Variable
     (Name  : POSIX.POSIX_String);
   function Length (Env : Environment) return Natural;
   function Length return Natural;
   generic
      with procedure Action
        (Name  : POSIX.POSIX_String;
         Value : POSIX.POSIX_String;
         Quit  : in out Boolean);
   procedure For_Every_Environment_Variable
     (Env :     Environment);
   generic
      with procedure Action
        (Name  : POSIX.POSIX_String;
         Value : POSIX.POSIX_String;
         Quit  : in out Boolean);
   procedure For_Every_Current_Environment_Variable;

   --  Process Working Directory

   procedure Change_Working_Directory
     (Directory_Name : POSIX.Pathname);
   function Get_Working_Directory return POSIX.Pathname;

private
   type Environment_List;
   type Environment is access Environment_List;

   function To_Environment is
      new Unchecked_Conversion (POSIX_String_List, Environment);

   function To_POSIX_String_List is
      new Unchecked_Conversion (Environment, POSIX_String_List);

end POSIX.Process_Environment;

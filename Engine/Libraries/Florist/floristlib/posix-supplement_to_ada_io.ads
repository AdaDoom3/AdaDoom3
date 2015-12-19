------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--            P O S I X . S U P P L E M E N T _ T O _ A D A _ I O           --
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

with Ada.IO_Exceptions,
     Ada.Text_IO,
     POSIX.IO,
     POSIX.Permissions;
package POSIX.Supplement_to_Ada_IO is

   type File_Structure_Values is (Regular, FIFO);
   type Terminal_Input_Values is (Lines, Characters);
   type Possible_File_Descriptor (Valid : Boolean := False) is
     record
      case Valid is
        when True =>
         Descriptor : POSIX.IO.File_Descriptor;
        when False => null;
      end case;
     end record;
   type Form_Values_for_Open is
      record
         Append           : Boolean := False;
         Blocking         : POSIX.Text_IO_Blocking_Behavior
                          := POSIX.IO_Blocking_Behavior;
         Terminal_Input   : Terminal_Input_Values := Lines;
         Page_Terminators : Boolean := True;
         File_Descriptor  : Possible_File_Descriptor;
      end record;
   type Form_Values_for_Create is
      record
         Permission_Mask  : POSIX.Permissions.Permission_Set
                          := POSIX.Permissions.Access_Permission_Set;
         Blocking         : POSIX.Text_IO_Blocking_Behavior
                          := POSIX.IO_Blocking_Behavior;
         Terminal_Input   : Terminal_Input_Values := Lines;
         File_Structure   : File_Structure_Values := Regular;
         Page_Terminators : Boolean := True;
      end record;

   function Form_String (Val : Form_Values_for_Open)
      return String;
   function Form_Value (Str : String)
      return Form_Values_for_Open;
   function Form_String (Val : Form_Values_for_Create)
      return String;
   function Form_Value (Str : String)
      return Form_Values_for_Create;

   procedure Flush_All;
   procedure Flush_Text_IO (File : Ada.Text_IO.File_Type);

   generic
      type File_Type is limited private;
   procedure Flush_Sequential_IO (File : File_Type);

   generic
      type File_Type is limited private;
   procedure Flush_Direct_IO (File : File_Type);

   --  .... Change POSIX.5?
   --  This is a terrible interface!
   --  These generic procedures can only be implemented by trickery,
   --  since we have no way of getting a handle for the corresponding
   --  instantiation, or the set of files that may be open using those
   --  the particular instantiation.

   Use_Error : exception renames Ada.IO_Exceptions.Use_Error;

end POSIX.Supplement_to_Ada_IO;

------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                  P O S I X . M E S S A G E _ Q U E U E S                 --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 1996-1997 Florida State University             --
--                     Copyright (C) 1998-2010, AdaCore                     --
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
     POSIX.IO,
     POSIX.Configurable_System_Limits,
     POSIX.Permissions,
     POSIX.Signals;
pragma Elaborate_All (POSIX.Configurable_System_Limits);
package POSIX.Message_Queues is

   type Message_Queue_Descriptor is private;
   type Attributes is private;
   type Message_Queue_Options is new POSIX.Option_Set;
   Non_Blocking : constant Message_Queue_Options :=
       Message_Queue_Options (POSIX.IO.Non_Blocking);
   subtype Message_Priority is
     Integer range 0 ..
       POSIX.Configurable_System_Limits.Message_Priority_Maximum;

   --  ?????
   --  POSIX.5b may need revision here.  By definining the range
   --  of Message_Priority sufficiently precisely, we end up raising
   --  Constraint_Error in all situations where the priority is out
   --  of the supported range, but the standard says we should raise
   --  POSIX_Error with Invalid_Argument in these situations.  In
   --  particular, see the procedure Send.  Technically, it is not
   --  necessary to list Constraint_Error as a possibility in the
   --  Error Handling section of the standard, since it follows from
   --  the Ada language definition that Constraint_Error is raised
   --  under such circumstances, but it might be better if there were
   --  an explicit note pointing this out.

   procedure Set_Max_Messages
     (Attrs : in out Attributes;
      Value : Natural);
   function Get_Max_Messages (Attrs : Attributes) return Natural;
   procedure Set_Message_Length
     (Attrs : in out Attributes;
      Value : Natural);
   function Get_Message_Length (Attrs : Attributes) return Natural;
   procedure Set_Options
     (Attrs : in out Attributes;
      Value : Message_Queue_Options);
   function Get_Options (Attrs : Attributes) return Message_Queue_Options;
   function Get_Message_Count (Attrs : Attributes) return Natural;
   function Open
     (Name           : POSIX.POSIX_String;
      Mode           : POSIX.IO.File_Mode;
      Options        : POSIX.IO.Open_Option_Set := POSIX.IO.Empty_Set;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
     return Message_Queue_Descriptor;
   function Open_Or_Create
     (Name           : POSIX.POSIX_String;
      Mode           : POSIX.IO.File_Mode;
      Permissions    : POSIX.Permissions.Permission_Set;
      Options        : POSIX.IO.Open_Option_Set := POSIX.IO.Empty_Set;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
     return Message_Queue_Descriptor;
   function Open_Or_Create
     (Name           : POSIX.POSIX_String;
      Mode           : POSIX.IO.File_Mode;
      Permissions    : POSIX.Permissions.Permission_Set;
      Options        : POSIX.IO.Open_Option_Set := POSIX.IO.Empty_Set;
      Attrs          : Attributes;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
     return Message_Queue_Descriptor;
   procedure Close (MQ : in out Message_Queue_Descriptor);
   procedure Unlink_Message_Queue (Name :  POSIX.POSIX_String);
   procedure Send
     (MQ             : Message_Queue_Descriptor;
      Message        : Ada.Streams.Stream_Element_Array;
      Priority       : Message_Priority;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals);
   procedure Receive
     (MQ             : Message_Queue_Descriptor;
      Message        : out Ada.Streams.Stream_Element_Array;
      Last           : out Ada.Streams.Stream_Element_Offset;
      Priority       : out Message_Priority;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals);
   generic
     type Message_Type is private;
   package Generic_Message_Queues is
      procedure Send
        (MQ             : Message_Queue_Descriptor;
         Message        : Message_Type;
         Priority       : Message_Priority;
         Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals);
      procedure Receive
        (MQ             : Message_Queue_Descriptor;
         Message        : out Message_Type;
         Priority       : out Message_Priority;
         Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals);
      function Get_Error_Buffer return Ada.Streams.Stream_Element_Array;
   end Generic_Message_Queues;
   procedure Request_Notify
     (MQ    : Message_Queue_Descriptor;
      Event : POSIX.Signals.Signal_Event);
   procedure Remove_Notify (MQ : Message_Queue_Descriptor);
   procedure Set_Attributes
     (MQ        : Message_Queue_Descriptor;
      New_Attrs : Attributes;
      Old_Attrs : out Attributes);
   procedure Set_Attributes
     (MQ        : Message_Queue_Descriptor;
      New_Attrs : Attributes);
   function Get_Attributes (MQ : Message_Queue_Descriptor) return Attributes;

private
   type Message_Queue_Descriptor is new POSIX.C.mqd_t;
   type Attributes is record
      Attrs : aliased POSIX.C.struct_mq_attr := (0, 0, 0, 0);
   end record;
end POSIX.Message_Queues;

--  .... Change POSIX.5b?????
--  These interfaces would be easier to use if it were possible to specify
--  a message buffer via its address, rather than an unconstrained array
--  parameter.  The present interface forces people to copy data into the
--  buffer, and possibly also out from the buffer.  It would be more
--  convenient and efficient to pass references to data to these calls and
--  leave the data itself in-place.

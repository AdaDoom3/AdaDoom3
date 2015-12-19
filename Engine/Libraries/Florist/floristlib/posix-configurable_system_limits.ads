------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--      P O S I X . C O N F I G U R A B L E _ S Y S T E M _ L I M I T S     --
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

with POSIX.C,
     POSIX.Limits,
     POSIX.Options;
package POSIX.Configurable_System_Limits is

   function Asynchronous_IO_Is_Supported
     return POSIX.Options.Asynchronous_IO_Support;
   function File_Synchronization_Is_Supported
     return POSIX.Options.File_Synchronization_Support;
   function Job_Control_Supported                          -- obsolescent
     return POSIX.Job_Control_Support;                     -- obsolescent
   function Job_Control_Is_Supported
     return POSIX.Options.Job_Control_Support
     renames Job_Control_Supported;
   function Memory_Mapped_Files_Are_Supported
     return POSIX.Options.Memory_Mapped_Files_Support;
   function Memory_Locking_Is_Supported
     return POSIX.Options.Memory_Locking_Support;
   function Memory_Range_Locking_Is_Supported
     return POSIX.Options.Memory_Range_Locking_Support;
   function Memory_Protection_Is_Supported
     return POSIX.Options.Memory_Protection_Support;
   function Message_Queues_Are_Supported
     return POSIX.Options.Message_Queues_Support;
   function Mutex_Priority_Ceiling_Is_Supported
     return POSIX.Options.Mutex_Priority_Ceiling_Support;
   function Mutex_Priority_Inheritance_Is_Supported
     return POSIX.Options.Mutex_Priority_Inheritance_Support;
   function Mutexes_Are_Supported
     return POSIX.Options.Mutexes_Support;
   function Prioritized_IO_Is_Supported
     return POSIX.Options.Prioritized_IO_Support;
   function Process_Shared_Is_Supported
     return POSIX.Options.Process_Shared_Support;
   function Priority_Process_Scheduling_Is_Supported
     return POSIX.Options.Priority_Process_Scheduling_Support;
   function Priority_Task_Scheduling_Is_Supported
     return POSIX.Options.Priority_Task_Scheduling_Support;
   function Realtime_Signals_Are_Supported
     return POSIX.Options.Realtime_Signals_Support;
   function Saved_IDs_Supported                        --  obsolescent
    return POSIX.Saved_IDs_Support;                    --  obsolescent
   function Saved_IDs_Are_Supported
     return POSIX.Options.Saved_IDs_Support
     renames Saved_IDs_Supported;
   function Semaphores_Are_Supported
     return POSIX.Options.Semaphores_Support;
   function Shared_Memory_Objects_Are_Supported
     return POSIX.Options.Shared_Memory_Objects_Support;
   function Synchronized_IO_Is_Supported
     return POSIX.Options.Synchronized_IO_Support;
   function Timers_Are_Supported
     return POSIX.Options.Timers_Support;

   type POSIX_Version is new POSIX.C.long;
   function System_POSIX_Version
     return POSIX_Version;
   function System_POSIX_Ada_Version
     return POSIX_Version;
   function Argument_List_Maximum
     return POSIX.Limits.Argument_List_Maxima;
   function Asynchronous_IO_Maximum
     return POSIX.Limits.Asynchronous_IO_Maxima;
   function Asynchronous_IO_Priority_Delta_Maximum
     return POSIX.Limits.Asynchronous_IO_Priority_Delta_Maxima;
   function Child_Processes_Maximum
     return POSIX.Limits.Child_Processes_Maxima;
   function Groups_Maximum
     return POSIX.Limits.Groups_Maxima;
   function List_IO_Maximum
     return POSIX.Limits.List_IO_Maxima;
   function Open_Message_Queues_Maximum
     return POSIX.Limits.Open_Message_Queues_Maxima;
   function Message_Priority_Maximum
     return POSIX.Limits.Message_Priority_Maxima;
   function Open_Files_Maximum
     return POSIX.Limits.Open_Files_Maxima;
   function Page_Size
     return POSIX.Limits.Page_Size_Range;
   function Queued_Signals_Maximum
     return POSIX.Limits.Queued_Signals_Maxima;
   function Realtime_Signals_Maximum
     return POSIX.Limits.Realtime_Signals_Maxima;
   function Semaphores_Maximum
     return POSIX.Limits.Semaphores_Maxima;
   function Semaphores_Value_Maximum
     return POSIX.Limits.Semaphores_Value_Maxima;
   function Stream_Maximum                            -- obsolescent
     return POSIX.Stream_Maxima;                      -- obsolescent
   function Streams_Maximum
     return POSIX.Limits.Streams_Maxima
     renames Stream_Maximum;
   function Timers_Maximum
     return POSIX.Limits.Timers_Maxima;
   function Timer_Overruns_Maximum
     return POSIX.Limits.Timer_Overruns_Maxima;
   function Time_Zone_String_Maximum
     return POSIX.Limits.Time_Zone_String_Maxima;

   --  POSIX.5c/D4 extensions

   function Internet_Datagram_Is_Supported
     return POSIX.Options.Internet_Datagram_Support;
   function Internet_Protocol_Is_Supported
     return POSIX.Options.Internet_Protocol_Support;
   function Internet_Stream_Is_Supported
     return POSIX.Options.Internet_Stream_Support;
   function ISO_OSI_Protocol_Is_Supported
     return POSIX.Options.ISO_OSI_Protocol_Support;
   function Network_Management_Is_Supported
     return POSIX.Options.Network_Management_Support;
   function OSI_Connectionless_Is_Supported
     return POSIX.Options.OSI_Connectionless_Support;
   function OSI_Connection_Is_Supported
     return POSIX.Options.OSI_Connection_Support;
   function OSI_Minimal_Is_Supported
     return POSIX.Options.OSI_Minimal_Support;
   function Poll_Is_Supported
     return POSIX.Options.Poll_Support;
   function Select_Is_Supported
     return POSIX.Options.Select_Support;
   function Sockets_DNI_Is_Supported
     return POSIX.Options.Sockets_DNI_Support;
   function Socket_IO_Vector_Maximum
     return POSIX.Limits.Socket_IO_Vector_Maxima;
   function XTI_DNI_Is_Supported
     return POSIX.Options.XTI_DNI_Support;

end POSIX.Configurable_System_Limits;

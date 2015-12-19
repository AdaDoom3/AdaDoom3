------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--      P O S I X . C O N F I G U R A B L E _ S Y S T E M _ L I M I T S     --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--             Copyright (C) 1996-1997 Florida State University             --
--                     Copyright (C) 1998-2014, AdaCore                     --
--                                                                          --
--  This file is a component of FLORIST, an  implementation of an  Ada API  --
--  for the POSIX OS services, for use with  the  GNAT  Ada  compiler  and  --
--  the FSU Gnu Ada Runtime Library (GNARL).   The  interface  is intended  --
--  to be close to that specified in  IEEE STD  1003.5: 1990  and IEEE STD  --
--  1003.5b: 1996.                                                          --
--                                                                          --
--  FLORIST is free software;  you can  redistribute  it and/or  modify it  --
--  under terms of the  GNU  General  Public  License as  published by the  --
--  Free Software Foundation;  either version  2, or (at  your option) any  --
--  later version.  FLORIST is distributed  in  the hope  that  it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without  even the implied  warranty  --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE.  See  the GNU  --
--  General Public License for more details.  You  should have  received a  --
--  copy of the GNU General Public License  distributed  with  GNARL;  see  --
--  file  COPYING.  If not,  write to  the  Free  Software  Foundation, 59  --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.                   --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
------------------------------------------------------------------------------

with POSIX;

package body POSIX.Configurable_System_Limits is

   package PO renames POSIX.Options;
   package PL renames POSIX.Limits;
   use POSIX.C;

   function sysconf (c_name : int) return long;
   pragma Import (C, sysconf, sysconf_LINKNAME);

   generic
      type Support_Subtype is range <>;
      Name : int;
   function Limit return Support_Subtype;

   function Limit return Support_Subtype is
      Result : long;
   begin
      Result := sysconf (Name);
      if Result = -1 then
         return Support_Subtype'Last;
      end if;
      return Support_Subtype (Result);
   end Limit;

   function Asynchronous_IO_Is_Supported
     return PO.Asynchronous_IO_Support is
   begin
      return sysconf (SC_ASYNCHRONOUS_IO) = 1;
   end Asynchronous_IO_Is_Supported;

   function File_Synchronization_Is_Supported
     return PO.File_Synchronization_Support is
   begin
      return sysconf (SC_FSYNC) = 1;
   end File_Synchronization_Is_Supported;

   function Job_Control_Supported
     return Job_Control_Support is
   begin
      return sysconf (SC_JOB_CONTROL) = 1;
   end Job_Control_Supported;

   function Memory_Mapped_Files_Are_Supported
     return PO.Memory_Mapped_Files_Support is
   begin
      return sysconf (SC_MAPPED_FILES) = 1;
   end Memory_Mapped_Files_Are_Supported;

   function Memory_Locking_Is_Supported
     return PO.Memory_Locking_Support is
   begin
      return sysconf (SC_MEMLOCK) = 1;
   end Memory_Locking_Is_Supported;

   function Memory_Range_Locking_Is_Supported
     return PO.Memory_Range_Locking_Support is
   begin
      return sysconf (SC_MEMLOCK_RANGE) = 1;
   end Memory_Range_Locking_Is_Supported;

   function Memory_Protection_Is_Supported
      return PO.Memory_Protection_Support is
   begin
      return sysconf (SC_MEMORY_PROTECTION) = 1;
   end Memory_Protection_Is_Supported;

   function Message_Queues_Are_Supported
      return PO.Message_Queues_Support is
   begin
      return sysconf (SC_MESSAGE_PASSING) = 1;
   end Message_Queues_Are_Supported;

   function Mutex_Priority_Ceiling_Is_Supported
      return PO.Mutex_Priority_Ceiling_Support is
   begin
      return sysconf (SC_THREAD_PRIO_PROTECT) = 1;
   end Mutex_Priority_Ceiling_Is_Supported;

   function Mutex_Priority_Inheritance_Is_Supported
      return PO.Mutex_Priority_Inheritance_Support is
   begin
      return sysconf (SC_THREAD_PRIO_INHERIT) = 1;
   end Mutex_Priority_Inheritance_Is_Supported;

   function Mutexes_Are_Supported
      return PO.Mutexes_Support is
   begin
      return True;
   end Mutexes_Are_Supported;

   function Prioritized_IO_Is_Supported
      return PO.Prioritized_IO_Support is
   begin
      return sysconf (SC_PRIORITIZED_IO) = 1;
   end Prioritized_IO_Is_Supported;

   function Priority_Process_Scheduling_Is_Supported
      return PO.Priority_Process_Scheduling_Support is
   begin
      return sysconf (SC_PRIORITY_SCHEDULING) = 1;
   end Priority_Process_Scheduling_Is_Supported;

   function Priority_Task_Scheduling_Is_Supported
      return PO.Priority_Task_Scheduling_Support is
   begin
      return sysconf (SC_THREAD_PRIORITY_SCHEDULING) = 1;
   end Priority_Task_Scheduling_Is_Supported;

   function Realtime_Signals_Are_Supported
      return PO.Realtime_Signals_Support is
   begin
      return sysconf (SC_REALTIME_SIGNALS) = 1;
   end Realtime_Signals_Are_Supported;

   function Saved_IDs_Supported
      return PO.Saved_IDs_Support is
   begin
      return sysconf (SC_SAVED_IDS) = 1;
   end Saved_IDs_Supported;

   function Semaphores_Are_Supported
      return PO.Semaphores_Support is
   begin
      return sysconf (SC_SEMAPHORES) = 1;
   end Semaphores_Are_Supported;

   function Shared_Memory_Objects_Are_Supported
      return PO.Shared_Memory_Objects_Support is
   begin
      return sysconf (SC_SHARED_MEMORY_OBJECTS) = 1;
   end Shared_Memory_Objects_Are_Supported;

   function Process_Shared_Is_Supported
      return PO.Process_Shared_Support is
   begin
      return sysconf (SC_THREAD_PROCESS_SHARED) = 1;
   end Process_Shared_Is_Supported;

   function Synchronized_IO_Is_Supported
      return PO.Synchronized_IO_Support is
   begin
      return sysconf (SC_SYNCHRONIZED_IO) = 1;
   end Synchronized_IO_Is_Supported;

   function Timers_Are_Supported
      return PO.Timers_Support is
   begin
      return sysconf (SC_TIMERS) = 1;
   end Timers_Are_Supported;

   function System_POSIX_Version
      return POSIX_Version is
   begin
      return POSIX_Version (sysconf (SC_VERSION));
   end System_POSIX_Version;

   function System_POSIX_Ada_Version
      return POSIX_Version is
   begin
      return POSIX_Ada_Version;
   end System_POSIX_Ada_Version;

   function ALM is new Limit
     (PL.Argument_List_Maxima, SC_ARG_MAX);
   function Argument_List_Maximum
     return POSIX.Limits.Argument_List_Maxima renames ALM;

   function AIOM is new Limit
     (PL.Asynchronous_IO_Maxima, SC_AIO_MAX);
   function Asynchronous_IO_Maximum
      return PL.Asynchronous_IO_Maxima renames AIOM;

   function AIOPDM is new Limit
     (PL.Asynchronous_IO_Priority_Delta_Maxima, SC_AIO_PRIO_DELTA_MAX);
   function Asynchronous_IO_Priority_Delta_Maximum
      return PL.Asynchronous_IO_Priority_Delta_Maxima renames AIOPDM;

   function CPM is new Limit
     (PL.Child_Processes_Maxima, SC_CHILD_MAX);
   function Child_Processes_Maximum
      return PL.Child_Processes_Maxima renames CPM;

   function GM is new Limit
     (PL.Groups_Maxima, SC_NGROUPS_MAX);
   function Groups_Maximum
      return PL.Groups_Maxima renames GM;

   function LIOM is new Limit
     (PL.List_IO_Maxima, SC_AIO_LISTIO_MAX);
   function List_IO_Maximum
      return PL.List_IO_Maxima renames LIOM;

   function OMQM is new Limit
     (PL.Open_Message_Queues_Maxima, SC_MQ_OPEN_MAX);
   function Open_Message_Queues_Maximum
      return PL.Open_Message_Queues_Maxima renames OMQM;

   function MPM is new Limit
     (PL.Message_Priority_Maxima, SC_MQ_PRIO_MAX);
   function Message_Priority_Maximum
      return PL.Message_Priority_Maxima renames MPM;

   function OFM is new Limit
     (PL.Open_Files_Maxima, SC_OPEN_MAX);
   function Open_Files_Maximum
      return PL.Open_Files_Maxima renames OFM;

   function PSR is new Limit
     (PL.Page_Size_Range, SC_PAGESIZE);
   function Page_Size
      return PL.Page_Size_Range renames PSR;

   function QSM is new Limit
     (PL.Queued_Signals_Maxima, SC_SIGQUEUE_MAX);
   function Queued_Signals_Maximum
      return PL.Queued_Signals_Maxima renames QSM;

   function RSM is new Limit
     (PL.Realtime_Signals_Maxima, SC_RTSIG_MAX);
   function Realtime_Signals_Maximum
      return PL.Realtime_Signals_Maxima renames RSM;

   function SEM is new Limit
     (PL.Semaphores_Maxima, SC_SEM_NSEMS_MAX);
   function Semaphores_Maximum
      return PL.Semaphores_Maxima renames SEM;

   function SVM is new Limit
     (PL.Semaphores_Value_Maxima, SC_SEM_VALUE_MAX);
   function Semaphores_Value_Maximum
      return PL.Semaphores_Value_Maxima renames SVM;

   function STM is new Limit
     (PL.Streams_Maxima, SC_STREAM_MAX);
   function Stream_Maximum
      return PL.Streams_Maxima renames STM;

   function TM is new Limit
     (PL.Timers_Maxima, SC_TIMER_MAX);
   function Timers_Maximum
     return PL.Timers_Maxima renames TM;

   function TOM is new Limit
     (PL.Timer_Overruns_Maxima, SC_DELAYTIMER_MAX);
   function Timer_Overruns_Maximum
     return PL.Timer_Overruns_Maxima renames TOM;

   function TZSM is new Limit
     (PL.Time_Zone_String_Maxima, SC_TZNAME_MAX);
   function Time_Zone_String_Maximum
     return PL.Time_Zone_String_Maxima renames TZSM;

   --  additions from POSIX.5c [D2]

   --  POSIX.5c/D4 extensions

   function Internet_Datagram_Is_Supported
     return POSIX.Options.Internet_Datagram_Support is
   begin
      return sysconf (SC_PII_INTERNET_DGRAM) = 1;
   end Internet_Datagram_Is_Supported;

   function Internet_Protocol_Is_Supported
     return POSIX.Options.Internet_Protocol_Support is
   begin
      return sysconf (SC_PII_INTERNET) = 1;
   end Internet_Protocol_Is_Supported;

   function Internet_Stream_Is_Supported
     return POSIX.Options.Internet_Stream_Support is
   begin
      return sysconf (SC_PII_INTERNET_STREAM) = 1;
   end Internet_Stream_Is_Supported;

   function ISO_OSI_Protocol_Is_Supported
     return POSIX.Options.ISO_OSI_Protocol_Support is
   begin
      return sysconf (SC_PII_OSI) = 1;
   end ISO_OSI_Protocol_Is_Supported;

   function Network_Management_Is_Supported
     return POSIX.Options.Network_Management_Support is
   begin
      return sysconf (SC_POSIX_PII_NET_SUPPORT) = 1;
   end Network_Management_Is_Supported;

   function OSI_Connectionless_Is_Supported
     return POSIX.Options.OSI_Connectionless_Support is
   begin
      return sysconf (SC_PII_OSI_CLTS) = 1;
   end OSI_Connectionless_Is_Supported;

   function OSI_Connection_Is_Supported
     return POSIX.Options.OSI_Connection_Support is
   begin
      return sysconf (SC_PII_OSI_COTS) = 1;
   end OSI_Connection_Is_Supported;

   function OSI_Minimal_Is_Supported
     return POSIX.Options.OSI_Minimal_Support is
   begin
      return sysconf (SC_PII_OSI_M) = 1;
   end OSI_Minimal_Is_Supported;

   function Poll_Is_Supported
     return POSIX.Options.Poll_Support is
   begin
      return sysconf (SC_POLL) = 1;
   end Poll_Is_Supported;

   function Select_Is_Supported
     return POSIX.Options.Select_Support is
   begin
      return sysconf (SC_SELECT) = 1;
   end Select_Is_Supported;

   function Sockets_DNI_Is_Supported
     return POSIX.Options.Sockets_DNI_Support is
   begin
      return sysconf (SC_PII_SOCKET) = 1;
   end Sockets_DNI_Is_Supported;

   function XTI_DNI_Is_Supported
     return POSIX.Options.XTI_DNI_Support is
   begin
      return sysconf (SC_PII_XTI) = 1;
   end XTI_DNI_Is_Supported;

   function SIOVM is new Limit
     (PL.Socket_IO_Vector_Maxima, SC_UIO_MAXIOV);
   function Socket_IO_Vector_Maximum
     return POSIX.Limits.Socket_IO_Vector_Maxima renames SIOVM;

end  POSIX.Configurable_System_Limits;

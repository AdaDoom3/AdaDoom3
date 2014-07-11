pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__main.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__main.adb");

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is
   pragma Warnings (Off);

   E120 : Short_Integer; pragma Import (Ada, E120, "system__os_lib_E");
   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E240 : Short_Integer; pragma Import (Ada, E240, "system__fat_llf_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "system__exception_table_E");
   E168 : Short_Integer; pragma Import (Ada, E168, "ada__containers_E");
   E110 : Short_Integer; pragma Import (Ada, E110, "ada__io_exceptions_E");
   E172 : Short_Integer; pragma Import (Ada, E172, "ada__strings_E");
   E176 : Short_Integer; pragma Import (Ada, E176, "ada__strings__maps_E");
   E212 : Short_Integer; pragma Import (Ada, E212, "ada__strings__maps__constants_E");
   E093 : Short_Integer; pragma Import (Ada, E093, "ada__tags_E");
   E109 : Short_Integer; pragma Import (Ada, E109, "ada__streams_E");
   E051 : Short_Integer; pragma Import (Ada, E051, "interfaces__c_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "interfaces__c__strings_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__exceptions_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "system__finalization_root_E");
   E116 : Short_Integer; pragma Import (Ada, E116, "ada__finalization_E");
   E129 : Short_Integer; pragma Import (Ada, E129, "system__storage_pools_E");
   E125 : Short_Integer; pragma Import (Ada, E125, "system__finalization_masters_E");
   E135 : Short_Integer; pragma Import (Ada, E135, "system__storage_pools__subpools_E");
   E069 : Short_Integer; pragma Import (Ada, E069, "system__task_info_E");
   E165 : Short_Integer; pragma Import (Ada, E165, "ada__calendar_E");
   E245 : Short_Integer; pragma Import (Ada, E245, "ada__calendar__delays_E");
   E253 : Short_Integer; pragma Import (Ada, E253, "ada__calendar__time_zones_E");
   E223 : Short_Integer; pragma Import (Ada, E223, "system__assertions_E");
   E131 : Short_Integer; pragma Import (Ada, E131, "system__pool_global_E");
   E123 : Short_Integer; pragma Import (Ada, E123, "system__file_control_block_E");
   E227 : Short_Integer; pragma Import (Ada, E227, "ada__streams__stream_io_E");
   E114 : Short_Integer; pragma Import (Ada, E114, "system__file_io_E");
   E201 : Short_Integer; pragma Import (Ada, E201, "ada__wide_text_io_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__secondary_stack_E");
   E184 : Short_Integer; pragma Import (Ada, E184, "ada__strings__wide_maps_E");
   E190 : Short_Integer; pragma Import (Ada, E190, "ada__strings__wide_unbounded_E");
   E225 : Short_Integer; pragma Import (Ada, E225, "system__strings__stream_ops_E");
   E263 : Short_Integer; pragma Import (Ada, E263, "system__tasking__initialization_E");
   E043 : Short_Integer; pragma Import (Ada, E043, "ada__real_time_E");
   E107 : Short_Integer; pragma Import (Ada, E107, "ada__text_io_E");
   E157 : Short_Integer; pragma Import (Ada, E157, "system__tasking__protected_objects_E");
   E267 : Short_Integer; pragma Import (Ada, E267, "system__tasking__protected_objects__entries_E");
   E265 : Short_Integer; pragma Import (Ada, E265, "system__tasking__queuing_E");
   E259 : Short_Integer; pragma Import (Ada, E259, "system__tasking__stages_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "neo_E");
   E203 : Short_Integer; pragma Import (Ada, E203, "neo__command_E");
   E231 : Short_Integer; pragma Import (Ada, E231, "neo__system_E");
   E235 : Short_Integer; pragma Import (Ada, E235, "neo__system__community_E");
   E237 : Short_Integer; pragma Import (Ada, E237, "neo__system__memory_E");
   E243 : Short_Integer; pragma Import (Ada, E243, "neo__system__processor_E");
   E247 : Short_Integer; pragma Import (Ada, E247, "neo__system__text_E");
   E249 : Short_Integer; pragma Import (Ada, E249, "neo__system__text__console_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E249 := E249 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "neo__system__text__console__finalize_spec");
      begin
         F1;
      end;
      E231 := E231 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "neo__system__finalize_spec");
      begin
         F2;
      end;
      E203 := E203 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "neo__command__finalize_spec");
      begin
         F3;
      end;
      E105 := E105 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "neo__finalize_spec");
      begin
         F4;
      end;
      E267 := E267 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F5;
      end;
      E107 := E107 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "ada__text_io__finalize_spec");
      begin
         F6;
      end;
      E201 := E201 - 1;
      E190 := E190 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "ada__strings__wide_unbounded__finalize_spec");
      begin
         F7;
      end;
      E184 := E184 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "ada__strings__wide_maps__finalize_spec");
      begin
         F8;
      end;
      E125 := E125 - 1;
      E135 := E135 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "system__file_io__finalize_body");
      begin
         E114 := E114 - 1;
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "ada__wide_text_io__finalize_spec");
      begin
         F10;
      end;
      E227 := E227 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "ada__streams__stream_io__finalize_spec");
      begin
         F11;
      end;
      declare
         procedure F12;
         pragma Import (Ada, F12, "system__file_control_block__finalize_spec");
      begin
         E123 := E123 - 1;
         F12;
      end;
      E131 := E131 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "system__pool_global__finalize_spec");
      begin
         F13;
      end;
      declare
         procedure F14;
         pragma Import (Ada, F14, "system__storage_pools__subpools__finalize_spec");
      begin
         F14;
      end;
      declare
         procedure F15;
         pragma Import (Ada, F15, "system__finalization_masters__finalize_spec");
      begin
         F15;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");
   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Exception_Tracebacks : Integer;
      pragma Import (C, Exception_Tracebacks, "__gl_exception_tracebacks");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");

      procedure Install_Handler;
      pragma Import (C, Install_Handler, "__gnat_install_handler");

      Handler_Installed : Integer;
      pragma Import (C, Handler_Installed, "__gnat_handler_installed");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := '8';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           True, False, False, False, False, False, False, False, 
           False, False, False, False, False, False),
         Value => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (False, False, False, True, True, True, False, True, 
           False, False, True, True, True, True, False, False, 
           True, False, False, True, True, False, True, True, 
           False, True, True, True, True, True, True, True, 
           False, True, False, True, True, False, True, False, 
           True, True, False, True, False, True, True, False, 
           False, True, False, True, True, False, True, False, 
           True, False, True, True, True, False, False, True, 
           False, False, True, False, True, True, False, True, 
           True, True, False, True, False, False, False, False, 
           True, True, True, False, True, False),
         Count => (0, 0, 0, 0, 1, 2, 2, 0, 4, 0),
         Unknown => (False, False, False, False, False, False, True, False, True, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Exception_Tracebacks := 1;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      if Handler_Installed = 0 then
         Install_Handler;
      end if;

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Fat_Llf'Elab_Spec;
      E240 := E240 + 1;
      System.Exception_Table'Elab_Body;
      E021 := E021 + 1;
      Ada.Containers'Elab_Spec;
      E168 := E168 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E110 := E110 + 1;
      Ada.Strings'Elab_Spec;
      E172 := E172 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E212 := E212 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E109 := E109 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E023 := E023 + 1;
      System.Finalization_Root'Elab_Spec;
      E118 := E118 + 1;
      Ada.Finalization'Elab_Spec;
      E116 := E116 + 1;
      System.Storage_Pools'Elab_Spec;
      E129 := E129 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      System.Task_Info'Elab_Spec;
      E069 := E069 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E165 := E165 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E245 := E245 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E253 := E253 + 1;
      System.Assertions'Elab_Spec;
      E223 := E223 + 1;
      System.Pool_Global'Elab_Spec;
      E131 := E131 + 1;
      System.File_Control_Block'Elab_Spec;
      E123 := E123 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E227 := E227 + 1;
      Ada.Wide_Text_Io'Elab_Spec;
      System.File_Io'Elab_Body;
      E114 := E114 + 1;
      E135 := E135 + 1;
      System.Finalization_Masters'Elab_Body;
      E125 := E125 + 1;
      E053 := E053 + 1;
      E051 := E051 + 1;
      Ada.Tags'Elab_Body;
      E093 := E093 + 1;
      E176 := E176 + 1;
      System.Soft_Links'Elab_Body;
      E011 := E011 + 1;
      System.Os_Lib'Elab_Body;
      E120 := E120 + 1;
      System.Secondary_Stack'Elab_Body;
      E015 := E015 + 1;
      Ada.Strings.Wide_Maps'Elab_Spec;
      E184 := E184 + 1;
      Ada.Strings.Wide_Unbounded'Elab_Spec;
      E190 := E190 + 1;
      System.Strings.Stream_Ops'Elab_Body;
      E225 := E225 + 1;
      System.Tasking.Initialization'Elab_Body;
      E263 := E263 + 1;
      Ada.Wide_Text_Io'Elab_Body;
      E201 := E201 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E043 := E043 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E107 := E107 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E157 := E157 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E267 := E267 + 1;
      System.Tasking.Queuing'Elab_Body;
      E265 := E265 + 1;
      System.Tasking.Stages'Elab_Body;
      E259 := E259 + 1;
      Neo'Elab_Spec;
      E105 := E105 + 1;
      Neo.Command'Elab_Spec;
      E203 := E203 + 1;
      Neo.System'Elab_Spec;
      E231 := E231 + 1;
      E235 := E235 + 1;
      Neo.System.Memory'Elab_Spec;
      E237 := E237 + 1;
      Neo.System.Processor'Elab_Spec;
      Neo.System.Processor'Elab_Body;
      E243 := E243 + 1;
      E247 := E247 + 1;
      Neo.System.Text.Console'Elab_Spec;
      Neo.System.Text.Console'Elab_Body;
      E249 := E249 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_main");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   C:\Users\User0\Desktop\Neo\Objects\neo.o
   --   C:\Users\User0\Desktop\Neo\Objects\neo-command.o
   --   C:\Users\User0\Desktop\Neo\Objects\neo-link.o
   --   C:\Users\User0\Desktop\Neo\Objects\neo-link-windows.o
   --   C:\Users\User0\Desktop\Neo\Objects\neo-system.o
   --   C:\Users\User0\Desktop\Neo\Objects\neo-system-community.o
   --   C:\Users\User0\Desktop\Neo\Objects\neo-system-memory.o
   --   C:\Users\User0\Desktop\Neo\Objects\neo-system-processor.o
   --   C:\Users\User0\Desktop\Neo\Objects\neo-system-text.o
   --   C:\Users\User0\Desktop\Neo\Objects\neo-system-text-console.o
   --   C:\Users\User0\Desktop\Neo\Objects\main.o
   --   -LC:\Users\User0\Desktop\Neo\Objects\
   --   -LC:\Users\User0\Desktop\Neo\Objects\
   --   -LC:/gnat/2014/lib/gcc/i686-pc-mingw32/4.7.4/adalib/
   --   /Windows/System32/gdi32.dll
   --   /Windows/System32/hid.dll
   --   /Windows/System32/setupapi.dll
   --   /Windows/System32/opengl32.dll
   --   -static
   --   -lgnarl
   --   -lgnat
   --   -Xlinker
   --   --stack=0x200000,0x1000
   --   -mthreads
   --   -Wl,--stack=0x2000000
--  END Object file/option list   

end ada_main;

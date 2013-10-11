pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__main.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__main.adb");

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is
   pragma Warnings (Off);

   E127 : Short_Integer; pragma Import (Ada, E127, "system__os_lib_E");
   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E243 : Short_Integer; pragma Import (Ada, E243, "system__fat_llf_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "system__exception_table_E");
   E169 : Short_Integer; pragma Import (Ada, E169, "ada__containers_E");
   E124 : Short_Integer; pragma Import (Ada, E124, "ada__io_exceptions_E");
   E173 : Short_Integer; pragma Import (Ada, E173, "ada__strings_E");
   E177 : Short_Integer; pragma Import (Ada, E177, "ada__strings__maps_E");
   E217 : Short_Integer; pragma Import (Ada, E217, "ada__strings__maps__constants_E");
   E098 : Short_Integer; pragma Import (Ada, E098, "ada__tags_E");
   E115 : Short_Integer; pragma Import (Ada, E115, "ada__streams_E");
   E054 : Short_Integer; pragma Import (Ada, E054, "interfaces__c_E");
   E056 : Short_Integer; pragma Import (Ada, E056, "interfaces__c__strings_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "system__exceptions_E");
   E123 : Short_Integer; pragma Import (Ada, E123, "system__finalization_root_E");
   E121 : Short_Integer; pragma Import (Ada, E121, "ada__finalization_E");
   E138 : Short_Integer; pragma Import (Ada, E138, "system__storage_pools_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "system__finalization_masters_E");
   E144 : Short_Integer; pragma Import (Ada, E144, "system__storage_pools__subpools_E");
   E075 : Short_Integer; pragma Import (Ada, E075, "system__task_info_E");
   E110 : Short_Integer; pragma Import (Ada, E110, "ada__calendar_E");
   E108 : Short_Integer; pragma Import (Ada, E108, "ada__calendar__delays_E");
   E208 : Short_Integer; pragma Import (Ada, E208, "system__assertions_E");
   E140 : Short_Integer; pragma Import (Ada, E140, "system__pool_global_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "system__file_control_block_E");
   E228 : Short_Integer; pragma Import (Ada, E228, "ada__streams__stream_io_E");
   E119 : Short_Integer; pragma Import (Ada, E119, "system__file_io_E");
   E204 : Short_Integer; pragma Import (Ada, E204, "ada__wide_text_io_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__secondary_stack_E");
   E187 : Short_Integer; pragma Import (Ada, E187, "ada__strings__wide_maps_E");
   E193 : Short_Integer; pragma Import (Ada, E193, "ada__strings__wide_unbounded_E");
   E226 : Short_Integer; pragma Import (Ada, E226, "system__strings__stream_ops_E");
   E256 : Short_Integer; pragma Import (Ada, E256, "system__tasking__initialization_E");
   E164 : Short_Integer; pragma Import (Ada, E164, "system__tasking__protected_objects_E");
   E046 : Short_Integer; pragma Import (Ada, E046, "ada__real_time_E");
   E114 : Short_Integer; pragma Import (Ada, E114, "ada__text_io_E");
   E260 : Short_Integer; pragma Import (Ada, E260, "system__tasking__protected_objects__entries_E");
   E258 : Short_Integer; pragma Import (Ada, E258, "system__tasking__queuing_E");
   E252 : Short_Integer; pragma Import (Ada, E252, "system__tasking__stages_E");
   E106 : Short_Integer; pragma Import (Ada, E106, "neo_E");
   E206 : Short_Integer; pragma Import (Ada, E206, "neo__command_E");
   E232 : Short_Integer; pragma Import (Ada, E232, "neo__system_E");
   E238 : Short_Integer; pragma Import (Ada, E238, "neo__system__community_E");
   E240 : Short_Integer; pragma Import (Ada, E240, "neo__system__memory_E");
   E246 : Short_Integer; pragma Import (Ada, E246, "neo__system__processor_E");
   E248 : Short_Integer; pragma Import (Ada, E248, "neo__system__text_E");
   E250 : Short_Integer; pragma Import (Ada, E250, "neo__system__text__console_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E250 := E250 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "neo__system__text__console__finalize_spec");
      begin
         F1;
      end;
      E232 := E232 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "neo__system__finalize_spec");
      begin
         F2;
      end;
      E206 := E206 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "neo__command__finalize_spec");
      begin
         F3;
      end;
      E106 := E106 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "neo__finalize_spec");
      begin
         F4;
      end;
      E260 := E260 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F5;
      end;
      E114 := E114 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "ada__text_io__finalize_spec");
      begin
         F6;
      end;
      E204 := E204 - 1;
      E193 := E193 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "ada__strings__wide_unbounded__finalize_spec");
      begin
         F7;
      end;
      E187 := E187 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "ada__strings__wide_maps__finalize_spec");
      begin
         F8;
      end;
      E132 := E132 - 1;
      E144 := E144 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "system__file_io__finalize_body");
      begin
         E119 := E119 - 1;
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "ada__wide_text_io__finalize_spec");
      begin
         F10;
      end;
      E228 := E228 - 1;
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
         E130 := E130 - 1;
         F12;
      end;
      E140 := E140 - 1;
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
           False, False, False, False, False, True, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False),
         Value => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (False, False, False, True, True, False, False, True, 
           False, False, True, True, True, True, False, False, 
           True, False, False, True, True, False, True, True, 
           True, True, True, True, True, True, True, False, 
           True, False, True, True, False, True, True, False, 
           True, False, True, True, False, False, True, False, 
           True, True, False, True, False, True, False, True, 
           True, True, False, False, True, False, False, True, 
           False, True, True, False, True, True, True, False, 
           True, False, False, False, False, True, True, True, 
           False, True, False),
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
      E243 := E243 + 1;
      System.Exception_Table'Elab_Body;
      E021 := E021 + 1;
      Ada.Containers'Elab_Spec;
      E169 := E169 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E124 := E124 + 1;
      Ada.Strings'Elab_Spec;
      E173 := E173 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E217 := E217 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E115 := E115 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E027 := E027 + 1;
      System.Finalization_Root'Elab_Spec;
      E123 := E123 + 1;
      Ada.Finalization'Elab_Spec;
      E121 := E121 + 1;
      System.Storage_Pools'Elab_Spec;
      E138 := E138 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      System.Task_Info'Elab_Spec;
      E075 := E075 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E110 := E110 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E108 := E108 + 1;
      System.Assertions'Elab_Spec;
      E208 := E208 + 1;
      System.Pool_Global'Elab_Spec;
      E140 := E140 + 1;
      System.File_Control_Block'Elab_Spec;
      E130 := E130 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E228 := E228 + 1;
      Ada.Wide_Text_Io'Elab_Spec;
      System.File_Io'Elab_Body;
      E119 := E119 + 1;
      E144 := E144 + 1;
      System.Finalization_Masters'Elab_Body;
      E132 := E132 + 1;
      E056 := E056 + 1;
      E054 := E054 + 1;
      Ada.Tags'Elab_Body;
      E098 := E098 + 1;
      E177 := E177 + 1;
      System.Soft_Links'Elab_Body;
      E011 := E011 + 1;
      System.Os_Lib'Elab_Body;
      E127 := E127 + 1;
      System.Secondary_Stack'Elab_Body;
      E015 := E015 + 1;
      Ada.Strings.Wide_Maps'Elab_Spec;
      E187 := E187 + 1;
      Ada.Strings.Wide_Unbounded'Elab_Spec;
      E193 := E193 + 1;
      System.Strings.Stream_Ops'Elab_Body;
      E226 := E226 + 1;
      System.Tasking.Initialization'Elab_Body;
      E256 := E256 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E164 := E164 + 1;
      Ada.Wide_Text_Io'Elab_Body;
      E204 := E204 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E046 := E046 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E114 := E114 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E260 := E260 + 1;
      System.Tasking.Queuing'Elab_Body;
      E258 := E258 + 1;
      System.Tasking.Stages'Elab_Body;
      E252 := E252 + 1;
      Neo'Elab_Spec;
      E106 := E106 + 1;
      Neo.Command'Elab_Spec;
      E206 := E206 + 1;
      Neo.System'Elab_Spec;
      Neo.System'Elab_Body;
      E232 := E232 + 1;
      E238 := E238 + 1;
      Neo.System.Memory'Elab_Spec;
      E240 := E240 + 1;
      Neo.System.Processor'Elab_Spec;
      Neo.System.Processor'Elab_Body;
      E246 := E246 + 1;
      Neo.System.Text'Elab_Spec;
      E248 := E248 + 1;
      Neo.System.Text.Console'Elab_Spec;
      Neo.System.Text.Console'Elab_Body;
      E250 := E250 + 1;
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
   --   C:\Engine\Neo\Objects\neo.o
   --   C:\Engine\Neo\Objects\neo-command.o
   --   C:\Engine\Neo\Objects\neo-link.o
   --   C:\Engine\Neo\Objects\neo-link-windows.o
   --   C:\Engine\Neo\Objects\neo-system.o
   --   C:\Engine\Neo\Objects\neo-system-community.o
   --   C:\Engine\Neo\Objects\neo-system-memory.o
   --   C:\Engine\Neo\Objects\neo-system-processor.o
   --   C:\Engine\Neo\Objects\neo-system-text.o
   --   C:\Engine\Neo\Objects\neo-system-text-console.o
   --   C:\Engine\Neo\Objects\main.o
   --   -LC:\Engine\Neo\Objects\
   --   -LC:\Engine\Neo\Objects\
   --   -LC:/gnat/2013/lib/gcc/i686-pc-mingw32/4.7.4/adalib/
   --   /Windows/System32/gdi32.dll
   --   /Windows/System32/hid.dll
   --   /Windows/System32/setupapi.dll
   --   /Windows/System32/opengl32.dll
   --   /Windows/System32/XInput9_1_0.dll
   --   /Windows/System32/D3DX9_42.dll
   --   -static
   --   -lgnarl
   --   -lgnat
   --   -Xlinker
   --   --stack=0x200000,0x1000
   --   -mthreads
   --   -Wl,--stack=0x2000000
--  END Object file/option list   

end ada_main;

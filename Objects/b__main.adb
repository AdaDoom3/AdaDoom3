pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__main.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__main.adb");

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is
   pragma Warnings (Off);

   E076 : Short_Integer; pragma Import (Ada, E076, "system__os_lib_E");
   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E268 : Short_Integer; pragma Import (Ada, E268, "system__fat_llf_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "system__exception_table_E");
   E166 : Short_Integer; pragma Import (Ada, E166, "ada__containers_E");
   E061 : Short_Integer; pragma Import (Ada, E061, "ada__io_exceptions_E");
   E171 : Short_Integer; pragma Import (Ada, E171, "ada__strings_E");
   E175 : Short_Integer; pragma Import (Ada, E175, "ada__strings__maps_E");
   E213 : Short_Integer; pragma Import (Ada, E213, "ada__strings__maps__constants_E");
   E045 : Short_Integer; pragma Import (Ada, E045, "ada__tags_E");
   E060 : Short_Integer; pragma Import (Ada, E060, "ada__streams_E");
   E072 : Short_Integer; pragma Import (Ada, E072, "interfaces__c_E");
   E125 : Short_Integer; pragma Import (Ada, E125, "interfaces__c__strings_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__exceptions_E");
   E070 : Short_Integer; pragma Import (Ada, E070, "system__finalization_root_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ada__finalization_E");
   E089 : Short_Integer; pragma Import (Ada, E089, "system__storage_pools_E");
   E081 : Short_Integer; pragma Import (Ada, E081, "system__finalization_masters_E");
   E095 : Short_Integer; pragma Import (Ada, E095, "system__storage_pools__subpools_E");
   E141 : Short_Integer; pragma Import (Ada, E141, "system__task_info_E");
   E163 : Short_Integer; pragma Import (Ada, E163, "ada__calendar_E");
   E247 : Short_Integer; pragma Import (Ada, E247, "ada__calendar__delays_E");
   E279 : Short_Integer; pragma Import (Ada, E279, "ada__calendar__time_zones_E");
   E202 : Short_Integer; pragma Import (Ada, E202, "system__assertions_E");
   E091 : Short_Integer; pragma Import (Ada, E091, "system__pool_global_E");
   E079 : Short_Integer; pragma Import (Ada, E079, "system__file_control_block_E");
   E206 : Short_Integer; pragma Import (Ada, E206, "ada__streams__stream_io_E");
   E066 : Short_Integer; pragma Import (Ada, E066, "system__file_io_E");
   E058 : Short_Integer; pragma Import (Ada, E058, "ada__wide_text_io_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__secondary_stack_E");
   E183 : Short_Integer; pragma Import (Ada, E183, "ada__strings__wide_maps_E");
   E189 : Short_Integer; pragma Import (Ada, E189, "ada__strings__wide_unbounded_E");
   E204 : Short_Integer; pragma Import (Ada, E204, "system__strings__stream_ops_E");
   E229 : Short_Integer; pragma Import (Ada, E229, "system__tasking__initialization_E");
   E249 : Short_Integer; pragma Import (Ada, E249, "ada__real_time_E");
   E200 : Short_Integer; pragma Import (Ada, E200, "ada__text_io_E");
   E117 : Short_Integer; pragma Import (Ada, E117, "system__tasking__protected_objects_E");
   E233 : Short_Integer; pragma Import (Ada, E233, "system__tasking__protected_objects__entries_E");
   E231 : Short_Integer; pragma Import (Ada, E231, "system__tasking__queuing_E");
   E259 : Short_Integer; pragma Import (Ada, E259, "system__tasking__stages_E");
   E043 : Short_Integer; pragma Import (Ada, E043, "neo_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "neo__command_E");
   E222 : Short_Integer; pragma Import (Ada, E222, "neo__system_E");
   E241 : Short_Integer; pragma Import (Ada, E241, "neo__system__graphics_E");
   E245 : Short_Integer; pragma Import (Ada, E245, "neo__system__input_E");
   E243 : Short_Integer; pragma Import (Ada, E243, "neo__system__graphics__window_E");
   E265 : Short_Integer; pragma Import (Ada, E265, "neo__system__memory_E");
   E271 : Short_Integer; pragma Import (Ada, E271, "neo__system__processor_E");
   E273 : Short_Integer; pragma Import (Ada, E273, "neo__system__text_E");
   E275 : Short_Integer; pragma Import (Ada, E275, "neo__system__text__console_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E275 := E275 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "neo__system__text__console__finalize_spec");
      begin
         F1;
      end;
      E243 := E243 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "neo__system__graphics__window__finalize_spec");
      begin
         F2;
      end;
      E245 := E245 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "neo__system__input__finalize_spec");
      begin
         F3;
      end;
      E222 := E222 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "neo__system__finalize_spec");
      begin
         F4;
      end;
      E210 := E210 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "neo__command__finalize_spec");
      begin
         F5;
      end;
      E043 := E043 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "neo__finalize_spec");
      begin
         F6;
      end;
      E233 := E233 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F7;
      end;
      E200 := E200 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "ada__text_io__finalize_spec");
      begin
         F8;
      end;
      E058 := E058 - 1;
      E189 := E189 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "ada__strings__wide_unbounded__finalize_spec");
      begin
         F9;
      end;
      E183 := E183 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "ada__strings__wide_maps__finalize_spec");
      begin
         F10;
      end;
      E081 := E081 - 1;
      E095 := E095 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "system__file_io__finalize_body");
      begin
         E066 := E066 - 1;
         F11;
      end;
      declare
         procedure F12;
         pragma Import (Ada, F12, "ada__wide_text_io__finalize_spec");
      begin
         F12;
      end;
      E206 := E206 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "ada__streams__stream_io__finalize_spec");
      begin
         F13;
      end;
      declare
         procedure F14;
         pragma Import (Ada, F14, "system__file_control_block__finalize_spec");
      begin
         E079 := E079 - 1;
         F14;
      end;
      E091 := E091 - 1;
      declare
         procedure F15;
         pragma Import (Ada, F15, "system__pool_global__finalize_spec");
      begin
         F15;
      end;
      declare
         procedure F16;
         pragma Import (Ada, F16, "system__storage_pools__subpools__finalize_spec");
      begin
         F16;
      end;
      declare
         procedure F17;
         pragma Import (Ada, F17, "system__finalization_masters__finalize_spec");
      begin
         F17;
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
          (False, False, False, True, True, False, False, True, 
           False, False, True, True, True, True, False, False, 
           True, True, False, True, True, False, True, True, 
           False, True, True, True, True, True, True, True, 
           False, True, False, False, True, False, True, False, 
           True, True, False, True, False, True, False, False, 
           False, True, False, True, True, False, False, False, 
           True, False, True, True, True, False, False, True, 
           False, False, True, False, True, True, False, True, 
           True, True, False, True, False, False, False, False, 
           False, True, True, False, True, False),
         Count => (0, 0, 0, 0, 0, 1, 1, 0, 6, 0),
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
      E268 := E268 + 1;
      System.Exception_Table'Elab_Body;
      E021 := E021 + 1;
      Ada.Containers'Elab_Spec;
      E166 := E166 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E061 := E061 + 1;
      Ada.Strings'Elab_Spec;
      E171 := E171 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E213 := E213 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E060 := E060 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E023 := E023 + 1;
      System.Finalization_Root'Elab_Spec;
      E070 := E070 + 1;
      Ada.Finalization'Elab_Spec;
      E068 := E068 + 1;
      System.Storage_Pools'Elab_Spec;
      E089 := E089 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      System.Task_Info'Elab_Spec;
      E141 := E141 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E163 := E163 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E247 := E247 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E279 := E279 + 1;
      System.Assertions'Elab_Spec;
      E202 := E202 + 1;
      System.Pool_Global'Elab_Spec;
      E091 := E091 + 1;
      System.File_Control_Block'Elab_Spec;
      E079 := E079 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E206 := E206 + 1;
      Ada.Wide_Text_Io'Elab_Spec;
      System.File_Io'Elab_Body;
      E066 := E066 + 1;
      E095 := E095 + 1;
      System.Finalization_Masters'Elab_Body;
      E081 := E081 + 1;
      E125 := E125 + 1;
      E072 := E072 + 1;
      Ada.Tags'Elab_Body;
      E045 := E045 + 1;
      E175 := E175 + 1;
      System.Soft_Links'Elab_Body;
      E011 := E011 + 1;
      System.Os_Lib'Elab_Body;
      E076 := E076 + 1;
      System.Secondary_Stack'Elab_Body;
      E015 := E015 + 1;
      Ada.Strings.Wide_Maps'Elab_Spec;
      E183 := E183 + 1;
      Ada.Strings.Wide_Unbounded'Elab_Spec;
      E189 := E189 + 1;
      System.Strings.Stream_Ops'Elab_Body;
      E204 := E204 + 1;
      System.Tasking.Initialization'Elab_Body;
      E229 := E229 + 1;
      Ada.Wide_Text_Io'Elab_Body;
      E058 := E058 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E249 := E249 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E200 := E200 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E117 := E117 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E233 := E233 + 1;
      System.Tasking.Queuing'Elab_Body;
      E231 := E231 + 1;
      System.Tasking.Stages'Elab_Body;
      E259 := E259 + 1;
      Neo'Elab_Spec;
      E043 := E043 + 1;
      Neo.Command'Elab_Spec;
      E210 := E210 + 1;
      Neo.System'Elab_Spec;
      Neo.System'Elab_Body;
      E222 := E222 + 1;
      E241 := E241 + 1;
      Neo.System.Input'Elab_Spec;
      Neo.System.Input'Elab_Body;
      E245 := E245 + 1;
      Neo.System.Graphics.Window'Elab_Spec;
      Neo.System.Graphics.Window'Elab_Body;
      E243 := E243 + 1;
      Neo.System.Memory'Elab_Spec;
      Neo.System.Memory'Elab_Body;
      E265 := E265 + 1;
      Neo.System.Processor'Elab_Spec;
      Neo.System.Processor'Elab_Body;
      E271 := E271 + 1;
      E273 := E273 + 1;
      Neo.System.Text.Console'Elab_Spec;
      E275 := E275 + 1;
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
   --   C:\git\Objects\neo.o
   --   C:\git\Objects\neo-command.o
   --   C:\git\Objects\neo-windows.o
   --   C:\git\Objects\neo-system.o
   --   C:\git\Objects\neo-system-graphics.o
   --   C:\git\Objects\neo-system-input.o
   --   C:\git\Objects\neo-system-graphics-window.o
   --   C:\git\Objects\neo-system-memory.o
   --   C:\git\Objects\neo-system-processor.o
   --   C:\git\Objects\neo-system-text.o
   --   C:\git\Objects\neo-system-text-console.o
   --   C:\git\Objects\main.o
   --   -LC:\git\Objects\
   --   -LC:\git\Objects\
   --   -LC:/gnat/2014/lib/gcc/i686-pc-mingw32/4.7.4/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
   --   -Xlinker
   --   --stack=0x200000,0x1000
   --   -mthreads
   --   -Wl,--stack=0x2000000
--  END Object file/option list   

end ada_main;

pragma Ada_95;
with System;
package ada_main is
   pragma Warnings (Off);

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: GPL 2014 (20140331)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_main" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#50e2384d#;
   pragma Export (C, u00001, "mainB");
   u00002 : constant Version_32 := 16#fbff4c67#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#5c291747#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#eaff8cdc#;
   pragma Export (C, u00004, "ada__exceptionsB");
   u00005 : constant Version_32 := 16#6a2091f5#;
   pragma Export (C, u00005, "ada__exceptionsS");
   u00006 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00006, "adaS");
   u00007 : constant Version_32 := 16#032105bb#;
   pragma Export (C, u00007, "ada__exceptions__last_chance_handlerB");
   u00008 : constant Version_32 := 16#2b293877#;
   pragma Export (C, u00008, "ada__exceptions__last_chance_handlerS");
   u00009 : constant Version_32 := 16#5fc8ae56#;
   pragma Export (C, u00009, "systemS");
   u00010 : constant Version_32 := 16#daf76b33#;
   pragma Export (C, u00010, "system__soft_linksB");
   u00011 : constant Version_32 := 16#1517cb64#;
   pragma Export (C, u00011, "system__soft_linksS");
   u00012 : constant Version_32 := 16#c8ed38da#;
   pragma Export (C, u00012, "system__parametersB");
   u00013 : constant Version_32 := 16#591236e4#;
   pragma Export (C, u00013, "system__parametersS");
   u00014 : constant Version_32 := 16#c96bf39e#;
   pragma Export (C, u00014, "system__secondary_stackB");
   u00015 : constant Version_32 := 16#f4a9613f#;
   pragma Export (C, u00015, "system__secondary_stackS");
   u00016 : constant Version_32 := 16#39a03df9#;
   pragma Export (C, u00016, "system__storage_elementsB");
   u00017 : constant Version_32 := 16#720be452#;
   pragma Export (C, u00017, "system__storage_elementsS");
   u00018 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00018, "system__stack_checkingB");
   u00019 : constant Version_32 := 16#d177c5be#;
   pragma Export (C, u00019, "system__stack_checkingS");
   u00020 : constant Version_32 := 16#393398c1#;
   pragma Export (C, u00020, "system__exception_tableB");
   u00021 : constant Version_32 := 16#f1d1c843#;
   pragma Export (C, u00021, "system__exception_tableS");
   u00022 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00022, "system__exceptionsB");
   u00023 : constant Version_32 := 16#37abc3a0#;
   pragma Export (C, u00023, "system__exceptionsS");
   u00024 : constant Version_32 := 16#2652ec14#;
   pragma Export (C, u00024, "system__exceptions__machineS");
   u00025 : constant Version_32 := 16#b895431d#;
   pragma Export (C, u00025, "system__exceptions_debugB");
   u00026 : constant Version_32 := 16#ec2ab7e8#;
   pragma Export (C, u00026, "system__exceptions_debugS");
   u00027 : constant Version_32 := 16#570325c8#;
   pragma Export (C, u00027, "system__img_intB");
   u00028 : constant Version_32 := 16#5d134e94#;
   pragma Export (C, u00028, "system__img_intS");
   u00029 : constant Version_32 := 16#ff5c7695#;
   pragma Export (C, u00029, "system__tracebackB");
   u00030 : constant Version_32 := 16#77cc310b#;
   pragma Export (C, u00030, "system__tracebackS");
   u00031 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00031, "system__wch_conB");
   u00032 : constant Version_32 := 16#44b58c84#;
   pragma Export (C, u00032, "system__wch_conS");
   u00033 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00033, "system__wch_stwB");
   u00034 : constant Version_32 := 16#69a4a085#;
   pragma Export (C, u00034, "system__wch_stwS");
   u00035 : constant Version_32 := 16#9b29844d#;
   pragma Export (C, u00035, "system__wch_cnvB");
   u00036 : constant Version_32 := 16#4b023677#;
   pragma Export (C, u00036, "system__wch_cnvS");
   u00037 : constant Version_32 := 16#69adb1b9#;
   pragma Export (C, u00037, "interfacesS");
   u00038 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00038, "system__wch_jisB");
   u00039 : constant Version_32 := 16#cb722f56#;
   pragma Export (C, u00039, "system__wch_jisS");
   u00040 : constant Version_32 := 16#8cb17bcd#;
   pragma Export (C, u00040, "system__traceback_entriesB");
   u00041 : constant Version_32 := 16#ead9cec4#;
   pragma Export (C, u00041, "system__traceback_entriesS");
   u00042 : constant Version_32 := 16#adbf339a#;
   pragma Export (C, u00042, "neoB");
   u00043 : constant Version_32 := 16#a198febf#;
   pragma Export (C, u00043, "neoS");
   u00044 : constant Version_32 := 16#034d7998#;
   pragma Export (C, u00044, "ada__tagsB");
   u00045 : constant Version_32 := 16#ce72c228#;
   pragma Export (C, u00045, "ada__tagsS");
   u00046 : constant Version_32 := 16#c3335bfd#;
   pragma Export (C, u00046, "system__htableB");
   u00047 : constant Version_32 := 16#db0a1dbc#;
   pragma Export (C, u00047, "system__htableS");
   u00048 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00048, "system__string_hashB");
   u00049 : constant Version_32 := 16#795476c2#;
   pragma Export (C, u00049, "system__string_hashS");
   u00050 : constant Version_32 := 16#0ece3cf9#;
   pragma Export (C, u00050, "system__unsigned_typesS");
   u00051 : constant Version_32 := 16#4266b2a8#;
   pragma Export (C, u00051, "system__val_unsB");
   u00052 : constant Version_32 := 16#1e66d1c2#;
   pragma Export (C, u00052, "system__val_unsS");
   u00053 : constant Version_32 := 16#27b600b2#;
   pragma Export (C, u00053, "system__val_utilB");
   u00054 : constant Version_32 := 16#f36818a8#;
   pragma Export (C, u00054, "system__val_utilS");
   u00055 : constant Version_32 := 16#d1060688#;
   pragma Export (C, u00055, "system__case_utilB");
   u00056 : constant Version_32 := 16#7bc1c781#;
   pragma Export (C, u00056, "system__case_utilS");
   u00057 : constant Version_32 := 16#1e31cab7#;
   pragma Export (C, u00057, "ada__wide_text_ioB");
   u00058 : constant Version_32 := 16#29525240#;
   pragma Export (C, u00058, "ada__wide_text_ioS");
   u00059 : constant Version_32 := 16#1b5643e2#;
   pragma Export (C, u00059, "ada__streamsB");
   u00060 : constant Version_32 := 16#2564c958#;
   pragma Export (C, u00060, "ada__streamsS");
   u00061 : constant Version_32 := 16#db5c917c#;
   pragma Export (C, u00061, "ada__io_exceptionsS");
   u00062 : constant Version_32 := 16#9f23726e#;
   pragma Export (C, u00062, "interfaces__c_streamsB");
   u00063 : constant Version_32 := 16#bb1012c3#;
   pragma Export (C, u00063, "interfaces__c_streamsS");
   u00064 : constant Version_32 := 16#75131373#;
   pragma Export (C, u00064, "system__crtlS");
   u00065 : constant Version_32 := 16#967994fc#;
   pragma Export (C, u00065, "system__file_ioB");
   u00066 : constant Version_32 := 16#e3384250#;
   pragma Export (C, u00066, "system__file_ioS");
   u00067 : constant Version_32 := 16#b7ab275c#;
   pragma Export (C, u00067, "ada__finalizationB");
   u00068 : constant Version_32 := 16#19f764ca#;
   pragma Export (C, u00068, "ada__finalizationS");
   u00069 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00069, "system__finalization_rootB");
   u00070 : constant Version_32 := 16#103addc6#;
   pragma Export (C, u00070, "system__finalization_rootS");
   u00071 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00071, "interfaces__cB");
   u00072 : constant Version_32 := 16#3b563890#;
   pragma Export (C, u00072, "interfaces__cS");
   u00073 : constant Version_32 := 16#d0432c8d#;
   pragma Export (C, u00073, "system__img_enum_newB");
   u00074 : constant Version_32 := 16#3e84a896#;
   pragma Export (C, u00074, "system__img_enum_newS");
   u00075 : constant Version_32 := 16#a25be73b#;
   pragma Export (C, u00075, "system__os_libB");
   u00076 : constant Version_32 := 16#94c13856#;
   pragma Export (C, u00076, "system__os_libS");
   u00077 : constant Version_32 := 16#1a817b8e#;
   pragma Export (C, u00077, "system__stringsB");
   u00078 : constant Version_32 := 16#2177bf30#;
   pragma Export (C, u00078, "system__stringsS");
   u00079 : constant Version_32 := 16#906f0f88#;
   pragma Export (C, u00079, "system__file_control_blockS");
   u00080 : constant Version_32 := 16#a4371844#;
   pragma Export (C, u00080, "system__finalization_mastersB");
   u00081 : constant Version_32 := 16#2bde8716#;
   pragma Export (C, u00081, "system__finalization_mastersS");
   u00082 : constant Version_32 := 16#57a37a42#;
   pragma Export (C, u00082, "system__address_imageB");
   u00083 : constant Version_32 := 16#fe24336c#;
   pragma Export (C, u00083, "system__address_imageS");
   u00084 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00084, "system__img_boolB");
   u00085 : constant Version_32 := 16#aa11dfbd#;
   pragma Export (C, u00085, "system__img_boolS");
   u00086 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00086, "system__ioB");
   u00087 : constant Version_32 := 16#c18a5919#;
   pragma Export (C, u00087, "system__ioS");
   u00088 : constant Version_32 := 16#6d4d969a#;
   pragma Export (C, u00088, "system__storage_poolsB");
   u00089 : constant Version_32 := 16#aa9329d2#;
   pragma Export (C, u00089, "system__storage_poolsS");
   u00090 : constant Version_32 := 16#e34550ca#;
   pragma Export (C, u00090, "system__pool_globalB");
   u00091 : constant Version_32 := 16#c88d2d16#;
   pragma Export (C, u00091, "system__pool_globalS");
   u00092 : constant Version_32 := 16#3a4ba6c3#;
   pragma Export (C, u00092, "system__memoryB");
   u00093 : constant Version_32 := 16#06b5c862#;
   pragma Export (C, u00093, "system__memoryS");
   u00094 : constant Version_32 := 16#7b002481#;
   pragma Export (C, u00094, "system__storage_pools__subpoolsB");
   u00095 : constant Version_32 := 16#e3b008dc#;
   pragma Export (C, u00095, "system__storage_pools__subpoolsS");
   u00096 : constant Version_32 := 16#63f11652#;
   pragma Export (C, u00096, "system__storage_pools__subpools__finalizationB");
   u00097 : constant Version_32 := 16#fe2f4b3a#;
   pragma Export (C, u00097, "system__storage_pools__subpools__finalizationS");
   u00098 : constant Version_32 := 16#3043af89#;
   pragma Export (C, u00098, "ada__wide_text_io__modular_auxB");
   u00099 : constant Version_32 := 16#be41796e#;
   pragma Export (C, u00099, "ada__wide_text_io__modular_auxS");
   u00100 : constant Version_32 := 16#c47395a9#;
   pragma Export (C, u00100, "ada__wide_text_io__generic_auxB");
   u00101 : constant Version_32 := 16#3ef64803#;
   pragma Export (C, u00101, "ada__wide_text_io__generic_auxS");
   u00102 : constant Version_32 := 16#d48b4eeb#;
   pragma Export (C, u00102, "system__img_biuB");
   u00103 : constant Version_32 := 16#c8ecb4b4#;
   pragma Export (C, u00103, "system__img_biuS");
   u00104 : constant Version_32 := 16#2b864520#;
   pragma Export (C, u00104, "system__img_llbB");
   u00105 : constant Version_32 := 16#892ba44a#;
   pragma Export (C, u00105, "system__img_llbS");
   u00106 : constant Version_32 := 16#3da6be5a#;
   pragma Export (C, u00106, "system__img_lluB");
   u00107 : constant Version_32 := 16#47073c3a#;
   pragma Export (C, u00107, "system__img_lluS");
   u00108 : constant Version_32 := 16#c2d63ebb#;
   pragma Export (C, u00108, "system__img_llwB");
   u00109 : constant Version_32 := 16#204787dc#;
   pragma Export (C, u00109, "system__img_llwS");
   u00110 : constant Version_32 := 16#22ab03a2#;
   pragma Export (C, u00110, "system__img_unsB");
   u00111 : constant Version_32 := 16#913a000e#;
   pragma Export (C, u00111, "system__img_unsS");
   u00112 : constant Version_32 := 16#8ed53197#;
   pragma Export (C, u00112, "system__img_wiuB");
   u00113 : constant Version_32 := 16#a6ad3326#;
   pragma Export (C, u00113, "system__img_wiuS");
   u00114 : constant Version_32 := 16#1e25d3f1#;
   pragma Export (C, u00114, "system__val_lluB");
   u00115 : constant Version_32 := 16#743c6b8b#;
   pragma Export (C, u00115, "system__val_lluS");
   u00116 : constant Version_32 := 16#21c37fe5#;
   pragma Export (C, u00116, "system__tasking__protected_objectsB");
   u00117 : constant Version_32 := 16#6fa056d1#;
   pragma Export (C, u00117, "system__tasking__protected_objectsS");
   u00118 : constant Version_32 := 16#1d8f750b#;
   pragma Export (C, u00118, "system__soft_links__taskingB");
   u00119 : constant Version_32 := 16#e47ef8be#;
   pragma Export (C, u00119, "system__soft_links__taskingS");
   u00120 : constant Version_32 := 16#17d21067#;
   pragma Export (C, u00120, "ada__exceptions__is_null_occurrenceB");
   u00121 : constant Version_32 := 16#8b1b3b36#;
   pragma Export (C, u00121, "ada__exceptions__is_null_occurrenceS");
   u00122 : constant Version_32 := 16#1da6a0d9#;
   pragma Export (C, u00122, "system__task_primitivesS");
   u00123 : constant Version_32 := 16#9d17a46c#;
   pragma Export (C, u00123, "system__os_interfaceS");
   u00124 : constant Version_32 := 16#877b0450#;
   pragma Export (C, u00124, "interfaces__c__stringsB");
   u00125 : constant Version_32 := 16#603c1c44#;
   pragma Export (C, u00125, "interfaces__c__stringsS");
   u00126 : constant Version_32 := 16#bc10dd48#;
   pragma Export (C, u00126, "system__win32S");
   u00127 : constant Version_32 := 16#6c27b264#;
   pragma Export (C, u00127, "system__task_primitives__operationsB");
   u00128 : constant Version_32 := 16#d11d0a3c#;
   pragma Export (C, u00128, "system__task_primitives__operationsS");
   u00129 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00129, "system__float_controlB");
   u00130 : constant Version_32 := 16#bf34ed6a#;
   pragma Export (C, u00130, "system__float_controlS");
   u00131 : constant Version_32 := 16#1826115c#;
   pragma Export (C, u00131, "system__interrupt_managementB");
   u00132 : constant Version_32 := 16#a0a25a36#;
   pragma Export (C, u00132, "system__interrupt_managementS");
   u00133 : constant Version_32 := 16#f65595cf#;
   pragma Export (C, u00133, "system__multiprocessorsB");
   u00134 : constant Version_32 := 16#67643125#;
   pragma Export (C, u00134, "system__multiprocessorsS");
   u00135 : constant Version_32 := 16#d950d226#;
   pragma Export (C, u00135, "system__os_primitivesB");
   u00136 : constant Version_32 := 16#ef19227f#;
   pragma Export (C, u00136, "system__os_primitivesS");
   u00137 : constant Version_32 := 16#0881bbf8#;
   pragma Export (C, u00137, "system__task_lockB");
   u00138 : constant Version_32 := 16#3e429938#;
   pragma Export (C, u00138, "system__task_lockS");
   u00139 : constant Version_32 := 16#1a9147da#;
   pragma Export (C, u00139, "system__win32__extS");
   u00140 : constant Version_32 := 16#b5dc4d53#;
   pragma Export (C, u00140, "system__task_infoB");
   u00141 : constant Version_32 := 16#882ea7a1#;
   pragma Export (C, u00141, "system__task_infoS");
   u00142 : constant Version_32 := 16#20babadf#;
   pragma Export (C, u00142, "system__taskingB");
   u00143 : constant Version_32 := 16#36778bfd#;
   pragma Export (C, u00143, "system__taskingS");
   u00144 : constant Version_32 := 16#4bc4ed76#;
   pragma Export (C, u00144, "system__stack_usageB");
   u00145 : constant Version_32 := 16#09222097#;
   pragma Export (C, u00145, "system__stack_usageS");
   u00146 : constant Version_32 := 16#13b33e5d#;
   pragma Export (C, u00146, "system__tasking__debugB");
   u00147 : constant Version_32 := 16#f32cb5c6#;
   pragma Export (C, u00147, "system__tasking__debugS");
   u00148 : constant Version_32 := 16#fd83e873#;
   pragma Export (C, u00148, "system__concat_2B");
   u00149 : constant Version_32 := 16#5d687986#;
   pragma Export (C, u00149, "system__concat_2S");
   u00150 : constant Version_32 := 16#2b70b149#;
   pragma Export (C, u00150, "system__concat_3B");
   u00151 : constant Version_32 := 16#54b8f2f3#;
   pragma Export (C, u00151, "system__concat_3S");
   u00152 : constant Version_32 := 16#a83b7c85#;
   pragma Export (C, u00152, "system__concat_6B");
   u00153 : constant Version_32 := 16#8d0f83e4#;
   pragma Export (C, u00153, "system__concat_6S");
   u00154 : constant Version_32 := 16#608e2cd1#;
   pragma Export (C, u00154, "system__concat_5B");
   u00155 : constant Version_32 := 16#d896ed78#;
   pragma Export (C, u00155, "system__concat_5S");
   u00156 : constant Version_32 := 16#932a4690#;
   pragma Export (C, u00156, "system__concat_4B");
   u00157 : constant Version_32 := 16#21ac8576#;
   pragma Export (C, u00157, "system__concat_4S");
   u00158 : constant Version_32 := 16#ee80728a#;
   pragma Export (C, u00158, "system__tracesB");
   u00159 : constant Version_32 := 16#add5c6fc#;
   pragma Export (C, u00159, "system__tracesS");
   u00160 : constant Version_32 := 16#c621f396#;
   pragma Export (C, u00160, "system__wch_wtsB");
   u00161 : constant Version_32 := 16#f7dca336#;
   pragma Export (C, u00161, "system__wch_wtsS");
   u00162 : constant Version_32 := 16#65712768#;
   pragma Export (C, u00162, "ada__calendarB");
   u00163 : constant Version_32 := 16#e791e294#;
   pragma Export (C, u00163, "ada__calendarS");
   u00164 : constant Version_32 := 16#12c24a43#;
   pragma Export (C, u00164, "ada__charactersS");
   u00165 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00165, "ada__characters__latin_1S");
   u00166 : constant Version_32 := 16#5e196e91#;
   pragma Export (C, u00166, "ada__containersS");
   u00167 : constant Version_32 := 16#654e2c4c#;
   pragma Export (C, u00167, "ada__containers__hash_tablesS");
   u00168 : constant Version_32 := 16#c24eaf4d#;
   pragma Export (C, u00168, "ada__containers__prime_numbersB");
   u00169 : constant Version_32 := 16#6d3af8ed#;
   pragma Export (C, u00169, "ada__containers__prime_numbersS");
   u00170 : constant Version_32 := 16#d9473c8c#;
   pragma Export (C, u00170, "ada__containers__red_black_treesS");
   u00171 : constant Version_32 := 16#af50e98f#;
   pragma Export (C, u00171, "ada__stringsS");
   u00172 : constant Version_32 := 16#e5480ede#;
   pragma Export (C, u00172, "ada__strings__fixedB");
   u00173 : constant Version_32 := 16#a86b22b3#;
   pragma Export (C, u00173, "ada__strings__fixedS");
   u00174 : constant Version_32 := 16#e2ea8656#;
   pragma Export (C, u00174, "ada__strings__mapsB");
   u00175 : constant Version_32 := 16#1e526bec#;
   pragma Export (C, u00175, "ada__strings__mapsS");
   u00176 : constant Version_32 := 16#374ed1bf#;
   pragma Export (C, u00176, "system__bit_opsB");
   u00177 : constant Version_32 := 16#0765e3a3#;
   pragma Export (C, u00177, "system__bit_opsS");
   u00178 : constant Version_32 := 16#c093955c#;
   pragma Export (C, u00178, "ada__strings__searchB");
   u00179 : constant Version_32 := 16#c1ab8667#;
   pragma Export (C, u00179, "ada__strings__searchS");
   u00180 : constant Version_32 := 16#bc438ed0#;
   pragma Export (C, u00180, "ada__strings__wide_fixedB");
   u00181 : constant Version_32 := 16#412537cd#;
   pragma Export (C, u00181, "ada__strings__wide_fixedS");
   u00182 : constant Version_32 := 16#83691158#;
   pragma Export (C, u00182, "ada__strings__wide_mapsB");
   u00183 : constant Version_32 := 16#f0f30b79#;
   pragma Export (C, u00183, "ada__strings__wide_mapsS");
   u00184 : constant Version_32 := 16#ffe20862#;
   pragma Export (C, u00184, "system__stream_attributesB");
   u00185 : constant Version_32 := 16#e5402c91#;
   pragma Export (C, u00185, "system__stream_attributesS");
   u00186 : constant Version_32 := 16#4f40de55#;
   pragma Export (C, u00186, "ada__strings__wide_searchB");
   u00187 : constant Version_32 := 16#1748eeac#;
   pragma Export (C, u00187, "ada__strings__wide_searchS");
   u00188 : constant Version_32 := 16#6391c20e#;
   pragma Export (C, u00188, "ada__strings__wide_unboundedB");
   u00189 : constant Version_32 := 16#592108ae#;
   pragma Export (C, u00189, "ada__strings__wide_unboundedS");
   u00190 : constant Version_32 := 16#1e5e46f6#;
   pragma Export (C, u00190, "system__compare_array_unsigned_16B");
   u00191 : constant Version_32 := 16#4bc9a3bd#;
   pragma Export (C, u00191, "system__compare_array_unsigned_16S");
   u00192 : constant Version_32 := 16#5f72f755#;
   pragma Export (C, u00192, "system__address_operationsB");
   u00193 : constant Version_32 := 16#4cc41065#;
   pragma Export (C, u00193, "system__address_operationsS");
   u00194 : constant Version_32 := 16#5073d598#;
   pragma Export (C, u00194, "system__machine_codeS");
   u00195 : constant Version_32 := 16#e5ac57f8#;
   pragma Export (C, u00195, "system__atomic_countersB");
   u00196 : constant Version_32 := 16#92b43a9c#;
   pragma Export (C, u00196, "system__atomic_countersS");
   u00197 : constant Version_32 := 16#15ec06fd#;
   pragma Export (C, u00197, "ada__strings__wide_unbounded__wide_hashB");
   u00198 : constant Version_32 := 16#f8259ef6#;
   pragma Export (C, u00198, "ada__strings__wide_unbounded__wide_hashS");
   u00199 : constant Version_32 := 16#1ac8b3b4#;
   pragma Export (C, u00199, "ada__text_ioB");
   u00200 : constant Version_32 := 16#ba9eea88#;
   pragma Export (C, u00200, "ada__text_ioS");
   u00201 : constant Version_32 := 16#a6cec3cf#;
   pragma Export (C, u00201, "system__strings__stream_opsB");
   u00202 : constant Version_32 := 16#5ed775a4#;
   pragma Export (C, u00202, "system__strings__stream_opsS");
   u00203 : constant Version_32 := 16#9f609ffa#;
   pragma Export (C, u00203, "ada__streams__stream_ioB");
   u00204 : constant Version_32 := 16#3aff46f1#;
   pragma Export (C, u00204, "ada__streams__stream_ioS");
   u00205 : constant Version_32 := 16#5de653db#;
   pragma Export (C, u00205, "system__communicationB");
   u00206 : constant Version_32 := 16#6f11cd91#;
   pragma Export (C, u00206, "system__communicationS");
   u00207 : constant Version_32 := 16#108898f9#;
   pragma Export (C, u00207, "neo__commandB");
   u00208 : constant Version_32 := 16#3273734b#;
   pragma Export (C, u00208, "neo__commandS");
   u00209 : constant Version_32 := 16#8f637df8#;
   pragma Export (C, u00209, "ada__characters__handlingB");
   u00210 : constant Version_32 := 16#3b3f6154#;
   pragma Export (C, u00210, "ada__characters__handlingS");
   u00211 : constant Version_32 := 16#92f05f13#;
   pragma Export (C, u00211, "ada__strings__maps__constantsS");
   u00212 : constant Version_32 := 16#9d44d325#;
   pragma Export (C, u00212, "ada__wide_charactersS");
   u00213 : constant Version_32 := 16#88fa0f3e#;
   pragma Export (C, u00213, "ada__wide_characters__handlingB");
   u00214 : constant Version_32 := 16#65805d01#;
   pragma Export (C, u00214, "ada__wide_characters__handlingS");
   u00215 : constant Version_32 := 16#8ae438bb#;
   pragma Export (C, u00215, "ada__wide_characters__unicodeB");
   u00216 : constant Version_32 := 16#7ae4a0bf#;
   pragma Export (C, u00216, "ada__wide_characters__unicodeS");
   u00217 : constant Version_32 := 16#27ccf663#;
   pragma Export (C, u00217, "system__utf_32B");
   u00218 : constant Version_32 := 16#c141479a#;
   pragma Export (C, u00218, "system__utf_32S");
   u00219 : constant Version_32 := 16#01b580b5#;
   pragma Export (C, u00219, "neo__systemB");
   u00220 : constant Version_32 := 16#964f410c#;
   pragma Export (C, u00220, "neo__systemS");
   u00221 : constant Version_32 := 16#43ffa9ae#;
   pragma Export (C, u00221, "neo__windowsS");
   u00222 : constant Version_32 := 16#cc40077e#;
   pragma Export (C, u00222, "ada__task_identificationB");
   u00223 : constant Version_32 := 16#c587da2a#;
   pragma Export (C, u00223, "ada__task_identificationS");
   u00224 : constant Version_32 := 16#a19fa84a#;
   pragma Export (C, u00224, "system__tasking__utilitiesB");
   u00225 : constant Version_32 := 16#35b8be73#;
   pragma Export (C, u00225, "system__tasking__utilitiesS");
   u00226 : constant Version_32 := 16#f0dca0fc#;
   pragma Export (C, u00226, "system__tasking__initializationB");
   u00227 : constant Version_32 := 16#f20398cb#;
   pragma Export (C, u00227, "system__tasking__initializationS");
   u00228 : constant Version_32 := 16#cbb1adb5#;
   pragma Export (C, u00228, "system__tasking__queuingB");
   u00229 : constant Version_32 := 16#3d02e133#;
   pragma Export (C, u00229, "system__tasking__queuingS");
   u00230 : constant Version_32 := 16#61dc6741#;
   pragma Export (C, u00230, "system__tasking__protected_objects__entriesB");
   u00231 : constant Version_32 := 16#7671a6ef#;
   pragma Export (C, u00231, "system__tasking__protected_objects__entriesS");
   u00232 : constant Version_32 := 16#100eaf58#;
   pragma Export (C, u00232, "system__restrictionsB");
   u00233 : constant Version_32 := 16#1344a388#;
   pragma Export (C, u00233, "system__restrictionsS");
   u00234 : constant Version_32 := 16#bd6fc52e#;
   pragma Export (C, u00234, "system__traces__taskingB");
   u00235 : constant Version_32 := 16#33a47127#;
   pragma Export (C, u00235, "system__traces__taskingS");
   u00236 : constant Version_32 := 16#f89f7823#;
   pragma Export (C, u00236, "system__val_boolB");
   u00237 : constant Version_32 := 16#a55ac248#;
   pragma Export (C, u00237, "system__val_boolS");
   u00238 : constant Version_32 := 16#191e63db#;
   pragma Export (C, u00238, "neo__system__communityB");
   u00239 : constant Version_32 := 16#5b435fe9#;
   pragma Export (C, u00239, "neo__system__communityS");
   u00240 : constant Version_32 := 16#fa0bb953#;
   pragma Export (C, u00240, "neo__system__graphicsB");
   u00241 : constant Version_32 := 16#a9805e48#;
   pragma Export (C, u00241, "neo__system__graphicsS");
   u00242 : constant Version_32 := 16#e66df59b#;
   pragma Export (C, u00242, "neo__system__graphics__windowB");
   u00243 : constant Version_32 := 16#ba8e3239#;
   pragma Export (C, u00243, "neo__system__graphics__windowS");
   u00244 : constant Version_32 := 16#37f53200#;
   pragma Export (C, u00244, "neo__system__inputB");
   u00245 : constant Version_32 := 16#92f9d32e#;
   pragma Export (C, u00245, "neo__system__inputS");
   u00246 : constant Version_32 := 16#7bf4f215#;
   pragma Export (C, u00246, "ada__calendar__delaysB");
   u00247 : constant Version_32 := 16#474dd4b1#;
   pragma Export (C, u00247, "ada__calendar__delaysS");
   u00248 : constant Version_32 := 16#4eafb909#;
   pragma Export (C, u00248, "ada__real_timeB");
   u00249 : constant Version_32 := 16#41de19c7#;
   pragma Export (C, u00249, "ada__real_timeS");
   u00250 : constant Version_32 := 16#1607bce4#;
   pragma Export (C, u00250, "system__arith_64B");
   u00251 : constant Version_32 := 16#7628ba1d#;
   pragma Export (C, u00251, "system__arith_64S");
   u00252 : constant Version_32 := 16#055106b8#;
   pragma Export (C, u00252, "system__tasking__rendezvousB");
   u00253 : constant Version_32 := 16#6c6c00bf#;
   pragma Export (C, u00253, "system__tasking__rendezvousS");
   u00254 : constant Version_32 := 16#8b31dbf8#;
   pragma Export (C, u00254, "system__tasking__entry_callsB");
   u00255 : constant Version_32 := 16#e5160f9e#;
   pragma Export (C, u00255, "system__tasking__entry_callsS");
   u00256 : constant Version_32 := 16#561f22c0#;
   pragma Export (C, u00256, "system__tasking__protected_objects__operationsB");
   u00257 : constant Version_32 := 16#fae24494#;
   pragma Export (C, u00257, "system__tasking__protected_objects__operationsS");
   u00258 : constant Version_32 := 16#ded60134#;
   pragma Export (C, u00258, "system__tasking__stagesB");
   u00259 : constant Version_32 := 16#4a36ba98#;
   pragma Export (C, u00259, "system__tasking__stagesS");
   u00260 : constant Version_32 := 16#4b37b589#;
   pragma Export (C, u00260, "system__val_enumB");
   u00261 : constant Version_32 := 16#e4d2ecc3#;
   pragma Export (C, u00261, "system__val_enumS");
   u00262 : constant Version_32 := 16#f8f38c17#;
   pragma Export (C, u00262, "system__val_intB");
   u00263 : constant Version_32 := 16#176d8469#;
   pragma Export (C, u00263, "system__val_intS");
   u00264 : constant Version_32 := 16#e2c21afe#;
   pragma Export (C, u00264, "neo__system__memoryB");
   u00265 : constant Version_32 := 16#95da4ee9#;
   pragma Export (C, u00265, "neo__system__memoryS");
   u00266 : constant Version_32 := 16#56e74f1a#;
   pragma Export (C, u00266, "system__img_realB");
   u00267 : constant Version_32 := 16#9860ffb4#;
   pragma Export (C, u00267, "system__img_realS");
   u00268 : constant Version_32 := 16#80f37066#;
   pragma Export (C, u00268, "system__fat_llfS");
   u00269 : constant Version_32 := 16#0fb8c821#;
   pragma Export (C, u00269, "system__powten_tableS");
   u00270 : constant Version_32 := 16#1c0a9ca0#;
   pragma Export (C, u00270, "neo__system__processorB");
   u00271 : constant Version_32 := 16#fa292284#;
   pragma Export (C, u00271, "neo__system__processorS");
   u00272 : constant Version_32 := 16#fc348704#;
   pragma Export (C, u00272, "neo__system__textB");
   u00273 : constant Version_32 := 16#5113dd26#;
   pragma Export (C, u00273, "neo__system__textS");
   u00274 : constant Version_32 := 16#c25f5af8#;
   pragma Export (C, u00274, "neo__system__text__consoleB");
   u00275 : constant Version_32 := 16#4bb04ff5#;
   pragma Export (C, u00275, "neo__system__text__consoleS");
   u00276 : constant Version_32 := 16#7a13e6d7#;
   pragma Export (C, u00276, "ada__calendar__formattingB");
   u00277 : constant Version_32 := 16#929f882b#;
   pragma Export (C, u00277, "ada__calendar__formattingS");
   u00278 : constant Version_32 := 16#e3cca715#;
   pragma Export (C, u00278, "ada__calendar__time_zonesB");
   u00279 : constant Version_32 := 16#98f012d7#;
   pragma Export (C, u00279, "ada__calendar__time_zonesS");
   u00280 : constant Version_32 := 16#8ff77155#;
   pragma Export (C, u00280, "system__val_realB");
   u00281 : constant Version_32 := 16#a1e1d947#;
   pragma Export (C, u00281, "system__val_realS");
   u00282 : constant Version_32 := 16#0be1b996#;
   pragma Export (C, u00282, "system__exn_llfB");
   u00283 : constant Version_32 := 16#de4cb0b9#;
   pragma Export (C, u00283, "system__exn_llfS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.handling%s
   --  ada.characters.latin_1%s
   --  ada.wide_characters%s
   --  ada.wide_characters.handling%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.arith_64%s
   --  system.atomic_counters%s
   --  system.case_util%s
   --  system.case_util%b
   --  system.exn_llf%s
   --  system.exn_llf%b
   --  system.float_control%s
   --  system.float_control%b
   --  system.htable%s
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_real%s
   --  system.io%s
   --  system.io%b
   --  system.machine_code%s
   --  system.atomic_counters%b
   --  system.multiprocessors%s
   --  system.os_primitives%s
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.powten_table%s
   --  system.restrictions%s
   --  system.restrictions%b
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.stack_usage%s
   --  system.stack_usage%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.os_lib%s
   --  system.task_lock%s
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  system.arith_64%b
   --  ada.exceptions.is_null_occurrence%s
   --  ada.exceptions.is_null_occurrence%b
   --  system.soft_links%s
   --  system.task_lock%b
   --  system.traces%s
   --  system.traces%b
   --  system.unsigned_types%s
   --  system.fat_llf%s
   --  system.img_biu%s
   --  system.img_biu%b
   --  system.img_llb%s
   --  system.img_llb%b
   --  system.img_llu%s
   --  system.img_llu%b
   --  system.img_llw%s
   --  system.img_llw%b
   --  system.img_uns%s
   --  system.img_uns%b
   --  system.img_real%b
   --  system.img_wiu%s
   --  system.img_wiu%b
   --  system.utf_32%s
   --  system.utf_32%b
   --  ada.wide_characters.unicode%s
   --  ada.wide_characters.unicode%b
   --  system.val_bool%s
   --  system.val_enum%s
   --  system.val_int%s
   --  system.val_llu%s
   --  system.val_real%s
   --  system.val_uns%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_uns%b
   --  system.val_real%b
   --  system.val_llu%b
   --  system.val_int%b
   --  system.val_enum%b
   --  system.val_bool%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  system.wch_wts%s
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  system.address_image%s
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.compare_array_unsigned_16%s
   --  system.compare_array_unsigned_16%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.concat_4%s
   --  system.concat_4%b
   --  system.concat_5%s
   --  system.concat_5%b
   --  system.concat_6%s
   --  system.concat_6%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.containers%s
   --  ada.containers.hash_tables%s
   --  ada.containers.prime_numbers%s
   --  ada.containers.prime_numbers%b
   --  ada.containers.red_black_trees%s
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.strings.maps%s
   --  ada.strings.fixed%s
   --  ada.strings.maps.constants%s
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.tags%s
   --  ada.streams%s
   --  ada.streams%b
   --  interfaces.c%s
   --  system.multiprocessors%b
   --  interfaces.c.strings%s
   --  system.communication%s
   --  system.communication%b
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  ada.finalization%b
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.storage_pools.subpools%s
   --  system.storage_pools.subpools.finalization%s
   --  system.storage_pools.subpools.finalization%b
   --  system.stream_attributes%s
   --  system.stream_attributes%b
   --  system.win32%s
   --  system.os_interface%s
   --  system.interrupt_management%s
   --  system.interrupt_management%b
   --  system.task_info%s
   --  system.task_info%b
   --  system.task_primitives%s
   --  system.tasking%s
   --  ada.task_identification%s
   --  system.task_primitives.operations%s
   --  system.tasking%b
   --  system.tasking.debug%s
   --  system.traces.tasking%s
   --  system.traces.tasking%b
   --  system.win32.ext%s
   --  system.task_primitives.operations%b
   --  system.os_primitives%b
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.calendar.delays%s
   --  ada.calendar.delays%b
   --  ada.calendar.time_zones%s
   --  ada.calendar.time_zones%b
   --  ada.calendar.formatting%s
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.file_control_block%s
   --  ada.streams.stream_io%s
   --  system.file_io%s
   --  ada.streams.stream_io%b
   --  ada.wide_text_io%s
   --  ada.wide_text_io.generic_aux%s
   --  ada.wide_text_io.generic_aux%b
   --  ada.wide_text_io.modular_aux%s
   --  ada.wide_text_io.modular_aux%b
   --  system.secondary_stack%s
   --  system.file_io%b
   --  system.tasking.debug%b
   --  system.storage_pools.subpools%b
   --  system.finalization_masters%b
   --  interfaces.c.strings%b
   --  interfaces.c%b
   --  ada.tags%b
   --  ada.strings.fixed%b
   --  ada.strings.maps%b
   --  system.wch_wts%b
   --  system.soft_links%b
   --  system.os_lib%b
   --  ada.wide_characters.handling%b
   --  ada.characters.handling%b
   --  system.secondary_stack%b
   --  ada.calendar.formatting%b
   --  system.address_image%b
   --  ada.strings.wide_maps%s
   --  ada.strings.wide_maps%b
   --  ada.strings.wide_fixed%s
   --  ada.strings.wide_search%s
   --  ada.strings.wide_search%b
   --  ada.strings.wide_fixed%b
   --  ada.strings.wide_unbounded%s
   --  ada.strings.wide_unbounded%b
   --  ada.strings.wide_unbounded.wide_hash%s
   --  ada.strings.wide_unbounded.wide_hash%b
   --  system.soft_links.tasking%s
   --  system.soft_links.tasking%b
   --  system.strings.stream_ops%s
   --  system.strings.stream_ops%b
   --  system.tasking.entry_calls%s
   --  system.tasking.initialization%s
   --  system.tasking.utilities%s
   --  ada.task_identification%b
   --  system.traceback%s
   --  ada.exceptions%b
   --  system.traceback%b
   --  system.tasking.initialization%b
   --  ada.wide_text_io%b
   --  ada.real_time%s
   --  ada.real_time%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  system.tasking.protected_objects%s
   --  system.tasking.protected_objects%b
   --  system.tasking.protected_objects.entries%s
   --  system.tasking.protected_objects.entries%b
   --  system.tasking.queuing%s
   --  system.tasking.queuing%b
   --  system.tasking.utilities%b
   --  system.tasking.rendezvous%s
   --  system.tasking.protected_objects.operations%s
   --  system.tasking.protected_objects.operations%b
   --  system.tasking.rendezvous%b
   --  system.tasking.entry_calls%b
   --  system.tasking.stages%s
   --  system.tasking.stages%b
   --  neo%s
   --  neo%b
   --  neo.command%s
   --  neo.command%b
   --  neo.windows%s
   --  neo.system%s
   --  neo.system%b
   --  neo.system.community%s
   --  neo.system.community%b
   --  neo.system.graphics%s
   --  neo.system.graphics%b
   --  neo.system.input%s
   --  neo.system.input%b
   --  neo.system.graphics.window%s
   --  neo.system.graphics.window%b
   --  neo.system.memory%s
   --  neo.system.memory%b
   --  neo.system.processor%s
   --  neo.system.processor%b
   --  neo.system.text%s
   --  neo.system.text%b
   --  neo.system.text.console%s
   --  neo.system.text.console%b
   --  main%b
   --  END ELABORATION ORDER


end ada_main;

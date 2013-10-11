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
                    "GNAT Version: GPL 2013 (20130314)" & ASCII.NUL;
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
   u00001 : constant Version_32 := 16#f416cc4a#;
   pragma Export (C, u00001, "mainB");
   u00002 : constant Version_32 := 16#3935bd10#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#51a8eec5#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#5ea2bd7b#;
   pragma Export (C, u00005, "ada__exceptionsB");
   u00006 : constant Version_32 := 16#6380a30f#;
   pragma Export (C, u00006, "ada__exceptionsS");
   u00007 : constant Version_32 := 16#16173147#;
   pragma Export (C, u00007, "ada__exceptions__last_chance_handlerB");
   u00008 : constant Version_32 := 16#1f42fb5e#;
   pragma Export (C, u00008, "ada__exceptions__last_chance_handlerS");
   u00009 : constant Version_32 := 16#5fc8ae56#;
   pragma Export (C, u00009, "systemS");
   u00010 : constant Version_32 := 16#0071025c#;
   pragma Export (C, u00010, "system__soft_linksB");
   u00011 : constant Version_32 := 16#3293d48b#;
   pragma Export (C, u00011, "system__soft_linksS");
   u00012 : constant Version_32 := 16#27940d94#;
   pragma Export (C, u00012, "system__parametersB");
   u00013 : constant Version_32 := 16#e92aa296#;
   pragma Export (C, u00013, "system__parametersS");
   u00014 : constant Version_32 := 16#17775d6d#;
   pragma Export (C, u00014, "system__secondary_stackB");
   u00015 : constant Version_32 := 16#4ba689f8#;
   pragma Export (C, u00015, "system__secondary_stackS");
   u00016 : constant Version_32 := 16#ace32e1e#;
   pragma Export (C, u00016, "system__storage_elementsB");
   u00017 : constant Version_32 := 16#a505d3ce#;
   pragma Export (C, u00017, "system__storage_elementsS");
   u00018 : constant Version_32 := 16#4f750b3b#;
   pragma Export (C, u00018, "system__stack_checkingB");
   u00019 : constant Version_32 := 16#fc6a127a#;
   pragma Export (C, u00019, "system__stack_checkingS");
   u00020 : constant Version_32 := 16#7b9f0bae#;
   pragma Export (C, u00020, "system__exception_tableB");
   u00021 : constant Version_32 := 16#cea672f3#;
   pragma Export (C, u00021, "system__exception_tableS");
   u00022 : constant Version_32 := 16#5665ab64#;
   pragma Export (C, u00022, "system__htableB");
   u00023 : constant Version_32 := 16#dc60e058#;
   pragma Export (C, u00023, "system__htableS");
   u00024 : constant Version_32 := 16#8b7dad61#;
   pragma Export (C, u00024, "system__string_hashB");
   u00025 : constant Version_32 := 16#795476c2#;
   pragma Export (C, u00025, "system__string_hashS");
   u00026 : constant Version_32 := 16#aad75561#;
   pragma Export (C, u00026, "system__exceptionsB");
   u00027 : constant Version_32 := 16#533666e1#;
   pragma Export (C, u00027, "system__exceptionsS");
   u00028 : constant Version_32 := 16#010db1dc#;
   pragma Export (C, u00028, "system__exceptions_debugB");
   u00029 : constant Version_32 := 16#67b88b82#;
   pragma Export (C, u00029, "system__exceptions_debugS");
   u00030 : constant Version_32 := 16#b012ff50#;
   pragma Export (C, u00030, "system__img_intB");
   u00031 : constant Version_32 := 16#5d134e94#;
   pragma Export (C, u00031, "system__img_intS");
   u00032 : constant Version_32 := 16#dc8e33ed#;
   pragma Export (C, u00032, "system__tracebackB");
   u00033 : constant Version_32 := 16#3e4f7a23#;
   pragma Export (C, u00033, "system__tracebackS");
   u00034 : constant Version_32 := 16#907d882f#;
   pragma Export (C, u00034, "system__wch_conB");
   u00035 : constant Version_32 := 16#e023806b#;
   pragma Export (C, u00035, "system__wch_conS");
   u00036 : constant Version_32 := 16#22fed88a#;
   pragma Export (C, u00036, "system__wch_stwB");
   u00037 : constant Version_32 := 16#cd32ac6a#;
   pragma Export (C, u00037, "system__wch_stwS");
   u00038 : constant Version_32 := 16#617a40f2#;
   pragma Export (C, u00038, "system__wch_cnvB");
   u00039 : constant Version_32 := 16#fedd06bd#;
   pragma Export (C, u00039, "system__wch_cnvS");
   u00040 : constant Version_32 := 16#cb4a8015#;
   pragma Export (C, u00040, "interfacesS");
   u00041 : constant Version_32 := 16#75729fba#;
   pragma Export (C, u00041, "system__wch_jisB");
   u00042 : constant Version_32 := 16#aaaf9da9#;
   pragma Export (C, u00042, "system__wch_jisS");
   u00043 : constant Version_32 := 16#ada34a87#;
   pragma Export (C, u00043, "system__traceback_entriesB");
   u00044 : constant Version_32 := 16#0de94017#;
   pragma Export (C, u00044, "system__traceback_entriesS");
   u00045 : constant Version_32 := 16#28c4f46d#;
   pragma Export (C, u00045, "ada__real_timeB");
   u00046 : constant Version_32 := 16#41de19c7#;
   pragma Export (C, u00046, "ada__real_timeS");
   u00047 : constant Version_32 := 16#aa574b29#;
   pragma Export (C, u00047, "system__arith_64B");
   u00048 : constant Version_32 := 16#d4cf8bb1#;
   pragma Export (C, u00048, "system__arith_64S");
   u00049 : constant Version_32 := 16#91dfc027#;
   pragma Export (C, u00049, "system__taskingB");
   u00050 : constant Version_32 := 16#d83d5e83#;
   pragma Export (C, u00050, "system__taskingS");
   u00051 : constant Version_32 := 16#4ff85dba#;
   pragma Export (C, u00051, "system__task_primitivesS");
   u00052 : constant Version_32 := 16#cf49590f#;
   pragma Export (C, u00052, "system__os_interfaceS");
   u00053 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00053, "interfaces__cB");
   u00054 : constant Version_32 := 16#29899d4e#;
   pragma Export (C, u00054, "interfaces__cS");
   u00055 : constant Version_32 := 16#507533cc#;
   pragma Export (C, u00055, "interfaces__c__stringsB");
   u00056 : constant Version_32 := 16#603c1c44#;
   pragma Export (C, u00056, "interfaces__c__stringsS");
   u00057 : constant Version_32 := 16#ee4e202b#;
   pragma Export (C, u00057, "system__win32S");
   u00058 : constant Version_32 := 16#07176fd5#;
   pragma Export (C, u00058, "system__task_primitives__operationsB");
   u00059 : constant Version_32 := 16#074ed32a#;
   pragma Export (C, u00059, "system__task_primitives__operationsS");
   u00060 : constant Version_32 := 16#6f001a54#;
   pragma Export (C, u00060, "system__exp_unsB");
   u00061 : constant Version_32 := 16#08e5518a#;
   pragma Export (C, u00061, "system__exp_unsS");
   u00062 : constant Version_32 := 16#3529f220#;
   pragma Export (C, u00062, "system__unsigned_typesS");
   u00063 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00063, "system__float_controlB");
   u00064 : constant Version_32 := 16#bf34ed6a#;
   pragma Export (C, u00064, "system__float_controlS");
   u00065 : constant Version_32 := 16#1826115c#;
   pragma Export (C, u00065, "system__interrupt_managementB");
   u00066 : constant Version_32 := 16#a0a25a36#;
   pragma Export (C, u00066, "system__interrupt_managementS");
   u00067 : constant Version_32 := 16#f65595cf#;
   pragma Export (C, u00067, "system__multiprocessorsB");
   u00068 : constant Version_32 := 16#67643125#;
   pragma Export (C, u00068, "system__multiprocessorsS");
   u00069 : constant Version_32 := 16#d950d226#;
   pragma Export (C, u00069, "system__os_primitivesB");
   u00070 : constant Version_32 := 16#ef19227f#;
   pragma Export (C, u00070, "system__os_primitivesS");
   u00071 : constant Version_32 := 16#863f9596#;
   pragma Export (C, u00071, "system__task_lockB");
   u00072 : constant Version_32 := 16#3e429938#;
   pragma Export (C, u00072, "system__task_lockS");
   u00073 : constant Version_32 := 16#48cfbab9#;
   pragma Export (C, u00073, "system__win32__extS");
   u00074 : constant Version_32 := 16#5052be8c#;
   pragma Export (C, u00074, "system__task_infoB");
   u00075 : constant Version_32 := 16#3ffea91d#;
   pragma Export (C, u00075, "system__task_infoS");
   u00076 : constant Version_32 := 16#1692df3b#;
   pragma Export (C, u00076, "system__tasking__debugB");
   u00077 : constant Version_32 := 16#f32cb5c6#;
   pragma Export (C, u00077, "system__tasking__debugS");
   u00078 : constant Version_32 := 16#39591e91#;
   pragma Export (C, u00078, "system__concat_2B");
   u00079 : constant Version_32 := 16#a4185caa#;
   pragma Export (C, u00079, "system__concat_2S");
   u00080 : constant Version_32 := 16#ae97ef6c#;
   pragma Export (C, u00080, "system__concat_3B");
   u00081 : constant Version_32 := 16#29e2ac3c#;
   pragma Export (C, u00081, "system__concat_3S");
   u00082 : constant Version_32 := 16#c9fdc962#;
   pragma Export (C, u00082, "system__concat_6B");
   u00083 : constant Version_32 := 16#98025b42#;
   pragma Export (C, u00083, "system__concat_6S");
   u00084 : constant Version_32 := 16#def1dd00#;
   pragma Export (C, u00084, "system__concat_5B");
   u00085 : constant Version_32 := 16#4ff160f7#;
   pragma Export (C, u00085, "system__concat_5S");
   u00086 : constant Version_32 := 16#3493e6c0#;
   pragma Export (C, u00086, "system__concat_4B");
   u00087 : constant Version_32 := 16#5d974de8#;
   pragma Export (C, u00087, "system__concat_4S");
   u00088 : constant Version_32 := 16#36e568f7#;
   pragma Export (C, u00088, "system__crtlS");
   u00089 : constant Version_32 := 16#1eab0e09#;
   pragma Export (C, u00089, "system__img_enum_newB");
   u00090 : constant Version_32 := 16#d8cf65a6#;
   pragma Export (C, u00090, "system__img_enum_newS");
   u00091 : constant Version_32 := 16#194ccd7b#;
   pragma Export (C, u00091, "system__img_unsB");
   u00092 : constant Version_32 := 16#aaddced7#;
   pragma Export (C, u00092, "system__img_unsS");
   u00093 : constant Version_32 := 16#083296f2#;
   pragma Export (C, u00093, "system__stack_usageB");
   u00094 : constant Version_32 := 16#7ccb26a7#;
   pragma Export (C, u00094, "system__stack_usageS");
   u00095 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00095, "system__ioB");
   u00096 : constant Version_32 := 16#c18a5919#;
   pragma Export (C, u00096, "system__ioS");
   u00097 : constant Version_32 := 16#afd62b40#;
   pragma Export (C, u00097, "ada__tagsB");
   u00098 : constant Version_32 := 16#f6fc5406#;
   pragma Export (C, u00098, "ada__tagsS");
   u00099 : constant Version_32 := 16#79817c71#;
   pragma Export (C, u00099, "system__val_unsB");
   u00100 : constant Version_32 := 16#25811f1b#;
   pragma Export (C, u00100, "system__val_unsS");
   u00101 : constant Version_32 := 16#aea309ed#;
   pragma Export (C, u00101, "system__val_utilB");
   u00102 : constant Version_32 := 16#f36818a8#;
   pragma Export (C, u00102, "system__val_utilS");
   u00103 : constant Version_32 := 16#b7fa72e7#;
   pragma Export (C, u00103, "system__case_utilB");
   u00104 : constant Version_32 := 16#f2d4cede#;
   pragma Export (C, u00104, "system__case_utilS");
   u00105 : constant Version_32 := 16#6cd44ed4#;
   pragma Export (C, u00105, "neoB");
   u00106 : constant Version_32 := 16#ff74aa2f#;
   pragma Export (C, u00106, "neoS");
   u00107 : constant Version_32 := 16#45724809#;
   pragma Export (C, u00107, "ada__calendar__delaysB");
   u00108 : constant Version_32 := 16#474dd4b1#;
   pragma Export (C, u00108, "ada__calendar__delaysS");
   u00109 : constant Version_32 := 16#8ba0787e#;
   pragma Export (C, u00109, "ada__calendarB");
   u00110 : constant Version_32 := 16#e791e294#;
   pragma Export (C, u00110, "ada__calendarS");
   u00111 : constant Version_32 := 16#ee80728a#;
   pragma Export (C, u00111, "system__tracesB");
   u00112 : constant Version_32 := 16#add5c6fc#;
   pragma Export (C, u00112, "system__tracesS");
   u00113 : constant Version_32 := 16#421d3150#;
   pragma Export (C, u00113, "ada__text_ioB");
   u00114 : constant Version_32 := 16#152cee1e#;
   pragma Export (C, u00114, "ada__text_ioS");
   u00115 : constant Version_32 := 16#1358602f#;
   pragma Export (C, u00115, "ada__streamsS");
   u00116 : constant Version_32 := 16#e0b7a7e8#;
   pragma Export (C, u00116, "interfaces__c_streamsB");
   u00117 : constant Version_32 := 16#95ad191f#;
   pragma Export (C, u00117, "interfaces__c_streamsS");
   u00118 : constant Version_32 := 16#228a5436#;
   pragma Export (C, u00118, "system__file_ioB");
   u00119 : constant Version_32 := 16#ce89cf71#;
   pragma Export (C, u00119, "system__file_ioS");
   u00120 : constant Version_32 := 16#8cbe6205#;
   pragma Export (C, u00120, "ada__finalizationB");
   u00121 : constant Version_32 := 16#22e22193#;
   pragma Export (C, u00121, "ada__finalizationS");
   u00122 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00122, "system__finalization_rootB");
   u00123 : constant Version_32 := 16#103addc6#;
   pragma Export (C, u00123, "system__finalization_rootS");
   u00124 : constant Version_32 := 16#b46168d5#;
   pragma Export (C, u00124, "ada__io_exceptionsS");
   u00125 : constant Version_32 := 16#d6bc4ecc#;
   pragma Export (C, u00125, "system__crtl__runtimeS");
   u00126 : constant Version_32 := 16#f6ee85e9#;
   pragma Export (C, u00126, "system__os_libB");
   u00127 : constant Version_32 := 16#89dce9aa#;
   pragma Export (C, u00127, "system__os_libS");
   u00128 : constant Version_32 := 16#4cd8aca0#;
   pragma Export (C, u00128, "system__stringsB");
   u00129 : constant Version_32 := 16#e822e492#;
   pragma Export (C, u00129, "system__stringsS");
   u00130 : constant Version_32 := 16#782cc428#;
   pragma Export (C, u00130, "system__file_control_blockS");
   u00131 : constant Version_32 := 16#91d2300e#;
   pragma Export (C, u00131, "system__finalization_mastersB");
   u00132 : constant Version_32 := 16#353d027a#;
   pragma Export (C, u00132, "system__finalization_mastersS");
   u00133 : constant Version_32 := 16#57a37a42#;
   pragma Export (C, u00133, "system__address_imageB");
   u00134 : constant Version_32 := 16#fe24336c#;
   pragma Export (C, u00134, "system__address_imageS");
   u00135 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00135, "system__img_boolB");
   u00136 : constant Version_32 := 16#aa11dfbd#;
   pragma Export (C, u00136, "system__img_boolS");
   u00137 : constant Version_32 := 16#a7a37cb6#;
   pragma Export (C, u00137, "system__storage_poolsB");
   u00138 : constant Version_32 := 16#8c66b13b#;
   pragma Export (C, u00138, "system__storage_poolsS");
   u00139 : constant Version_32 := 16#ba5d60c7#;
   pragma Export (C, u00139, "system__pool_globalB");
   u00140 : constant Version_32 := 16#d56df0a6#;
   pragma Export (C, u00140, "system__pool_globalS");
   u00141 : constant Version_32 := 16#742a8355#;
   pragma Export (C, u00141, "system__memoryB");
   u00142 : constant Version_32 := 16#95431243#;
   pragma Export (C, u00142, "system__memoryS");
   u00143 : constant Version_32 := 16#1fd820b1#;
   pragma Export (C, u00143, "system__storage_pools__subpoolsB");
   u00144 : constant Version_32 := 16#951e0de9#;
   pragma Export (C, u00144, "system__storage_pools__subpoolsS");
   u00145 : constant Version_32 := 16#1777d351#;
   pragma Export (C, u00145, "system__storage_pools__subpools__finalizationB");
   u00146 : constant Version_32 := 16#12aaf1de#;
   pragma Export (C, u00146, "system__storage_pools__subpools__finalizationS");
   u00147 : constant Version_32 := 16#a347755d#;
   pragma Export (C, u00147, "ada__text_io__modular_auxB");
   u00148 : constant Version_32 := 16#534ccfb2#;
   pragma Export (C, u00148, "ada__text_io__modular_auxS");
   u00149 : constant Version_32 := 16#cd6ba629#;
   pragma Export (C, u00149, "ada__text_io__generic_auxB");
   u00150 : constant Version_32 := 16#a6c327d3#;
   pragma Export (C, u00150, "ada__text_io__generic_auxS");
   u00151 : constant Version_32 := 16#ef6c8032#;
   pragma Export (C, u00151, "system__img_biuB");
   u00152 : constant Version_32 := 16#f30b7a6d#;
   pragma Export (C, u00152, "system__img_biuS");
   u00153 : constant Version_32 := 16#10618bf9#;
   pragma Export (C, u00153, "system__img_llbB");
   u00154 : constant Version_32 := 16#b2cc6a93#;
   pragma Export (C, u00154, "system__img_llbS");
   u00155 : constant Version_32 := 16#06417083#;
   pragma Export (C, u00155, "system__img_lluB");
   u00156 : constant Version_32 := 16#7ce0f2e3#;
   pragma Export (C, u00156, "system__img_lluS");
   u00157 : constant Version_32 := 16#f931f062#;
   pragma Export (C, u00157, "system__img_llwB");
   u00158 : constant Version_32 := 16#1ba04905#;
   pragma Export (C, u00158, "system__img_llwS");
   u00159 : constant Version_32 := 16#b532ff4e#;
   pragma Export (C, u00159, "system__img_wiuB");
   u00160 : constant Version_32 := 16#9d4afdff#;
   pragma Export (C, u00160, "system__img_wiuS");
   u00161 : constant Version_32 := 16#25c21d28#;
   pragma Export (C, u00161, "system__val_lluB");
   u00162 : constant Version_32 := 16#4fdba552#;
   pragma Export (C, u00162, "system__val_lluS");
   u00163 : constant Version_32 := 16#bb8952df#;
   pragma Export (C, u00163, "system__tasking__protected_objectsB");
   u00164 : constant Version_32 := 16#09cb1bb5#;
   pragma Export (C, u00164, "system__tasking__protected_objectsS");
   u00165 : constant Version_32 := 16#1151ce70#;
   pragma Export (C, u00165, "system__soft_links__taskingB");
   u00166 : constant Version_32 := 16#6ac0d6d0#;
   pragma Export (C, u00166, "system__soft_links__taskingS");
   u00167 : constant Version_32 := 16#17d21067#;
   pragma Export (C, u00167, "ada__exceptions__is_null_occurrenceB");
   u00168 : constant Version_32 := 16#d832eaef#;
   pragma Export (C, u00168, "ada__exceptions__is_null_occurrenceS");
   u00169 : constant Version_32 := 16#5e196e91#;
   pragma Export (C, u00169, "ada__containersS");
   u00170 : constant Version_32 := 16#654e2c4c#;
   pragma Export (C, u00170, "ada__containers__hash_tablesS");
   u00171 : constant Version_32 := 16#c24eaf4d#;
   pragma Export (C, u00171, "ada__containers__prime_numbersB");
   u00172 : constant Version_32 := 16#6d3af8ed#;
   pragma Export (C, u00172, "ada__containers__prime_numbersS");
   u00173 : constant Version_32 := 16#af50e98f#;
   pragma Export (C, u00173, "ada__stringsS");
   u00174 : constant Version_32 := 16#914b496f#;
   pragma Export (C, u00174, "ada__strings__fixedB");
   u00175 : constant Version_32 := 16#dc686502#;
   pragma Export (C, u00175, "ada__strings__fixedS");
   u00176 : constant Version_32 := 16#96e9c1e7#;
   pragma Export (C, u00176, "ada__strings__mapsB");
   u00177 : constant Version_32 := 16#24318e4c#;
   pragma Export (C, u00177, "ada__strings__mapsS");
   u00178 : constant Version_32 := 16#d7ba84a5#;
   pragma Export (C, u00178, "system__bit_opsB");
   u00179 : constant Version_32 := 16#c30e4013#;
   pragma Export (C, u00179, "system__bit_opsS");
   u00180 : constant Version_32 := 16#12c24a43#;
   pragma Export (C, u00180, "ada__charactersS");
   u00181 : constant Version_32 := 16#051b1b7b#;
   pragma Export (C, u00181, "ada__characters__latin_1S");
   u00182 : constant Version_32 := 16#b490d2ed#;
   pragma Export (C, u00182, "ada__strings__searchB");
   u00183 : constant Version_32 := 16#b5a8c1d6#;
   pragma Export (C, u00183, "ada__strings__searchS");
   u00184 : constant Version_32 := 16#bc438ed0#;
   pragma Export (C, u00184, "ada__strings__wide_fixedB");
   u00185 : constant Version_32 := 16#412537cd#;
   pragma Export (C, u00185, "ada__strings__wide_fixedS");
   u00186 : constant Version_32 := 16#83691158#;
   pragma Export (C, u00186, "ada__strings__wide_mapsB");
   u00187 : constant Version_32 := 16#cbe64e20#;
   pragma Export (C, u00187, "ada__strings__wide_mapsS");
   u00188 : constant Version_32 := 16#a6e358bc#;
   pragma Export (C, u00188, "system__stream_attributesB");
   u00189 : constant Version_32 := 16#e89b4b3f#;
   pragma Export (C, u00189, "system__stream_attributesS");
   u00190 : constant Version_32 := 16#4f40de55#;
   pragma Export (C, u00190, "ada__strings__wide_searchB");
   u00191 : constant Version_32 := 16#1748eeac#;
   pragma Export (C, u00191, "ada__strings__wide_searchS");
   u00192 : constant Version_32 := 16#6391c20e#;
   pragma Export (C, u00192, "ada__strings__wide_unboundedB");
   u00193 : constant Version_32 := 16#e5d4b589#;
   pragma Export (C, u00193, "ada__strings__wide_unboundedS");
   u00194 : constant Version_32 := 16#cd2c1152#;
   pragma Export (C, u00194, "system__compare_array_unsigned_16B");
   u00195 : constant Version_32 := 16#4bc9a3bd#;
   pragma Export (C, u00195, "system__compare_array_unsigned_16S");
   u00196 : constant Version_32 := 16#9d3d925a#;
   pragma Export (C, u00196, "system__address_operationsB");
   u00197 : constant Version_32 := 16#9fb647c1#;
   pragma Export (C, u00197, "system__address_operationsS");
   u00198 : constant Version_32 := 16#5073d598#;
   pragma Export (C, u00198, "system__machine_codeS");
   u00199 : constant Version_32 := 16#8d43fb6a#;
   pragma Export (C, u00199, "system__atomic_countersB");
   u00200 : constant Version_32 := 16#1554c2e2#;
   pragma Export (C, u00200, "system__atomic_countersS");
   u00201 : constant Version_32 := 16#15ec06fd#;
   pragma Export (C, u00201, "ada__strings__wide_unbounded__wide_hashB");
   u00202 : constant Version_32 := 16#f8259ef6#;
   pragma Export (C, u00202, "ada__strings__wide_unbounded__wide_hashS");
   u00203 : constant Version_32 := 16#8226ac78#;
   pragma Export (C, u00203, "ada__wide_text_ioB");
   u00204 : constant Version_32 := 16#f9748350#;
   pragma Export (C, u00204, "ada__wide_text_ioS");
   u00205 : constant Version_32 := 16#568aa377#;
   pragma Export (C, u00205, "neo__commandB");
   u00206 : constant Version_32 := 16#25fa70f1#;
   pragma Export (C, u00206, "neo__commandS");
   u00207 : constant Version_32 := 16#2d08d4ae#;
   pragma Export (C, u00207, "system__assertionsB");
   u00208 : constant Version_32 := 16#fc6c48fd#;
   pragma Export (C, u00208, "system__assertionsS");
   u00209 : constant Version_32 := 16#f89f7823#;
   pragma Export (C, u00209, "system__val_boolB");
   u00210 : constant Version_32 := 16#a55ac248#;
   pragma Export (C, u00210, "system__val_boolS");
   u00211 : constant Version_32 := 16#c31442ce#;
   pragma Export (C, u00211, "system__val_intB");
   u00212 : constant Version_32 := 16#176d8469#;
   pragma Export (C, u00212, "system__val_intS");
   u00213 : constant Version_32 := 16#73fec35c#;
   pragma Export (C, u00213, "system__wch_wtsB");
   u00214 : constant Version_32 := 16#534aafd9#;
   pragma Export (C, u00214, "system__wch_wtsS");
   u00215 : constant Version_32 := 16#6239f067#;
   pragma Export (C, u00215, "ada__characters__handlingB");
   u00216 : constant Version_32 := 16#3006d996#;
   pragma Export (C, u00216, "ada__characters__handlingS");
   u00217 : constant Version_32 := 16#7a69aa90#;
   pragma Export (C, u00217, "ada__strings__maps__constantsS");
   u00218 : constant Version_32 := 16#9d44d325#;
   pragma Export (C, u00218, "ada__wide_charactersS");
   u00219 : constant Version_32 := 16#ff2c5091#;
   pragma Export (C, u00219, "ada__wide_characters__handlingB");
   u00220 : constant Version_32 := 16#c2a97ea6#;
   pragma Export (C, u00220, "ada__wide_characters__handlingS");
   u00221 : constant Version_32 := 16#8ae438bb#;
   pragma Export (C, u00221, "ada__wide_characters__unicodeB");
   u00222 : constant Version_32 := 16#614def43#;
   pragma Export (C, u00222, "ada__wide_characters__unicodeS");
   u00223 : constant Version_32 := 16#1d46c85d#;
   pragma Export (C, u00223, "system__utf_32B");
   u00224 : constant Version_32 := 16#dae80866#;
   pragma Export (C, u00224, "system__utf_32S");
   u00225 : constant Version_32 := 16#ce0e2acb#;
   pragma Export (C, u00225, "system__strings__stream_opsB");
   u00226 : constant Version_32 := 16#8453d1c6#;
   pragma Export (C, u00226, "system__strings__stream_opsS");
   u00227 : constant Version_32 := 16#9d848be4#;
   pragma Export (C, u00227, "ada__streams__stream_ioB");
   u00228 : constant Version_32 := 16#31db4e88#;
   pragma Export (C, u00228, "ada__streams__stream_ioS");
   u00229 : constant Version_32 := 16#5de653db#;
   pragma Export (C, u00229, "system__communicationB");
   u00230 : constant Version_32 := 16#aae38b10#;
   pragma Export (C, u00230, "system__communicationS");
   u00231 : constant Version_32 := 16#e77ae5fe#;
   pragma Export (C, u00231, "neo__systemB");
   u00232 : constant Version_32 := 16#a58f8d92#;
   pragma Export (C, u00232, "neo__systemS");
   u00233 : constant Version_32 := 16#251824db#;
   pragma Export (C, u00233, "neo__linkS");
   u00234 : constant Version_32 := 16#3b6ea31c#;
   pragma Export (C, u00234, "neo__link__windowsS");
   u00235 : constant Version_32 := 16#988a5eb6#;
   pragma Export (C, u00235, "ada__command_lineB");
   u00236 : constant Version_32 := 16#df5044bd#;
   pragma Export (C, u00236, "ada__command_lineS");
   u00237 : constant Version_32 := 16#191e63db#;
   pragma Export (C, u00237, "neo__system__communityB");
   u00238 : constant Version_32 := 16#c37085e8#;
   pragma Export (C, u00238, "neo__system__communityS");
   u00239 : constant Version_32 := 16#404698bd#;
   pragma Export (C, u00239, "neo__system__memoryB");
   u00240 : constant Version_32 := 16#7cf37bef#;
   pragma Export (C, u00240, "neo__system__memoryS");
   u00241 : constant Version_32 := 16#6d0081c3#;
   pragma Export (C, u00241, "system__img_realB");
   u00242 : constant Version_32 := 16#9860ffb4#;
   pragma Export (C, u00242, "system__img_realS");
   u00243 : constant Version_32 := 16#80f37066#;
   pragma Export (C, u00243, "system__fat_llfS");
   u00244 : constant Version_32 := 16#0fb8c821#;
   pragma Export (C, u00244, "system__powten_tableS");
   u00245 : constant Version_32 := 16#c8b20734#;
   pragma Export (C, u00245, "neo__system__processorB");
   u00246 : constant Version_32 := 16#01ba7fa5#;
   pragma Export (C, u00246, "neo__system__processorS");
   u00247 : constant Version_32 := 16#cc7edc23#;
   pragma Export (C, u00247, "neo__system__textB");
   u00248 : constant Version_32 := 16#7095fec2#;
   pragma Export (C, u00248, "neo__system__textS");
   u00249 : constant Version_32 := 16#ad00af30#;
   pragma Export (C, u00249, "neo__system__text__consoleB");
   u00250 : constant Version_32 := 16#daeedd52#;
   pragma Export (C, u00250, "neo__system__text__consoleS");
   u00251 : constant Version_32 := 16#b3acec93#;
   pragma Export (C, u00251, "system__tasking__stagesB");
   u00252 : constant Version_32 := 16#79eb9051#;
   pragma Export (C, u00252, "system__tasking__stagesS");
   u00253 : constant Version_32 := 16#386436bc#;
   pragma Export (C, u00253, "system__restrictionsB");
   u00254 : constant Version_32 := 16#fd243b13#;
   pragma Export (C, u00254, "system__restrictionsS");
   u00255 : constant Version_32 := 16#a4cc7b44#;
   pragma Export (C, u00255, "system__tasking__initializationB");
   u00256 : constant Version_32 := 16#9468d5af#;
   pragma Export (C, u00256, "system__tasking__initializationS");
   u00257 : constant Version_32 := 16#7b8939c7#;
   pragma Export (C, u00257, "system__tasking__queuingB");
   u00258 : constant Version_32 := 16#5b69ac57#;
   pragma Export (C, u00258, "system__tasking__queuingS");
   u00259 : constant Version_32 := 16#8b7a9f50#;
   pragma Export (C, u00259, "system__tasking__protected_objects__entriesB");
   u00260 : constant Version_32 := 16#4d64e3b6#;
   pragma Export (C, u00260, "system__tasking__protected_objects__entriesS");
   u00261 : constant Version_32 := 16#195cdc00#;
   pragma Export (C, u00261, "system__tasking__rendezvousB");
   u00262 : constant Version_32 := 16#592e9c02#;
   pragma Export (C, u00262, "system__tasking__rendezvousS");
   u00263 : constant Version_32 := 16#3b094f8a#;
   pragma Export (C, u00263, "system__tasking__entry_callsB");
   u00264 : constant Version_32 := 16#837d42fa#;
   pragma Export (C, u00264, "system__tasking__entry_callsS");
   u00265 : constant Version_32 := 16#47da7ff7#;
   pragma Export (C, u00265, "system__tasking__protected_objects__operationsB");
   u00266 : constant Version_32 := 16#a9cb954d#;
   pragma Export (C, u00266, "system__tasking__protected_objects__operationsS");
   u00267 : constant Version_32 := 16#11a73c38#;
   pragma Export (C, u00267, "system__tasking__utilitiesB");
   u00268 : constant Version_32 := 16#53d3f317#;
   pragma Export (C, u00268, "system__tasking__utilitiesS");
   u00269 : constant Version_32 := 16#bd6fc52e#;
   pragma Export (C, u00269, "system__traces__taskingB");
   u00270 : constant Version_32 := 16#55cf3c43#;
   pragma Export (C, u00270, "system__traces__taskingS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.handling%s
   --  ada.characters.latin_1%s
   --  ada.command_line%s
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
   --  system.exp_uns%s
   --  system.exp_uns%b
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
   --  system.val_int%s
   --  system.val_llu%s
   --  system.val_uns%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_uns%b
   --  system.val_llu%b
   --  system.val_int%b
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
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.strings.maps%s
   --  ada.strings.fixed%s
   --  ada.strings.maps.constants%s
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.tags%s
   --  ada.streams%s
   --  interfaces.c%s
   --  system.multiprocessors%b
   --  interfaces.c.strings%s
   --  system.communication%s
   --  system.communication%b
   --  system.crtl.runtime%s
   --  system.exceptions%s
   --  system.exceptions%b
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
   --  system.task_primitives.operations%s
   --  system.tasking%b
   --  system.tasking.debug%s
   --  system.tasking.debug%b
   --  system.traces.tasking%s
   --  system.traces.tasking%b
   --  system.win32.ext%s
   --  system.task_primitives.operations%b
   --  system.os_primitives%b
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.calendar.delays%s
   --  ada.calendar.delays%b
   --  system.assertions%s
   --  system.assertions%b
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
   --  system.secondary_stack%s
   --  system.file_io%b
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
   --  ada.command_line%b
   --  ada.characters.handling%b
   --  system.secondary_stack%b
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
   --  system.tasking.initialization%b
   --  system.tasking.protected_objects%s
   --  system.tasking.protected_objects%b
   --  system.tasking.utilities%s
   --  system.traceback%s
   --  ada.exceptions%b
   --  system.traceback%b
   --  ada.wide_text_io%b
   --  ada.real_time%s
   --  ada.real_time%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  ada.text_io.generic_aux%s
   --  ada.text_io.generic_aux%b
   --  ada.text_io.modular_aux%s
   --  ada.text_io.modular_aux%b
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
   --  neo.link%s
   --  neo.link.windows%s
   --  neo.system%s
   --  neo.system%b
   --  neo.system.community%s
   --  neo.system.community%b
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

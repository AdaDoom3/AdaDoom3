-- $Source: /home/harp/1/proto/monoBANK/xbind/x-args.ads,v $ 
-- $Revision: 1.9 $ $Date: 96/10/31 13:30:10 $ $Author: mg $ 

-- --------------------------------------------------------------------------
-- THIS FILE AND ANY ASSOCIATED DOCUMENTATION IS FURNISHED "AS IS" WITHOUT 
-- WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED 
-- TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A 
-- PARTICULAR PURPOSE.  The user assumes the entire risk as to the accuracy 
-- and the use of this file. 
--  
-- Copyright (c) Intermetrics, Inc. 1994 
-- Royalty-free, unlimited, worldwide, non-exclusive use, modification, 
-- reproduction and further distribution of this file is permitted. 
-- --------------------------------------------------------------------------

-- This version is for the GNAT compiler:

with Interfaces.C;
with X.Strings;

package X.Args is

    -- Argc and Argv in a format usable by X (and Unix) programs.  
    -- If the program was started with the command line
    --    prog arg1 arg2
    -- the variables have values:
    --    Argc=3 
    --    Argv(0) points to a nul-terminated string "prog"
    --    Argv(1) points to a nul-terminated string "arg1"
    --    Argv(2) points to a nul-terminated string "arg2"
    --    Argv(3) is null.
    --    Argv(0)'access is a pointer to a string vector that can be
    --            used in many X calls.

    Argc: aliased X.Strings.Natural_Int;

    Argv: X.Strings.charp_vector;

    Env : X.Strings.charp_vector;

private

    -- These declarations would have to change with a compiler other
    -- than GNAT.  Or a real body might be needed for this package.
    pragma Import(C, argc, "gnat_argc");
    pragma Import(C, argv, "gnat_argv");
    pragma Import(C, env,  "gnat_env");

end X.Args;


-- This version for the OCS PowerAda compiler

-- with X.Strings;
-- package X.Args is
-- 
--     -- Argc and Argv in a format usable by X (and Unix) programs.
--     -- If the program was started with the command line
--     --    prog arg1 arg2
--     -- the variables have values:
--     --    Argc=3
--     --    Argv(0) points to a nul-terminated string "prog"
--     --    Argv(1) points to a nul-terminated string "arg1"
--     --    Argv(2) points to a nul-terminated string "arg2"
--     --    Argv(3) is null.
--     --    Argv(0)'access is a pointer to a string vector that can be
--     --            used in many X calls.
-- 
--    type Arg_Info_Rec is
--       record
--          Argc: aliased X.Strings.Natural_Int;
--          Argv: aliased X.Strings.charp_vector;
--          Env : aliased X.Strings.charp_vector;
--       end record;
--    pragma Convention(C, Arg_Info_Rec);
-- 
--    The_Arg_Info_Rec:  Arg_Info_Rec;
-- 
--    Argc: X.Strings.Natural_Int renames The_Arg_Info_Rec.Argc;
-- 
--    Argv: X.Strings.charp_vector renames The_Arg_Info_Rec.Argv;
-- 
--    Env : X.Strings.charp_vector renames The_Arg_Info_Rec.Env;
-- 
-- private
--    pragma Import( C, The_Arg_Info_Rec, "xADA_MAIN_PARAMS" );
-- end X.Args;

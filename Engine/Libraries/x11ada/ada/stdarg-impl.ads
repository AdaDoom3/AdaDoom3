-- $Source: /home/harp/1/proto/monoBANK/xbind/stdarg-impl.ads,v $ 
-- $Revision: 1.7 $ $Date: 95/12/05 08:53:30 $ $Author: mg $ 

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

-- Written by Mitch Gart.

with Interfaces.C.Pointers;

package Stdarg.Impl is

    -- *******************************************************
    -- Getting arguments out of lists, for use by implementers
    -- of variable-parameter functions
    -- *******************************************************

    subtype Int is Interfaces.C.Int;

    type Param_Access is private;

    function Address_of_First_Arg (Args: ArgList) return Param_Access;

    function Address_of_Vararg_List (Args: ArgList) return Param_Access;

    function ArgCount (Args: ArgList) return Int;

    function "&" (Left, Right: ArgList) return ArgList;

    procedure Do_Varargs (Proc     : in System.Address;
                          Nb_Args  : Int;
                          Arg_Addr : Param_Access);

    function F_Varargs (Func     : in System.Address;
                        Nb_Args  : Int;
                        Arg_Addr : Param_Access) return Stdarg.C_Param;

    -- debugging
    -- procedure Dump(Addr: Param_Access; Nb_Args  : Stdarg.Int);
    -- pragma Import(C, Dump, "dump");

private

    package Arith is new Interfaces.C.Pointers(
	Integer, C_Param, Stdarg.ArgVector, 0);

    type Param_Access is new Arith.Pointer;

    pragma Import(C, Do_Varargs, "do_varargs");
    pragma Import(C, F_Varargs, "do_varargs");

end Stdarg.Impl;

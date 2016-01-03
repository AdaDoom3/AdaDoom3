-- $Source: /home/harp/1/proto/monoBANK/xbind/stdarg.ads,v $ 
-- $Revision: 1.7 $ $Date: 95/12/08 13:05:58 $ $Author: mg $ 

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

with -- Ada.Finalization,
     Interfaces.C,
     Interfaces.C.Strings,
     System;

package Stdarg is

    -- *****************************************
    -- Setting up variable-length argument lists
    -- *****************************************

    MaxArguments: constant := 50;
    -- "&" and Concat functions raise Constraint_Error if more than
    -- MaxArguments integer paramters are catenated.
    -- If you change this, change it in var.c also.

    type ArgList is private;

    -- An empty arglist, to be used in constructors:
    function Empty return ArgList;

    generic
        type T is private;
	T_Is_Modular: Boolean := False;
	T_Is_Float  : Boolean := False;
    function Concat(Args: ArgList; Arg: T) return ArgList;

    subtype C_Param is Interfaces.C.Long;

private

    type ArgVector is array(Integer range <>) of aliased C_Param;

    type ArgBlock is record
        Vector      : ArgVector(1..MaxArguments) := (others => 0);
        RefCount    : Natural := 1;
        CurrentArgs : Natural := 0;
        FirstHole   : Natural := 0;
    end record;

    AS: constant := MaxArguments*C_Param'Size;
    NS: constant := Natural'Size;

    -- This rep clause is needed for HP, but causes warning messages
    -- on Intel.  Commented out here, add it back if building on HP.
    -- All this just to make vector be it aligned at mod 8, like a double!
    -- for ArgBlock use record at mod 8;
	-- Vector      at 0        range 0..AS-1;
	-- RefCount    at AS       range 0..NS-1;
	-- CurrentArgs at AS+NS    range 0..NS-1;
	-- FirstHole   at AS+NS+NS range 0..NS-1;
    -- end record;
	
    type ArgBlockP is access ArgBlock;

    -- type ArgList is new Ada.Finalization.Controlled with record
    type ArgList is record
        Contents: ArgBlockP;
    end record;

    -- **************
    -- Memory Control
    -- **************

    -- procedure Initialize (A: in out ArgList);	-- not needed
    -- procedure Adjust (A: in out ArgList);
    -- procedure Finalize (A: in out ArgList);
        
end Stdarg;

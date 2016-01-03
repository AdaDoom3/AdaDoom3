-- $Source: /home/harp/1/proto/monoBANK/xbind/stdarg-machine.ads,v $ 
-- $Revision: 1.6 $ $Date: 95/12/05 08:53:31 $ $Author: mg $ 

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

with Interfaces.C;

package Stdarg.Machine is
    -- **********************************************************
    -- This package describes the differences in machine 
    -- architectures that need to be known by Stdarg.
    --
    -- I386 is Intel 386/486/Pentium PC's
    -- Sparc is Sun-4 Sparcstation and Sparcserver 
    -- HP is Hewlett-Packard HP-9000 series 700 and 800
    -- Mips is machines based on the MIPS chip, such as SGI
    -- PowerPC is Apple-IBM-Motorola Power PC, and IBM RS/6000
    -- Alpha is the Digital Equipment Corporation chip.
    -- 
    -- To build these packages for a different architecture,
    -- change the constant This_Arch to one of the allowed values
    -- and recompile.
    -- **********************************************************
    type Arch is (I386, Sparc, HP, Mips, Alpha, PowerPC);

    This_Arch: constant Arch := Sparc;

    type Stack_Growth_Direction is (
	Up,                          -- toward address 0
	Down);                       -- toward high numbered addresses

    type Arch_Description_Rec is record
	Int_Param_Alignment,
	Float_Param_Alignment: Positive; 
	Stack_Growth: Stack_Growth_Direction;
    end record;

    SU: constant := System.Storage_Unit;

    Arch_Description: constant array (Arch) of Arch_Description_Rec := (
	I386 => (
	    Int_Param_Alignment   => C_Param'Size/SU,
	    Float_Param_Alignment => C_Param'Size/SU,
	    Stack_Growth          => Up)
	, Sparc => (
	    Int_Param_Alignment   => C_Param'Size/SU,
	    Float_Param_Alignment => C_Param'Size/SU,
	    Stack_Growth          => Up)
	, HP => (
	    Int_Param_Alignment   => C_Param'Size/SU,
	    Float_Param_Alignment => Interfaces.C.Double'Size/SU,
	    Stack_Growth          => Down)
	, Mips => (
	    Int_Param_Alignment   => C_Param'Size/SU,
	    Float_Param_Alignment => Interfaces.C.Double'Size/SU,
	    Stack_Growth          => Up)
	, Alpha => (
	    Int_Param_Alignment   => C_Param'Size/SU,
	    Float_Param_Alignment => Interfaces.C.Double'Size/SU,
	    Stack_Growth          => Up)
	, PowerPC => (
	    Int_Param_Alignment   => C_Param'Size/SU,
	    Float_Param_Alignment => C_Param'Size/SU,
	    Stack_Growth          => Up)
    );

    Desc                 : Arch_Description_Rec renames 
			   Arch_Description(This_Arch);
    Int_Param_Alignment  : Positive renames Desc.Int_Param_Alignment;
    Float_Param_Alignment: Positive renames Desc.Float_Param_Alignment;
    Stack_Growth         : Stack_Growth_Direction renames Desc.Stack_Growth;
    Param_Size           : constant Positive := C_Param'Size/SU;

end Stdarg.Machine;

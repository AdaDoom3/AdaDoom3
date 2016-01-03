-- $Source: /home/harp/1/proto/monoBANK/xbind/stdarg-impl.adb,v $ 
-- $Revision: 1.14 $ $Date: 96/03/04 07:52:54 $ $Author: mg $ 

-- Written by Mitch Gart.
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


with Stdarg.Machine,
     Text_IO;

package body Stdarg.Impl is

    use Stdarg.Machine;

    -- ******************************
    -- Getting arguments out of lists
    -- ******************************

    type Which_Arg is (Ellipsis, VA_List);

    function Address_of_Arg (Args: ArgList; Which: Which_Arg) 
	return Param_Access is
    begin
        if Args.Contents.CurrentArgs = 0 then
            return null;              		-- might not be an error
        end if;

	if This_Arch = Alpha then
	    return Args.Contents.Vector(7)'access;
        elsif Stack_Growth = Up then
            return Args.Contents.Vector(1)'access;
        elsif Which = Ellipsis then
            return Args.Contents.Vector(
		   MaxArguments-Args.Contents.CurrentArgs+1)'access;
	else
	    declare
		use Arith;
		P: Pointer := Args.Contents.Vector(MaxArguments)'access;
	    begin
		return Param_Access(P+1);
	    end;
        end if;
    end Address_of_Arg;

    function Address_of_First_Arg (Args: ArgList) return Param_Access is
    begin
        return Address_of_Arg(Args, Ellipsis);
    end Address_of_First_Arg;

    function Address_of_Vararg_List (Args: ArgList) return Param_Access is
    begin
        return Address_of_Arg(Args, VA_List);
    end Address_of_Vararg_List;

    function ArgCount (Args: ArgList) return Int is
    begin
	return Int(Args.Contents.CurrentArgs);
    end ArgCount;

    -- **************
    -- Concatenations
    -- **************

    function "&" (Left, Right: ArgList) return ArgList is
        Hole_Change : Integer := 0;
        Incr        : Integer;
        Left_Index,
        Right_Index : Positive;
        
        procedure Do_Incr(Index: in out Natural) is
        begin
            Index := Index + Incr;
        end Do_Incr;
        pragma Inline(Do_Incr);
    begin
        if Left.Contents = null or else Left.Contents.CurrentArgs = 0 then
            return Right;
        elsif Right.Contents = null or else Right.Contents.CurrentArgs = 0 then
            return Left;
        end if;

	if This_Arch = Alpha then
	    for Right_Index in 7..Right.Contents.CurrentArgs loop
		Left_Index := Left.Contents.CurrentArgs + Right_Index - 6;
		if Left_Index <= 12 then
		    Left.Contents.Vector(Left_Index-6) := 
			Right.Contents.Vector(Right_Index-6);
		end if;
		Left.Contents.Vector(Left_Index) := 
		    Right.Contents.Vector(Right_Index);
	    end loop;
	    Left.Contents.CurrentArgs := Left.Contents.CurrentArgs + 
		Right.Contents.CurrentArgs - 6;

	    -- Dump(Left.Contents.Vector(1)'access, 
		    -- Int(Left.Contents.CurrentArgs));

	    return Left;
        elsif Stack_Growth = Up then
            Left_Index  := Left.Contents.CurrentArgs + 1;
            Right_Index := 1;
            Incr        := 1;
            -- Dump(Left.Contents.Vector(1)'access, 
                 -- Int(Left.Contents.CurrentArgs));
            -- Dump(Right.Contents.Vector(1)'access, 
                 -- Int(Right.Contents.CurrentArgs));
        else
            Left_Index  := MaxArguments - Left.Contents.CurrentArgs;
            Right_Index := MaxArguments;
            Incr        := -1;
            -- Dump(Left.Contents.Vector(Left_Index+1)'access, 
                 -- Int(Left.Contents.CurrentArgs));
            -- Dump(Right.Contents.Vector(
                 -- MaxArguments-Right.Contents.CurrentArgs+1)'access, 
                 -- Int(Right.Contents.CurrentArgs));
            -- Text_IO.Put_Line("Right first hole" & 
                              -- Integer'Image(Right.Contents.FirstHole));
        end if;

        for I in 1..Right.Contents.CurrentArgs loop
            if Right.Contents.FirstHole = I and then
	       Left.Contents.CurrentArgs mod 2 /= 0 then
               if Float_Param_Alignment > Int_Param_Alignment then 
		    if I mod 2 = 0 then
			-- remove hole
			Do_Incr(Right_Index);
			Hole_Change := -1;
		    else
			-- add hole
			Do_Incr(Left_Index);
			Hole_Change := 1;
		    end if;
		end if;
            end if;
            Left.Contents.Vector(Left_Index) :=
                Right.Contents.Vector(Right_Index);
            Do_Incr(Left_Index);
            Do_Incr(Right_Index);
        end loop;

        Left.Contents.CurrentArgs := Left.Contents.CurrentArgs + 
            Right.Contents.CurrentArgs + Hole_Change;

        -- Dump(Left.Contents.Vector(1)'access, 
		-- Int(Left.Contents.CurrentArgs));

        return Left;
    end "&";

end Stdarg.Impl;

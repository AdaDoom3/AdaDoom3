-- $Source: /home/harp/1/proto/monoBANK/xbind/stdarg.adb,v $ 
-- $Revision: 1.8 $ $Date: 95/12/05 09:07:46 $ $Author: mg $ 

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

with Ada.Unchecked_Deallocation,
     Stdarg.Machine,
     System,
     System.Storage_Elements;

-- with Ada.Unchecked_Conversion, Text_IO;           -- debugging

package body Stdarg is

    use Stdarg.Machine;

    -- ********
    -- Internal
    -- ********

    function RoundUp(Siz: Natural) return Natural is
        Slop: constant Natural := Siz mod Int_Param_Alignment;
        Res : Natural;
    begin
        -- How many int's in this argument.
        -- Round up size to 4, then divide by 4.
        -- Processor-dependent, this is right for Intel,
        -- more complicated for others, see va_arg in stdarg.h
        if Slop = 0 then 
            Res := Siz;
        else
            Res := Siz + (Int_Param_Alignment-Slop);
        end if;
        return Res/Param_Size;
    end Roundup;

    -- **************
    -- Memory control
    -- **************

    -- -----------------------------------------------------------------
    -- The intention of this reference counting code is to deallocate
    -- argument lists after they are used, so there are no memory leaks.
    -- At the time of testing this software, GNAT 2.02 is not working
    -- well with controlled types.  So the code is commented out for now.
    -- This means there will potentially be memory leaks, if these 
    -- "&" expressions with arguments are used a lot, but otherwise the
    -- code should work correctly.
    -- In the future when controlled types are working right this code
    -- should be put back and tested.
    -- -----------------------------------------------------------------

    -- procedure Initialize (A: in out ArgList) is
    -- begin
        -- A.Contents := new ArgBlock;
        -- null;
    -- end Initialize;

    -- LFN: constant string := ascii.lf & ascii.nul;

    -- procedure printf(Format: String; P: ArgBlockP);
    -- pragma import(c, printf, "printf");

    -- procedure Adjust (A: in out ArgList) is
    -- begin
	-- printf("adjusting %x" & LFN, A.Contents);
        -- A.Contents.RefCount := A.Contents.RefCount + 1;
    -- end Adjust;

    -- null_counter: integer := 0;

    -- procedure Finalize (A: in out ArgList) is
        -- procedure Free is new Ada.Unchecked_Deallocation (
	    -- ArgBlock, ArgBlockP);
    -- begin
	-- if A.Contents = null then
	    -- printf("finalizing null" & LFN, null);
	    -- null_counter := null_counter + 1;
	-- else
	    -- declare
		-- msg1: constant String := "finalizing %x, ref count" & 
		    -- integer'image(A.Contents.RefCount) & LFN;
		-- msg2: constant string := "deallocating %x" & LFN;
		-- msg3: constant string := "after deallocation %x" & LFN;
	    -- begin
		-- A.Contents.RefCount := A.Contents.RefCount - 1;
		-- printf(msg1, A.Contents);
		-- if A.Contents.RefCount <= 0 then
		    -- printf(msg2, A.Contents);
		    -- Free(A.Contents);
		    -- printf(msg3, null);
		-- end if;
	    -- end;
	-- end if;
    -- end Finalize;

    -- ************************
    -- Visible - argument lists
    -- ************************

    procedure put(s: string; a: system.address) is
        -- function to_int is new ada.unchecked_conversion(
            -- system.address, integer);
    begin
        null;
        -- text_io.put_line(s & integer'image(to_int(a)));
    end put;

    function Concat(Args: ArgList; Arg: T) return ArgList is
        Nb_Int: Natural := Roundup(T'Size/System.Storage_Unit);
        Uncopied_bytes  : Natural := 
            (Nb_Int * Param_Size) - (T'Size/System.Storage_Unit);
        use System.Storage_Elements;
        Arg_Addr : System.Address := Arg'Address - 
            (Arg'Address mod Storage_Offset(Int_Param_Alignment));
        Buf_Addr : System.Address;
	Arg_Size : Integer := Arg'Size;
	Index    : Integer;

        procedure Memcpy(To, From: System.Address; Nbytes: Natural);
        pragma Import(C, Memcpy, "memcpy");

	Double_Arg: Interfaces.C.Double;
	Long_Arg  : C_Param;

        function To_Double (Arg_Addr: System.Address; 
			    Arg_Size: Interfaces.C.Int) 
		    return Interfaces.C.Double;
	pragma Import(C, To_Double, "to_double");

        function To_Long (Arg_Addr  : System.Address; 
			  Arg_Size  : Interfaces.C.Int;
			  Is_Modular: Interfaces.C.Int) 
		    return C_Param;
	pragma Import(C, To_Long, "to_long");

    begin
	if T_Is_Float then 
	    Double_Arg     := To_Double(Arg'Address, Arg'Size); 
	    Arg_Addr       := Double_Arg'Address;
	    Nb_Int         := Roundup(Double_Arg'Size/System.Storage_Unit);
	    Uncopied_bytes := 0;
	    Arg_Size       := Double_Arg'Size;
	elsif T'Size < C_Param'Size then
	    Long_Arg       := To_Long(Arg'Address, Arg'Size, 
			      Boolean'Pos(T_Is_Modular));
	    Arg_Addr       := Long_Arg'Address;
	    Nb_Int         := Roundup(Long_Arg'Size/System.Storage_Unit);
	    Uncopied_bytes := 0;
	    Arg_Size       := Long_Arg'Size;
	end if;

        if (Nb_Int + Args.Contents.CurrentArgs > MaxArguments) then
            raise Constraint_Error;
        end if;

        -- Impossible to make this common, just try to do the right
        -- thing for each machine, one at a time!
        case This_Arch is
            when I386 =>
                Buf_Addr := Args.Contents.Vector(
                    Args.Contents.CurrentArgs+1)'Address;
                Memcpy(Buf_Addr, Arg_Addr, 
		       (Nb_Int*Param_Size) - Uncopied_Bytes);

	    when Alpha =>
		if Args.Contents.CurrentArgs = 0 then
		    Args.Contents.CurrentArgs := 6;
		end if;
		if T_Is_Float then
		    Index := Args.Contents.CurrentArgs - 6 + 1;
		else
		    Index := Args.Contents.CurrentArgs + 1;
		end if;
                Buf_Addr := Args.Contents.Vector(Index)'Address;
                Memcpy(Buf_Addr, Arg_Addr, 
		       (Nb_Int*Param_Size) - Uncopied_Bytes);

            when Sparc | PowerPC =>
                Buf_Addr := Args.Contents.Vector(
                    Args.Contents.CurrentArgs+1)'Address;
                Memcpy(Buf_Addr + Storage_Offset(Uncopied_bytes), 
                       Arg_Addr + Storage_Offset(Uncopied_bytes), 
                       Nb_Int*Param_Size - Uncopied_bytes);

	    when Mips =>
                Buf_Addr := Args.Contents.Vector(
                    Args.Contents.CurrentArgs+1)'Address;
                if (Arg_Size in 33..64) then
                    if Args.Contents.FirstHole = 0 then
                        Args.Contents.FirstHole :=
                            Args.Contents.CurrentArgs + 1;
                    end if;
                    if (Args.Contents.CurrentArgs mod 2 /= 0) then
                        Args.Contents.CurrentArgs :=
                            Args.Contents.CurrentArgs + 1;
                        Buf_Addr := Buf_Addr + Storage_Offset(Param_Size);
                    end if;
                end if;
                Memcpy(Buf_Addr + Storage_Offset(Uncopied_bytes),
                       Arg_Addr + Storage_Offset(Uncopied_bytes),
                       Nb_Int*Param_Size - Uncopied_bytes);

            when HP =>
                Buf_Addr := Args.Contents.Vector(MaxArguments - 
                            Args.Contents.CurrentArgs - Nb_Int + 1)'Address;
                if (Arg_Size in 33..64) then
                    if Args.Contents.FirstHole = 0 then
                        Args.Contents.FirstHole := 
                            Args.Contents.CurrentArgs + 1;
                    end if;
                    if (Args.Contents.CurrentArgs mod 2 /= 0) then
                        Args.Contents.CurrentArgs := 
                            Args.Contents.CurrentArgs + 1;
                        Buf_Addr := Buf_Addr - Storage_Offset(Param_Size);
                    end if;
                end if;
                -- HP <varargs.h> says args are "right justified" and
                -- I guess this is what they mean:
                Memcpy(Buf_Addr + Storage_Offset(Uncopied_bytes),
                       Arg_Addr, Nb_Int*Param_Size-Uncopied_bytes);

	    -- when Power_PC => null;		-- not yet implemented
        end case;
        Args.Contents.CurrentArgs := Args.Contents.CurrentArgs + Nb_Int;
        put("arg_addr", arg_addr);
        put("buf_addr", buf_addr);
        put("param addr", Arg'address);
        return Args;
    end Concat;

    function Empty return ArgList is
        Res: ArgList;
    begin
        Res.Contents := new ArgBlock;
        -- Res.Contents.RefCount := 2;                         -- GNAT bug
        return Res;
    end Empty;

end Stdarg;

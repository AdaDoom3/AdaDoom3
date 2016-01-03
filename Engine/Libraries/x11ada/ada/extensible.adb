-- $Source: /home/harp/1/proto/monoBANK/xbind/extensible.adb,v $ 
-- $Revision: 1.3 $ $Date: 95/12/05 09:07:14 $ $Author: mg $ 


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

with Ada.Unchecked_Conversion;
with System;
with System.Storage_Elements;

package body Extensible is

    type Extended_Rec is new Integer;

    function Allocate (Actual_Elems: Big_Range) return Extended_Ptr is
        function Malloc(Nbytes: Interfaces.C.Unsigned) return Extended_Ptr;
        pragma Import(C, Malloc, "malloc");
        use Interfaces.C;
    begin
        return Malloc(
            Interfaces.C.Unsigned(Position_Of_Extensible_Array) + 
            (((Actual_Elems-1) * Extensible_Elem'Size) / System.Storage_Unit));
    end Allocate;

    procedure Free (Ptr: in out Extended_Ptr) is
        procedure Do_Free(Ptr: Extended_Ptr);
        pragma Import(C, Do_Free, "free");
    begin
        Do_Free(Ptr);
        Ptr := null;
    end Free;

    function Fixed_Part (Ptr: Extended_Ptr) return Fixed_Ptr is
        function To_Fixed is new Ada.Unchecked_Conversion (
            Extended_Ptr, Fixed_Ptr);
    begin
        return To_Fixed(Ptr);
    end Fixed_Part;

    function Array_Part (Ptr: Extended_Ptr) return Big_Array_Ptr is
        function To_Array is new Ada.Unchecked_Conversion (
            System.Address, Big_Array_Ptr);
        use System.Storage_Elements;
    begin
        return To_Array(Ptr.all'Address + 
                        Storage_Offset(Position_Of_Extensible_Array));
            
    end Array_Part;

end Extensible;

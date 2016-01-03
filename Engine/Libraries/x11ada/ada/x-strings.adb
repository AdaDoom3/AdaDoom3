-- $Source: /home/harp/1/proto/monoBANK/xbind/x-strings.adb,v $ 
-- $Revision: 1.10 $ $Date: 95/12/05 09:07:48 $ $Author: mg $ 

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
with Ada.Unchecked_Deallocation;
with System;

package body X.Strings is

    function To_X is new Ada.Unchecked_Conversion(
        Interfaces.C.strings.chars_ptr, charp);

    function To_X is new Ada.Unchecked_Conversion(
        const_charp, charp);

    function To_X is new Ada.Unchecked_Conversion(
        System.Address, charp);

    function To_X is new Ada.Unchecked_Conversion(
        System.Address, const_charp);

    function To_C is new Ada.Unchecked_Conversion(
        charp, Interfaces.C.strings.chars_ptr);

    function to_chars_ptr(p: charp) return Interfaces.C.strings.chars_ptr is
    begin
        return To_C(P);
    end to_chars_ptr;

    function to_charp(p: Interfaces.C.strings.chars_ptr) return charp is
    begin
        return To_X(P);
    end to_charp;

    function Value (Item : in const_charp) return Interfaces.C.char_array is
    begin
        return Interfaces.C.Strings.Value(To_C(To_X(Item)));
    end Value;

    function Value
     (Item   : in const_charp;
      Length : in Interfaces.C.size_t)
      return   Interfaces.C.char_array is
    begin
        return Interfaces.C.Strings.Value(To_C(To_X(Item)), Length);
    end Value;

    function Value (Item : in const_charp) return String is
    begin
        return Interfaces.C.Strings.Value(To_C(To_X(Item)));
    end Value;

    function Value
     (Item   : in const_charp;
      Length : in Interfaces.C.size_t)
      return   String is
    begin
        return Interfaces.C.Strings.Value(To_C(To_X(Item)), Length);
    end Value;

    function Strlen (Item : in const_charp) return Interfaces.C.size_t is
    begin
        if Item = null then 
            return 0; 
        else
            return Interfaces.C.Strings.Strlen(To_C(To_X(Item)));
        end if;
    end Strlen;

    function Strlen (Item : in Interfaces.C.char_array) 
        return Interfaces.C.size_t is
    begin
        return Strlen(Item(Item'First)'unchecked_access);
    end Strlen;

    function Addr (S: String) return charp is
    begin
        return To_X(S(S'First)'Address);
    end Addr;

    function Addr (S: String) return const_charp is
    begin
        return To_X(S(S'First)'Address);
    end Addr;

    function Addr (S: Interfaces.C.Char_Array) return charp is
    begin
        return To_X(S(S'First)'Address);
    end Addr;

    function Addr (S: Interfaces.C.Char_Array) return const_charp is
    begin
        return To_X(S(S'First)'Address);
    end Addr;

    function Cat (Left, Right: String) return String is
        Nul: constant Character := Character'First;
    begin
        if Left(Left'Last) = Nul then
            if Right(Right'Last) = Nul then
                return Left(Left'First..Left'last-1) & Right;
            else
                return Left(Left'First..Left'last-1) & Right & Nul;
            end if;
        else
            if Right(Right'Last) = Nul then
                return Left & Right;
            else
                return Left & Right & Nul;
            end if;
        end if;
    end Cat;

    function Cat (Left, Right: Interfaces.C.Char_Array) 
        return Interfaces.C.Char_Array is
        Nul: constant Interfaces.C.char := Interfaces.C.char'First;
        use Interfaces.C;
    begin
        if Left(Left'Last) = Nul then
            if Right(Right'Last) = Nul then
                return Left(Left'First..Left'last-1) & Right;
            else
                return Left(Left'First..Left'last-1) & Right & Nul;
            end if;
        else
            if Right(Right'Last) = Nul then
                return Left & Right;
            else
                return Left & Right & Nul;
            end if;
        end if;
    end Cat;

    function New_String (S: String) return charp is
    begin
	return To_Charp(Interfaces.C.Strings.New_String(S));
    end New_String;

    function New_String (S: String) return const_charp is
    begin
	return const_charp(To_Charp(Interfaces.C.Strings.New_String(S)));
    end New_String;

end X.Strings;

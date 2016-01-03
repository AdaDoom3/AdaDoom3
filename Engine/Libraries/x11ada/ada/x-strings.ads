-- $Source: /home/harp/1/proto/monoBANK/xbind/x-strings.ads,v $ 
-- $Revision: 1.11 $ $Date: 95/12/05 08:53:35 $ $Author: mg $ 

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

with X;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;
with System;

package X.Strings is

    subtype char is X.char;

    -- ********************************************
    -- Pointer to character, fully convertible with 
    -- Interfaces.C.strings.chars_ptr:
    -- ********************************************

    type charp is access all char;                          -- C char *

    function to_chars_ptr(p: charp) return Interfaces.C.strings.chars_ptr;
    pragma Inline(to_chars_ptr);

    function to_charp(p: Interfaces.C.strings.chars_ptr) return charp;
    pragma Inline(to_charp);

    -- ***************************************************************
    -- Pointer to constant character.
    -- Value and strlen functions (like those in Interfaces.C.Strings) 
    -- are available, but not allocation/free and Update functions.
    -- ***************************************************************

    type const_charp is access constant char;

    function Value (Item : in const_charp) return Interfaces.C.char_array;
    pragma Inline(Value);

    function Value
     (Item   : in const_charp;
      Length : in Interfaces.C.size_t)
      return   Interfaces.C.char_array;
    pragma Inline(Value);

    function Value (Item : in const_charp) return String;
    pragma Inline(Value);

    function Value
     (Item   : in const_charp;
      Length : in Interfaces.C.size_t)
      return   String;
    pragma Inline(Value);

    -- Strlen counts the number of leading non-Nul characters.

    function Strlen (Item : in const_charp) return Interfaces.C.size_t;
    pragma Inline(Strlen);

    function Strlen (Item : in Interfaces.C.char_array) 
        return Interfaces.C.size_t;
    pragma Inline(Strlen);

    -- *******************************************************
    -- Allocate new nul-terminated strings and return pointers
    -- *******************************************************

    function New_String (S: String) return charp;

    function New_String (S: String) return const_charp;

    -- ******************************************************
    -- Vectors of char pointers, equivalent to C char **.
    -- Pointer arithmetic supported by Interfaces.C.Pointers.
    -- ******************************************************

    subtype Natural_Int is Interfaces.C.Int range 0..Interfaces.C.Int'Last;

    type charp_array is array (Natural_Int range <>) of aliased charp;

    -- Go from an array of pointers to a pointer to the first pointer.
    -- This may seem like the same thing but in the GNAT model it involves
    -- going from a 64-bit "fat" pointer to a 32-bit "thin" pointer.

    package charp_vectors is new Interfaces.C.Pointers (
	Index              => Natural_Int,
	Element            => charp,
	Element_Array      => charp_array,
	Default_Terminator => null);

    subtype charp_vector is charp_vectors.pointer;

    -- To get a charp_vector from a charp_array A, use A(0)'access.

    -- ********************************************************************
    -- These functions return the address of the first element of a string.
    -- They could be useful with a Standard.String that is NUL-terminated.
    -- See also the conversion functions in Interfaces.C.
    -- 
    -- Note:  be careful about using these.  Addr(S), where S is a 
    -- constant or variable, is fine.  But Addr(S1&S2) is dangerous
    -- because what is returned is the address of a function result,
    -- but the function result is probably deallocated before the
    -- address can be used in the program.
    -- ********************************************************************

    function Addr (S: String) return charp;
    pragma Inline(Addr);

    function Addr (S: String) return const_charp;
    pragma Inline(Addr);

    function Addr (S: Interfaces.C.Char_Array) return charp;
    pragma Inline(Addr);

    function Addr (S: Interfaces.C.Char_Array) return const_charp;
    pragma Inline(Addr);

    -- **************************************************************
    -- Special string-catenation functions that will trim a trailing
    -- nul from the left string, if present, and will add a trailing nul
    -- to the result, if not present at the end of the right string.
    -- The easiest way to use these is with a local renaming, e.g.
    --   function "&" (Left, Right: String) return String 
    --      renames X.Strings.Cat;
    -- **************************************************************

    function Cat (Left, Right: String) return String;

    function Cat (Left, Right: Interfaces.C.Char_Array) 
        return Interfaces.C.Char_Array;

    -- **************************************************
    -- A null string, to be passed in to certain routines
    -- **************************************************

    Null_Char_Array : constant Interfaces.C.Char_Array(1..0) := "";
    use System;				-- needed with GNAT 2.06
    for Null_Char_Array'address use Null_Address;

end X.Strings;

-- $Source: /home/harp/1/proto/monoBANK/xbind/x-xmd.ads,v $ 
-- $Revision: 1.9 $ $Date: 95/12/05 08:53:40 $ $Author: mg $ 

-- --------------------------------------------------------------------------
-- THIS FILE AND ANY ASSOCIATED DOCUMENTATION IS PROVIDED "AS IS" WITHOUT 
-- WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED 
-- TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A 
-- PARTICULAR PURPOSE.  The user assumes the entire risk as to the accuracy 
-- and the use of this file.  
--  
-- Ada version Copyright (c) Intermetrics, Inc. 1994 
-- Royalty-free, unlimited, worldwide, non-exclusive use, modification, 
-- reproduction and further distribution of this Ada file is permitted. 
--
-- C version contains additional copyrights, 
-- see the accompanying file Xmd.h.
-- --------------------------------------------------------------------------

with X;

package X.Xmd is

    XMD_H      : constant := 1;                             -- Xmd.h:28

    type INT32 is new X.long;                               -- Xmd.h:86
    type INT16 is new X.signed_short;                       -- Xmd.h:87
    type INT8 is new X.signed_char;                         -- Xmd.h:89
    type CARD32 is new X.long;                              -- Xmd.h:94
    type CARD16 is new X.unsigned_short;                    -- Xmd.h:95
    type CARD8 is new X.unsigned_char;                      -- Xmd.h:96
    type BITS32 is new X.long;                              -- Xmd.h:98
    type BITS16 is new X.unsigned_short;                    -- Xmd.h:99
    type BYTE is new X.unsigned_char;                       -- Xmd.h:100
    type BOOL is new X.unsigned_char;                       -- Xmd.h:102

    -- ************
    -- Xmd.h macros
    -- ************

    function cvtINT8toInt   (Val : INT8)  return Interfaces.C.Int;
    function cvtINT16toInt  (Val : INT16) return Interfaces.C.Int;
    function cvtINT32toInt  (Val : INT32) return Interfaces.C.Int;
    function cvtINT8toShort (Val: INT8)   return Interfaces.C.Short;
    function cvtINT16toShort(Val: INT16)  return Interfaces.C.Short;
    function cvtINT32toShort(Val: INT32)  return Interfaces.C.Short;
    function cvtINT8toLong  (Val: INT8)   return Interfaces.C.Long;
    function cvtINT16toLong (Val: INT16)  return Interfaces.C.Long;
    function cvtINT32toLong (Val: INT32)  return Interfaces.C.Long;

    -- the macro NEXTPTR is not directly translated from C to Ada.
    -- Instead use the built-in package Interfaces.C.Pointers.

private

    pragma Inline(cvtINT8toInt);
    pragma Inline(cvtINT16toInt);
    pragma Inline(cvtINT32toInt);
    pragma Inline(cvtINT8toShort);
    pragma Inline(cvtINT16toShort);
    pragma Inline(cvtINT32toShort);
    pragma Inline(cvtINT8toLong);
    pragma Inline(cvtINT16toLong);
    pragma Inline(cvtINT32toLong);

end X.Xmd;

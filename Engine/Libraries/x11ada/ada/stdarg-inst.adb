-- $Source: /home/harp/1/proto/monoBANK/xbind/stdarg-inst.adb,v $ 
-- $Revision: 1.5 $ $Date: 95/12/05 09:07:46 $ $Author: mg $ 

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

-- Written by Mitch Gart

package body Stdarg.Inst is

    function "&" (Args: ArgList; Arg: Interfaces.C.Char_Array) return ArgList is
        type CP is access constant Interfaces.C.Char;
        function "&" is new Stdarg.Concat(CP);
    begin
        return Args & Arg(0)'unchecked_access;
    end "&";

end Stdarg.Inst;

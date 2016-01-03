-- $Source: /home/harp/1/proto/monoBANK/xbind/x-xmd.adb,v $ 
-- $Revision: 1.5 $ $Date: 95/12/05 09:07:50 $ $Author: mg $ 

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

package body X.Xmd is

    function cvtINT8toInt(Val : INT8) return Interfaces.C.Int is
    begin
        return Interfaces.C.Int(Val);
    end;

    function cvtINT16toInt(Val : INT16) return Interfaces.C.Int is
    begin
        return Interfaces.C.Int(Val);
    end;

    function cvtINT32toInt(Val : INT32) return Interfaces.C.Int is
    begin
        return Interfaces.C.Int(Val);
    end;

    function cvtINT8toShort(Val: INT8) return Interfaces.C.Short is
    begin
        return Interfaces.C.Short(Val);
    end;

    function cvtINT16toShort(Val: INT16) return Interfaces.C.Short is
    begin
        return Interfaces.C.Short(Val);
    end;

    function cvtINT32toShort(Val: INT32) return Interfaces.C.Short is
    begin
        return Interfaces.C.Short(Val);
    end;

    function cvtINT8toLong(Val: INT8) return Interfaces.C.Long is
    begin
        return Interfaces.C.Long(Val);
    end;

    function cvtINT16toLong(Val: INT16) return Interfaces.C.Long is
    begin
        return Interfaces.C.Long(Val);
    end;

    function cvtINT32toLong(Val: INT32) return Interfaces.C.Long is
    begin
        return Interfaces.C.Long(Val);
    end;

end X.Xmd;

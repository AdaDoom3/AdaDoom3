-- $Source: /home/harp/1/proto/monoBANK/xbind/x-tasking.adb,v $ 
-- $Revision: 1.1 $ $Date: 96/02/29 14:56:47 $ $Author: mg $ 

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

package body X.Tasking is

    protected body Resource is 
        entry Seize when not X_Is_In_Use is
        begin
            X_Is_In_Use := True;
        end Seize;

        procedure Release is
        begin
            X_Is_In_Use := False;
        end Release;
    end Resource;

end X.Tasking;

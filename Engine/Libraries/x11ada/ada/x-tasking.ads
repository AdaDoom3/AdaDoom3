-- $Source: /home/harp/1/proto/monoBANK/xbind/x-tasking.ads,v $ 
-- $Revision: 1.1 $ $Date: 96/02/29 14:56:47 $ $Author: mg $ 

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
-- --------------------------------------------------------------------------

package X.Tasking is

    -- ************************************************************
    -- The underlying X system, written in C, is not tasking-safe.
    -- The Ada binding does nothing to change this.
    -- An application that makes calls to X from multiple Ada tasks
    -- probably should protect those calls.  This resource could
    -- be used.  The usage would be:
    --
    --    X.Resource.Seize;
    --    X.Some_Package.Some_Function;
    --    X.Resource.Release;
    --
    -- Alternatively one could build a protected type that does
    -- the same thing, and then declare an object of the protected
    -- type in each scope that makes a call to X.
    -- ************************************************************

    protected Resource is 
        entry Seize;
        procedure Release;
    private
        X_Is_In_Use: Boolean := False;
    end Resource;

end X.Tasking;

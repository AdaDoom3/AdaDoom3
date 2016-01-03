-- $Source: /home/harp/1/proto/monoBANK/xbind/xt-fd.ads,v $ 
-- $Revision: 1.2 $ $Date: 95/12/05 08:53:47 $ $Author: mg $ 

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

package Xt.fd is

    type fd_mask is new X.long;                             -- <sys/types.h>:84

    type fd_array is array                                  -- <sys/types.h>:95
        (integer range 0..7) of aliased fd_mask;

    type Fd_set is                                          -- fd.h:61
        record
            fds_bits: fd_array;                             -- fd.h:62
        end record;

private

    pragma Convention(C,  Fd_set);
end Xt.fd;

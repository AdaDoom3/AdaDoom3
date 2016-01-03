-- $Source: /home/harp/1/proto/monoBANK/xbind/xt-core.ads,v $ 
-- $Revision: 1.3 $ $Date: 95/12/05 08:53:46 $ $Author: mg $ 

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
-- see the accompanying file Core.h.
-- --------------------------------------------------------------------------

package Xt.Core is

    type CoreWidgetClass is private;
    type CoreWidget is private;

private

    type CoreWidgetClassRec;
    type CoreWidgetClass is access all CoreWidgetClassRec;
    type CoreWidgetRec;
    type CoreWidget is access all CoreWidgetRec;

end Xt.Core;

-- $Source: /home/harp/1/proto/monoBANK/xbind/xt-core-objs.ads,v $ 
-- $Revision: 1.2 $ $Date: 95/12/05 08:53:46 $ $Author: mg $ 

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

package Xt.Core.Objs is

    coreWidgetClass: Xt.Core.CoreWidgetClass;          -- Core.h:38
    widgetClass    : Xt.Core.CoreWidgetClass;          -- Core.h:41

private

    pragma Import(C, coreWidgetClass, "coreWidgetClass");
                                                            -- Core.h:38
    pragma Import(C, widgetClass, "widgetClass");           -- Core.h:41

end Xt.Core.Objs;

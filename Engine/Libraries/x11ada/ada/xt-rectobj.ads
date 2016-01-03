-- $Source: /home/harp/1/proto/monoBANK/xbind/xt-rectobj.ads,v $ 
-- $Revision: 1.2 $ $Date: 95/12/05 08:53:52 $ $Author: mg $ 

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
-- see the accompanying file RectObj.h.
-- --------------------------------------------------------------------------

with Xt.Intrinsic;

package Xt.RectObj is

    type RectObjRec is private;                             -- RectObj.h:33
    type RectObjClassRec is private;                        -- RectObj.h:34

    type RectObj is access all RectObjRec;                  -- RectObj.h:33
    type RectObjClass is access all RectObjClassRec;        -- RectObj.h:34

    rectObjClass_obj: Xt.Intrinsic.WidgetClass;             -- RectObj.h:37

private

    type RectObjClassRec is null record;                    -- RectObj.h:34
    type RectObjRec is null record;                         -- RectObj.h:33
    pragma Import(C, rectObjClass_obj, "rectObjClass");     -- RectObj.h:37

end Xt.RectObj;

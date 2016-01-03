-- $Source: /home/harp/1/proto/monoBANK/xbind/xt-object.ads,v $ 
-- $Revision: 1.2 $ $Date: 95/12/05 08:53:51 $ $Author: mg $ 

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
-- see the accompanying file Object.h.
-- --------------------------------------------------------------------------

with Xt.Intrinsic;

package Xt.Object is

    type ObjectRec is private;                              -- Object.h:33
    type ObjectClassRec is private;                         -- Object.h:34

    type Object is access all ObjectRec;                    -- Object.h:33
    type ObjectClass is access all ObjectClassRec;          -- Object.h:34

    objectClass_obj: Xt.Intrinsic.WidgetClass;              -- Object.h:37

private

    type ObjectClassRec is null record;                     -- Object.h:34
    type ObjectRec is null record;                          -- Object.h:33
    pragma Import(C, objectClass_obj, "objectClass");       -- Object.h:37

end Xt.Object;

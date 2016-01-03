-- $Source: /home/harp/1/proto/monoBANK/xbind/xt-composite.ads,v $ 
-- $Revision: 1.3 $ $Date: 95/12/19 15:35:29 $ $Author: mg $ 

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
-- see the accompanying file Intrinsic.h.
-- --------------------------------------------------------------------------

with Xt.Intrinsic;

package Xt.Composite is


    type CompositeClassRec is private;                      -- Composite.h:33

    type CompositeWidgetClass is access all CompositeClassRec;
                                                            -- Composite.h:33

    type XtOrderProc is access function (
                child: Xt.Intrinsic.Widget)
               return Xt.Intrinsic.Cardinal;                -- Composite.h:35

    compositeWidgetClass_obj: Xt.Intrinsic.WidgetClass;     -- Composite.h:72

    procedure XtManageChildren(
                children    : Xt.Intrinsic.WidgetList;
                num_children: Xt.Intrinsic.Cardinal);       -- Composite.h:43

    procedure XtManageChild(
                child: Xt.Intrinsic.Widget);                -- Composite.h:50

    procedure XtUnmanageChildren(
                children    : Xt.Intrinsic.WidgetList;
                num_children: Xt.Intrinsic.Cardinal);       -- Composite.h:56

    procedure XtUnmanageChild(
                child: Xt.Intrinsic.Widget);                -- Composite.h:63

private

    type CompositeClassRec is null record;                  -- Composite.h:33
    pragma Import(C, compositeWidgetClass_obj, "compositeWidgetClass");
                                                            -- Composite.h:72
    pragma Import(C, XtManageChildren, "XtManageChildren"); -- Composite.h:43
    pragma Import(C, XtManageChild, "XtManageChild");       -- Composite.h:50
    pragma Import(C, XtUnmanageChildren, "XtUnmanageChildren");
                                                            -- Composite.h:56
    pragma Import(C, XtUnmanageChild, "XtUnmanageChild");   -- Composite.h:63

    pragma Convention(C, XtOrderProc);

end Xt.Composite;

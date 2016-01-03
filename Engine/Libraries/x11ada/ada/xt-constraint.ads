-- $Source: /home/harp/1/proto/monoBANK/xbind/xt-constraint.ads,v $ 
-- $Revision: 1.2 $ $Date: 95/12/05 08:53:45 $ $Author: mg $ 

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
-- see the accompanying file Constraint.h.
-- --------------------------------------------------------------------------

with Xt.Intrinsic;

package Xt.Constraint is


    type ConstraintClassRec is private;                     -- Constraint.h:33

    type ConstraintWidgetClass is access all ConstraintClassRec;
                                                            -- Constraint.h:33

    constraintWidgetClass_obj: Xt.Intrinsic.WidgetClass;    -- Constraint.h:36

private

    type ConstraintClassRec is null record;                 -- Constraint.h:33
    pragma Import(C, constraintWidgetClass_obj, "constraintWidgetClass");
                                                            -- Constraint.h:36

end Xt.Constraint;

-- $Source: /home/harp/1/proto/monoBANK/xbind/xt-objectp.ads,v $ 
-- $Revision: 1.4 $ $Date: 96/02/27 13:08:48 $ $Author: mg $ 

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
-- C version contains additional copyrights, see below.
-- --------------------------------------------------------------------------
-- **********************************************************
-- Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
-- and the Massachusetts Institute of Technology, Cambridge, Massachusetts.
--                         All Rights Reserved
-- Permission to use, copy, modify, and distribute this software and its 
-- documentation for any purpose and without fee is hereby granted, 
-- provided that the above copyright notice appear in all copies and that
-- both that copyright notice and this permission notice appear in 
-- supporting documentation, and that the names of Digital or MIT not be
-- used in advertising or publicity pertaining to distribution of the
-- software without specific, written prior permission.  
-- DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
-- ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
-- DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
-- ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
-- WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
-- ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
-- SOFTWARE.
-- *****************************************************************

-- *********************************************************
--  * Object Instance Data Structures
--  *
--  *********************************************************
--  these fields match CorePart and can not be changed 

with Ada.Unchecked_Conversion;
with X.Strings;
with X.Xresource;
with Xt.Intrinsic;
with Xt.IntrinsicP;

package Xt.ObjectP is

    --  $XConsortium: ObjectP.h,v 1.10 89/10/04 12:22:44 swick Exp $ 
    --  $oHeader: ObjectP.h,v 1.2 88/08/18 15:55:35 asente Exp $ 
    Xt_ObjectP_h                : constant Boolean := True;

    type ObjectPart;
    type ObjectRec;
    type ObjectClassPart;
    type ObjectClassRec;

    type ObjectPart is
        record
            self             : Xt.Intrinsic.Widget;             -- pointer to widget itself          
            widget_class     : Xt.Intrinsic.WidgetClass;        -- pointer to Widget's ClassRec      
            parent           : Xt.Intrinsic.Widget;             -- parent widget                     
            xrm_name         : X.Xresource.XrmName;             -- widget resource name quarkified   
            being_destroyed  : Xt.Intrinsic.Boolean;            -- marked for destroy                
            destroy_callbacks: Xt.Intrinsic.XtCallbackList;     -- who to call when widget destroyed 
            constraints      : Xt.Intrinsic.XtPointer;          -- constraint record                 
        end record;

    pragma Convention(C, ObjectPart);


    type ObjectRec is
        record
            object: ObjectPart;
        end record;

    type ObjectRec_access is access all ObjectRec;

    function From_Widget is new Ada.Unchecked_Conversion (
	Xt.Intrinsic.Widget, ObjectRec_access);


    type ObjectClassPart is
        record
            superclass           : Xt.Intrinsic.WidgetClass;    -- pointer to superclass ClassRec   
            class_name           : X.Strings.charp;         -- widget resource class name       
            widget_size          : Xt.Intrinsic.Cardinal;       -- size in bytes of widget record   
            class_initialize     : Xt.IntrinsicP.XtProc;        -- class initialization proc        
            class_part_initialize: Xt.IntrinsicP.XtWidgetClassProc;-- dynamic initialization      
            class_inited         : Xt.Intrinsic.XtEnum;         -- has class been initialized?      
            initialize           : Xt.IntrinsicP.XtInitProc;    -- initialize subclass fields       
            initialize_hook      : Xt.IntrinsicP.XtArgsProc;    -- notify that initialize called    
            obj1                 : Xt.IntrinsicP.XtProc;        -- NULL                             
            obj2                 : Xt.IntrinsicP.XtProc;        -- NULL                             
            obj3                 : Xt.Intrinsic.Cardinal;       -- NULL                             
            resources            : Xt.Intrinsic.XtResourceList; -- resources for subclass fields    
            num_resources        : Xt.Intrinsic.Cardinal;       -- number of entries in resources   
            xrm_class            : X.Xresource.XrmClass;        -- resource class quarkified        
            obj4                 : Xt.Intrinsic.Boolean;        -- NULL                             
            obj5                 : Xt.Intrinsic.Boolean;        -- NULL                             
            obj6                 : Xt.Intrinsic.Boolean;        -- NULL				    
            obj7                 : Xt.Intrinsic.Boolean;        -- NULL                             
            destroy              : Xt.IntrinsicP.XtWidgetProc;  -- free data for subclass pointers  
            obj8                 : Xt.IntrinsicP.XtProc;        -- NULL                             
            obj9                 : Xt.IntrinsicP.XtProc;        -- NULL			            
            set_values           : Xt.IntrinsicP.XtSetValuesFunc;-- set subclass resource values     
            set_values_hook      : Xt.IntrinsicP.XtArgsFunc;    -- notify that set_values called    
            obj10                : Xt.IntrinsicP.XtProc;        -- NULL                             
            get_values_hook      : Xt.IntrinsicP.XtArgsProc;    -- notify that get_values called    
            obj11                : Xt.IntrinsicP.XtProc;        -- NULL                             
            version              : Xt.IntrinsicP.XtVersionType; -- version of intrinsics used       
            callback_private     : Xt.Intrinsic.XtPointer;      -- list of callback offsets       
            obj12                : X.Strings.charp;         -- NULL                             
            obj13                : Xt.IntrinsicP.XtProc;        -- NULL                             
            obj14                : Xt.IntrinsicP.XtProc;        -- NULL                             
            extension            : Xt.Intrinsic.XtPointer;      -- pointer to extension record      
        end record;


    type ObjectClassRec is
        record
            object_class: ObjectClassPart;
        end record;

    pragma Convention(C, ObjectClassRec);


    objectClass_Rec: ObjectClassRec;

private
    pragma Import(C, objectClass_Rec, "objectClassRec");

end Xt.ObjectP;
--  $XConsortium: ObjectP.h,v 1.10 89/10/04 12:22:44 swick Exp $ 
--  $oHeader: ObjectP.h,v 1.2 88/08/18 15:55:35 asente Exp $ 
-- **********************************************************
-- Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
-- and the Massachusetts Institute of Technology, Cambridge, Massachusetts.
--                         All Rights Reserved
-- Permission to use, copy, modify, and distribute this software and its 
-- documentation for any purpose and without fee is hereby granted, 
-- provided that the above copyright notice appear in all copies and that
-- both that copyright notice and this permission notice appear in 
-- supporting documentation, and that the names of Digital or MIT not be
-- used in advertising or publicity pertaining to distribution of the
-- software without specific, written prior permission.  
-- DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
-- ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
-- DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
-- ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
-- WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
-- ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
-- SOFTWARE.
-- *****************************************************************

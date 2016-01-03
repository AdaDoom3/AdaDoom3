-- $Source: /home/harp/1/proto/monoBANK/xbind/xt-corep.ads,v $ 
-- $Revision: 1.6 $ $Date: 96/11/04 08:47:38 $ $Author: mg $ 

with Ada.Unchecked_Conversion;
with X;
with X.Strings;
with X.Xlib;
with X.Xresource;
with Xt.Intrinsic;
with Xt.IntrinsicP;


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

package Xt.CoreP is

    -- * $XConsortium: CoreP.h,v 1.16 89/10/04 12:22:50 swick Exp $
    -- * $oHeader: CoreP.h,v 1.2 88/08/18 15:54:37 asente Exp $
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
    XtCoreP_h                   : constant Boolean := True;

    type CorePart is
        record
            self               : Xt.Intrinsic.Widget;           -- pointer to widget itself	     
            widget_class       : Xt.Intrinsic.WidgetClass;      -- pointer to Widget's ClassRec	     
            parent             : Xt.Intrinsic.Widget;           -- parent widget	  	     
            xrm_name           : X.Xresource.XrmName;           -- widget resource name quarkified   
            being_destroyed    : Xt.Intrinsic.Boolean;          -- marked for destroy		     
            destroy_callbacks  : Xt.Intrinsic.XtCallbackList;   -- who to call when widget destroyed 
            constraints        : Xt.Intrinsic.XtPointer;        -- constraint record                 
            xx                 : Xt.Intrinsic.Position;
            y                  : Xt.Intrinsic.Position;         -- window position		     
            width              : Xt.Intrinsic.Dimension;
            height             : Xt.Intrinsic.Dimension;        -- window dimensions		     
            border_width       : Xt.Intrinsic.Dimension;        -- window border width		     
            managed            : Xt.Intrinsic.Boolean;          -- is widget geometry managed?       
            sensitive          : Xt.Intrinsic.Boolean;          -- is widget sensitive to user events
            ancestor_sensitive : Xt.Intrinsic.Boolean;          -- are all ancestors sensitive?      
            event_table        : Xt.Intrinsic.XtEventTable;     -- private to event dispatcher       
            tm                 : Xt.IntrinsicP.XtTMRec;         -- translation management            
            accelerators       : Xt.Intrinsic.XtTranslations;   -- accelerator translations          
            border_pixel       : Xt.Intrinsic.Pixel;            -- window border pixel		     
            border_pixmap      : X.Pixmap;                      -- window border pixmap or NULL      
            popup_list         : Xt.Intrinsic.WidgetList;       -- list of popups                    
            num_popups         : Xt.Intrinsic.Cardinal;         -- how many popups                   
            name               : X.Strings.charp;           -- widget resource name		     
            screen             : X.Xlib.Screen_access;          -- window's screen		     
            colormap           : X.Colormap;                    -- colormap                          
            window             : X.Window;                      -- window ID			     
            depth              : Xt.Intrinsic.Cardinal;         -- number of planes in window        
            background_pixel   : Xt.Intrinsic.Pixel;            -- window background pixel	     
            background_pixmap  : X.Pixmap;                      -- window background pixmap or NULL  
            visible            : Xt.Intrinsic.Boolean;          -- is window mapped and not occluded?
            mapped_when_managed: Xt.Intrinsic.Boolean;          -- map window if it's managed?       
        end record;

    pragma Convention(C, CorePart);


    type WidgetRec is
        record
            core: CorePart;
        end record;

    pragma Convention(C, WidgetRec);

    type WidgetRec_access is access all WidgetRec;

    function From_Widget is new Ada.Unchecked_Conversion (
	Xt.Intrinsic.Widget, WidgetRec_access);


    subtype CoreRec is WidgetRec;

    type CoreClassPart is
        record
            superclass           : Xt.Intrinsic.WidgetClass;    -- pointer to superclass ClassRec   
            class_name           : X.Strings.charp;         -- widget resource class name       
            widget_size          : Xt.Intrinsic.Cardinal;       -- size in bytes of widget record   
            class_initialize     : Xt.IntrinsicP.XtProc;        -- class initialization proc	    
            class_part_initialize: Xt.IntrinsicP.XtWidgetClassProc;-- dynamic initialization	    
            class_inited         : Xt.Intrinsic.XtEnum;         -- has class been initialized?      
            initialize           : Xt.IntrinsicP.XtInitProc;    -- initialize subclass fields       
            initialize_hook      : Xt.IntrinsicP.XtArgsProc;    -- notify that initialize called    
            realize              : Xt.IntrinsicP.XtRealizeProc; -- XCreateWindow for widget	    
            actions              : Xt.Intrinsic.XtActionList;   -- widget semantics name to proc map 
            num_actions          : Xt.Intrinsic.Cardinal;       -- number of entries in actions     
            resources            : Xt.Intrinsic.XtResourceList; -- resources for subclass fields    
            num_resources        : Xt.Intrinsic.Cardinal;       -- number of entries in resources   
            xrm_class            : X.Xresource.XrmClass;        -- resource class quarkified	    
            compress_motion      : Xt.Intrinsic.Boolean;        -- compress MotionNotify for widget 
            compress_exposure    : Xt.Intrinsic.XtEnum;         -- compress Expose events for widget
            compress_enterleave  : Xt.Intrinsic.Boolean;        -- compress enter and leave events  
            visible_interest     : Xt.Intrinsic.Boolean;        -- select for VisibilityNotify      
            destroy              : Xt.IntrinsicP.XtWidgetProc;  -- free data for subclass pointers  
            resize               : Xt.IntrinsicP.XtWidgetProc;  -- geom manager changed widget size 
            expose               : Xt.IntrinsicP.XtExposeProc;  -- rediplay window		    
            set_values           : Xt.IntrinsicP.XtSetValuesFunc;-- set subclass resource values     
            set_values_hook      : Xt.IntrinsicP.XtArgsFunc;    -- notify that set_values called    
            set_values_almost    : Xt.IntrinsicP.XtAlmostProc;  -- set_values got "Almost" geo reply 
            get_values_hook      : Xt.IntrinsicP.XtArgsProc;    -- notify that get_values called    
            accept_focus         : Xt.IntrinsicP.XtAcceptFocusProc;-- assign input focus to widget     
            version              : Xt.IntrinsicP.XtVersionType; -- version of intrinsics used	    
            callback_private     : Xt.Intrinsic.XtPointer;      -- list of callback offsets       
            tm_table             : X.Strings.charp;         -- state machine                    
            query_geometry       : Xt.IntrinsicP.XtGeometryHandler;-- return preferred geometry        
            display_accelerator  : Xt.IntrinsicP.XtStringProc;  -- display your accelerator	    
            extension            : Xt.Intrinsic.XtPointer;      -- pointer to extension record      
        end record;

    pragma Convention(C, CoreClassPart);


    type WidgetClassRec is
        record
            core_class: CoreClassPart;
        end record;

    pragma Convention(C, WidgetClassRec);


    subtype CoreClassRec is WidgetClassRec;

    XtInheritTranslations_obj: aliased X.signed_int;

    widgetClass_Rec           : WidgetClassRec;
    pragma Import(C, widgetClass_Rec, "widgetClassRec");
    coreClass_Rec: WidgetClassRec renames widgetClass_Rec;

    function XtInheritTranslations return X.Strings.charp;

    pragma Inline(XtInheritTranslations);


    function XtInheritRealize return Xt.IntrinsicP.XtRealizeProc;

    pragma Inline(XtInheritRealize);

    function XtInheritResize return Xt.IntrinsicP.XtWidgetProc;

    pragma Inline(XtInheritResize);

    function XtInheritExpose return Xt.IntrinsicP.XtExposeProc;

    pragma Inline(XtInheritExpose);

    function XtInheritSetValuesAlmost return Xt.IntrinsicP.XtAlmostProc;

    pragma Inline(XtInheritSetValuesAlmost);

    function XtInheritAcceptFocus return Xt.IntrinsicP.XtAcceptFocusProc;

    pragma Inline(XtInheritAcceptFocus);

    function XtInheritQueryGeometry return Xt.IntrinsicP.XtGeometryHandler;

    pragma Inline(XtInheritQueryGeometry);

    function XtInheritDisplayAccelerator return Xt.IntrinsicP.XtStringProc;

    pragma Inline(XtInheritDisplayAccelerator);


private
    pragma Import(C, XtInheritTranslations_obj, "_XtInheritTranslations");

end Xt.CoreP;
-- * $XConsortium: CoreP.h,v 1.16 89/10/04 12:22:50 swick Exp $
-- * $oHeader: CoreP.h,v 1.2 88/08/18 15:54:37 asente Exp $
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
--  DON'T ADD STUFF AFTER THIS #endif 

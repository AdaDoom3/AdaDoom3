-- $Source: /home/harp/1/proto/monoBANK/xbind/xt-intrinsicp.ads,v $ 
-- $Revision: 1.4 $ $Date: 95/12/19 15:35:40 $ $Author: mg $ 

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

with Interfaces.c;
with X;
with X.Strings;
with X.Xlib;
with X.Xutil;
with X.Xresource;
with Xt.Intrinsic;
with Xt.Object;

package Xt.IntrinsicP is
    subtype Char_Array is Interfaces.C.Char_Array;


    XtintrinsicP_h             : constant Boolean := True;
    XT_VERSION                 : constant  := 11;
    XT_REVISION                : constant  := 5;
    XtVersion                  : constant  := 11005;
    XtVersionDontCheck         : constant  := 0;
    XtExposeCompressMultiple   : constant  := 2;
    XtExposeCompressMaximal    : constant  := 3;
    --  modifiers 
    XtExposeGraphicsExpose     : constant  := 16#10#;
    XtExposeGraphicsExposeMerged: constant  := 16#20#;
    XtExposeNoExpose            : constant  := 16#40#;

    type XtVersionType is new X.unsigned_long;

    type anonymous1_t is access constant X.char;

    type XrmResource;
    type XtTMRec;

    type XrmResource_access is access all XrmResource;
    type XrmResourceList is access all XrmResource;
    type XtWidgetGeometry_access is access all Xt.Intrinsic.XtWidgetGeometry;
    type XSetWindowAttributes_access is access all X.Xlib.XSetWindowAttributes;
    type XtTM is access all XtTMRec;

    type XrmResource is
        record
            xrm_name        : X.Xresource.XrmQuark;             -- Resource name quark		
            xrm_class       : X.Xresource.XrmQuark;             -- Resource class quark		
            xrm_type        : X.Xresource.XrmQuark;             -- Resource representation type quark 
            xrm_size        : Xt.Intrinsic.Cardinal;            -- Size in bytes of representation	
            xrm_offset      : X.signed_long;                    -- -offset-1				
            xrm_default_type: X.Xresource.XrmQuark;             -- Default representation type quark	
            xrm_default_addr: Xt.Intrinsic.XtPointer;           -- Default resource address		
        end record;


    type XtProc is access procedure;


    type XtWidgetClassProc is access procedure(
                p1: Xt.Intrinsic.WidgetClass);


    type XtWidgetProc is access procedure(
                p1: Xt.Intrinsic.Widget);


    type XtAcceptFocusProc is access function(
                p1: Xt.Intrinsic.Widget;
                p2: access X.Time)
               return Xt.Intrinsic.Boolean;


    type XtArgsProc is access procedure(
                p1: Xt.Intrinsic.Widget;
                p2: Xt.Intrinsic.ArgList;
                p3: access Xt.Intrinsic.Cardinal);


    type XtInitProc is access procedure(
                p1: Xt.Intrinsic.Widget;
                p2: Xt.Intrinsic.Widget;
                p3: Xt.Intrinsic.ArgList;
                p4: access Xt.Intrinsic.Cardinal);


    type XtSetValuesFunc is access function(
                p1: Xt.Intrinsic.Widget;
                p2: Xt.Intrinsic.Widget;
                p3: Xt.Intrinsic.Widget;
                p4: Xt.Intrinsic.ArgList;
                p5: access Xt.Intrinsic.Cardinal)
               return Xt.Intrinsic.Boolean;


    type XtArgsFunc is access function(
                p1: Xt.Intrinsic.Widget;
                p2: Xt.Intrinsic.ArgList;
                p3: access Xt.Intrinsic.Cardinal)
               return Xt.Intrinsic.Boolean;


    type XtAlmostProc is access procedure(
                p1: Xt.Intrinsic.Widget;
                p2: Xt.Intrinsic.Widget;
                p3: access Xt.Intrinsic.XtWidgetGeometry;
                p4: access Xt.Intrinsic.XtWidgetGeometry);


    type XtExposeProc is access procedure(
                p1: Xt.Intrinsic.Widget;
                p2: access X.Xlib.XEvent;
                p3: X.Xutil.Region);


    type XtRealizeProc is access procedure(
                p1: Xt.Intrinsic.Widget;
                p2: access Xt.Intrinsic.XtValueMask;
                p3: access X.Xlib.XSetWindowAttributes);


    type XtGeometryHandler is access function(
                p1: Xt.Intrinsic.Widget;
                p2: access Xt.Intrinsic.XtWidgetGeometry;
                p3: access Xt.Intrinsic.XtWidgetGeometry)
               return Xt.Intrinsic.XtGeometryResult;


    type XtStringProc is access procedure(
                p1: Xt.Intrinsic.Widget;
                p2: X.Strings.charp);



    type XtTMRec is
        record
            translations : Xt.Intrinsic.XtTranslations;         -- private to Translation Manager    
            proc_table   : Xt.Intrinsic.XtBoundActions;         -- procedure bindings for actions    
            current_state: X.Xlib.XFontSet;       -- Translation Manager state ptr     
            lastEventTime: X.unsigned_long;
        end record;

    --  compress_exposure options
    function XtExposeNoCompress return Xt.Intrinsic.XtEnum;

    pragma Inline(XtExposeNoCompress);

    function XtExposeCompressSeries return Xt.Intrinsic.XtEnum;

    pragma Inline(XtExposeCompressSeries);

    function XtDisplay(
                widget: Xt.Intrinsic.Widget)
               return X.Xlib.XDisplay_access;


    function XtScreen(
                widget: Xt.Intrinsic.Widget)
               return X.Xlib.Screen_access;


    function XtClass(
                widget: Xt.Intrinsic.Widget)
               return Xt.Intrinsic.WidgetClass;

    pragma Inline(XtClass);

    function XtSuperclass(
                widget: Xt.Intrinsic.Widget)
               return Xt.Intrinsic.WidgetClass;

    pragma Inline(XtSuperclass);

    function XtParent(
                widget: Xt.Intrinsic.Widget)
               return Xt.Intrinsic.Widget;

    pragma Inline(XtParent);

    function XtIsRectObj(
                obj: Xt.Object.Object)
               return X.signed_int;

    pragma Inline(XtIsRectObj);

    function XtIsWidget(
                obj: Xt.Object.Object)
               return X.signed_int;

    pragma Inline(XtIsWidget);

    function XtIsComposite(
                obj: Xt.Object.Object)
               return X.signed_int;

    pragma Inline(XtIsComposite);

    function XtIsConstraint(
                obj: Xt.Object.Object)
               return X.signed_int;

    pragma Inline(XtIsConstraint);

    function XtIsShell(
                obj: Xt.Object.Object)
               return X.signed_int;

    pragma Inline(XtIsShell);

    function XtIsWMShell(
                obj: Xt.Object.Object)
               return X.signed_int;

    pragma Inline(XtIsWMShell);

    function XtIsTopLevelShell(
                obj: Xt.Object.Object)
               return X.signed_int;

    pragma Inline(XtIsTopLevelShell);

    procedure XtCheckSubclass(
                w           : Xt.Intrinsic.Widget;
                widget_class: Xt.Intrinsic.WidgetClass;
                message     : X.Strings.const_charp);

    pragma Inline(XtCheckSubclass);

    procedure XtCheckSubclass(
                w           : Xt.Intrinsic.Widget;
                widget_class: Xt.Intrinsic.WidgetClass;
                message     : Char_Array);

    pragma Inline(XtCheckSubclass);

    function XtWindowedAncestor(
                p1: Xt.Intrinsic.Widget)
               return Xt.Intrinsic.Widget;                      -- internal; implementation-dependent 



    procedure XtInherit;


    procedure XtCreateWindow(
                p1: Xt.Intrinsic.Widget;
                p2: X.unsigned_int;
                p3: access X.Xlib.Visual;
                p4: Xt.Intrinsic.XtValueMask;
                p5: access X.Xlib.XSetWindowAttributes);


    procedure XtResizeWidget(
                p1: Xt.Intrinsic.Widget;
                p2: X.unsigned_int;
                p3: X.unsigned_int;
                p4: X.unsigned_int);


    procedure XtMoveWidget(
                p1: Xt.Intrinsic.Widget;
                p2: X.signed_int;
                p3: X.signed_int);


    procedure XtConfigureWidget(
                p1: Xt.Intrinsic.Widget;
                p2: X.signed_int;
                p3: X.signed_int;
                p4: X.unsigned_int;
                p5: X.unsigned_int;
                p6: X.unsigned_int);


    procedure XtResizeWindow(
                p1: Xt.Intrinsic.Widget);



private
    pragma Import(C, XtWindowedAncestor, "_XtWindowedAncestor");
    pragma Import(C, XtInherit, "_XtInherit");
    pragma Import(C, XtCreateWindow, "XtCreateWindow");
    pragma Import(C, XtResizeWidget, "XtResizeWidget");
    pragma Import(C, XtMoveWidget, "XtMoveWidget");
    pragma Import(C, XtConfigureWidget, "XtConfigureWidget");
    pragma Import(C, XtResizeWindow, "XtResizeWindow");

    pragma Convention(C, XtProc);
    pragma Convention(C, XtWidgetClassProc);
    pragma Convention(C, XtWidgetProc);
    pragma Convention(C, XtAcceptFocusProc);
    pragma Convention(C, XtArgsProc);
    pragma Convention(C, XtInitProc);
    pragma Convention(C, XtSetValuesFunc);
    pragma Convention(C, XtArgsFunc);
    pragma Convention(C, XtAlmostProc);
    pragma Convention(C, XtExposeProc);
    pragma Convention(C, XtRealizeProc);
    pragma Convention(C, XtGeometryHandler);
    pragma Convention(C, XtStringProc);

end Xt.IntrinsicP;

--  $XConsortium: IntrinsicP.h,v 1.57 91/06/26 19:33:20 converse Exp $ 
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

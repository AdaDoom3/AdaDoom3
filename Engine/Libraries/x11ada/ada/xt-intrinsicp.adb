-- $Source: /home/harp/1/proto/monoBANK/xbind/xt-intrinsicp.adb,v $ 
-- $Revision: 1.3 $ $Date: 95/12/08 15:39:01 $ $Author: mg $ 

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

with Ada.Unchecked_Conversion;
with Xt.CoreP;
with Xt.ObjectP;

package body Xt.IntrinsicP is

    type CoreWidget is access all Xt.CoreP.WidgetRec;
    function To_CoreWidget is new Ada.Unchecked_Conversion (
	Xt.Intrinsic.Widget, CoreWidget);

    type CoreWidgetClass is access all Xt.CoreP.WidgetClassRec;
    function To_CoreWidgetClass is new Ada.Unchecked_Conversion (
	Xt.Intrinsic.WidgetClass, CoreWidgetClass);

    function Get_class_inited (obj: Xt.Object.Object) return X.unsigned_int is
	type Object is access all Xt.ObjectP.ObjectRec;
	function To_Object is new Ada.Unchecked_Conversion (
	    Xt.Object.Object, Object);
	Ob: Object := To_Object(obj);
    begin
	return X.unsigned_int(To_CoreWidgetClass(ob.object.widget_class).
		  core_class.class_inited);
    end Get_class_inited;

    pragma Inline(Get_class_inited);

    use type Interfaces.C.unsigned;	-- to get "and"

    function To_Bool (N: X.unsigned_int) return X.signed_int is
    begin
	if N /= 0 then return 1; else return 0; end if;
    end To_Bool;

    pragma Inline(To_Bool);

    --  compress_exposure options
    function XtExposeNoCompress return Xt.Intrinsic.XtEnum is
    begin
        return X.Xlib.False;
    end XtExposeNoCompress;

    function XtExposeCompressSeries return Xt.Intrinsic.XtEnum is
    begin
        return X.Xlib.True;
    end XtExposeCompressSeries;

    function XtDisplay(
                widget: Xt.Intrinsic.Widget)
               return X.Xlib.XDisplay_access is
    begin
        return To_CoreWidget(widget).core.screen.display;
    end XtDisplay;

    function XtScreen(
                widget: Xt.Intrinsic.Widget)
               return X.Xlib.Screen_access is
    begin
        return To_CoreWidget(widget).core.screen;
    end XtScreen;

    function XtClass(
                widget: Xt.Intrinsic.Widget)
               return Xt.Intrinsic.WidgetClass is
    begin
        return To_CoreWidget(widget).core.widget_class;
    end XtClass;

    function XtSuperclass(
                widget: Xt.Intrinsic.Widget)
               return Xt.Intrinsic.WidgetClass is
    begin
        return To_CoreWidgetClass(Xt.Intrinsic.XtClass( widget )).
		  core_class.superclass;
    end XtSuperclass;

    function XtParent(
                widget: Xt.Intrinsic.Widget)
               return Xt.Intrinsic.Widget is
    begin
        return To_CoreWidget(widget).core.parent;
    end XtParent;

    function XtIsRectObj(
                obj: Xt.Object.Object)
               return X.signed_int is
    begin
        return To_Bool(Get_class_inited(obj) and 16#2#);
    end XtIsRectObj;

    function XtIsWidget(
                obj: Xt.Object.Object)
               return X.signed_int is
    begin
        return To_Bool(Get_class_inited(obj) and 16#4#);
    end XtIsWidget;

    function XtIsComposite(
                obj: Xt.Object.Object)
               return X.signed_int is
    begin
        return To_Bool(Get_class_inited(obj) and 16#8#);
    end XtIsComposite;

    function XtIsConstraint(
                obj: Xt.Object.Object)
               return X.signed_int is
    begin
        return To_Bool(Get_class_inited(obj) and 16#10#);
    end XtIsConstraint;

    function XtIsShell(
                obj: Xt.Object.Object)
               return X.signed_int is
    begin
        return To_Bool(Get_class_inited(obj) and 16#20#);
    end XtIsShell;

    function XtIsWMShell(
                obj: Xt.Object.Object)
               return X.signed_int is
    begin
        return To_Bool(Get_class_inited(obj) and 16#40#);
    end XtIsWMShell;

    function XtIsTopLevelShell(
                obj: Xt.Object.Object)
               return X.signed_int is
    begin
        return To_Bool(Get_class_inited(obj) and 16#80#);
    end XtIsTopLevelShell;

    procedure XtCheckSubclass(
                w           : Xt.Intrinsic.Widget;
                widget_class: Xt.Intrinsic.WidgetClass;
                message     : Char_Array) is

        function "&"(S: Interfaces.C.Char_Array; C: Interfaces.C.Char)
	    return Interfaces.C.Char_Array renames Interfaces.C."&";
        Nul: Interfaces.C.Char renames Interfaces.C.Nul;

        Tmp_message: constant Char_Array := 
            message & Nul;
    begin
        XtCheckSubclass(
                w,
                widget_class,
                Tmp_message(Tmp_message'First)'unchecked_access);
    end XtCheckSubclass;

    procedure XtCheckSubclass(
                w           : Xt.Intrinsic.Widget;
                widget_class: Xt.Intrinsic.WidgetClass;
                message     : X.Strings.const_charp) is
    begin
        null;
    end XtCheckSubclass;


end Xt.IntrinsicP;

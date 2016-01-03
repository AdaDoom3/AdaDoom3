-- $Source: /home/harp/1/proto/monoBANK/xbind/xt-intrinsic.adb,v $ 
-- $Revision: 1.15 $ $Date: 96/10/31 12:49:14 $ $Author: mg $ 

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
with Interfaces.C;
with Stdarg;
with Stdarg.Impl;
with System;
with X.Strings;
with Xt.Shell;
with Xt.Vendor;
with Xt.CoreP;

package body Xt.Intrinsic is

    -- **************************
    -- Incomplete types from spec
    -- **************************
    type WidgetClassRec is new Xt.CoreP.WidgetClassRec;
    type WidgetRec is new Xt.CoreP.WidgetRec;

    -- *********************************
    -- Stdarg functions that need bodies
    -- *********************************

    type XtAppContext_access is access all XtAppContext;
    function "&" is new Stdarg.Concat(XtAppContext_access);

    type Natural_access is access all X.Strings.Natural_Int;
    function "&" is new Stdarg.Concat(Natural_access);

    function "&" is new Stdarg.Concat(X.Strings.const_charp);
    function "&" is new Stdarg.Concat(WidgetClass);
    function "&" is new Stdarg.Concat(Widget);
    function "&" is new Stdarg.Concat(X.Xlib.XDisplay_access);
    function "&" is new Stdarg.Concat(X.Xresource.XrmOptionDescList);
    function "&" is new Stdarg.Concat(XtPointer);
    function "&" is new Stdarg.Concat(Cardinal);
    function "&" is new Stdarg.Concat(XtResourceList);
    function "&" is new Stdarg.Concat(X.Strings.charp_vector);
    function "&" is new Stdarg.Concat(System.Address);

    Null_Addr: System.Address renames System.Null_Address;

    function To_Widget is new Ada.Unchecked_Conversion(
        Stdarg.C_Param, Xt.Intrinsic.Widget);

    function XtVaCreatePopupShell(
                name        : X.Strings.const_charp;
                widget_class: WidgetClass;
                parent      : Xt.Intrinsic.Widget;
                args        : Stdarg.ArgList := Stdarg.Empty)
               return Widget is                             -- Intrinsic.h:1456

        use Stdarg, Stdarg.Impl;

        Complete_Args: Stdarg.ArgList :=
            Stdarg.Empty & name & widget_class & parent & args & Null_Addr;

        function C_XtVaCreatePopupShell return Integer;
        pragma Import(C, C_XtVaCreatePopupShell, "XtVaCreatePopupShell");

    begin
        return To_Widget(F_Varargs(
            C_XtVaCreatePopupShell'Address,
            ArgCount(Complete_Args),
            Address_of_First_Arg(Complete_Args)));

    end XtVaCreatePopupShell;

    function XtVaCreateWidget(
                name        : X.Strings.const_charp;
                widget_class: WidgetClass;
                parent      : Xt.Intrinsic.Widget;
                args        : Stdarg.ArgList := Stdarg.Empty)
               return Xt.Intrinsic.Widget is                -- Intrinsic.h:1545

        use Stdarg, Stdarg.Impl;

        Complete_Args: Stdarg.ArgList :=
            Stdarg.Empty & name & widget_class & parent & args & Null_Addr;

        function C_XtVaCreateWidget return Integer;
        pragma Import(C, C_XtVaCreateWidget, "XtVaCreateWidget");

    begin
        return To_Widget(F_Varargs(
            C_XtVaCreateWidget'Address,
            ArgCount(Complete_Args),
            Address_of_First_Arg(Complete_Args)));

    end XtVaCreateWidget;

    function XtVaCreateManagedWidget(
                name        : X.Strings.const_charp;
                widget_class: WidgetClass;
                parent      : Widget;
                args        : Stdarg.ArgList := Stdarg.Empty)
               return Widget is                             -- Intrinsic.h:1554

        use Stdarg, Stdarg.Impl;

        Complete_Args: Stdarg.ArgList :=
            Stdarg.Empty & name & widget_class & parent & args & Null_Addr;

        function C_XtVaCreateManagedWidget return Integer;
        pragma Import(C, C_XtVaCreateManagedWidget, "XtVaCreateManagedWidget");

    begin
        return To_Widget(F_Varargs(
            C_XtVaCreateManagedWidget'Address,
            ArgCount(Complete_Args),
            Address_of_First_Arg(Complete_Args)));

    end XtVaCreateManagedWidget;

    function XtVaAppCreateShell(
                application_name : X.Strings.const_charp;
                application_class: X.Strings.const_charp;
                widget_class     : WidgetClass;
                display          : access X.Xlib.Display;
                args             : Stdarg.ArgList := Stdarg.Empty)
               return Widget is                             -- Intrinsic.h:1583

        use Stdarg, Stdarg.Impl;

        Complete_Args: Stdarg.ArgList :=
            Stdarg.Empty & application_name & application_class & 
                           widget_class & 
			   X.Xlib.XDisplay_access(display)
			   & args & Null_Addr;

        function C_XtVaAppCreateShell return Integer;
        pragma Import(C, C_XtVaAppCreateShell, "XtVaAppCreateShell");

    begin
        return To_Widget(F_Varargs(
            C_XtVaAppCreateShell'Address,
            ArgCount(Complete_Args),
            Address_of_First_Arg(Complete_Args)));

    end XtVaAppCreateShell;

    function XtVaAppInitialize(
                app_context_return: access XtAppContext;
                application_class : X.Strings.const_charp;
                options           : X.Xresource.XrmOptionDescList;
                num_options       : Cardinal;
                argc_in_out       : access X.Strings.Natural_Int;
                argv_in_out       : X.Strings.charp_vector;
                fallback_resources: X.Strings.charp_vector;
                args              : Stdarg.ArgList := Stdarg.Empty)
               return Widget is                             -- Intrinsic.h:1640

        use Stdarg, Stdarg.Impl;

        Complete_Args: Stdarg.ArgList :=
            Stdarg.Empty & XtAppContext_access(app_context_return) & 
		 application_class & 
		 options & num_options & argc_in_out.all'access & argv_in_out &
		 fallback_resources & args & Null_Addr;

        function C_XtVaAppInitialize return Integer;
        pragma Import(C, C_XtVaAppInitialize, "XtVaAppInitialize");

    begin
        return To_Widget(F_Varargs(
            C_XtVaAppInitialize'Address,
            ArgCount(Complete_Args),
            Address_of_First_Arg(Complete_Args)));

    end XtVaAppInitialize;

    procedure XtVaGetApplicationResources(
                widget       : Xt.Intrinsic.Widget;
                base         : XtPointer;
                resources    : XtResourceList;
                num_resources: Cardinal;
                args         : Stdarg.ArgList := Stdarg.Empty) is
                                                            -- Intrinsic.h:1743
        use Stdarg, Stdarg.Impl;

        Complete_Args: Stdarg.ArgList :=
            Stdarg.Empty & widget & base & resources & num_resources & args & 
	    Null_Addr;

        function C_XtVaGetApplicationResources return Integer;
        pragma Import(C, C_XtVaGetApplicationResources, 
                     "XtVaGetApplicationResources");
    begin
        Do_Varargs(
            C_XtVaGetApplicationResources'Address,
            ArgCount(Complete_Args),
            Address_of_First_Arg(Complete_Args));

    end XtVaGetApplicationResources;


    procedure XtVaGetSubresources(
                widget       : Xt.Intrinsic.Widget;
                base         : XtPointer;
                name         : X.Strings.const_charp;
                class        : X.Strings.const_charp;
                resources    : XtResourceList;
                num_resources: Cardinal;
                args         : Stdarg.ArgList := Stdarg.Empty) is
                                                            -- Intrinsic.h:1766
        use Stdarg, Stdarg.Impl;

        Complete_Args: Stdarg.ArgList :=
            Stdarg.Empty & widget & base & name & class &
                           resources & num_resources & args & Null_Addr;

        function C_XtVaGetSubresources return Integer;
        pragma Import(C, C_XtVaGetSubresources, "XtVaGetSubresources");

    begin
        Do_Varargs(
            C_XtVaGetSubresources'Address,
            ArgCount(Complete_Args),
            Address_of_First_Arg(Complete_Args));

    end XtVaGetSubresources;

    procedure XtVaSetValues(
                widget: Xt.Intrinsic.Widget;
                args  : Stdarg.ArgList := Stdarg.Empty) is  -- Intrinsic.h:1786

        use Stdarg, Stdarg.Impl;

        Complete_Args: Stdarg.ArgList :=
            Stdarg.Empty & widget & args & Null_Addr;

        function C_XtVaSetValues return Integer;
        pragma Import(C, C_XtVaSetValues, "XtVaSetValues");

    begin
        Do_Varargs(
            C_XtVaSetValues'Address,
            ArgCount(Complete_Args),
            Address_of_First_Arg(Complete_Args));

    end XtVaSetValues;

    procedure XtVaGetValues(
                widget: Xt.Intrinsic.Widget;
                args  : Stdarg.ArgList := Stdarg.Empty) is  -- Intrinsic.h:1801
        use Stdarg, Stdarg.Impl;

        Complete_Args: Stdarg.ArgList :=
            Stdarg.Empty & widget & args & Null_Addr;

        function C_XtVaGetValues return Integer;
        pragma Import(C, C_XtVaGetValues, "XtVaGetValues");

    begin
        Do_Varargs(
            C_XtVaGetValues'Address,
            ArgCount(Complete_Args),
            Address_of_First_Arg(Complete_Args));

    end XtVaGetValues;

    procedure XtVaSetSubvalues(
                base         : XtPointer;
                resources    : XtResourceList;
                num_resources: Cardinal;
                args         : Stdarg.ArgList := Stdarg.Empty) is
                                                            -- Intrinsic.h:1818
        use Stdarg, Stdarg.Impl;

        Complete_Args: Stdarg.ArgList :=
            Stdarg.Empty & base & resources & num_resources & args & Null_Addr;

        function C_XtVaSetSubvalues return Integer;
        pragma Import(C, C_XtVaSetSubvalues, "XtVaSetSubvalues");

    begin
        Do_Varargs(
            C_XtVaSetSubvalues'Address,
            ArgCount(Complete_Args),
            Address_of_First_Arg(Complete_Args));

    end XtVaSetSubvalues;

    procedure XtVaGetSubvalues(
                base         : XtPointer;
                resources    : XtResourceList;
                num_resources: Cardinal;
                args         : Stdarg.ArgList := Stdarg.Empty) is
                                                            -- Intrinsic.h:1837
        use Stdarg, Stdarg.Impl;

        Complete_Args: Stdarg.ArgList :=
            Stdarg.Empty & base & resources & num_resources & args & Null_Addr;

        function C_XtVaGetSubvalues return Integer;
        pragma Import(C, C_XtVaGetSubvalues, "XtVaGetSubvalues");

    begin
        Do_Varargs(
            C_XtVaGetSubvalues'Address,
            ArgCount(Complete_Args),
            Address_of_First_Arg(Complete_Args));

    end XtVaGetSubvalues;

    -- ******************
    -- Intrinsic.h macros
    -- ******************

    function XtIsRectObj (W : Widget) return Xt.Intrinsic.Boolean is
    begin
        return XtCheckSubclassFlag(W, 16#2#);
    end XtIsRectObj;

    function XtIsWidget (W : Widget) return Xt.Intrinsic.Boolean is
    begin
        return XtCheckSubclassFlag(W, 16#4#);
    end XtIsWidget;

    function XtIsComposite (W : Widget) return Xt.Intrinsic.Boolean is
    begin
        return XtCheckSubclassFlag(W, 16#8#);
    end XtIsComposite;

    function XtIsConstraint (W : Widget) return Xt.Intrinsic.Boolean is
    begin
        return XtCheckSubclassFlag(W, 16#10#);
    end XtIsConstraint;

    function XtIsShell (W : Widget) return Xt.Intrinsic.Boolean is
    begin
        return XtCheckSubclassFlag(W, 16#20#);
    end XtIsShell;

    function XtIsOverrideShell (W : Widget) return Xt.Intrinsic.Boolean is
    begin
        return XtIsSubclassOf(
            W, 
            Xt.Shell.overrideShellWidgetClass_obj,
            Xt.Shell.shellWidgetClass_obj,
            16#20#);
    end XtIsOverrideShell;

    function XtIsWMShell (W : Widget) return Xt.Intrinsic.Boolean is
    begin
        return XtCheckSubclassFlag(W, 16#40#);
    end XtIsWMShell;

    function XtIsVendorShell (W : Widget) return Xt.Intrinsic.Boolean is
    begin
        return XtIsSubclassOf(
            W,
            Xt.Vendor.vendorShellWidgetClass_obj,
            Xt.Shell.wmshellWidgetClass_obj,
            16#40#);
    end XtIsVendorShell;

    function XtIsTransientShell (W : Widget) return Xt.Intrinsic.Boolean is
    begin
        return XtIsSubclassOf(
            W,
            Xt.Shell.transientShellWidgetClass_obj,
            Xt.Shell.wmshellWidgetClass_obj,
            16#40#);
    end XtIsTransientShell;

    function XtIsTopLevelShell (W : Widget) return Xt.Intrinsic.Boolean is
    begin
        return XtCheckSubclassFlag(W, 16#80#);
    end XtIsTopLevelShell;

    function XtIsApplicationShell (W : Widget) return Xt.Intrinsic.Boolean is
    begin
        return XtIsSubclassOf(
            W,
            Xt.Shell.applicationShellWidgetClass_obj,
            Xt.Shell.topLevelShellWidgetClass_obj,
            16#80#);
    end XtIsApplicationShell;

    procedure XtSetArg (A    : out Arg; 
                        Name : in X.Strings.const_charp;
                        Value: in XtArgVal) is
	function To_charp is new Ada.Unchecked_Conversion (
	    X.Strings.const_charp, X.Strings.charp);
    begin
        A.Name  := To_charp(Name);
        A.Value := Value;
    end XtSetArg;

    procedure XtMapWidget (W: Widget) is
    begin
        X.Xlib.XMapWindow(XtDisplay(W), XtWindow(W));
    end XtMapWidget;

    procedure XtUnmapWidget (W: Widget) is
    begin
        X.Xlib.XUnmapWindow(XtDisplay(W), XtWindow(W));
    end XtUnmapWidget;

    function XtNewString(Str: X.Strings.const_charp) return X.Strings.charp is
        Res: X.Strings.charp := null;
        function Strlen(Str: X.Strings.const_charp) return Interfaces.C.Int;
        procedure Strcpy(Dest: X.Strings.charp; Src: X.Strings.const_charp);
        pragma Import(C, Strlen, "strlen");
        pragma Import(C, Strcpy, "strcpy");
	use type X.Strings.const_charp;
    begin
        if Str /= null then
            Res := XtMalloc(Cardinal(Strlen(Str)));
            Strcpy(Res, Str);
        end if;
        return Res;
    end XtNewString;

    -- ****************************************
    -- Functions with extra Char_Array versions
    -- ****************************************

    function XtConvertAndStore(
                widget   : Xt.Intrinsic.Widget;
                from_type: Interfaces.C.Char_Array;
                from     : access X.Xresource.XrmValue;
                to_type  : Interfaces.C.Char_Array;
                to_in_out: access X.Xresource.XrmValue) return Boolean is

        use type Interfaces.C.Char_Array;
        Tmp_from_type: constant Interfaces.C.Char_Array :=
            from_type & Interfaces.C.Nul;
        Tmp_to_type: constant Interfaces.C.Char_Array :=
            to_type & Interfaces.C.Nul;
    begin
        return XtConvertAndStore(
                widget,
                Tmp_from_type(Tmp_from_type'First)'unchecked_access,
                from,
                Tmp_to_type(Tmp_to_type'First)'unchecked_access,
                to_in_out);
    end XtConvertAndStore;                                  -- Intrinsic.h:529

    procedure XtDisplayStringConversionWarning(
                dpy       : access X.Xlib.Display;
                from_value: Interfaces.C.Char_Array;
                to_type   : Interfaces.C.Char_Array) is

        use type Interfaces.C.Char_Array;
        Tmp_from_value: constant Interfaces.C.Char_Array :=
            from_value & Interfaces.C.Nul;
        Tmp_to_type: constant Interfaces.C.Char_Array :=
            to_type & Interfaces.C.Nul;
    begin
        XtDisplayStringConversionWarning(
                dpy,
                Tmp_from_value(Tmp_from_value'First)'unchecked_access,
                Tmp_to_type(Tmp_to_type'First)'unchecked_access);
    end XtDisplayStringConversionWarning;                   -- Intrinsic.h:692

    procedure XtAppAddConverter(
                app_context : XtAppContext;
                from_type   : Interfaces.C.Char_Array;
                to_type     : Interfaces.C.Char_Array;
                converter   : XtConverter;
                convert_args: XtConvertArgList;
                num_args    : Cardinal) is

        use type Interfaces.C.Char_Array;
        Tmp_from_type: constant Interfaces.C.Char_Array :=
            from_type & Interfaces.C.Nul;
        Tmp_to_type: constant Interfaces.C.Char_Array :=
            to_type & Interfaces.C.Nul;
    begin
        XtAppAddConverter(
                app_context,
                Tmp_from_type(Tmp_from_type'First)'unchecked_access,
                Tmp_to_type(Tmp_to_type'First)'unchecked_access,
                converter,
                convert_args,
                num_args);
    end XtAppAddConverter;                                  -- Intrinsic.h:708

    procedure XtAddConverter(
                from_type   : Interfaces.C.Char_Array;
                to_type     : Interfaces.C.Char_Array;
                converter   : XtConverter;
                convert_args: XtConvertArgList;
                num_args    : Cardinal) is

        use type Interfaces.C.Char_Array;
        Tmp_from_type: constant Interfaces.C.Char_Array :=
            from_type & Interfaces.C.Nul;
        Tmp_to_type: constant Interfaces.C.Char_Array :=
            to_type & Interfaces.C.Nul;
    begin
        XtAddConverter(
                Tmp_from_type(Tmp_from_type'First)'unchecked_access,
                Tmp_to_type(Tmp_to_type'First)'unchecked_access,
                converter,
                convert_args,
                num_args);
    end XtAddConverter;                                     -- Intrinsic.h:719

    procedure XtSetTypeConverter(
                from_type   : Interfaces.C.Char_Array;
                to_type     : Interfaces.C.Char_Array;
                converter   : XtTypeConverter;
                convert_args: XtConvertArgList;
                num_args    : Cardinal;
                cache_type  : XtCacheType;
                destructor  : XtDestructor) is

        use type Interfaces.C.Char_Array;
        Tmp_from_type: constant Interfaces.C.Char_Array :=
            from_type & Interfaces.C.Nul;
        Tmp_to_type: constant Interfaces.C.Char_Array :=
            to_type & Interfaces.C.Nul;
    begin
        XtSetTypeConverter(
                Tmp_from_type(Tmp_from_type'First)'unchecked_access,
                Tmp_to_type(Tmp_to_type'First)'unchecked_access,
                converter,
                convert_args,
                num_args,
                cache_type,
                destructor);
    end XtSetTypeConverter;                                 -- Intrinsic.h:729

    procedure XtAppSetTypeConverter(
                app_context : XtAppContext;
                from_type   : Interfaces.C.Char_Array;
                to_type     : Interfaces.C.Char_Array;
                converter   : XtTypeConverter;
                convert_args: XtConvertArgList;
                num_args    : Cardinal;
                cache_type  : XtCacheType;
                destructor  : XtDestructor) is

        use type Interfaces.C.Char_Array;
        Tmp_from_type: constant Interfaces.C.Char_Array :=
            from_type & Interfaces.C.Nul;
        Tmp_to_type: constant Interfaces.C.Char_Array :=
            to_type & Interfaces.C.Nul;
    begin
        XtAppSetTypeConverter(
                app_context,
                Tmp_from_type(Tmp_from_type'First)'unchecked_access,
                Tmp_to_type(Tmp_to_type'First)'unchecked_access,
                converter,
                convert_args,
                num_args,
                cache_type,
                destructor);
    end XtAppSetTypeConverter;                              -- Intrinsic.h:741

    procedure XtConvert(
                widget   : Xt.Intrinsic.Widget;
                from_type: Interfaces.C.Char_Array;
                from     : access X.Xresource.XrmValue;
                to_type  : Interfaces.C.Char_Array;
                to_return: access X.Xresource.XrmValue) is

        use type Interfaces.C.Char_Array;
        Tmp_from_type: constant Interfaces.C.Char_Array :=
            from_type & Interfaces.C.Nul;
        Tmp_to_type: constant Interfaces.C.Char_Array :=
            to_type & Interfaces.C.Nul;
    begin
        XtConvert(
                widget,
                Tmp_from_type(Tmp_from_type'First)'unchecked_access,
                from,
                Tmp_to_type(Tmp_to_type'First)'unchecked_access,
                to_return);
    end XtConvert;                                          -- Intrinsic.h:754

    function XtParseTranslationTable(
                table: Interfaces.C.Char_Array) return XtTranslations is

        use type Interfaces.C.Char_Array;
        Tmp_table: constant Interfaces.C.Char_Array :=
            table & Interfaces.C.Nul;
    begin
        return XtParseTranslationTable(
                Tmp_table(Tmp_table'First)'unchecked_access);
    end XtParseTranslationTable;                            -- Intrinsic.h:780

    function XtParseAcceleratorTable(
                source: Interfaces.C.Char_Array) return XtAccelerators is

        use type Interfaces.C.Char_Array;
        Tmp_source: constant Interfaces.C.Char_Array :=
            source & Interfaces.C.Nul;
    begin
        return XtParseAcceleratorTable(
                Tmp_source(Tmp_source'First)'unchecked_access);
    end XtParseAcceleratorTable;                            -- Intrinsic.h:786

    procedure XtCallActionProc(
                widget    : Xt.Intrinsic.Widget;
                action    : Interfaces.C.Char_Array;
                event     : access X.Xlib.XEvent;
                params    : X.Strings.charp_vector;
                num_params: Cardinal) is

        use type Interfaces.C.Char_Array;
        Tmp_action: constant Interfaces.C.Char_Array :=
            action & Interfaces.C.Nul;
    begin
        XtCallActionProc(
                widget,
                Tmp_action(Tmp_action'First)'unchecked_access,
                event,
                params,
                num_params);
    end XtCallActionProc;                                   -- Intrinsic.h:863

    function XtNameToWidget(
                reference: Widget;
                names    : Interfaces.C.Char_Array) return Widget is

        use type Interfaces.C.Char_Array;
        Tmp_names: constant Interfaces.C.Char_Array :=
            names & Interfaces.C.Nul;
    begin
        return XtNameToWidget(
                reference,
                Tmp_names(Tmp_names'First)'unchecked_access);
    end XtNameToWidget;                                     -- Intrinsic.h:1226

    procedure XtAddCallback(
                widget       : Xt.Intrinsic.Widget;
                callback_name: Interfaces.C.Char_Array;
                callback     : XtCallbackProc;
                closure      : XtPointer) is

        use type Interfaces.C.Char_Array;
        Tmp_callback_name: constant Interfaces.C.Char_Array :=
            callback_name & Interfaces.C.Nul;
    begin
        XtAddCallback(
                widget,
                Tmp_callback_name(Tmp_callback_name'First)'unchecked_access,
                callback,
                closure);
    end XtAddCallback;                                      -- Intrinsic.h:1357

    procedure XtRemoveCallback(
                widget       : Xt.Intrinsic.Widget;
                callback_name: Interfaces.C.Char_Array;
                callback     : XtCallbackProc;
                closure      : XtPointer) is

        use type Interfaces.C.Char_Array;
        Tmp_callback_name: constant Interfaces.C.Char_Array :=
            callback_name & Interfaces.C.Nul;
    begin
        XtRemoveCallback(
                widget,
                Tmp_callback_name(Tmp_callback_name'First)'unchecked_access,
                callback,
                closure);
    end XtRemoveCallback;                                   -- Intrinsic.h:1366

    procedure XtAddCallbacks(
                widget       : Xt.Intrinsic.Widget;
                callback_name: Interfaces.C.Char_Array;
                callbacks    : XtCallbackList) is

        use type Interfaces.C.Char_Array;
        Tmp_callback_name: constant Interfaces.C.Char_Array :=
            callback_name & Interfaces.C.Nul;
    begin
        XtAddCallbacks(
                widget,
                Tmp_callback_name(Tmp_callback_name'First)'unchecked_access,
                callbacks);
    end XtAddCallbacks;                                     -- Intrinsic.h:1375

    procedure XtRemoveCallbacks(
                widget       : Xt.Intrinsic.Widget;
                callback_name: Interfaces.C.Char_Array;
                callbacks    : XtCallbackList) is

        use type Interfaces.C.Char_Array;
        Tmp_callback_name: constant Interfaces.C.Char_Array :=
            callback_name & Interfaces.C.Nul;
    begin
        XtRemoveCallbacks(
                widget,
                Tmp_callback_name(Tmp_callback_name'First)'unchecked_access,
                callbacks);
    end XtRemoveCallbacks;                                  -- Intrinsic.h:1383

    procedure XtRemoveAllCallbacks(
                widget       : Xt.Intrinsic.Widget;
                callback_name: Interfaces.C.Char_Array) is

        use type Interfaces.C.Char_Array;
        Tmp_callback_name: constant Interfaces.C.Char_Array :=
            callback_name & Interfaces.C.Nul;
    begin
        XtRemoveAllCallbacks(
                widget,
                Tmp_callback_name(Tmp_callback_name'First)'unchecked_access);
    end XtRemoveAllCallbacks;                               -- Intrinsic.h:1391

    procedure XtCallCallbacks(
                widget       : Xt.Intrinsic.Widget;
                callback_name: Interfaces.C.Char_Array;
                call_data    : XtPointer) is

        use type Interfaces.C.Char_Array;
        Tmp_callback_name: constant Interfaces.C.Char_Array :=
            callback_name & Interfaces.C.Nul;
    begin
        XtCallCallbacks(
                widget,
                Tmp_callback_name(Tmp_callback_name'First)'unchecked_access,
                call_data);
    end XtCallCallbacks;                                    -- Intrinsic.h:1399

    function XtHasCallbacks(
                widget       : Xt.Intrinsic.Widget;
                callback_name: Interfaces.C.Char_Array) return XtCallbackStatus
is

        use type Interfaces.C.Char_Array;
        Tmp_callback_name: constant Interfaces.C.Char_Array :=
            callback_name & Interfaces.C.Nul;
    begin
        return XtHasCallbacks(
                widget,
                Tmp_callback_name(Tmp_callback_name'First)'unchecked_access);
    end XtHasCallbacks;                                     -- Intrinsic.h:1415

    function XtCreatePopupShell(
                name       : Interfaces.C.Char_Array;
                widgetClass: Xt.Intrinsic.WidgetClass;
                parent     : Widget;
                args       : ArgList;
                num_args   : Cardinal) return Widget is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array :=
            name & Interfaces.C.Nul;
    begin
        return XtCreatePopupShell(
                Tmp_name(Tmp_name'First)'unchecked_access,
                widgetClass,
                parent,
                args,
                num_args);
    end XtCreatePopupShell;                                 -- Intrinsic.h:1446

    function XtVaCreatePopupShell(
                name       : Interfaces.C.Char_Array;
                widgetClass: Xt.Intrinsic.WidgetClass;
                parent     : Widget;
                Args       : Stdarg.ArgList := Stdarg.Empty) return Widget is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array :=
            name & Interfaces.C.Nul;
    begin
        return XtVaCreatePopupShell(
                Tmp_name(Tmp_name'First)'unchecked_access,
                widgetClass,
                parent,
                Args);
    end XtVaCreatePopupShell;                               -- Intrinsic.h:1456

    function XtCreateWidget(
                name        : Interfaces.C.Char_Array;
                widget_class: WidgetClass;
                parent      : Widget;
                args        : ArgList;
                num_args    : Cardinal) return Widget is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array :=
            name & Interfaces.C.Nul;
    begin
        return XtCreateWidget(
                Tmp_name(Tmp_name'First)'unchecked_access,
                widget_class,
                parent,
                args,
                num_args);
    end XtCreateWidget;                                     -- Intrinsic.h:1525

    function XtCreateManagedWidget(
                name        : Interfaces.C.Char_Array;
                widget_class: WidgetClass;
                parent      : Widget;
                args        : ArgList;
                num_args    : Cardinal) return Widget is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array :=
            name & Interfaces.C.Nul;
    begin
        return XtCreateManagedWidget(
                Tmp_name(Tmp_name'First)'unchecked_access,
                widget_class,
                parent,
                args,
                num_args);
    end XtCreateManagedWidget;                              -- Intrinsic.h:1535

    function XtVaCreateWidget(
                name        : Interfaces.C.Char_Array;
                widget_class: WidgetClass;
                parent      : Xt.Intrinsic.Widget;
                Args        : Stdarg.ArgList := Stdarg.Empty) 
               return Xt.Intrinsic.Widget is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array :=
            name & Interfaces.C.Nul;
    begin
        return XtVaCreateWidget(
                Tmp_name(Tmp_name'First)'unchecked_access,
                widget_class,
                parent,
                Args);
    end XtVaCreateWidget;                                   -- Intrinsic.h:1545

    function XtVaCreateManagedWidget(
                name        : Interfaces.C.Char_Array;
                widget_class: WidgetClass;
                parent      : Widget;
                Args        : Stdarg.ArgList := Stdarg.Empty) return Widget is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array :=
            name & Interfaces.C.Nul;
    begin
        return XtVaCreateManagedWidget(
                Tmp_name(Tmp_name'First)'unchecked_access,
                widget_class,
                parent,
                Args);
    end XtVaCreateManagedWidget;                            -- Intrinsic.h:1554

    function XtCreateApplicationShell(
                name        : Interfaces.C.Char_Array;
                widget_class: WidgetClass;
                args        : ArgList;
                num_args    : Cardinal) return Widget is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array :=
            name & Interfaces.C.Nul;
    begin
        return XtCreateApplicationShell(
                Tmp_name(Tmp_name'First)'unchecked_access,
                widget_class,
                args,
                num_args);
    end XtCreateApplicationShell;                           -- Intrinsic.h:1563

    function XtAppCreateShell(
                application_name : Interfaces.C.Char_Array;
                application_class: Interfaces.C.Char_Array;
                widget_class     : WidgetClass;
                display          : access X.Xlib.Display;
                args             : ArgList;
                num_args         : Cardinal) return Widget is

        use type Interfaces.C.Char_Array;
        Tmp_application_name: constant Interfaces.C.Char_Array :=
            application_name & Interfaces.C.Nul;
        Tmp_application_class: constant Interfaces.C.Char_Array :=
            application_class & Interfaces.C.Nul;
    begin
        return XtAppCreateShell(
                Tmp_application_name(Tmp_application_name'First)'unchecked_access,
                Tmp_application_class(Tmp_application_class'First)'unchecked_access,
                widget_class,
                display,
                args,
                num_args);
    end XtAppCreateShell;                                   -- Intrinsic.h:1572

    function XtVaAppCreateShell(
                application_name : Interfaces.C.Char_Array;
                application_class: Interfaces.C.Char_Array;
                widget_class     : WidgetClass;
                display          : access X.Xlib.Display;
                Args             : Stdarg.ArgList := Stdarg.Empty) return Widget
 is

        use type Interfaces.C.Char_Array;
        Tmp_application_name: constant Interfaces.C.Char_Array :=
            application_name & Interfaces.C.Nul;
        Tmp_application_class: constant Interfaces.C.Char_Array :=
            application_class & Interfaces.C.Nul;
    begin
        return XtVaAppCreateShell(
                Tmp_application_name(Tmp_application_name'First)'unchecked_access,
                Tmp_application_class(Tmp_application_class'First)'unchecked_access,
                widget_class,
                display,
                Args);
    end XtVaAppCreateShell;                                 -- Intrinsic.h:1583

    procedure XtDisplayInitialize(
                app_context      : XtAppContext;
                dpy              : access X.Xlib.Display;
                application_name : Interfaces.C.Char_Array;
                application_class: Interfaces.C.Char_Array;
                options          : access X.Xresource.XrmOptionDescRec;
                num_options      : Cardinal;
                argc             : access X.Strings.Natural_Int;
                argv             : X.Strings.charp_vector) is

        use type Interfaces.C.Char_Array;
        Tmp_application_name: constant Interfaces.C.Char_Array :=
            application_name & Interfaces.C.Nul;
        Tmp_application_class: constant Interfaces.C.Char_Array :=
            application_class & Interfaces.C.Nul;
    begin
        XtDisplayInitialize(
                app_context,
                dpy,
                Tmp_application_name(Tmp_application_name'First)'unchecked_access,
                Tmp_application_class(Tmp_application_class'First)'unchecked_access,
                options,
                num_options,
                argc,
                argv);
    end XtDisplayInitialize;                                -- Intrinsic.h:1613

    function XtAppInitialize(
                app_context_return: access XtAppContext;
                application_class : Interfaces.C.Char_Array;
                options           : X.Xresource.XrmOptionDescList;
                num_options       : Cardinal;
                argc_in_out       : access X.Strings.Natural_Int;
                argv_in_out       : X.Strings.charp_vector;
                fallback_resources: X.Strings.charp_vector;
                args              : ArgList;
                num_args          : Cardinal) return Widget is

        use type Interfaces.C.Char_Array;
        Tmp_application_class: constant Interfaces.C.Char_Array :=
            application_class & Interfaces.C.Nul;
    begin
        return XtAppInitialize(
                app_context_return,
                Tmp_application_class(Tmp_application_class'First)'unchecked_access,
                options,
                num_options,
                argc_in_out,
                argv_in_out,
                fallback_resources,
                args,
                num_args);
    end XtAppInitialize;                                    -- Intrinsic.h:1626

    function XtVaAppInitialize(
                app_context_return: access XtAppContext;
                application_class : Interfaces.C.Char_Array;
                options           : X.Xresource.XrmOptionDescList;
                num_options       : Cardinal;
                argc_in_out       : access X.Strings.Natural_Int;
                argv_in_out       : X.Strings.charp_vector;
                fallback_resources: X.Strings.charp_vector;
                Args              : Stdarg.ArgList := Stdarg.Empty) 
               return Widget is

        use type Interfaces.C.Char_Array;
        Tmp_application_class: constant Interfaces.C.Char_Array :=
            application_class & Interfaces.C.Nul;
    begin
        return XtVaAppInitialize(
                app_context_return,
                Tmp_application_class(Tmp_application_class'First)'unchecked_access,
                options,
                num_options,
                argc_in_out,
                argv_in_out,
                fallback_resources,
                Args);
    end XtVaAppInitialize;                                  -- Intrinsic.h:1640

    function XtInitialize(
                shell_name       : Interfaces.C.Char_Array;
                application_class: Interfaces.C.Char_Array;
                options          : access X.Xresource.XrmOptionDescRec;
                num_options      : Cardinal;
                argc             : access X.Strings.Natural_Int;
                argv             : X.Strings.charp_vector) return Widget is

        use type Interfaces.C.Char_Array;
        Tmp_shell_name: constant Interfaces.C.Char_Array :=
            shell_name & Interfaces.C.Nul;
        Tmp_application_class: constant Interfaces.C.Char_Array :=
            application_class & Interfaces.C.Nul;
    begin
        return XtInitialize(
                Tmp_shell_name(Tmp_shell_name'First)'unchecked_access,
                Tmp_application_class(Tmp_application_class'First)'unchecked_access,
                options,
                num_options,
                argc,
                argv);
    end XtInitialize;                                       -- Intrinsic.h:1653

    function XtOpenDisplay(
                app_context      : XtAppContext;
                display_string   : Interfaces.C.Char_Array;
                application_name : Interfaces.C.Char_Array;
                application_class: Interfaces.C.Char_Array;
                options          : X.Xresource.XrmOptionDescList;
                num_options      : Cardinal;
                argc             : X.Int_Access;
                argv             : X.Strings.charp_vector) 
               return X.Xlib.XDisplay_access is

        use type Interfaces.C.Char_Array;
        Tmp_display_string: constant Interfaces.C.Char_Array :=
            display_string & Interfaces.C.Nul;
        Tmp_application_name: constant Interfaces.C.Char_Array :=
            application_name & Interfaces.C.Nul;
        Tmp_application_class: constant Interfaces.C.Char_Array :=
            application_class & Interfaces.C.Nul;
    begin
        return XtOpenDisplay(
                app_context,
                Tmp_display_string(Tmp_display_string'First)'unchecked_access,
                Tmp_application_name(Tmp_application_name'First)'unchecked_access,
                Tmp_application_class(Tmp_application_class'First)'unchecked_access,
                options,
                num_options,
                argc,
                argv);
    end XtOpenDisplay;                                      -- Intrinsic.h:1664

    procedure XtGetSubresources(
                widget       : Xt.Intrinsic.Widget;
                base         : XtPointer;
                name         : Interfaces.C.Char_Array;
                class        : Interfaces.C.Char_Array;
                resources    : XtResourceList;
                num_resources: Cardinal;
                args         : ArgList;
                num_args     : Cardinal) is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array :=
            name & Interfaces.C.Nul;
        Tmp_class: constant Interfaces.C.Char_Array :=
            class & Interfaces.C.Nul;
    begin
        XtGetSubresources(
                widget,
                base,
                Tmp_name(Tmp_name'First)'unchecked_access,
                Tmp_class(Tmp_class'First)'unchecked_access,
                resources,
                num_resources,
                args,
                num_args);
    end XtGetSubresources;                                  -- Intrinsic.h:1753

    procedure XtVaGetSubresources(
                widget       : Xt.Intrinsic.Widget;
                base         : XtPointer;
                name         : Interfaces.C.Char_Array;
                class        : Interfaces.C.Char_Array;
                resources    : XtResourceList;
                num_resources: Cardinal;
                Args         : Stdarg.ArgList := Stdarg.Empty) is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array :=
            name & Interfaces.C.Nul;
        Tmp_class: constant Interfaces.C.Char_Array :=
            class & Interfaces.C.Nul;
    begin
        XtVaGetSubresources(
                widget,
                base,
                Tmp_name(Tmp_name'First)'unchecked_access,
                Tmp_class(Tmp_class'First)'unchecked_access,
                resources,
                num_resources,
                Args);
    end XtVaGetSubresources;                                -- Intrinsic.h:1766

    procedure XtAppErrorMsg(
                app_context   : XtAppContext;
                name          : Interfaces.C.Char_Array;
                c_type        : Interfaces.C.Char_Array;
                class         : Interfaces.C.Char_Array;
                default_string: Interfaces.C.Char_Array;
                params        : X.Strings.charp_vector;
                num_params    : access Cardinal) is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array :=
            name & Interfaces.C.Nul;
        Tmp_c_type: constant Interfaces.C.Char_Array :=
            c_type & Interfaces.C.Nul;
        Tmp_class: constant Interfaces.C.Char_Array :=
            class & Interfaces.C.Nul;
        Tmp_default_string: constant Interfaces.C.Char_Array :=
            default_string & Interfaces.C.Nul;
    begin
        XtAppErrorMsg(
                app_context,
                Tmp_name(Tmp_name'First)'unchecked_access,
                Tmp_c_type(Tmp_c_type'First)'unchecked_access,
                Tmp_class(Tmp_class'First)'unchecked_access,
                Tmp_default_string(Tmp_default_string'First)'unchecked_access,
                params,
                num_params);
    end XtAppErrorMsg;                                      -- Intrinsic.h:1930

    procedure XtErrorMsg(
                name          : Interfaces.C.Char_Array;
                c_type        : Interfaces.C.Char_Array;
                class         : Interfaces.C.Char_Array;
                default_string: Interfaces.C.Char_Array;
                params        : X.Strings.charp_vector;
                num_params    : access Cardinal) is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array :=
            name & Interfaces.C.Nul;
        Tmp_c_type: constant Interfaces.C.Char_Array :=
            c_type & Interfaces.C.Nul;
        Tmp_class: constant Interfaces.C.Char_Array :=
            class & Interfaces.C.Nul;
        Tmp_default_string: constant Interfaces.C.Char_Array :=
            default_string & Interfaces.C.Nul;
    begin
        XtErrorMsg(
                Tmp_name(Tmp_name'First)'unchecked_access,
                Tmp_c_type(Tmp_c_type'First)'unchecked_access,
                Tmp_class(Tmp_class'First)'unchecked_access,
                Tmp_default_string(Tmp_default_string'First)'unchecked_access,
                params,
                num_params);
    end XtErrorMsg;                                         -- Intrinsic.h:1942

    procedure XtAppWarningMsg(
                app_context   : XtAppContext;
                name          : Interfaces.C.Char_Array;
                c_type        : Interfaces.C.Char_Array;
                class         : Interfaces.C.Char_Array;
                default_string: Interfaces.C.Char_Array;
                params        : X.Strings.charp_vector;
                num_params    : access Cardinal) is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array :=
            name & Interfaces.C.Nul;
        Tmp_c_type: constant Interfaces.C.Char_Array :=
            c_type & Interfaces.C.Nul;
        Tmp_class: constant Interfaces.C.Char_Array :=
            class & Interfaces.C.Nul;
        Tmp_default_string: constant Interfaces.C.Char_Array :=
            default_string & Interfaces.C.Nul;
    begin
        XtAppWarningMsg(
                app_context,
                Tmp_name(Tmp_name'First)'unchecked_access,
                Tmp_c_type(Tmp_c_type'First)'unchecked_access,
                Tmp_class(Tmp_class'First)'unchecked_access,
                Tmp_default_string(Tmp_default_string'First)'unchecked_access,
                params,
                num_params);
    end XtAppWarningMsg;                                    -- Intrinsic.h:1953

    procedure XtWarningMsg(
                name          : Interfaces.C.Char_Array;
                c_type        : Interfaces.C.Char_Array;
                class         : Interfaces.C.Char_Array;
                default_string: Interfaces.C.Char_Array;
                params        : X.Strings.charp_vector;
                num_params    : access Cardinal) is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array :=
            name & Interfaces.C.Nul;
        Tmp_c_type: constant Interfaces.C.Char_Array :=
            c_type & Interfaces.C.Nul;
        Tmp_class: constant Interfaces.C.Char_Array :=
            class & Interfaces.C.Nul;
        Tmp_default_string: constant Interfaces.C.Char_Array :=
            default_string & Interfaces.C.Nul;
    begin
        XtWarningMsg(
                Tmp_name(Tmp_name'First)'unchecked_access,
                Tmp_c_type(Tmp_c_type'First)'unchecked_access,
                Tmp_class(Tmp_class'First)'unchecked_access,
                Tmp_default_string(Tmp_default_string'First)'unchecked_access,
                params,
                num_params);
    end XtWarningMsg;                                       -- Intrinsic.h:1965

    procedure XtAppError(
                app_context: XtAppContext;
                message    : Interfaces.C.Char_Array) is

        use type Interfaces.C.Char_Array;
        Tmp_message: constant Interfaces.C.Char_Array :=
            message & Interfaces.C.Nul;
    begin
        XtAppError(
                app_context,
                Tmp_message(Tmp_message'First)'unchecked_access);
    end XtAppError;                                         -- Intrinsic.h:2002

    procedure XtError(
                message: Interfaces.C.Char_Array) is

        use type Interfaces.C.Char_Array;
        Tmp_message: constant Interfaces.C.Char_Array :=
            message & Interfaces.C.Nul;
    begin
        XtError(
                Tmp_message(Tmp_message'First)'unchecked_access);
    end XtError;                                            -- Intrinsic.h:2009

    procedure XtAppWarning(
                app_context: XtAppContext;
                message    : Interfaces.C.Char_Array) is

        use type Interfaces.C.Char_Array;
        Tmp_message: constant Interfaces.C.Char_Array :=
            message & Interfaces.C.Nul;
    begin
        XtAppWarning(
                app_context,
                Tmp_message(Tmp_message'First)'unchecked_access);
    end XtAppWarning;                                       -- Intrinsic.h:2015

    procedure XtWarning(
                message: Interfaces.C.Char_Array) is

        use type Interfaces.C.Char_Array;
        Tmp_message: constant Interfaces.C.Char_Array :=
            message & Interfaces.C.Nul;
    begin
        XtWarning(
                Tmp_message(Tmp_message'First)'unchecked_access);
    end XtWarning;                                          -- Intrinsic.h:2022

    procedure XtAppGetErrorDatabaseText(
                app_context   : XtAppContext;
                name          : Interfaces.C.Char_Array;
                c_type        : Interfaces.C.Char_Array;
                class         : Interfaces.C.Char_Array;
                default_string: Interfaces.C.Char_Array;
                buffer_return : in out Interfaces.C.Char_Array;
                nbytes        : X.signed_int;
                database      : X.Xresource.XrmDatabase) is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array :=
            name & Interfaces.C.Nul;
        Tmp_c_type: constant Interfaces.C.Char_Array :=
            c_type & Interfaces.C.Nul;
        Tmp_class: constant Interfaces.C.Char_Array :=
            class & Interfaces.C.Nul;
        Tmp_default_string: constant Interfaces.C.Char_Array :=
            default_string & Interfaces.C.Nul;
    begin
        XtAppGetErrorDatabaseText(
                app_context,
                Tmp_name(Tmp_name'First)'unchecked_access,
                Tmp_c_type(Tmp_c_type'First)'unchecked_access,
                Tmp_class(Tmp_class'First)'unchecked_access,
                Tmp_default_string(Tmp_default_string'First)'unchecked_access,
                buffer_return(buffer_return'First)'unchecked_access,
                nbytes,
                database);
    end XtAppGetErrorDatabaseText;                          -- Intrinsic.h:2040

    procedure XtGetErrorDatabaseText(
                name          : Interfaces.C.Char_Array;
                c_type        : Interfaces.C.Char_Array;
                class         : Interfaces.C.Char_Array;
                default_string: Interfaces.C.Char_Array;
                buffer_return : in out Interfaces.C.Char_Array;
                nbytes        : X.signed_int) is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array :=
            name & Interfaces.C.Nul;
        Tmp_c_type: constant Interfaces.C.Char_Array :=
            c_type & Interfaces.C.Nul;
        Tmp_class: constant Interfaces.C.Char_Array :=
            class & Interfaces.C.Nul;
        Tmp_default_string: constant Interfaces.C.Char_Array :=
            default_string & Interfaces.C.Nul;
    begin
        XtGetErrorDatabaseText(
                Tmp_name(Tmp_name'First)'unchecked_access,
                Tmp_c_type(Tmp_c_type'First)'unchecked_access,
                Tmp_class(Tmp_class'First)'unchecked_access,
                Tmp_default_string(Tmp_default_string'First)'unchecked_access,
                buffer_return(buffer_return'First)'unchecked_access,
                nbytes);
    end XtGetErrorDatabaseText;                             -- Intrinsic.h:2053

    function XtFindFile(
                path             : Interfaces.C.Char_Array;
                substitutions    : Substitution;
                num_substitutions: Cardinal;
                predicate        : XtFilePredicate) return X.Strings.charp is

        use type Interfaces.C.Char_Array;
        Tmp_path: constant Interfaces.C.Char_Array :=
            path & Interfaces.C.Nul;
    begin
        return XtFindFile(
                Tmp_path(Tmp_path'First)'unchecked_access,
                substitutions,
                num_substitutions,
                predicate);
    end XtFindFile;                                         -- Intrinsic.h:2243

    function XtResolvePathname(
                dpy              : access X.Xlib.Display;
                c_type           : Interfaces.C.Char_Array;
                filename         : Interfaces.C.Char_Array;
                suffix           : Interfaces.C.Char_Array;
                path             : Interfaces.C.Char_Array;
                substitutions    : Substitution;
                num_substitutions: Cardinal;
                predicate        : XtFilePredicate) return X.Strings.charp is

        use type Interfaces.C.Char_Array;
        Tmp_c_type: constant Interfaces.C.Char_Array :=
            c_type & Interfaces.C.Nul;
        Tmp_filename: constant Interfaces.C.Char_Array :=
            filename & Interfaces.C.Nul;
        Tmp_suffix: constant Interfaces.C.Char_Array :=
            suffix & Interfaces.C.Nul;
        Tmp_path: constant Interfaces.C.Char_Array :=
            path & Interfaces.C.Nul;
    begin
        return XtResolvePathname(
                dpy,
                Tmp_c_type(Tmp_c_type'First)'unchecked_access,
                Tmp_filename(Tmp_filename'First)'unchecked_access,
                Tmp_suffix(Tmp_suffix'First)'unchecked_access,
                Tmp_path(Tmp_path'First)'unchecked_access,
                substitutions,
                num_substitutions,
                predicate);
    end XtResolvePathname;                                  -- Intrinsic.h:2252

end Xt.Intrinsic;

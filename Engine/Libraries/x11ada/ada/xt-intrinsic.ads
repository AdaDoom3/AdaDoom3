-- $Source: /home/harp/1/proto/monoBANK/xbind/xt-intrinsic.ads,v $ 
-- $Revision: 1.23 $ $Date: 96/10/31 12:49:25 $ $Author: mg $ 

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

with Interfaces.C;
with Stdarg;
with X;
with X.Strings;
with X.Xlib;
with X.Xresource;
with X.Xutil;

package Xt.Intrinsic is

    subtype Char_Array is Interfaces.C.Char_Array;
    Nul: Interfaces.C.Char renames Interfaces.C.Nul;
    -- use type Interfaces.C.Char_Array;                       -- to get "&"
    function "&" (S: Interfaces.C.Char_Array; C: Interfaces.C.Char)
	return Interfaces.C.Char_Array renames Interfaces.C."&";

    type EventMask is new X.unsigned_long;                  -- Intrinsic.h:306
    XtSpecificationRelease     : constant := 5;             -- Intrinsic.h:52
    FALSE                      : constant := 0;             -- Intrinsic.h:95
    TRUE                       : constant := 1;             -- Intrinsic.h:96
    XtCacheNone                : constant := 16#1#;         -- Intrinsic.h:118
    XtCacheAll                 : constant := 16#2#;         -- Intrinsic.h:119
    XtCacheByDisplay           : constant := 16#3#;         -- Intrinsic.h:120
    XtCacheRefCount            : constant := 16#100#;       -- Intrinsic.h:121
    XtCWQueryOnly              : constant := 128;           -- Intrinsic.h:229
    XtSMDontChange             : constant := 5;             -- Intrinsic.h:232
    XtInputNoneMask            : constant := 0;             -- Intrinsic.h:311
    XtInputReadMask            : constant := 1;             -- Intrinsic.h:312
    XtInputWriteMask           : constant := 2;             -- Intrinsic.h:313
    XtInputExceptMask          : constant := 4;             -- Intrinsic.h:314
    XtAllEvents                : constant EventMask := -1;  -- Intrinsic.h:963
    XtIMXEvent                 : constant := 1;             -- Intrinsic.h:1159
    XtIMTimer                  : constant := 2;             -- Intrinsic.h:1160
    XtIMAlternateInput         : constant := 4;             -- Intrinsic.h:1161
    XtIMAll                    : constant := 7;             -- Intrinsic.h:1162
    XtVaNestedList             : constant Char_Array := "XtVaNestedList" & Nul;
                                                            -- Intrinsic.h:1273
    XtVaTypedArg               : constant Char_Array := "XtVaTypedArg" & Nul;
                                                            -- Intrinsic.h:1274
    XtUnspecifiedPixmap        : constant X.Pixmap := 2;    -- Intrinsic.h:1862
    XtUnspecifiedShellInt      : constant := -1;            -- Intrinsic.h:1863
    XtUnspecifiedWindow        : constant X.Window := 2;    -- Intrinsic.h:1864
    XtUnspecifiedWindowGroup   : constant X.Window := 3;    -- Intrinsic.h:1865
    XtDefaultForeground        : constant Char_Array := "XtDefaultForeground" 
                                                        & Nul;
                                                            -- Intrinsic.h:1866
    XtDefaultBackground        : constant Char_Array := "XtDefaultBackground" 
                                                        & Nul;
                                                            -- Intrinsic.h:1867
    XtDefaultFont              : constant Char_Array := "XtDefaultFont" & Nul;
                                                            -- Intrinsic.h:1868
    XtDefaultFontSet           : constant Char_Array := "XtDefaultFontSet" 
                                                        & Nul;
                                                            -- Intrinsic.h:1869
    XT_CONVERT_FAIL            : constant X.Atom := 16#8000_0001#;
                                                            -- Intrinsic.h:2271

    type XtValueMask is new X.unsigned_long;                -- Intrinsic.h:110
    type XtIntervalId is new X.unsigned_long;               -- Intrinsic.h:111
    type XtInputId is new X.unsigned_long;                  -- Intrinsic.h:112
    type XtWorkProcId is new X.unsigned_long;               -- Intrinsic.h:113
    type XtGeometryMask is new X.unsigned_long;             -- Intrinsic.h:114
    type XtGCMask is new X.unsigned_long;                   -- Intrinsic.h:115
    type Pixel is new X.unsigned_long;                      -- Intrinsic.h:116
    type Pixel_access is access all Pixel;
    type XtCacheType is new X.signed_int;                   -- Intrinsic.h:117
    type Boolean is new X.unsigned_char;                    -- Intrinsic.h:149
    type Boolean_access is access all Boolean;
    -- type XtBoolean is new X.signed_int;                  -- Intrinsic.h:67
    subtype XtBoolean is X.signed_int;                      -- Intrinsic.h:67
    type XtArgVal is new X.long;                            -- Intrinsic.h:150
    type XtEnum is new X.unsigned_char;                     -- Intrinsic.h:151
    type Cardinal is new X.unsigned_int;                    -- Intrinsic.h:154
    type Cardinal_access is access all Cardinal;
    type Dimension is new X.unsigned_short;                 -- Intrinsic.h:155
    type Dimension_access is access all Dimension;
    type Position is new X.signed_short;                    -- Intrinsic.h:156
    type Modifiers is new X.unsigned_int;                   -- Intrinsic.h:176

    type XtAddressMode is (                                 -- Intrinsic.h:204
        XtAddress,                                          -- Intrinsic.h:197
        XtBaseOffset,                                       -- Intrinsic.h:198
        XtImmediate,                                        -- Intrinsic.h:199
        XtResourceString,                                   -- Intrinsic.h:200
        XtResourceQuark,                                    -- Intrinsic.h:201
        XtWidgetBaseOffset,                                 -- Intrinsic.h:202
        XtProcedureArg                                      -- Intrinsic.h:204
    );
    for XtAddressMode'Size use X.Int'Size;                  -- Intrinsic.h:204

    type XtListPosition is (                                -- Intrinsic.h:308
        XtListHead,                                         -- Intrinsic.h:308
        XtListTail                                          -- Intrinsic.h:308
    );
    for XtListPosition'Size use X.Int'Size;                 -- Intrinsic.h:308

    type XtInputMask is new X.unsigned_long;                -- Intrinsic.h:310

    type XtCallbackStatus is (                              -- Intrinsic.h:355
        XtCallbackNoList,                                   -- Intrinsic.h:352
        XtCallbackHasNone,                                  -- Intrinsic.h:353
        XtCallbackHasSome                                   -- Intrinsic.h:355
    );
    for XtCallbackStatus'Size use X.Int'Size;               -- Intrinsic.h:355

    type XtGeometryResult is (                              -- Intrinsic.h:362
        XtGeometryYes,                                      -- Intrinsic.h:358
        XtGeometryNo,                                       -- Intrinsic.h:359
        XtGeometryAlmost,                                   -- Intrinsic.h:360
        XtGeometryDone                                      -- Intrinsic.h:362
    );
    for XtGeometryResult'Size use X.Int'Size;               -- Intrinsic.h:362

    type XtGrabKind is (                                    -- Intrinsic.h:364
        XtGrabNone,                                         -- Intrinsic.h:364
        XtGrabNonexclusive,                                 -- Intrinsic.h:364
        XtGrabExclusive                                     -- Intrinsic.h:364
    );
    for XtGrabKind'Size use X.Int'Size;                     -- Intrinsic.h:364

    type XtPointerRec is private;
    type XtPointer is access all XtPointerRec;              -- Intrinsic.h:159
    null_XtPointer: constant XtPointer := null;
    type Opaque is new XtPointer;                           -- Intrinsic.h:166
    type XtCacheRef is new XtPointer;                       -- Intrinsic.h:264
    type XtActionHookId is new XtPointer;                   -- Intrinsic.h:266
    type XtRequestId is new XtPointer;                      -- Intrinsic.h:437

    type CompositeRec is private;                           -- Intrinsic.h:104
    type XtEventRec is private;                             -- Intrinsic.h:106
    type XtBoundAccActionRec is private;                    -- Intrinsic.h:107
    type XtAppStruct is private;                            -- Intrinsic.h:109
    type TranslationData is private;                        -- Intrinsic.h:174

    type XtActionsRec;                                      -- Intrinsic.h:105
    type XtConvertArgRec;                                   -- Intrinsic.h:210
    type Arg;                                               -- Intrinsic.h:334
    type XtCallbackRec;                                     -- Intrinsic.h:346
    type XtPopdownIDRec;                                    -- Intrinsic.h:369
    type XtResource;                                        -- Intrinsic.h:371
    type SubstitutionRec;                                   -- Intrinsic.h:429

    type Widget is private;                                 -- Intrinsic.h:101
    null_Widget: constant Widget;
    type WidgetList is access all Widget;                   -- Intrinsic.h:102
    type WidgetClass is private;                            -- Intrinsic.h:103
    type CompositeWidget is access all CompositeRec;        -- Intrinsic.h:104
    type XtActionList is access all XtActionsRec;           -- Intrinsic.h:105
    type XtEventTable is access all XtEventRec;             -- Intrinsic.h:106
    type XtBoundAccActions is access all XtBoundAccActionRec;
                                                            -- Intrinsic.h:107
    type XtAppContext is access all XtAppStruct;            -- Intrinsic.h:109
    type XtTranslations is access all TranslationData;      -- Intrinsic.h:174
    type XtAccelerators is new XtTranslations;              -- Intrinsic.h:175
    type XtConvertArgList is access all XtConvertArgRec;    -- Intrinsic.h:210
    type ArgList is access all Arg;                         -- Intrinsic.h:334
    type XtCallbackList is access all XtCallbackRec;        -- Intrinsic.h:349
    type XtPopdownID is access all XtPopdownIDRec;          -- Intrinsic.h:369
    type XtResourceList is access all XtResource;           -- Intrinsic.h:379
    type Substitution is access all SubstitutionRec;        -- Intrinsic.h:429
    type XSelectionRequestEvent_access is 
             access all X.Xlib.XSelectionRequestEvent;      -- Intrinsic.h:2335

    type XtActionProc is access procedure (
                widget    : Xt.Intrinsic.Widget;
                event     : access X.Xlib.XEvent;
                params    : X.Strings.charp_vector;
                num_params: access Cardinal);               -- Intrinsic.h:178


    type XtActionsRec is                                    -- Intrinsic.h:105
        record
            string: X.Strings.charp;                        -- Intrinsic.h:190
            proc  : XtActionProc;                           -- Intrinsic.h:191
        end record;

    type XtBoundActions is access all XtActionProc;         -- Intrinsic.h:187


    type XtConvertArgRec is                                 -- Intrinsic.h:210
        record
            address_mode: XtAddressMode;                    -- Intrinsic.h:207
            address_id  : XtPointer;                        -- Intrinsic.h:208
            size        : Cardinal;                         -- Intrinsic.h:209
        end record;


    type XtConvertArgProc is access procedure (
                widget: Xt.Intrinsic.Widget;
                size  : access Cardinal;
                value : access X.Xresource.XrmValue);       -- Intrinsic.h:212


    type XtWidgetGeometry is                                -- Intrinsic.h:226
        record
            request_mode: XtGeometryMask;                   -- Intrinsic.h:221
            xx          : Position;                         -- Intrinsic.h:222
            y           : Position;                         -- Intrinsic.h:222
            width       : Dimension;                        -- Intrinsic.h:223
            height      : Dimension;                        -- Intrinsic.h:223
            border_width: Dimension;                        -- Intrinsic.h:223
            sibling     : Widget;                           -- Intrinsic.h:224
            stack_mode  : X.signed_int;                     -- Intrinsic.h:225
        end record;

    type XtConverter is access procedure (
                args    : access X.Xresource.XrmValue;
                num_args: access Cardinal;
                from    : access X.Xresource.XrmValue;
                to      : access X.Xresource.XrmValue);     -- Intrinsic.h:234


    type XtTypeConverter is access function (
                dpy           : access X.Xlib.Display;
                args          : access X.Xresource.XrmValue;
                num_args      : access Cardinal;
                from          : access X.Xresource.XrmValue;
                to            : access X.Xresource.XrmValue;
                converter_data: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:243


    type XtDestructor is access procedure (
                app           : XtAppContext;
                to            : access X.Xresource.XrmValue;
                converter_data: XtPointer;
                args          : access X.Xresource.XrmValue;
                num_args      : access Cardinal);           -- Intrinsic.h:254


    type XtActionHookProc is access procedure (
                w          : Widget;
                client_data: XtPointer;
                action_name: X.Strings.const_charp;
                event      : access X.Xlib.XEvent;
                params     : X.Strings.charp_vector;
                num_params : access Cardinal);              -- Intrinsic.h:268


    type XtKeyProc is access procedure (
                dpy             : access X.Xlib.Display;
                keycode         : X.KeyCode;
                modifiers       : Xt.Intrinsic.Modifiers;
                modifiers_return: access Xt.Intrinsic.Modifiers;
                keysym_return   : access X.KeySym);         -- Intrinsic.h:279


    type XtCaseProc is access procedure (
                display     : access X.Xlib.Display;
                keysym      : X.KeySym;
                lower_return: access X.KeySym;
                upper_return: access X.KeySym);             -- Intrinsic.h:289


    type XtEventHandler is access procedure (
                widget              : Xt.Intrinsic.Widget;
                closure             : XtPointer;
                event               : access X.Xlib.XEvent;
                continue_to_dispatch: access Xt.Intrinsic.Boolean);
                                                            -- Intrinsic.h:298


    type XtTimerCallbackProc is access procedure (
                closure: XtPointer;
                id     : access XtIntervalId);              -- Intrinsic.h:316


    type XtInputCallbackProc is access procedure (
                closure: XtPointer;
                source : access X.int;
                id     : access XtInputId);                 -- Intrinsic.h:323


    type Arg is                                             -- Intrinsic.h:334
        record
            name : X.Strings.charp;                         -- Intrinsic.h:332
            value: XtArgVal;                                -- Intrinsic.h:333
        end record;


    type XtCallbackProc is access procedure (
                widget   : Xt.Intrinsic.Widget;
                closure  : XtPointer;
                call_data: XtPointer);                      -- Intrinsic.h:338


    type XtCallbackRec is                                   -- Intrinsic.h:346
        record
            callback: XtCallbackProc;                       -- Intrinsic.h:347
            closure : XtPointer;                            -- Intrinsic.h:348
        end record;

    type XtPopdownIDRec is                                  -- Intrinsic.h:369
        record
            shell_widget : Widget;                          -- Intrinsic.h:367
            enable_widget: Widget;                          -- Intrinsic.h:368
        end record;


    type XtResource is                                      -- Intrinsic.h:371
        record
            resource_name  : X.Strings.const_charp;         -- Intrinsic.h:372
            resource_class : X.Strings.const_charp;         -- Intrinsic.h:373
            resource_type  : X.Strings.const_charp;         -- Intrinsic.h:374
            resource_size  : Cardinal;                      -- Intrinsic.h:375
            resource_offset: Cardinal;                      -- Intrinsic.h:376
            default_type   : X.Strings.const_charp;         -- Intrinsic.h:377
            default_addr   : XtPointer;                     -- Intrinsic.h:378
        end record;

    type XtResourceDefaultProc is access procedure (
                widget: Xt.Intrinsic.Widget;
                offset: X.signed_int;
                value : access X.Xresource.XrmValue);       -- Intrinsic.h:381


    type XtLanguageProc is access function (
                dpy        : access X.Xlib.Display;
                xnl        : X.Strings.charp;
                client_data: XtPointer)
               return X.Strings.charp;                      -- Intrinsic.h:389


    type XtErrorMsgHandler is access procedure (
                name          : X.Strings.const_charp;
                c_type        : X.Strings.const_charp;
                class         : X.Strings.const_charp;
                default_string: X.Strings.const_charp;
                params        : X.Strings.charp_vector;
                num_params    : access Cardinal);           -- Intrinsic.h:397


    type XtErrorHandler is access procedure (
                msg: X.Strings.const_charp);                -- Intrinsic.h:408


    type XtCreatePopupChildProc is access procedure (
                shell: Widget);                             -- Intrinsic.h:414


    type XtWorkProc is access function (
                closure: XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:420


    type SubstitutionRec is                                 -- Intrinsic.h:429
        record
            match       : X.signed_char;                    -- Intrinsic.h:427
            substitution: X.Strings.charp;                  -- Intrinsic.h:428
        end record;


    type XtFilePredicate is access function (
                filename: X.Strings.const_charp)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:431


    type XtConvertSelectionProc is access function (
                widget       : Xt.Intrinsic.Widget;
                selection    : access X.Atom;
                target       : access X.Atom;
                type_return  : access X.Atom;
                value_return : access XtPointer;
                length_return: access X.unsigned_long;
                format_return: access X.int)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:439


    type XtLoseSelectionProc is access procedure (
                widget   : Xt.Intrinsic.Widget;
                selection: access X.Atom);                  -- Intrinsic.h:451


    type XtSelectionDoneProc is access procedure (
                widget   : Xt.Intrinsic.Widget;
                selection: access X.Atom;
                target   : access X.Atom);                  -- Intrinsic.h:458


    type XtSelectionCallbackProc is access procedure (
                widget   : Xt.Intrinsic.Widget;
                closure  : XtPointer;
                selection: access X.Atom;
                c_type   : access X.Atom;
                value    : XtPointer;
                length   : access X.unsigned_long;
                format   : access X.int);                   -- Intrinsic.h:466


    type XtLoseSelectionIncrProc is access procedure (
                widget     : Xt.Intrinsic.Widget;
                selection  : access X.Atom;
                client_data: XtPointer);                    -- Intrinsic.h:478


    type XtSelectionDoneIncrProc is access procedure (
                widget     : Xt.Intrinsic.Widget;
                selection  : access X.Atom;
                target     : access X.Atom;
                receiver_id: access XtPointer;
                client_data: XtPointer);                    -- Intrinsic.h:486


    type XtConvertSelectionIncrProc is access function (
                widget     : Xt.Intrinsic.Widget;
                selection  : access X.Atom;
                target     : access X.Atom;
                c_type     : access X.Atom;
                value      : access XtPointer;
                length     : access X.unsigned_long;
                format     : access X.int;
                max_length : access X.unsigned_long;
                client_data: XtPointer;
                receiver_id: access XtRequestId)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:496


    type XtCancelConvertSelectionProc is access procedure (
                widget     : Xt.Intrinsic.Widget;
                selection  : access X.Atom;
                target     : access X.Atom;
                receiver_id: access XtRequestId;
                client_data: XtPointer);                    -- Intrinsic.h:511
 
   pragma Convention(C, XtConvertArgRec);


    type XtConvertArgRec_array is                           -- Intrinsic.h:701
        array(integer range <>) of aliased XtConvertArgRec;

    colorConvertArgs: XtConvertArgRec_array(0..X.Anysize_Array);
                                                            -- Intrinsic.h:701
    screenConvertArg: XtConvertArgRec_array(0..X.Anysize_Array);
                                                            -- Intrinsic.h:702

    function XtConvertAndStore(
                widget   : Xt.Intrinsic.Widget;
                from_type: X.Strings.const_charp;
                from     : access X.Xresource.XrmValue;
                to_type  : X.Strings.const_charp;
                to_in_out: access X.Xresource.XrmValue)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:529

    pragma Import(C, XtConvertAndStore, "XtConvertAndStore");

    function XtConvertAndStore(
                widget   : Xt.Intrinsic.Widget;
                from_type: Interfaces.C.Char_Array;
                from     : access X.Xresource.XrmValue;
                to_type  : Interfaces.C.Char_Array;
                to_in_out: access X.Xresource.XrmValue)
               return Boolean;                              -- Intrinsic.h:529

    pragma Inline(XtConvertAndStore);

    function XtCallConverter(
                dpy             : access X.Xlib.Display;
                converter       : XtTypeConverter;
                args            : X.Xresource.XrmValuePtr;
                num_args        : Cardinal;
                from            : X.Xresource.XrmValuePtr;
                to_in_out       : access X.Xresource.XrmValue;
                cache_ref_return: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:539

    function XtDispatchEvent(
                event: access X.Xlib.XEvent)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:551

    function XtCallAcceptFocus(
                widget: Xt.Intrinsic.Widget;
                time  : access X.Time)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:557

    function XtPeekEvent(
                event_return: access X.Xlib.XEvent)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:564

    function XtAppPeekEvent(
                app_context : XtAppContext;
                event_return: access X.Xlib.XEvent)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:570

    function XtIsSubclass(
                widget      : Xt.Intrinsic.Widget;
                widget_class: WidgetClass)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:577

    function XtIsObject(
                object: Widget)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:584

    function XtCheckSubclassFlag(
                object   : Widget;
                type_flag: XtEnum)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:590

    function XtIsSubclassOf(
                object      : Widget;
                widget_class: WidgetClass;
                flag_class  : WidgetClass;
                type_flag   : XtEnum)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:597

    function XtIsManaged(
                rectobj: Widget)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:606

    function XtIsRealized(
                widget: Xt.Intrinsic.Widget)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:612

    function XtIsSensitive(
                widget: Xt.Intrinsic.Widget)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:618

    function XtOwnSelection(
                widget   : Xt.Intrinsic.Widget;
                selection: X.Atom;
                time     : X.Time;
                convert  : XtConvertSelectionProc;
                lose     : XtLoseSelectionProc;
                done     : XtSelectionDoneProc)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:624

    function XtOwnSelectionIncremental(
                widget          : Xt.Intrinsic.Widget;
                selection       : X.Atom;
                time            : X.Time;
                convert_callback: XtConvertSelectionIncrProc;
                lose_callback   : XtLoseSelectionIncrProc;
                done_callback   : XtSelectionDoneIncrProc;
                cancel_callback : XtCancelConvertSelectionProc;
                client_data     : XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:635

    function XtMakeResizeRequest(
                widget       : Xt.Intrinsic.Widget;
                width        : X.unsigned_int;
                height       : X.unsigned_int;
                width_return : access Dimension;
                height_return: access Dimension)
               return XtGeometryResult;                     -- Intrinsic.h:648

    procedure XtTranslateCoords(
                widget      : Xt.Intrinsic.Widget;
                xx          : X.signed_int;
                y           : X.signed_int;
                rootx_return: access Position;
                rooty_return: access Position);             -- Intrinsic.h:658

    type KeySym_access is access all X.KeySym;

    function XtGetKeysymTable(
                dpy                       : access X.Xlib.Display;
                min_keycode_return        : access X.KeyCode;
                keysyms_per_keycode_return: access X.int)
               return KeySym_access;                        -- Intrinsic.h:668

    type access_KeyCode is access X.KeyCode;

    procedure XtKeysymToKeycodeList(
                dpy            : access X.Xlib.Display;
                keysym         : access X.KeySym;
                keycodes_return: access access_KeyCode;
                keycount_return: access Cardinal);          -- Intrinsic.h:676

    procedure XtDisplayStringConversionWarning(
                dpy       : access X.Xlib.Display;
                from_value: X.Strings.const_charp;
                to_type   : X.Strings.const_charp);         -- Intrinsic.h:692

    pragma Import(C, XtDisplayStringConversionWarning, 
                     "XtDisplayStringConversionWarning");

    procedure XtDisplayStringConversionWarning(
                dpy       : access X.Xlib.Display;
                from_value: Interfaces.C.Char_Array;
                to_type   : Interfaces.C.Char_Array);       -- Intrinsic.h:692

    pragma Inline(XtDisplayStringConversionWarning);

    procedure XtAppAddConverter(
                app_context : XtAppContext;
                from_type   : X.Strings.const_charp;
                to_type     : X.Strings.const_charp;
                converter   : XtConverter;
                convert_args: XtConvertArgList;
                num_args    : Cardinal);                    -- Intrinsic.h:708

    pragma Import(C, XtAppAddConverter, "XtAppAddConverter");

    procedure XtAppAddConverter(
                app_context : XtAppContext;
                from_type   : Interfaces.C.Char_Array;
                to_type     : Interfaces.C.Char_Array;
                converter   : XtConverter;
                convert_args: XtConvertArgList;
                num_args    : Cardinal);                    -- Intrinsic.h:708

    pragma Inline(XtAppAddConverter);

    procedure XtAddConverter(
                from_type   : X.Strings.const_charp;
                to_type     : X.Strings.const_charp;
                converter   : XtConverter;
                convert_args: XtConvertArgList;
                num_args    : Cardinal);                    -- Intrinsic.h:719

    pragma Import(C, XtAddConverter, "XtAddConverter");

    procedure XtAddConverter(
                from_type   : Interfaces.C.Char_Array;
                to_type     : Interfaces.C.Char_Array;
                converter   : XtConverter;
                convert_args: XtConvertArgList;
                num_args    : Cardinal);                    -- Intrinsic.h:719

    pragma Inline(XtAddConverter);

    procedure XtSetTypeConverter(
                from_type   : X.Strings.const_charp;
                to_type     : X.Strings.const_charp;
                converter   : XtTypeConverter;
                convert_args: XtConvertArgList;
                num_args    : Cardinal;
                cache_type  : XtCacheType;
                destructor  : XtDestructor);                -- Intrinsic.h:729

    pragma Import(C, XtSetTypeConverter, "XtSetTypeConverter");

    procedure XtSetTypeConverter(
                from_type   : Interfaces.C.Char_Array;
                to_type     : Interfaces.C.Char_Array;
                converter   : XtTypeConverter;
                convert_args: XtConvertArgList;
                num_args    : Cardinal;
                cache_type  : XtCacheType;
                destructor  : XtDestructor);                -- Intrinsic.h:729

    pragma Inline(XtSetTypeConverter);

    procedure XtAppSetTypeConverter(
                app_context : XtAppContext;
                from_type   : X.Strings.const_charp;
                to_type     : X.Strings.const_charp;
                converter   : XtTypeConverter;
                convert_args: XtConvertArgList;
                num_args    : Cardinal;
                cache_type  : XtCacheType;
                destructor  : XtDestructor);                -- Intrinsic.h:741

    pragma Import(C, XtAppSetTypeConverter, "XtAppSetTypeConverter");

    procedure XtAppSetTypeConverter(
                app_context : XtAppContext;
                from_type   : Interfaces.C.Char_Array;
                to_type     : Interfaces.C.Char_Array;
                converter   : XtTypeConverter;
                convert_args: XtConvertArgList;
                num_args    : Cardinal;
                cache_type  : XtCacheType;
                destructor  : XtDestructor);                -- Intrinsic.h:741

    pragma Inline(XtAppSetTypeConverter);

    procedure XtConvert(
                widget   : Xt.Intrinsic.Widget;
                from_type: X.Strings.const_charp;
                from     : access X.Xresource.XrmValue;
                to_type  : X.Strings.const_charp;
                to_return: access X.Xresource.XrmValue);    -- Intrinsic.h:754

    pragma Import(C, XtConvert, "XtConvert");

    procedure XtConvert(
                widget   : Xt.Intrinsic.Widget;
                from_type: Interfaces.C.Char_Array;
                from     : access X.Xresource.XrmValue;
                to_type  : Interfaces.C.Char_Array;
                to_return: access X.Xresource.XrmValue);
                                                            -- Intrinsic.h:754

    pragma Inline(XtConvert);

    procedure XtDirectConvert(
                converter: XtConverter;
                args     : X.Xresource.XrmValuePtr;
                num_args : Cardinal;
                from     : X.Xresource.XrmValuePtr;
                to_return: access X.Xresource.XrmValue);    -- Intrinsic.h:764

    function XtParseTranslationTable(
                table: X.Strings.const_charp)
               return XtTranslations;                       -- Intrinsic.h:780

    pragma Import(C, XtParseTranslationTable, "XtParseTranslationTable");

    function XtParseTranslationTable(
                table: Interfaces.C.Char_Array)
               return XtTranslations;                       -- Intrinsic.h:780

    pragma Inline(XtParseTranslationTable);

    function XtParseAcceleratorTable(
                source: X.Strings.const_charp)
               return XtAccelerators;                       -- Intrinsic.h:786

    pragma Import(C, XtParseAcceleratorTable, "XtParseAcceleratorTable");

    function XtParseAcceleratorTable(
                source: Interfaces.C.Char_Array)
               return XtAccelerators;                       -- Intrinsic.h:786

    pragma Inline(XtParseAcceleratorTable);

    procedure XtOverrideTranslations(
                widget      : Xt.Intrinsic.Widget;
                translations: XtTranslations);              -- Intrinsic.h:792

    procedure XtAugmentTranslations(
                widget      : Xt.Intrinsic.Widget;
                translations: XtTranslations);              -- Intrinsic.h:799

    procedure XtInstallAccelerators(
                destination: Widget;
                source     : Widget);                       -- Intrinsic.h:806

    procedure XtInstallAllAccelerators(
                destination: Widget;
                source     : Widget);                       -- Intrinsic.h:813

    procedure XtUninstallTranslations(
                widget: Xt.Intrinsic.Widget);               -- Intrinsic.h:820

    procedure XtAppAddActions(
                app_context: XtAppContext;
                actions    : XtActionList;
                num_actions: Cardinal);                     -- Intrinsic.h:826

    procedure XtAddActions(
                actions    : XtActionList;
                num_actions: Cardinal);                     -- Intrinsic.h:834

    function XtAppAddActionHook(
                app_context: XtAppContext;
                proc       : XtActionHookProc;
                client_data: XtPointer)
               return XtActionHookId;                       -- Intrinsic.h:841

    procedure XtRemoveActionHook(
                id: XtActionHookId);                        -- Intrinsic.h:849

    procedure XtGetActionList(
                widget_class      : WidgetClass;
                actions_return    : access XtActionList;
                num_actions_return: access Cardinal);       -- Intrinsic.h:855

    procedure XtCallActionProc(
                widget    : Xt.Intrinsic.Widget;
                action    : X.Strings.const_charp;
                event     : access X.Xlib.XEvent;
                params    : X.Strings.charp_vector;
                num_params: Cardinal);                      -- Intrinsic.h:863

    pragma Import(C, XtCallActionProc, "XtCallActionProc");

    procedure XtCallActionProc(
                widget    : Xt.Intrinsic.Widget;
                action    : Interfaces.C.Char_Array;
                event     : access X.Xlib.XEvent;
                params    : X.Strings.charp_vector;
                num_params: Cardinal);                      -- Intrinsic.h:863

    pragma Inline(XtCallActionProc);

    procedure XtRegisterGrabAction(
                action_proc  : XtActionProc;
                owner_events : Xt.Intrinsic.Boolean;
                event_mask   : X.unsigned_int;
                pointer_mode : X.signed_int;
                keyboard_mode: X.signed_int);               -- Intrinsic.h:873

    procedure XtSetMultiClickTime(
                dpy         : access X.Xlib.Display;
                milliseconds: X.signed_int);                -- Intrinsic.h:883

    function XtGetMultiClickTime(
                dpy: access X.Xlib.Display)
               return X.signed_int;                         -- Intrinsic.h:890

    function XtGetActionKeysym(
                event           : access X.Xlib.XEvent;
                modifiers_return: access Modifiers)
               return X.KeySym;                             -- Intrinsic.h:896

    procedure XtTranslateKeycode(
                dpy             : access X.Xlib.Display;
                keycode         : X.KeyCode;
                modifiers       : Xt.Intrinsic.Modifiers;
                modifiers_return: access Xt.Intrinsic.Modifiers;
                keysym_return   : access X.KeySym);         -- Intrinsic.h:909

    procedure XtTranslateKey(
                dpy             : access X.Xlib.Display;
                keycode         : X.KeyCode;
                modifiers       : Xt.Intrinsic.Modifiers;
                modifiers_return: access Xt.Intrinsic.Modifiers;
                keysym_return   : access X.KeySym);         -- Intrinsic.h:919

    procedure XtSetKeyTranslator(
                dpy : access X.Xlib.Display;
                proc: XtKeyProc);                           -- Intrinsic.h:929

    procedure XtRegisterCaseConverter(
                dpy  : access X.Xlib.Display;
                proc : XtCaseProc;
                start: X.KeySym;
                stop : X.KeySym);                           -- Intrinsic.h:936

    procedure XtConvertCase(
                dpy         : access X.Xlib.Display;
                keysym      : X.KeySym;
                lower_return: access X.KeySym;
                upper_return: access X.KeySym);             -- Intrinsic.h:945

    procedure XtAddEventHandler(
                widget     : Xt.Intrinsic.Widget;
                event_mask : EventMask;
                nonmaskable: XtBoolean;
                proc       : XtEventHandler;
                closure    : XtPointer);                    -- Intrinsic.h:965

    procedure XtRemoveEventHandler(
                widget     : Xt.Intrinsic.Widget;
                event_mask : EventMask;
                nonmaskable: XtBoolean;
                proc       : XtEventHandler;
                closure    : XtPointer);                    -- Intrinsic.h:975

    procedure XtAddRawEventHandler(
                widget     : Xt.Intrinsic.Widget;
                event_mask : EventMask;
                nonmaskable: XtBoolean;
                proc       : XtEventHandler;
                closure    : XtPointer);                    -- Intrinsic.h:985

    procedure XtRemoveRawEventHandler(
                widget     : Xt.Intrinsic.Widget;
                event_mask : EventMask;
                nonmaskable: XtBoolean;
                proc       : XtEventHandler;
                closure    : XtPointer);                    -- Intrinsic.h:995

    procedure XtInsertEventHandler(
                widget     : Xt.Intrinsic.Widget;
                event_mask : EventMask;
                nonmaskable: XtBoolean;
                proc       : XtEventHandler;
                closure    : XtPointer;
                position   : XtListPosition);               -- Intrinsic.h:1005

    procedure XtInsertRawEventHandler(
                widget     : Xt.Intrinsic.Widget;
                event_mask : EventMask;
                nonmaskable: X.signed_int;
                proc       : XtEventHandler;
                closure    : XtPointer;
                position   : XtListPosition);               -- Intrinsic.h:1016

    function XtBuildEventMask(
                widget: Xt.Intrinsic.Widget)
               return EventMask;                            -- Intrinsic.h:1027

    procedure XtAddGrab(
                widget       : Xt.Intrinsic.Widget;
                exclusive    : XtBoolean;
                spring_loaded: XtBoolean);                  -- Intrinsic.h:1033

    procedure XtRemoveGrab(
                widget: Xt.Intrinsic.Widget);               -- Intrinsic.h:1041

    procedure XtProcessEvent(
                mask: XtInputMask);                         -- Intrinsic.h:1047

    procedure XtAppProcessEvent(
                app_context: XtAppContext;
                mask       : XtInputMask);                  -- Intrinsic.h:1053

    procedure XtMainLoop;                                   -- Intrinsic.h:1060

    procedure XtAppMainLoop(
                app_context: XtAppContext);                 -- Intrinsic.h:1066

    procedure XtAddExposureToRegion(
                event : access X.Xlib.XEvent;
                region: X.Xutil.Region);                    -- Intrinsic.h:1072

    procedure XtSetKeyboardFocus(
                subtree   : Widget;
                descendent: Widget);                        -- Intrinsic.h:1079

    function XtLastTimestampProcessed(
                dpy: access X.Xlib.Display)
               return X.Time;                               -- Intrinsic.h:1086

    function XtAddTimeOut(
                interval: X.unsigned_int;
                proc    : XtTimerCallbackProc;
                closure : XtPointer)
               return XtIntervalId;                         -- Intrinsic.h:1098

    function XtAppAddTimeOut(
                app_context: XtAppContext;
                interval   : X.unsigned_int;
                proc       : XtTimerCallbackProc;
                closure    : XtPointer)
               return XtIntervalId;                         -- Intrinsic.h:1106

    procedure XtRemoveTimeOut(
                timer: XtIntervalId);                       -- Intrinsic.h:1115

    function XtAddInput(
                source   : X.signed_int;
                condition: XtPointer;
                proc     : XtInputCallbackProc;
                closure  : XtPointer)
               return XtInputId;                            -- Intrinsic.h:1121

    function XtAppAddInput(
                app_context: XtAppContext;
                source     : X.signed_int;
                condition  : XtPointer;
                proc       : XtInputCallbackProc;
                closure    : XtPointer)
               return XtInputId;                            -- Intrinsic.h:1130

    procedure XtRemoveInput(
                id: XtInputId);                             -- Intrinsic.h:1140

    procedure XtNextEvent(
                event: access X.Xlib.XEvent);               -- Intrinsic.h:1146

    procedure XtAppNextEvent(
                app_context : XtAppContext;
                event_return: access X.Xlib.XEvent);        -- Intrinsic.h:1152

    function XtPending return XtInputMask;                  -- Intrinsic.h:1164

    function XtAppPending(
                app_context: XtAppContext)
               return XtInputMask;                          -- Intrinsic.h:1170

    procedure XtRealizeWidget(
                widget: Xt.Intrinsic.Widget);               -- Intrinsic.h:1194

    procedure XtUnrealizeWidget(
                widget: Xt.Intrinsic.Widget);               -- Intrinsic.h:1200

    procedure XtDestroyWidget(
                widget: Xt.Intrinsic.Widget);               -- Intrinsic.h:1206

    procedure XtSetSensitive(
                widget   : Xt.Intrinsic.Widget;
                sensitive: XtBoolean);                      -- Intrinsic.h:1212

    procedure XtSetMappedWhenManaged(
                widget             : Xt.Intrinsic.Widget;
                mapped_when_managed: XtBoolean);            -- Intrinsic.h:1219

    function XtNameToWidget(
                reference: Widget;
                names    :X.Strings.const_charp)
               return Widget;                               -- Intrinsic.h:1226

    pragma Import(C, XtNameToWidget, "XtNameToWidget");

    function XtNameToWidget(
                reference: Widget;
                names    : Interfaces.C.Char_Array)
               return Widget;                               -- Intrinsic.h:1226

    pragma Inline(XtNameToWidget);

    function XtWindowToWidget(
                display: access X.Xlib.Display;
                window : X.Window)
               return Widget;                               -- Intrinsic.h:1233

    function XtMergeArgLists(
                args1    : ArgList;
                num_args1: Cardinal;
                args2    : ArgList;
                num_args2: Cardinal)
               return ArgList;                              -- Intrinsic.h:1257

    -- ------------------------------------------------------------------------
    -- use the Ada Stdarg package instead:
    -- 
    -- type XtVarArgsList is new XtPointer;               -- Intrinsic.h:336
    -- function XtVaCreateArgsList(
    --             unused: XtPointer)
    --            return XtVarArgsList;                   -- Intrinsic.h:1276
    -- ------------------------------------------------------------------------

    function XtDisplay(
                widget: Xt.Intrinsic.Widget)
               return X.Xlib.XDisplay_access;               -- Intrinsic.h:1292

    function XtDisplayOfObject(
                object: Widget)
               return X.Xlib.XDisplay_access;               -- Intrinsic.h:1298

    function XtScreen(
                widget: Xt.Intrinsic.Widget)
               return X.Xlib.Screen_access;                 -- Intrinsic.h:1304

    function XtScreenOfObject(
                object: Widget)
               return X.Xlib.Screen_access;                 -- Intrinsic.h:1310

    function XtWindow(
                widget: Xt.Intrinsic.Widget)
               return X.Window;                             -- Intrinsic.h:1316

    function XtWindowOfObject(
                object: Widget)
               return X.Window;                             -- Intrinsic.h:1322

    function XtName(
                object: Widget)
               return X.Strings.charp;                      -- Intrinsic.h:1328

    function XtSuperclass(
                object: Widget)
               return WidgetClass;                          -- Intrinsic.h:1334

    function XtClass(
                object: Widget)
               return WidgetClass;                          -- Intrinsic.h:1340

    function XtParent(
                widget: Xt.Intrinsic.Widget)
               return Xt.Intrinsic.Widget;                  -- Intrinsic.h:1346

    procedure XtAddCallback(
                widget       : Xt.Intrinsic.Widget;
                callback_name: X.Strings.const_charp;
                callback     : XtCallbackProc;
                closure      : XtPointer);                  -- Intrinsic.h:1357

    pragma Import(C, XtAddCallback, "XtAddCallback");

    procedure XtAddCallback(
                widget       : Xt.Intrinsic.Widget;
                callback_name: Interfaces.C.Char_Array;
                callback     : XtCallbackProc;
                closure      : XtPointer);                  -- Intrinsic.h:1357

    pragma Inline(XtAddCallback);

    procedure XtRemoveCallback(
                widget       : Xt.Intrinsic.Widget;
                callback_name: X.Strings.const_charp;
                callback     : XtCallbackProc;
                closure      : XtPointer);                  -- Intrinsic.h:1366

    pragma Import(C, XtRemoveCallback, "XtRemoveCallback");

    procedure XtRemoveCallback(
                widget       : Xt.Intrinsic.Widget;
                callback_name: Interfaces.C.Char_Array;
                callback     : XtCallbackProc;
                closure      : XtPointer);                  -- Intrinsic.h:1366

    pragma Inline(XtRemoveCallback);

    procedure XtAddCallbacks(
                widget       : Xt.Intrinsic.Widget;
                callback_name: X.Strings.const_charp;
                callbacks    : XtCallbackList);             -- Intrinsic.h:1375

    pragma Import(C, XtAddCallbacks, "XtAddCallbacks");

    procedure XtAddCallbacks(
                widget       : Xt.Intrinsic.Widget;
                callback_name: Interfaces.C.Char_Array;
                callbacks    : XtCallbackList);             -- Intrinsic.h:1375

    pragma Inline(XtAddCallbacks);

    procedure XtRemoveCallbacks(
                widget       : Xt.Intrinsic.Widget;
                callback_name: X.Strings.const_charp;
                callbacks    : XtCallbackList);             -- Intrinsic.h:1383

    pragma Import(C, XtRemoveCallbacks, "XtRemoveCallbacks");

    procedure XtRemoveCallbacks(
                widget       : Xt.Intrinsic.Widget;
                callback_name: Interfaces.C.Char_Array;
                callbacks    : XtCallbackList);             -- Intrinsic.h:1383

    pragma Inline(XtRemoveCallbacks);

    procedure XtRemoveAllCallbacks(
                widget       : Xt.Intrinsic.Widget;
                callback_name: X.Strings.const_charp);      -- Intrinsic.h:1391

    pragma Import(C, XtRemoveAllCallbacks, "XtRemoveAllCallbacks");

    procedure XtRemoveAllCallbacks(
                widget       : Xt.Intrinsic.Widget;
                callback_name: Interfaces.C.Char_Array);    -- Intrinsic.h:1391

    pragma Inline(XtRemoveAllCallbacks);

    procedure XtCallCallbacks(
                widget       : Xt.Intrinsic.Widget;
                callback_name: X.Strings.const_charp;
                call_data    : XtPointer);                  -- Intrinsic.h:1399

    pragma Import(C, XtCallCallbacks, "XtCallCallbacks");

    procedure XtCallCallbacks(
                widget       : Xt.Intrinsic.Widget;
                callback_name: Interfaces.C.Char_Array;
                call_data    : XtPointer);                  -- Intrinsic.h:1399

    pragma Inline(XtCallCallbacks);

    procedure XtCallCallbackList(
                widget   : Xt.Intrinsic.Widget;
                callbacks: XtCallbackList;
                call_data: XtPointer);                      -- Intrinsic.h:1407

    function XtHasCallbacks(
                widget       : Xt.Intrinsic.Widget;
                callback_name: X.Strings.const_charp)
               return XtCallbackStatus;                     -- Intrinsic.h:1415

    pragma Import(C, XtHasCallbacks, "XtHasCallbacks");

    function XtHasCallbacks(
                widget       : Xt.Intrinsic.Widget;
                callback_name: Interfaces.C.Char_Array)
               return XtCallbackStatus;                     -- Intrinsic.h:1415

    pragma Inline(XtHasCallbacks);

    function XtMakeGeometryRequest(
                widget      : Xt.Intrinsic.Widget;
                request     : access XtWidgetGeometry;
                reply_return: access XtWidgetGeometry)
               return XtGeometryResult;                     -- Intrinsic.h:1430

    function XtQueryGeometry(
                widget          : Xt.Intrinsic.Widget;
                intended        : access XtWidgetGeometry;
                preferred_return: access XtWidgetGeometry)
               return XtGeometryResult;                     -- Intrinsic.h:1438

    function XtCreatePopupShell(
                name        : X.Strings.const_charp;
                widget_class: WidgetClass;
                parent      : Widget;
                args        : ArgList;
                num_args    : Cardinal)
               return Widget;                               -- Intrinsic.h:1446

    pragma Import(C, XtCreatePopupShell, "XtCreatePopupShell");

    function XtCreatePopupShell(
                name       : Interfaces.C.Char_Array;
                widgetClass: Xt.Intrinsic.WidgetClass;
                parent     : Widget;
                args       : ArgList;
                num_args   : Cardinal)
               return Widget;                               -- Intrinsic.h:1446

    pragma Inline(XtCreatePopupShell);

    function XtVaCreatePopupShell(
                name        : X.Strings.const_charp;
                widget_class: WidgetClass;
                parent      : Widget;
                args        : Stdarg.ArgList := Stdarg.Empty)
               return Widget;                               -- Intrinsic.h:1456

    function XtVaCreatePopupShell(
                name       : Interfaces.C.Char_Array;
                widgetClass: Xt.Intrinsic.WidgetClass;
                parent     : Widget;
                Args       : Stdarg.ArgList := Stdarg.Empty)
               return Widget;                               -- Intrinsic.h:1456

    pragma Inline(XtVaCreatePopupShell);

    procedure XtPopup(
                popup_shell: Widget;
                grab_kind  : XtGrabKind);                   -- Intrinsic.h:1465

    procedure XtPopupSpringLoaded(
                popup_shell: Widget);                       -- Intrinsic.h:1472

    procedure XtCallbackNone(
                widget   : Xt.Intrinsic.Widget;
                closure  : XtPointer;
                call_data: XtPointer);                      -- Intrinsic.h:1478

    procedure XtCallbackNonexclusive(
                widget   : Xt.Intrinsic.Widget;
                closure  : XtPointer;
                call_data: XtPointer);                      -- Intrinsic.h:1486

    procedure XtCallbackExclusive(
                widget   : Xt.Intrinsic.Widget;
                closure  : XtPointer;
                call_data: XtPointer);                      -- Intrinsic.h:1494

    procedure XtPopdown(
                popup_shell: Widget);                       -- Intrinsic.h:1502

    procedure XtCallbackPopdown(
                widget   : Xt.Intrinsic.Widget;
                closure  : XtPointer;
                call_data: XtPointer);                      -- Intrinsic.h:1508

    procedure XtMenuPopupAction(
                widget    : Xt.Intrinsic.Widget;
                event     : access X.Xlib.XEvent;
                params    : X.Strings.charp_vector;
                num_params: access Cardinal);               -- Intrinsic.h:1516

    function XtCreateWidget(
                name        : X.Strings.const_charp;
                widget_class: WidgetClass;
                parent      : Widget;
                args        : ArgList;
                num_args    : Cardinal)
               return Widget;                               -- Intrinsic.h:1525

    pragma Import(C, XtCreateWidget, "XtCreateWidget");

    function XtCreateWidget(
                name        : Interfaces.C.Char_Array;
                widget_class: WidgetClass;
                parent      : Widget;
                args        : ArgList;
                num_args    : Cardinal)
               return Widget;                               -- Intrinsic.h:1525

    pragma Inline(XtCreateWidget);

    function XtCreateManagedWidget(
                name        : X.Strings.const_charp;
                widget_class: WidgetClass;
                parent      : Widget;
                args        : ArgList;
                num_args    : Cardinal)
               return Widget;                               -- Intrinsic.h:1535

    pragma Import(C, XtCreateManagedWidget, "XtCreateManagedWidget");

    function XtCreateManagedWidget(
                name        : Interfaces.C.Char_Array;
                widget_class: WidgetClass;
                parent      : Widget;
                args        : ArgList;
                num_args    : Cardinal)
               return Widget;                               -- Intrinsic.h:1535

    pragma Inline(XtCreateManagedWidget);

    function XtVaCreateWidget(
                name        : X.Strings.const_charp;
                widget_class: WidgetClass;
                parent      : Xt.Intrinsic.Widget;
                args        : Stdarg.ArgList := Stdarg.Empty)
               return Xt.Intrinsic.Widget;                  -- Intrinsic.h:1545

    function XtVaCreateWidget(
                name        : Interfaces.C.Char_Array;
                widget_class: WidgetClass;
                parent      : Xt.Intrinsic.Widget;
                Args        : Stdarg.ArgList := Stdarg.Empty)
               return Xt.Intrinsic.Widget;                  -- Intrinsic.h:1545

    pragma Inline(XtVaCreateWidget);

    function XtVaCreateManagedWidget(
                name        : X.Strings.const_charp;
                widget_class: WidgetClass;
                parent      : Widget;
                args        : Stdarg.ArgList := Stdarg.Empty)
               return Widget;                               -- Intrinsic.h:1554

    function XtVaCreateManagedWidget(
                name        : Interfaces.C.Char_Array;
                widget_class: WidgetClass;
                parent      : Widget;
                Args        : Stdarg.ArgList := Stdarg.Empty) return Widget;
                                                            -- Intrinsic.h:1554

    pragma Inline(XtVaCreateManagedWidget);

    function XtCreateApplicationShell(
                name        : X.Strings.const_charp;
                widget_class: WidgetClass;
                args        : ArgList;
                num_args    : Cardinal)
               return Widget;                               -- Intrinsic.h:1563

    pragma Import(C, XtCreateApplicationShell, "XtCreateApplicationShell");

    function XtCreateApplicationShell(
                name        : Interfaces.C.Char_Array;
                widget_class: WidgetClass;
                args        : ArgList;
                num_args    : Cardinal)
               return Widget;                               -- Intrinsic.h:1563

    pragma Inline(XtCreateApplicationShell);

    function XtAppCreateShell(
                application_name : X.Strings.const_charp;
                application_class: X.Strings.const_charp;
                widget_class     : WidgetClass;
                display          : access X.Xlib.Display;
                args             : ArgList;
                num_args         : Cardinal)
               return Widget;                               -- Intrinsic.h:1572

    pragma Import(C, XtAppCreateShell, "XtAppCreateShell");

    function XtAppCreateShell(
                application_name : Interfaces.C.Char_Array;
                application_class: Interfaces.C.Char_Array;
                widget_class     : WidgetClass;
                display          : access X.Xlib.Display;
                args             : ArgList;
                num_args         : Cardinal)
               return Widget;                               -- Intrinsic.h:1572

    pragma Inline(XtAppCreateShell);

    function XtVaAppCreateShell(
                application_name : X.Strings.const_charp;
                application_class: X.Strings.const_charp;
                widget_class     : WidgetClass;
                display          : access X.Xlib.Display;
                args             : Stdarg.ArgList := Stdarg.Empty)
               return Widget;                               -- Intrinsic.h:1583

    function XtVaAppCreateShell(
                application_name : Interfaces.C.Char_Array;
                application_class: Interfaces.C.Char_Array;
                widget_class     : WidgetClass;
                display          : access X.Xlib.Display;
                Args             : Stdarg.ArgList := Stdarg.Empty)
               return Widget;                               -- Intrinsic.h:1583

    pragma Inline(XtVaAppCreateShell);

    procedure XtToolkitInitialize;                          -- Intrinsic.h:1599

    function XtSetLanguageProc(
                app_context: XtAppContext;
                proc       : XtLanguageProc;
                client_data: XtPointer)
               return XtLanguageProc;                       -- Intrinsic.h:1605
    pragma Import(C, XtSetLanguageProc, "XtSetLanguageProc");
                                                            -- Intrinsic.h:1605

    -- so function result can be ignored:
    procedure XtSetLanguageProc(
                app_context: XtAppContext;
                proc       : XtLanguageProc;
                client_data: XtPointer);                    -- Intrinsic.h:1605
    pragma Import(C, XtSetLanguageProc, "XtSetLanguageProc");
                                                            -- Intrinsic.h:1605

    procedure XtDisplayInitialize(
                app_context      : XtAppContext;
                dpy              : access X.Xlib.Display;
                application_name : X.Strings.const_charp;
                application_class: X.Strings.const_charp;
                options          : access X.Xresource.XrmOptionDescRec;
                num_options      : Cardinal;
                argc             : access X.Strings.Natural_Int;
                argv             : X.Strings.charp_vector); -- Intrinsic.h:1613

    pragma Import(C, XtDisplayInitialize, "XtDisplayInitialize");

    procedure XtDisplayInitialize(
                app_context      : XtAppContext;
                dpy              : access X.Xlib.Display;
                application_name : Interfaces.C.Char_Array;
                application_class: Interfaces.C.Char_Array;
                options          : access X.Xresource.XrmOptionDescRec;
                num_options      : Cardinal;
                argc             : access X.Strings.Natural_Int;
                argv             : X.Strings.charp_vector); -- Intrinsic.h:1613

    pragma Inline(XtDisplayInitialize);

    function XtAppInitialize(
                app_context_return: access XtAppContext;
                application_class : X.Strings.const_charp;
                options           : X.Xresource.XrmOptionDescList;
                num_options       : Cardinal;
                argc_in_out       : access X.Strings.Natural_Int;
                argv_in_out       : X.Strings.charp_vector;
                fallback_resources: X.Strings.charp_vector;
                args              : ArgList;
                num_args          : Cardinal)
               return Widget;                               -- Intrinsic.h:1626

    pragma Import(C, XtAppInitialize, "XtAppInitialize");

    function XtAppInitialize(
                app_context_return: access XtAppContext;
                application_class : Interfaces.C.Char_Array;
                options           : X.Xresource.XrmOptionDescList;
                num_options       : Cardinal;
                argc_in_out       : access X.Strings.Natural_Int;
                argv_in_out       : X.Strings.charp_vector;
                fallback_resources: X.Strings.charp_vector;
                args              : ArgList;
                num_args          : Cardinal)
               return Widget;                               -- Intrinsic.h:1626

    pragma Inline(XtAppInitialize);

    function XtVaAppInitialize(
                app_context_return: access XtAppContext;
                application_class : X.Strings.const_charp;
                options           : X.Xresource.XrmOptionDescList;
                num_options       : Cardinal;
                argc_in_out       : access X.Strings.Natural_Int;
                argv_in_out       : X.Strings.charp_vector;
                fallback_resources: X.Strings.charp_vector;
                args              : Stdarg.ArgList := Stdarg.Empty)
               return Widget;                               -- Intrinsic.h:1640

    function XtVaAppInitialize(
                app_context_return: access XtAppContext;
                application_class : Interfaces.C.Char_Array;
                options           : X.Xresource.XrmOptionDescList;
                num_options       : Cardinal;
                argc_in_out       : access X.Strings.Natural_Int;
                argv_in_out       : X.Strings.charp_vector;
                fallback_resources: X.Strings.charp_vector;
                Args              : Stdarg.ArgList := Stdarg.Empty)
               return Widget;                               -- Intrinsic.h:1640

    pragma Inline(XtVaAppInitialize);

    function XtInitialize(
                shell_name       : X.Strings.const_charp;
                application_class: X.Strings.const_charp;
                options          : access X.Xresource.XrmOptionDescRec;
                num_options      : Cardinal;
                argc             : access X.Strings.Natural_Int;
                argv             : X.Strings.charp_vector)
               return Widget;                               -- Intrinsic.h:1653

    pragma Import(C, XtInitialize, "XtInitialize");

    function XtInitialize(
                shell_name       : Interfaces.C.Char_Array;
                application_class: Interfaces.C.Char_Array;
                options          : access X.Xresource.XrmOptionDescRec;
                num_options      : Cardinal;
                argc             : access X.Strings.Natural_Int;
                argv             : X.Strings.charp_vector)
               return Widget;                               -- Intrinsic.h:1653

    pragma Inline(XtInitialize);

    function XtOpenDisplay(
                app_context      : XtAppContext;
                display_string   : X.Strings.const_charp;
                application_name : X.Strings.const_charp;
                application_class: X.Strings.const_charp;
                options          : X.Xresource.XrmOptionDescList;
                num_options      : Cardinal;
                argc             : X.Int_Access;
                argv             : X.Strings.charp_vector)
               return X.Xlib.XDisplay_access;               -- Intrinsic.h:1664

    pragma Import(C, XtOpenDisplay, "XtOpenDisplay");

    function XtOpenDisplay(
                app_context      : XtAppContext;
                display_string   : Interfaces.C.Char_Array;
                application_name : Interfaces.C.Char_Array;
                application_class: Interfaces.C.Char_Array;
                options          : X.Xresource.XrmOptionDescList;
                num_options      : Cardinal;
                argc             : X.Int_Access;
                argv             : X.Strings.charp_vector)
               return X.Xlib.XDisplay_access;               -- Intrinsic.h:1664

    pragma Inline(XtOpenDisplay);

    function XtCreateApplicationContext return XtAppContext;-- Intrinsic.h:1677

    procedure XtAppSetFallbackResources(
                app_context       : XtAppContext;
                specification_list: X.Strings.charp_vector);-- Intrinsic.h:1683

    procedure XtDestroyApplicationContext(
                app_context: XtAppContext);                 -- Intrinsic.h:1690

    procedure XtInitializeWidgetClass(
                widget_class: WidgetClass);                 -- Intrinsic.h:1696

    function XtWidgetToApplicationContext(
                widget: Xt.Intrinsic.Widget)
               return XtAppContext;                         -- Intrinsic.h:1702

    function XtDisplayToApplicationContext(
                dpy: access X.Xlib.Display)
               return XtAppContext;                         -- Intrinsic.h:1708

    function XtDatabase(
                dpy: access X.Xlib.Display)
               return X.Xresource.XrmDatabase;              -- Intrinsic.h:1714

    function XtScreenDatabase(
                screen: access X.Xlib.Screen)
               return X.Xresource.XrmDatabase;              -- Intrinsic.h:1720

    procedure XtCloseDisplay(
                dpy: access X.Xlib.Display);                -- Intrinsic.h:1726

    procedure XtGetApplicationResources(
                widget       : Xt.Intrinsic.Widget;
                base         : XtPointer;
                resources    : XtResourceList;
                num_resources: Cardinal;
                args         : ArgList;
                num_args     : Cardinal);                   -- Intrinsic.h:1732

    procedure XtVaGetApplicationResources(
                widget       : Xt.Intrinsic.Widget;
                base         : XtPointer;
                resources    : XtResourceList;
                num_resources: Cardinal;
                args         : Stdarg.ArgList := Stdarg.Empty);
                                                            -- Intrinsic.h:1743

    procedure XtGetSubresources(
                widget       : Xt.Intrinsic.Widget;
                base         : XtPointer;
                name         : X.Strings.const_charp;
                class        : X.Strings.const_charp;
                resources    : XtResourceList;
                num_resources: Cardinal;
                args         : ArgList;
                num_args     : Cardinal);                   -- Intrinsic.h:1753

    pragma Import(C, XtGetSubresources, "XtGetSubresources");

    procedure XtGetSubresources(
                widget       : Xt.Intrinsic.Widget;
                base         : XtPointer;
                name         : Interfaces.C.Char_Array;
                class        : Interfaces.C.Char_Array;
                resources    : XtResourceList;
                num_resources: Cardinal;
                args         : ArgList;
                num_args     : Cardinal);                   -- Intrinsic.h:1753

    pragma Inline(XtGetSubresources);

    procedure XtVaGetSubresources(
                widget       : Xt.Intrinsic.Widget;
                base         : XtPointer;
                name         : X.Strings.const_charp;
                class        : X.Strings.const_charp;
                resources    : XtResourceList;
                num_resources: Cardinal;
                args         : Stdarg.ArgList := Stdarg.Empty);
                                                            -- Intrinsic.h:1766

    procedure XtVaGetSubresources(
                widget       : Xt.Intrinsic.Widget;
                base         : XtPointer;
                name         : Interfaces.C.Char_Array;
                class        : Interfaces.C.Char_Array;
                resources    : XtResourceList;
                num_resources: Cardinal;
                Args         : Stdarg.ArgList := Stdarg.Empty);
                                                            -- Intrinsic.h:1766

    pragma Inline(XtVaGetSubresources);

    procedure XtSetValues(
                widget  : Xt.Intrinsic.Widget;
                args    : ArgList;
                num_args: Cardinal);                        -- Intrinsic.h:1778

    procedure XtVaSetValues(
                widget: Xt.Intrinsic.Widget;
                args  : Stdarg.ArgList := Stdarg.Empty);    -- Intrinsic.h:1786

    procedure XtGetValues(
                widget  : Xt.Intrinsic.Widget;
                args    : ArgList;
                num_args: Cardinal);                        -- Intrinsic.h:1793

    procedure XtVaGetValues(
                widget: Xt.Intrinsic.Widget;
                args  : Stdarg.ArgList := Stdarg.Empty);    -- Intrinsic.h:1801

    procedure XtSetSubvalues(
                base         : XtPointer;
                resources    : XtResourceList;
                num_resources: Cardinal;
                args         : ArgList;
                num_args     : Cardinal);                   -- Intrinsic.h:1808

    procedure XtVaSetSubvalues(
                base         : XtPointer;
                resources    : XtResourceList;
                num_resources: Cardinal;
                args         : Stdarg.ArgList := Stdarg.Empty);
                                                            -- Intrinsic.h:1818

    procedure XtGetSubvalues(
                base         : XtPointer;
                resources    : XtResourceList;
                num_resources: Cardinal;
                args         : ArgList;
                num_args     : Cardinal);                   -- Intrinsic.h:1827

    procedure XtVaGetSubvalues(
                base         : XtPointer;
                resources    : XtResourceList;
                num_resources: Cardinal;
                args         : Stdarg.ArgList := Stdarg.Empty);
                                                            -- Intrinsic.h:1837

    procedure XtGetResourceList(
                widget_class        : WidgetClass;
                resources_return    : access XtResourceList;
                num_resources_return: access Cardinal);     -- Intrinsic.h:1846

    procedure XtGetConstraintResourceList(
                widget_class        : WidgetClass;
                resources_return    : access XtResourceList;
                num_resources_return: access Cardinal);     -- Intrinsic.h:1854

    function XtAppSetErrorMsgHandler(
                app_context: XtAppContext;
                handler    : XtErrorMsgHandler)
               return XtErrorMsgHandler;                    -- Intrinsic.h:1904

    procedure XtSetErrorMsgHandler(
                handler: XtErrorMsgHandler);                -- Intrinsic.h:1911

    function XtAppSetWarningMsgHandler(
                app_context: XtAppContext;
                handler    : XtErrorMsgHandler)
               return XtErrorMsgHandler;                    -- Intrinsic.h:1917

    procedure XtSetWarningMsgHandler(
                handler: XtErrorMsgHandler);                -- Intrinsic.h:1924

    procedure XtAppErrorMsg(
                app_context   : XtAppContext;
                name          : X.Strings.const_charp;
                c_type        : X.Strings.const_charp;
                class         : X.Strings.const_charp;
                default_string: X.Strings.const_charp;
                params        : X.Strings.charp_vector;
                num_params    : access Cardinal);           -- Intrinsic.h:1930

    pragma Import(C, XtAppErrorMsg, "XtAppErrorMsg");

    procedure XtAppErrorMsg(
                app_context   : XtAppContext;
                name          : Interfaces.C.Char_Array;
                c_type        : Interfaces.C.Char_Array;
                class         : Interfaces.C.Char_Array;
                default_string: Interfaces.C.Char_Array;
                params        : X.Strings.charp_vector;
                num_params    : access Cardinal);           -- Intrinsic.h:1930

    pragma Inline(XtAppErrorMsg);

    procedure XtErrorMsg(
                name          : X.Strings.const_charp;
                c_type        : X.Strings.const_charp;
                class         : X.Strings.const_charp;
                default_string: X.Strings.const_charp;
                params        : X.Strings.charp_vector;
                num_params    : access Cardinal);           -- Intrinsic.h:1942

    pragma Import(C, XtErrorMsg, "XtErrorMsg");

    procedure XtErrorMsg(
                name          : Interfaces.C.Char_Array;
                c_type        : Interfaces.C.Char_Array;
                class         : Interfaces.C.Char_Array;
                default_string: Interfaces.C.Char_Array;
                params        : X.Strings.charp_vector;
                num_params    : access Cardinal);           -- Intrinsic.h:1942

    pragma Inline(XtErrorMsg);

    procedure XtAppWarningMsg(
                app_context   : XtAppContext;
                name          : X.Strings.const_charp;
                c_type        : X.Strings.const_charp;
                class         : X.Strings.const_charp;
                default_string: X.Strings.const_charp;
                params        : X.Strings.charp_vector;
                num_params    : access Cardinal);           -- Intrinsic.h:1953

    pragma Import(C, XtAppWarningMsg, "XtAppWarningMsg");

    procedure XtAppWarningMsg(
                app_context   : XtAppContext;
                name          : Interfaces.C.Char_Array;
                c_type        : Interfaces.C.Char_Array;
                class         : Interfaces.C.Char_Array;
                default_string: Interfaces.C.Char_Array;
                params        : X.Strings.charp_vector;
                num_params    : access Cardinal);           -- Intrinsic.h:1953

    pragma Inline(XtAppWarningMsg);

    procedure XtWarningMsg(
                name          : X.Strings.const_charp;
                c_type        : X.Strings.const_charp;
                class         : X.Strings.const_charp;
                default_string: X.Strings.const_charp;
                params        : X.Strings.charp_vector;
                num_params    : access Cardinal);           -- Intrinsic.h:1965

    pragma Import(C, XtWarningMsg, "XtWarningMsg");

    procedure XtWarningMsg(
                name          : Interfaces.C.Char_Array;
                c_type        : Interfaces.C.Char_Array;
                class         : Interfaces.C.Char_Array;
                default_string: Interfaces.C.Char_Array;
                params        : X.Strings.charp_vector;
                num_params    : access Cardinal);           -- Intrinsic.h:1965

    pragma Inline(XtWarningMsg);

    function XtAppSetErrorHandler(
                app_context: XtAppContext;
                handler    : XtErrorHandler)
               return XtErrorHandler;                       -- Intrinsic.h:1976

    procedure XtSetErrorHandler(
                handler: XtErrorHandler);                   -- Intrinsic.h:1983

    function XtAppSetWarningHandler(
                app_context: XtAppContext;
                handler    : XtErrorHandler)
               return XtErrorHandler;                       -- Intrinsic.h:1989

    procedure XtSetWarningHandler(
                handler: XtErrorHandler);                   -- Intrinsic.h:1996

    procedure XtAppError(
                app_context: XtAppContext;
                message    : X.Strings.const_charp);        -- Intrinsic.h:2002

    pragma Import(C, XtAppError, "XtAppError");

    procedure XtAppError(
                app_context: XtAppContext;
                message    : Interfaces.C.Char_Array);      -- Intrinsic.h:2002

    pragma Inline(XtAppError);

    procedure XtError(
                message: X.Strings.const_charp);            -- Intrinsic.h:2009

    pragma Import(C, XtError, "XtError");

    procedure XtError(
                message: Interfaces.C.Char_Array);          -- Intrinsic.h:2009

    pragma Inline(XtError);

    procedure XtAppWarning(
                app_context: XtAppContext;
                message    : X.Strings.const_charp);        -- Intrinsic.h:2015

    pragma Import(C, XtAppWarning, "XtAppWarning");

    procedure XtAppWarning(
                app_context: XtAppContext;
                message    : Interfaces.C.Char_Array);      -- Intrinsic.h:2015

    pragma Inline(XtAppWarning);

    procedure XtWarning(
                message: X.Strings.const_charp);            -- Intrinsic.h:2022

    pragma Import(C, XtWarning, "XtWarning");

    procedure XtWarning(
                message: Interfaces.C.Char_Array);          -- Intrinsic.h:2022

    pragma Inline(XtWarning);

    type XrmDatabase_access is access all X.Xresource.XrmDatabase;

    function XtAppGetErrorDatabase(
                app_context: XtAppContext)
               return XrmDatabase_access;                   -- Intrinsic.h:2028

    function XtGetErrorDatabase return XrmDatabase_access;  -- Intrinsic.h:2034

    procedure XtAppGetErrorDatabaseText(
                app_context   : XtAppContext;
                name          : X.Strings.const_charp;
                c_type        : X.Strings.const_charp;
                class         : X.Strings.const_charp;
                default_string: X.Strings.const_charp;
                buffer_return : X.Strings.charp;
                nbytes        : X.signed_int;
                database      : X.Xresource.XrmDatabase);   -- Intrinsic.h:2040

    pragma Import(C, XtAppGetErrorDatabaseText, "XtAppGetErrorDatabaseText");

    procedure XtAppGetErrorDatabaseText(
                app_context   : XtAppContext;
                name          : Interfaces.C.Char_Array;
                c_type        : Interfaces.C.Char_Array;
                class         : Interfaces.C.Char_Array;
                default_string: Interfaces.C.Char_Array;
                buffer_return : in out Interfaces.C.Char_Array;
                nbytes        : X.signed_int;
                database      : X.Xresource.XrmDatabase);   -- Intrinsic.h:2040

    pragma Inline(XtAppGetErrorDatabaseText);

    procedure XtGetErrorDatabaseText(                       -- Intrinsic.h:2053
                name          : X.Strings.const_charp;
                c_type        : X.Strings.const_charp;
                class         : X.Strings.const_charp;
                default_string: X.Strings.const_charp;
                buffer_return : X.Strings.charp;
                nbytes        : X.signed_int);              -- Intrinsic.h:2053

    pragma Import(C, XtGetErrorDatabaseText, "XtGetErrorDatabaseText");

    procedure XtGetErrorDatabaseText(
                name          : Interfaces.C.Char_Array;
                c_type        : Interfaces.C.Char_Array;
                class         : Interfaces.C.Char_Array;
                default_string: Interfaces.C.Char_Array;
                buffer_return : in out Interfaces.C.Char_Array;
                nbytes        : X.signed_int);              -- Intrinsic.h:2053

    pragma Inline(XtGetErrorDatabaseText);

    function XtMalloc(
                size: Cardinal)
               return X.Strings.charp;                      -- Intrinsic.h:2070

    function XtCalloc(
                num : Cardinal;
                size: Cardinal)
               return X.Strings.charp;                      -- Intrinsic.h:2076

    function XtRealloc(
                ptr: X.Strings.charp;
                num: Cardinal)
               return X.Strings.charp;                      -- Intrinsic.h:2083

    procedure XtFree(
                ptr: X.Strings.charp);                      -- Intrinsic.h:2090

    function XtAddWorkProc(
                proc   : XtWorkProc;
                closure: XtPointer)
               return XtWorkProcId;                         -- Intrinsic.h:2147

    function XtAppAddWorkProc(
                app_context: XtAppContext;
                proc       : XtWorkProc;
                closure    : XtPointer)
               return XtWorkProcId;                         -- Intrinsic.h:2154

    procedure XtRemoveWorkProc(
                id: XtWorkProcId);                          -- Intrinsic.h:2162

    function XtGetGC(
                widget   : Xt.Intrinsic.Widget;
                valueMask: XtGCMask;
                values   : access X.Xlib.XGCValues)
               return X.Xlib.GC;                            -- Intrinsic.h:2174

    function XtAllocateGC(
                widget     : Xt.Intrinsic.Widget;
                depth      : Cardinal;
                valueMask  : XtGCMask;
                values     : access X.Xlib.XGCValues;
                dynamicMask: XtGCMask;
                unusedMask : XtGCMask)
               return X.Xlib.GC;                            -- Intrinsic.h:2182

    procedure XtDestroyGC(
                gc: X.Xlib.GC);                             -- Intrinsic.h:2197

    procedure XtReleaseGC(
                object: Widget;
                gc    : X.Xlib.GC);                         -- Intrinsic.h:2203

    procedure XtAppReleaseCacheRefs(
                app_context: XtAppContext;
                cache_ref  : access XtCacheRef);            -- Intrinsic.h:2212

    procedure XtCallbackReleaseCacheRef(
                widget   : Xt.Intrinsic.Widget;
                closure  : XtPointer;
                call_data: XtPointer);                      -- Intrinsic.h:2219

    procedure XtCallbackReleaseCacheRefList(
                widget   : Xt.Intrinsic.Widget;
                closure  : XtPointer;
                call_data: XtPointer);                      -- Intrinsic.h:2227

    procedure XtSetWMColormapWindows(
                widget: Xt.Intrinsic.Widget;
                list  : access Xt.Intrinsic.Widget;
                count : Cardinal);                          -- Intrinsic.h:2235

    function XtFindFile(
                path             : X.Strings.const_charp;
                substitutions    : Substitution;
                num_substitutions: Cardinal;
                predicate        : XtFilePredicate)
               return X.Strings.charp;                      -- Intrinsic.h:2243

    pragma Import(C, XtFindFile, "XtFindFile");

    function XtFindFile(
                path             : Interfaces.C.Char_Array;
                substitutions    : Substitution;
                num_substitutions: Cardinal;
                predicate        : XtFilePredicate)
               return X.Strings.charp;                      -- Intrinsic.h:2243

    pragma Inline(XtFindFile);

    function XtResolvePathname(
                dpy              : access X.Xlib.Display;
                c_type           : X.Strings.const_charp;
                filename         : X.Strings.const_charp;
                suffix           : X.Strings.const_charp;
                path             : X.Strings.const_charp;
                substitutions    : Substitution;
                num_substitutions: Cardinal;
                predicate        : XtFilePredicate)
               return X.Strings.charp;                      -- Intrinsic.h:2252

    pragma Import(C, XtResolvePathname, "XtResolvePathname");

    function XtResolvePathname(
                dpy              : access X.Xlib.Display;
                c_type           : Interfaces.C.Char_Array;
                filename         : Interfaces.C.Char_Array;
                suffix           : Interfaces.C.Char_Array;
                path             : Interfaces.C.Char_Array;
                substitutions    : Substitution;
                num_substitutions: Cardinal;
                predicate        : XtFilePredicate)
               return X.Strings.charp;                      -- Intrinsic.h:2252

    pragma Inline(XtResolvePathname);

    procedure XtDisownSelection(
                widget   : Xt.Intrinsic.Widget;
                selection: X.Atom;
                time     : X.Time);                         -- Intrinsic.h:2273

    procedure XtGetSelectionValue(
                widget   : Xt.Intrinsic.Widget;
                selection: X.Atom;
                target   : X.Atom;
                callback : XtSelectionCallbackProc;
                closure  : XtPointer;
                time     : X.Time);                         -- Intrinsic.h:2281

    procedure XtGetSelectionValues(
                widget   : Xt.Intrinsic.Widget;
                selection: X.Atom;
                targets  : access X.Atom;
                count    : X.signed_int;
                callback : XtSelectionCallbackProc;
                closures : access XtPointer;
                time     : X.Time);                         -- Intrinsic.h:2292

    procedure XtAppSetSelectionTimeout(
                app_context: XtAppContext;
                timeout    : X.unsigned_int);               -- Intrinsic.h:2304

    procedure XtSetSelectionTimeout(
                timeout: X.unsigned_int);                   -- Intrinsic.h:2311

    function XtAppGetSelectionTimeout(
                app_context: XtAppContext)
               return X.unsigned_int;                       -- Intrinsic.h:2317

    function XtGetSelectionTimeout return X.unsigned_int;   -- Intrinsic.h:2323

    function XtGetSelectionRequest(
                widget    : Xt.Intrinsic.Widget;
                selection : X.Atom;
                request_id: XtRequestId)
               return XSelectionRequestEvent_access;        -- Intrinsic.h:2329

    procedure XtGetSelectionValueIncremental(
                widget            : Xt.Intrinsic.Widget;
                selection         : X.Atom;
                target            : X.Atom;
                selection_callback: XtSelectionCallbackProc;
                client_data       : XtPointer;
                time              : X.Time);                -- Intrinsic.h:2337

    procedure XtGetSelectionValuesIncremental(
                widget     : Xt.Intrinsic.Widget;
                selection  : X.Atom;
                targets    : access X.Atom;
                count      : X.signed_int;
                callback   : XtSelectionCallbackProc;
                client_data: access XtPointer;
                time       : X.Time);                       -- Intrinsic.h:2348

    procedure XtGrabKey(
                widget       : Xt.Intrinsic.Widget;
                keycode      : X.KeyCode;
                modifiers    : Xt.Intrinsic.Modifiers;
                owner_events : XtBoolean;
                pointer_mode : X.signed_int;
                keyboard_mode: X.signed_int);               -- Intrinsic.h:2360

    procedure XtUngrabKey(
                widget   : Xt.Intrinsic.Widget;
                keycode  : X.KeyCode;
                modifiers: Xt.Intrinsic.Modifiers);         -- Intrinsic.h:2371

    function XtGrabKeyboard(
                widget       : Xt.Intrinsic.Widget;
                owner_events : XtBoolean;
                pointer_mode : X.signed_int;
                keyboard_mode: X.signed_int;
                time         : X.Time)
               return X.signed_int;                         -- Intrinsic.h:2379

    procedure XtUngrabKeyboard(
                widget: Xt.Intrinsic.Widget;
                time  : X.Time);                            -- Intrinsic.h:2389

    procedure XtGrabButton(
                widget       : Xt.Intrinsic.Widget;
                button       : X.signed_int;
                modifiers    : Xt.Intrinsic.Modifiers;
                owner_events : XtBoolean;
                event_mask   : X.unsigned_int;
                pointer_mode : X.signed_int;
                keyboard_mode: X.signed_int;
                confine_to   : X.Window;
                cursor       : X.Cursor);                   -- Intrinsic.h:2396

    procedure XtUngrabButton(
                widget   : Xt.Intrinsic.Widget;
                button   : X.unsigned_int;
                modifiers: Xt.Intrinsic.Modifiers);         -- Intrinsic.h:2410

    function XtGrabPointer(
                widget       : Xt.Intrinsic.Widget;
                owner_events : XtBoolean;
                event_mask   : X.unsigned_int;
                pointer_mode : X.signed_int;
                keyboard_mode: X.signed_int;
                confine_to   : X.Window;
                cursor       : X.Cursor;
                time         : X.Time)
               return X.signed_int;                         -- Intrinsic.h:2418

    procedure XtUngrabPointer(
                widget: Xt.Intrinsic.Widget;
                time  : X.Time);                            -- Intrinsic.h:2431

    procedure XtGetApplicationNameAndClass(
                dpy         : access X.Xlib.Display;
                name_return : access X.Strings.charp;
                class_return: access X.Strings.charp);      -- Intrinsic.h:2438

    function XtCvtStringToAcceleratorTable(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2454

    function XtCvtStringToAtom(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2465

    function XtCvtStringToBoolean(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2476

    function XtCvtStringToBool(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2487

    function XtCvtStringToCursor(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2498

    function XtCvtStringToDimension(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2509

    function XtCvtStringToDisplay(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2520

    function XtCvtStringToFile(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2531

    function XtCvtStringToFloat(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2542

    function XtCvtStringToFont(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2553

    function XtCvtStringToFontSet(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2564

    function XtCvtStringToFontStruct(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2575

    function XtCvtStringToInt(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2586

    function XtCvtStringToInitialState(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2597

    function XtCvtStringToPixel(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2608

    function XtCvtStringToShort(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2621

    function XtCvtStringToPosition(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean
               renames XtCvtStringToShort;                  -- Intrinsic.h:2621

    function XtCvtStringToTranslationTable(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2632

    function XtCvtStringToUnsignedChar(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2643

    function XtCvtStringToVisual(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2654

    function XtCvtIntToBoolean(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2667

    function XtCvtIntToBool(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2678

    function XtCvtIntToColor(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2689

    function XtCvtPixelToColor(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean
               renames XtCvtIntToColor;                     -- Intrinsic.h:2689

    function XtCvtIntToFloat(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2702

    function XtCvtIntToFont(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2713

    function XtCvtIntToPixel(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2724

    function XtCvtIntToPixmap(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2735

    function XtCvtIntToShort(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2748

    function XtCvtIntToPosition(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean
               renames XtCvtIntToShort;                     -- Intrinsic.h:2748

    function XtCvtIntToDimension(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean
               renames XtCvtIntToShort;                     -- Intrinsic.h:2748

    function XtCvtIntToUnsignedChar(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2759

    function XtCvtColorToPixel(
                dpy        : access X.Xlib.Display;
                args       : X.Xresource.XrmValuePtr;
                num_args   : access Cardinal;
                fromVal    : X.Xresource.XrmValuePtr;
                toVal      : X.Xresource.XrmValuePtr;
                closure_ret: access XtPointer)
               return Xt.Intrinsic.Boolean;                 -- Intrinsic.h:2772

    -- ******************
    -- Intrinsic.h macros
    -- ******************

    -- -----------------------------------------------------------------
    -- Instead of the XtNumber macro, in Ada use Array'Length
    --
    -- Instead of the XtOffset and XtOffsetOf macros, 
    -- in Ada use Record.Component'Position.
    -- 
    -- Instead of the XtNew macro, use Ada predefined "new" or XtMalloc.
    -- -----------------------------------------------------------------

    function XtIsRectObj (W : Widget) return Xt.Intrinsic.Boolean;
                                                            -- Intrinsic.h:1182

    function XtIsWidget (W : Widget) return Xt.Intrinsic.Boolean;
                                                            -- Intrinsic.h:1183

    function XtIsComposite (W : Widget) return Xt.Intrinsic.Boolean;
                                                            -- Intrinsic.h:1184

    function XtIsConstraint (W : Widget) return Xt.Intrinsic.Boolean;
                                                            -- Intrinsic.h:1185

    function XtIsShell (W : Widget) return Xt.Intrinsic.Boolean;
                                                            -- Intrinsic.h:1186

    function XtIsOverrideShell (W : Widget) return Xt.Intrinsic.Boolean;
                                                            -- Intrinsic.h:1187

    function XtIsWMShell (W : Widget) return Xt.Intrinsic.Boolean;
                                                            -- Intrinsic.h:1190

    function XtIsVendorShell (W : Widget) return Xt.Intrinsic.Boolean;
                                                            -- Intrinsic.h:1191

    function XtIsTransientShell (W : Widget) return Xt.Intrinsic.Boolean;
                                                            -- Intrinsic.h:1194

    function XtIsTopLevelShell (W : Widget) return Xt.Intrinsic.Boolean;
                                                            -- Intrinsic.h:1197

    function XtIsApplicationShell (W : Widget) return Xt.Intrinsic.Boolean;
                                                            -- Intrinsic.h:1198

    procedure XtSetArg (A    : out Arg; 
                        Name : in  X.Strings.const_charp;
                        Value: in  XtArgVal);               -- Intrinsic.h:1255

    procedure XtMapWidget (W: Widget);
                                                            -- Intrinsic.h:1354

    procedure XtUnmapWidget (W: Widget);
                                                            -- Intrinsic.h:1355

    function XtNewString(Str: X.Strings.const_charp) return X.Strings.charp;
                                                            -- Intrinsic.h:2138

private
    type XtPointerRec is null record;                       -- Intrinsic.h:159

    type TranslationData is null record;                    -- Intrinsic.h:174
    type XtAppStruct is null record;                        -- Intrinsic.h:109
    type XtBoundAccActionRec is null record;                -- Intrinsic.h:107
    type XtEventRec is null record;                         -- Intrinsic.h:106
    type CompositeRec is null record;                       -- Intrinsic.h:104

    type WidgetClassRec;                                    -- Intrinsic.h:103
    type WidgetClass is access all WidgetClassRec;          -- Intrinsic.h:103
    type WidgetRec;                                         -- Intrinsic.h:101
    type Widget is access all WidgetRec;                    -- Intrinsic.h:101
    null_Widget: constant Widget := null;

    pragma Import(C, colorConvertArgs, "colorConvertArgs"); -- Intrinsic.h:701
    pragma Import(C, screenConvertArg, "screenConvertArg"); -- Intrinsic.h:702
    pragma Import(C, XtCallConverter, "XtCallConverter");   -- Intrinsic.h:539
    pragma Import(C, XtDispatchEvent, "XtDispatchEvent");   -- Intrinsic.h:551
    pragma Import(C, XtCallAcceptFocus, "XtCallAcceptFocus");
                                                            -- Intrinsic.h:557
    pragma Import(C, XtPeekEvent, "XtPeekEvent");           -- Intrinsic.h:564
    pragma Import(C, XtAppPeekEvent, "XtAppPeekEvent");     -- Intrinsic.h:570
    pragma Import(C, XtIsSubclass, "XtIsSubclass");         -- Intrinsic.h:577
    pragma Import(C, XtIsObject, "XtIsObject");             -- Intrinsic.h:584
    pragma Import(C, XtCheckSubclassFlag, "_XtCheckSubclassFlag");
                                                            -- Intrinsic.h:590
    pragma Import(C, XtIsSubclassOf, "_XtIsSubclassOf");    -- Intrinsic.h:597
    pragma Import(C, XtIsManaged, "XtIsManaged");           -- Intrinsic.h:606
    pragma Import(C, XtIsRealized, "XtIsRealized");         -- Intrinsic.h:612
    pragma Import(C, XtIsSensitive, "XtIsSensitive");       -- Intrinsic.h:618
    pragma Import(C, XtOwnSelection, "XtOwnSelection");     -- Intrinsic.h:624
    pragma Import(C, XtOwnSelectionIncremental, "XtOwnSelectionIncremental");
                                                            -- Intrinsic.h:635
    pragma Import(C, XtMakeResizeRequest, "XtMakeResizeRequest");
                                                            -- Intrinsic.h:648
    pragma Import(C, XtTranslateCoords, "XtTranslateCoords");
                                                            -- Intrinsic.h:658
    pragma Import(C, XtGetKeysymTable, "XtGetKeysymTable"); -- Intrinsic.h:668
    pragma Import(C, XtKeysymToKeycodeList, "XtKeysymToKeycodeList");
                                                            -- Intrinsic.h:676
    pragma Import(C, XtDirectConvert, "XtDirectConvert");   -- Intrinsic.h:764
    pragma Import(C, XtOverrideTranslations, "XtOverrideTranslations");
                                                            -- Intrinsic.h:792
    pragma Import(C, XtAugmentTranslations, "XtAugmentTranslations");
                                                            -- Intrinsic.h:799
    pragma Import(C, XtInstallAccelerators, "XtInstallAccelerators");
                                                            -- Intrinsic.h:806
    pragma Import(C, XtInstallAllAccelerators, "XtInstallAllAccelerators");
                                                            -- Intrinsic.h:813
    pragma Import(C, XtUninstallTranslations, "XtUninstallTranslations");
                                                            -- Intrinsic.h:820
    pragma Import(C, XtAppAddActions, "XtAppAddActions");   -- Intrinsic.h:826
    pragma Import(C, XtAddActions, "XtAddActions");         -- Intrinsic.h:834
    pragma Import(C, XtAppAddActionHook, "XtAppAddActionHook");
                                                            -- Intrinsic.h:841
    pragma Import(C, XtRemoveActionHook, "XtRemoveActionHook");
                                                            -- Intrinsic.h:849
    pragma Import(C, XtGetActionList, "XtGetActionList");   -- Intrinsic.h:855
    pragma Import(C, XtRegisterGrabAction, "XtRegisterGrabAction");
                                                            -- Intrinsic.h:873
    pragma Import(C, XtSetMultiClickTime, "XtSetMultiClickTime");
                                                            -- Intrinsic.h:883
    pragma Import(C, XtGetMultiClickTime, "XtGetMultiClickTime");
                                                            -- Intrinsic.h:890
    pragma Import(C, XtGetActionKeysym, "XtGetActionKeysym");
                                                            -- Intrinsic.h:896
    pragma Import(C, XtTranslateKeycode, "XtTranslateKeycode");
                                                            -- Intrinsic.h:909
    pragma Import(C, XtTranslateKey, "XtTranslateKey");     -- Intrinsic.h:919
    pragma Import(C, XtSetKeyTranslator, "XtSetKeyTranslator");
                                                            -- Intrinsic.h:929
    pragma Import(C, XtRegisterCaseConverter, "XtRegisterCaseConverter");
                                                            -- Intrinsic.h:936
    pragma Import(C, XtConvertCase, "XtConvertCase");       -- Intrinsic.h:945
    pragma Import(C, XtAddEventHandler, "XtAddEventHandler");
                                                            -- Intrinsic.h:965
    pragma Import(C, XtRemoveEventHandler, "XtRemoveEventHandler");
                                                            -- Intrinsic.h:975
    pragma Import(C, XtAddRawEventHandler, "XtAddRawEventHandler");
                                                            -- Intrinsic.h:985
    pragma Import(C, XtRemoveRawEventHandler, "XtRemoveRawEventHandler");
                                                            -- Intrinsic.h:995
    pragma Import(C, XtInsertEventHandler, "XtInsertEventHandler");
                                                            -- Intrinsic.h:1005
    pragma Import(C, XtInsertRawEventHandler, "XtInsertRawEventHandler");
                                                            -- Intrinsic.h:1016
    pragma Import(C, XtBuildEventMask, "XtBuildEventMask"); -- Intrinsic.h:1027
    pragma Import(C, XtAddGrab, "XtAddGrab");               -- Intrinsic.h:1033
    pragma Import(C, XtRemoveGrab, "XtRemoveGrab");         -- Intrinsic.h:1041
    pragma Import(C, XtProcessEvent, "XtProcessEvent");     -- Intrinsic.h:1047
    pragma Import(C, XtAppProcessEvent, "XtAppProcessEvent");
                                                            -- Intrinsic.h:1053
    pragma Import(C, XtMainLoop, "XtMainLoop");             -- Intrinsic.h:1060
    pragma Import(C, XtAppMainLoop, "XtAppMainLoop");       -- Intrinsic.h:1066
    pragma Import(C, XtAddExposureToRegion, "XtAddExposureToRegion");
                                                            -- Intrinsic.h:1072
    pragma Import(C, XtSetKeyboardFocus, "XtSetKeyboardFocus");
                                                            -- Intrinsic.h:1079
    pragma Import(C, XtLastTimestampProcessed, "XtLastTimestampProcessed");
                                                            -- Intrinsic.h:1086
    pragma Import(C, XtAddTimeOut, "XtAddTimeOut");         -- Intrinsic.h:1098
    pragma Import(C, XtAppAddTimeOut, "XtAppAddTimeOut");   -- Intrinsic.h:1106
    pragma Import(C, XtRemoveTimeOut, "XtRemoveTimeOut");   -- Intrinsic.h:1115
    pragma Import(C, XtAddInput, "XtAddInput");             -- Intrinsic.h:1121
    pragma Import(C, XtAppAddInput, "XtAppAddInput");       -- Intrinsic.h:1130
    pragma Import(C, XtRemoveInput, "XtRemoveInput");       -- Intrinsic.h:1140
    pragma Import(C, XtNextEvent, "XtNextEvent");           -- Intrinsic.h:1146
    pragma Import(C, XtAppNextEvent, "XtAppNextEvent");     -- Intrinsic.h:1152
    pragma Import(C, XtPending, "XtPending");               -- Intrinsic.h:1164
    pragma Import(C, XtAppPending, "XtAppPending");         -- Intrinsic.h:1170
    pragma Import(C, XtRealizeWidget, "XtRealizeWidget");   -- Intrinsic.h:1194
    pragma Import(C, XtUnrealizeWidget, "XtUnrealizeWidget");
                                                            -- Intrinsic.h:1200
    pragma Import(C, XtDestroyWidget, "XtDestroyWidget");   -- Intrinsic.h:1206
    pragma Import(C, XtSetSensitive, "XtSetSensitive");     -- Intrinsic.h:1212
    pragma Import(C, XtSetMappedWhenManaged, "XtSetMappedWhenManaged");
                                                            -- Intrinsic.h:1219
    pragma Import(C, XtWindowToWidget, "XtWindowToWidget"); -- Intrinsic.h:1233
    pragma Import(C, XtMergeArgLists, "XtMergeArgLists");   -- Intrinsic.h:1257
    pragma Import(C, XtDisplay, "XtDisplay");               -- Intrinsic.h:1292
    pragma Import(C, XtDisplayOfObject, "XtDisplayOfObject");
                                                            -- Intrinsic.h:1298
    pragma Import(C, XtScreen, "XtScreen");                 -- Intrinsic.h:1304
    pragma Import(C, XtScreenOfObject, "XtScreenOfObject"); -- Intrinsic.h:1310
    pragma Import(C, XtWindow, "XtWindow");                 -- Intrinsic.h:1316
    pragma Import(C, XtWindowOfObject, "XtWindowOfObject"); -- Intrinsic.h:1322
    pragma Import(C, XtName, "XtName");                     -- Intrinsic.h:1328
    pragma Import(C, XtSuperclass, "XtSuperclass");         -- Intrinsic.h:1334
    pragma Import(C, XtClass, "XtClass");                   -- Intrinsic.h:1340
    pragma Import(C, XtParent, "XtParent");                 -- Intrinsic.h:1346
    pragma Import(C, XtCallCallbackList, "XtCallCallbackList");
                                                            -- Intrinsic.h:1407
    pragma Import(C, XtMakeGeometryRequest, "XtMakeGeometryRequest");
                                                            -- Intrinsic.h:1430
    pragma Import(C, XtQueryGeometry, "XtQueryGeometry");   -- Intrinsic.h:1438
    pragma Import(C, XtPopup, "XtPopup");                   -- Intrinsic.h:1465
    pragma Import(C, XtPopupSpringLoaded, "XtPopupSpringLoaded");
                                                            -- Intrinsic.h:1472
    pragma Import(C, XtCallbackNone, "XtCallbackNone");     -- Intrinsic.h:1478
    pragma Import(C, XtCallbackNonexclusive, "XtCallbackNonexclusive");
                                                            -- Intrinsic.h:1486
    pragma Import(C, XtCallbackExclusive, "XtCallbackExclusive");
                                                            -- Intrinsic.h:1494
    pragma Import(C, XtPopdown, "XtPopdown");               -- Intrinsic.h:1502
    pragma Import(C, XtCallbackPopdown, "XtCallbackPopdown");
                                                            -- Intrinsic.h:1508
    pragma Import(C, XtMenuPopupAction, "XtMenuPopupAction");
                                                            -- Intrinsic.h:1516
    pragma Import(C, XtToolkitInitialize, "XtToolkitInitialize");
                                                            -- Intrinsic.h:1599
    pragma Import(C, XtCreateApplicationContext, "XtCreateApplicationContext");
                                                            -- Intrinsic.h:1677
    pragma Import(C, XtAppSetFallbackResources, "XtAppSetFallbackResources");
                                                            -- Intrinsic.h:1683
    pragma Import(C, XtDestroyApplicationContext, 
                     "XtDestroyApplicationContext");        -- Intrinsic.h:1690
    pragma Import(C, XtInitializeWidgetClass, "XtInitializeWidgetClass");
                                                            -- Intrinsic.h:1696
    pragma Import(C, XtWidgetToApplicationContext, 
                     "XtWidgetToApplicationContext");       -- Intrinsic.h:1702
    pragma Import(C, XtDisplayToApplicationContext, 
                     "XtDisplayToApplicationContext");      -- Intrinsic.h:1708
    pragma Import(C, XtDatabase, "XtDatabase");             -- Intrinsic.h:1714
    pragma Import(C, XtScreenDatabase, "XtScreenDatabase"); -- Intrinsic.h:1720
    pragma Import(C, XtCloseDisplay, "XtCloseDisplay");     -- Intrinsic.h:1726
    pragma Import(C, XtGetApplicationResources, "XtGetApplicationResources");
                                                            -- Intrinsic.h:1732
    pragma Import(C, XtSetValues, "XtSetValues");           -- Intrinsic.h:1778
    pragma Import(C, XtGetValues, "XtGetValues");           -- Intrinsic.h:1793
    pragma Import(C, XtSetSubvalues, "XtSetSubvalues");     -- Intrinsic.h:1808
    pragma Import(C, XtGetSubvalues, "XtGetSubvalues");     -- Intrinsic.h:1827
    pragma Import(C, XtGetResourceList, "XtGetResourceList");
                                                            -- Intrinsic.h:1846
    pragma Import(C, XtGetConstraintResourceList, 
                     "XtGetConstraintResourceList");        -- Intrinsic.h:1854
    pragma Import(C, XtAppSetErrorMsgHandler, "XtAppSetErrorMsgHandler");
                                                            -- Intrinsic.h:1904
    pragma Import(C, XtSetErrorMsgHandler, "XtSetErrorMsgHandler");
                                                            -- Intrinsic.h:1911
    pragma Import(C, XtAppSetWarningMsgHandler, "XtAppSetWarningMsgHandler");
                                                            -- Intrinsic.h:1917
    pragma Import(C, XtSetWarningMsgHandler, "XtSetWarningMsgHandler");
                                                            -- Intrinsic.h:1924
    pragma Import(C, XtAppSetErrorHandler, "XtAppSetErrorHandler");
                                                            -- Intrinsic.h:1976
    pragma Import(C, XtSetErrorHandler, "XtSetErrorHandler");
                                                            -- Intrinsic.h:1983
    pragma Import(C, XtAppSetWarningHandler, "XtAppSetWarningHandler");
                                                            -- Intrinsic.h:1989
    pragma Import(C, XtSetWarningHandler, "XtSetWarningHandler");
                                                            -- Intrinsic.h:1996
    pragma Import(C, XtAppGetErrorDatabase, "XtAppGetErrorDatabase");
                                                            -- Intrinsic.h:2028
    pragma Import(C, XtGetErrorDatabase, "XtGetErrorDatabase");
                                                            -- Intrinsic.h:2034
    pragma Import(C, XtMalloc, "XtMalloc");                 -- Intrinsic.h:2070
    pragma Import(C, XtCalloc, "XtCalloc");                 -- Intrinsic.h:2076
    pragma Import(C, XtRealloc, "XtRealloc");               -- Intrinsic.h:2083
    pragma Import(C, XtFree, "XtFree");                     -- Intrinsic.h:2090
    pragma Import(C, XtAddWorkProc, "XtAddWorkProc");       -- Intrinsic.h:2147
    pragma Import(C, XtAppAddWorkProc, "XtAppAddWorkProc"); -- Intrinsic.h:2154
    pragma Import(C, XtRemoveWorkProc, "XtRemoveWorkProc"); -- Intrinsic.h:2162
    pragma Import(C, XtGetGC, "XtGetGC");                   -- Intrinsic.h:2174
    pragma Import(C, XtAllocateGC, "XtAllocateGC");         -- Intrinsic.h:2182
    pragma Import(C, XtDestroyGC, "XtDestroyGC");           -- Intrinsic.h:2197
    pragma Import(C, XtReleaseGC, "XtReleaseGC");           -- Intrinsic.h:2203
    pragma Import(C, XtAppReleaseCacheRefs, "XtAppReleaseCacheRefs");
                                                            -- Intrinsic.h:2212
    pragma Import(C, XtCallbackReleaseCacheRef, "XtCallbackReleaseCacheRef");
                                                            -- Intrinsic.h:2219
    pragma Import(C, XtCallbackReleaseCacheRefList, 
                     "XtCallbackReleaseCacheRefList");      -- Intrinsic.h:2227
    pragma Import(C, XtSetWMColormapWindows, "XtSetWMColormapWindows");
                                                            -- Intrinsic.h:2235
    pragma Import(C, XtDisownSelection, "XtDisownSelection");
                                                            -- Intrinsic.h:2273
    pragma Import(C, XtGetSelectionValue, "XtGetSelectionValue");
                                                            -- Intrinsic.h:2281
    pragma Import(C, XtGetSelectionValues, "XtGetSelectionValues");
                                                            -- Intrinsic.h:2292
    pragma Import(C, XtAppSetSelectionTimeout, "XtAppSetSelectionTimeout");
                                                            -- Intrinsic.h:2304
    pragma Import(C, XtSetSelectionTimeout, "XtSetSelectionTimeout");
                                                            -- Intrinsic.h:2311
    pragma Import(C, XtAppGetSelectionTimeout, "XtAppGetSelectionTimeout");
                                                            -- Intrinsic.h:2317
    pragma Import(C, XtGetSelectionTimeout, "XtGetSelectionTimeout");
                                                            -- Intrinsic.h:2323
    pragma Import(C, XtGetSelectionRequest, "XtGetSelectionRequest");
                                                            -- Intrinsic.h:2329
    pragma Import(C, XtGetSelectionValueIncremental, 
                     "XtGetSelectionValueIncremental");     -- Intrinsic.h:2337
    pragma Import(C, XtGetSelectionValuesIncremental, 
                     "XtGetSelectionValuesIncremental");    -- Intrinsic.h:2348
    pragma Import(C, XtGrabKey, "XtGrabKey");               -- Intrinsic.h:2360
    pragma Import(C, XtUngrabKey, "XtUngrabKey");           -- Intrinsic.h:2371
    pragma Import(C, XtGrabKeyboard, "XtGrabKeyboard");     -- Intrinsic.h:2379
    pragma Import(C, XtUngrabKeyboard, "XtUngrabKeyboard"); -- Intrinsic.h:2389
    pragma Import(C, XtGrabButton, "XtGrabButton");         -- Intrinsic.h:2396
    pragma Import(C, XtUngrabButton, "XtUngrabButton");     -- Intrinsic.h:2410
    pragma Import(C, XtGrabPointer, "XtGrabPointer");       -- Intrinsic.h:2418
    pragma Import(C, XtUngrabPointer, "XtUngrabPointer");   -- Intrinsic.h:2431
    pragma Import(C, XtGetApplicationNameAndClass, 
                     "XtGetApplicationNameAndClass");       -- Intrinsic.h:2438
    pragma Import(C, XtCvtStringToAcceleratorTable, 
                     "XtCvtStringToAcceleratorTable");      -- Intrinsic.h:2454
    pragma Import(C, XtCvtStringToAtom, "XtCvtStringToAtom");
                                                            -- Intrinsic.h:2465
    pragma Import(C, XtCvtStringToBoolean, "XtCvtStringToBoolean");
                                                            -- Intrinsic.h:2476
    pragma Import(C, XtCvtStringToBool, "XtCvtStringToBool");
                                                            -- Intrinsic.h:2487
    pragma Import(C, XtCvtStringToCursor, "XtCvtStringToCursor");
                                                            -- Intrinsic.h:2498
    pragma Import(C, XtCvtStringToDimension, "XtCvtStringToDimension");
                                                            -- Intrinsic.h:2509
    pragma Import(C, XtCvtStringToDisplay, "XtCvtStringToDisplay");
                                                            -- Intrinsic.h:2520
    pragma Import(C, XtCvtStringToFile, "XtCvtStringToFile");
                                                            -- Intrinsic.h:2531
    pragma Import(C, XtCvtStringToFloat, "XtCvtStringToFloat");
                                                            -- Intrinsic.h:2542
    pragma Import(C, XtCvtStringToFont, "XtCvtStringToFont");
                                                            -- Intrinsic.h:2553
    pragma Import(C, XtCvtStringToFontSet, "XtCvtStringToFontSet");
                                                            -- Intrinsic.h:2564
    pragma Import(C, XtCvtStringToFontStruct, "XtCvtStringToFontStruct");
                                                            -- Intrinsic.h:2575
    pragma Import(C, XtCvtStringToInt, "XtCvtStringToInt"); -- Intrinsic.h:2586
    pragma Import(C, XtCvtStringToInitialState, "XtCvtStringToInitialState");
                                                            -- Intrinsic.h:2597
    pragma Import(C, XtCvtStringToPixel, "XtCvtStringToPixel");
                                                            -- Intrinsic.h:2608
    pragma Import(C, XtCvtStringToShort, "XtCvtStringToShort");
                                                            -- Intrinsic.h:2621
    pragma Import(C, XtCvtStringToTranslationTable, 
                     "XtCvtStringToTranslationTable");      -- Intrinsic.h:2632
    pragma Import(C, XtCvtStringToUnsignedChar, "XtCvtStringToUnsignedChar");
                                                            -- Intrinsic.h:2643
    pragma Import(C, XtCvtStringToVisual, "XtCvtStringToVisual");
                                                            -- Intrinsic.h:2654
    pragma Import(C, XtCvtIntToBoolean, "XtCvtIntToBoolean");
                                                            -- Intrinsic.h:2667
    pragma Import(C, XtCvtIntToBool, "XtCvtIntToBool");     -- Intrinsic.h:2678
    pragma Import(C, XtCvtIntToColor, "XtCvtIntToColor");   -- Intrinsic.h:2689
    pragma Import(C, XtCvtIntToFloat, "XtCvtIntToFloat");   -- Intrinsic.h:2702
    pragma Import(C, XtCvtIntToFont, "XtCvtIntToFont");     -- Intrinsic.h:2713
    pragma Import(C, XtCvtIntToPixel, "XtCvtIntToPixel");   -- Intrinsic.h:2724
    pragma Import(C, XtCvtIntToPixmap, "XtCvtIntToPixmap"); -- Intrinsic.h:2735
    pragma Import(C, XtCvtIntToShort, "XtCvtIntToShort");   -- Intrinsic.h:2748
    pragma Import(C, XtCvtIntToUnsignedChar, "XtCvtIntToUnsignedChar");
                                                            -- Intrinsic.h:2759
    pragma Import(C, XtCvtColorToPixel, "XtCvtColorToPixel");
                                                            -- Intrinsic.h:2772

    pragma Convention(C, XtActionsRec);
    pragma Convention(C, XtWidgetGeometry);
    pragma Convention(C, Arg);
    pragma Convention(C, XtCallbackRec);
    pragma Convention(C, XtPopdownIDRec);
    pragma Convention(C, XtResource);
    pragma Convention(C, SubstitutionRec);

    pragma Inline(XtIsRectObj);
    pragma Inline(XtIsWidget);
    pragma Inline(XtIsComposite);
    pragma Inline(XtIsConstraint);
    pragma Inline(XtIsShell);
    pragma Inline(XtIsOverrideShell);
    pragma Inline(XtIsWMShell);
    pragma Inline(XtIsVendorShell);
    pragma Inline(XtIsTransientShell);
    pragma Inline(XtIsTopLevelShell);
    pragma Inline(XtIsApplicationShell);
    pragma Inline(XtSetArg);
    pragma Inline(XtMapWidget);
    pragma Inline(XtUnmapWidget);
    pragma Inline(XtNewString);

    pragma Convention(C, XtActionProc);
     pragma Convention(C, XtConvertArgProc);
    pragma Convention(C, XtConverter);
    pragma Convention(C, XtTypeConverter);
    pragma Convention(C, XtDestructor);
    pragma Convention(C, XtActionHookProc);
    pragma Convention(C, XtKeyProc);
    pragma Convention(C, XtCaseProc);
    pragma Convention(C, XtEventHandler);
    pragma Convention(C, XtTimerCallbackProc);
    pragma Convention(C, XtInputCallbackProc);
    pragma Convention(C, XtCallbackProc);
    pragma Convention(C, XtResourceDefaultProc);
    pragma Convention(C, XtLanguageProc);
    pragma Convention(C, XtErrorMsgHandler);
    pragma Convention(C, XtErrorHandler);
    pragma Convention(C, XtCreatePopupChildProc);
    pragma Convention(C, XtWorkProc);
    pragma Convention(C, XtFilePredicate);
    pragma Convention(C, XtConvertSelectionProc);
    pragma Convention(C, XtLoseSelectionProc);
    pragma Convention(C, XtSelectionDoneProc);
    pragma Convention(C, XtSelectionCallbackProc);
    pragma Convention(C, XtLoseSelectionIncrProc);
    pragma Convention(C, XtSelectionDoneIncrProc);
    pragma Convention(C, XtConvertSelectionIncrProc);
    pragma Convention(C, XtCancelConvertSelectionProc);
end Xt.Intrinsic;

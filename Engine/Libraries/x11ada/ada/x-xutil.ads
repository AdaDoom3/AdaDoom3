-- $Source: /home/harp/1/proto/monoBANK/xbind/x-xutil.ads,v $ 
-- $Revision: 1.13 $ $Date: 95/12/05 08:53:42 $ $Author: mg $ 

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
-- see the accompanying file Xutil.h.
-- --------------------------------------------------------------------------

with X;
with X.Strings;
with X.Xlib;

package X.Xutil is

    NoValue                    : constant := 16#0#;         -- Xutil.h:39
    XValue                     : constant := 16#1#;         -- Xutil.h:40
    YValue                     : constant := 16#2#;         -- Xutil.h:41
    WidthValue                 : constant := 16#4#;         -- Xutil.h:42
    HeightValue                : constant := 16#8#;         -- Xutil.h:43
    AllValues                  : constant := 16#f#;         -- Xutil.h:44
    XNegative                  : constant := 16#10#;        -- Xutil.h:45
    YNegative                  : constant := 16#20#;        -- Xutil.h:46
    USPosition                 : constant := 1;             -- Xutil.h:73
    USSize                     : constant := 2;             -- Xutil.h:74
    PPosition                  : constant := 4;             -- Xutil.h:76
    PSize                      : constant := 8;             -- Xutil.h:77
    PMinSize                   : constant := 16;            -- Xutil.h:78
    PMaxSize                   : constant := 32;            -- Xutil.h:79
    PResizeInc                 : constant := 64;            -- Xutil.h:80
    PAspect                    : constant := 128;           -- Xutil.h:81
    PBaseSize                  : constant := 256;           -- Xutil.h:82
    PWinGravity                : constant := 512;           -- Xutil.h:83
    PAllHints                  : constant := 252;           -- Xutil.h:86
    InputHint                  : constant := 1;             -- Xutil.h:105
    StateHint                  : constant := 2;             -- Xutil.h:106
    IconPixmapHint             : constant := 4;             -- Xutil.h:107
    IconWindowHint             : constant := 8;             -- Xutil.h:108
    IconPositionHint           : constant := 16;            -- Xutil.h:109
    IconMaskHint               : constant := 32;            -- Xutil.h:110
    WindowGroupHint            : constant := 64;            -- Xutil.h:111
    AllHints                   : constant := 127;           -- Xutil.h:112
    WithdrawnState             : constant := 0;             -- Xutil.h:116
    NormalState                : constant := 1;             -- Xutil.h:117
    IconicState                : constant := 3;             -- Xutil.h:118
    DontCareState              : constant := 0;             -- Xutil.h:123
    ZoomState                  : constant := 2;             -- Xutil.h:124
    InactiveState              : constant := 4;             -- Xutil.h:125
    XNoMemory                  : constant := -1;            -- Xutil.h:140
    XLocaleNotSupported        : constant := -2;            -- Xutil.h:141
    XConverterNotFound         : constant := -3;            -- Xutil.h:142
    RectangleOut               : constant := 0;             -- Xutil.h:214
    RectangleIn                : constant := 1;             -- Xutil.h:215
    RectanglePart              : constant := 2;             -- Xutil.h:216
    VisualNoMask               : constant := 16#0#;         -- Xutil.h:241
    VisualIDMask               : constant := 16#1#;         -- Xutil.h:242
    VisualScreenMask           : constant := 16#2#;         -- Xutil.h:243
    VisualDepthMask            : constant := 16#4#;         -- Xutil.h:244
    VisualClassMask            : constant := 16#8#;         -- Xutil.h:245
    VisualRedMaskMask          : constant := 16#10#;        -- Xutil.h:246
    VisualGreenMaskMask        : constant := 16#20#;        -- Xutil.h:247
    VisualBlueMaskMask         : constant := 16#40#;        -- Xutil.h:248
    VisualColormapSizeMask     : constant := 16#80#;        -- Xutil.h:249
    VisualBitsPerRGBMask       : constant := 16#100#;       -- Xutil.h:250
    VisualAllMask              : constant := 16#1ff#;       -- Xutil.h:251
    ReleaseByFreeingColormap   : constant XID := 1;         -- Xutil.h:270
    BitmapSuccess              : constant := 0;             -- Xutil.h:276
    BitmapOpenFailed           : constant := 1;             -- Xutil.h:277
    BitmapFileInvalid          : constant := 2;             -- Xutil.h:278
    BitmapNoMemory             : constant := 3;             -- Xutil.h:279
    XCSUCCESS                  : constant := 0;             -- Xutil.h:290
    XCNOMEM                    : constant := 1;             -- Xutil.h:291
    XCNOENT                    : constant := 2;             -- Xutil.h:292

    type XICCEncodingStyle is (                             -- Xutil.h:149
        XStringStyle,                                       -- Xutil.h:145
        XCompoundTextStyle,                                 -- Xutil.h:146
        XTextStyle,                                         -- Xutil.h:147
        XStdICCTextStyle                                    -- Xutil.h:149
    );
    for XICCEncodingStyle'Size use X.Int'Size;              -- Xutil.h:149

    type XContext is new X.signed_int;                      -- Xutil.h:294


    type XRegion is private;                                -- Xutil.h:210

    type XSizeHints;                                        -- Xutil.h:65
    type XWMHints;                                          -- Xutil.h:101
    type XTextProperty;                                     -- Xutil.h:138
    type XIconSize;                                         -- Xutil.h:155
    type XClassHint;                                        -- Xutil.h:160
    type XComposeStatus;                                    -- Xutil.h:180
    type XVisualInfo;                                       -- Xutil.h:239
    type XStandardColormap;                                 -- Xutil.h:268

    type Region is access all XRegion;                      -- Xutil.h:210
    type XClassHint_access is access all XClassHint;        -- Xutil.h:307
    type XIconSize_access is access all XIconSize;          -- Xutil.h:313
    type XSizeHints_access is access all XSizeHints;        -- Xutil.h:319
    type XStandardColormap_access is access all XStandardColormap;
                                                            -- Xutil.h:325
    type XWMHints_access is access all XWMHints;            -- Xutil.h:331
    type XVisualInfo_access is access all XVisualInfo;      -- Xutil.h:457

    type struct_anonymous0_t is                             -- Xutil.h:62
        record
            x: signed_int;                                  -- Xutil.h:60
            y: signed_int;                                  -- Xutil.h:61
        end record;

    type XSizeHints is                                      -- Xutil.h:65
        record
            flags      : long;                              -- Xutil.h:53
            x          : signed_int;                        -- Xutil.h:54
            y          : signed_int;                        -- Xutil.h:54
            width      : signed_int;                        -- Xutil.h:55
            height     : signed_int;                        -- Xutil.h:55
            min_width  : signed_int;                        -- Xutil.h:56
            min_height : signed_int;                        -- Xutil.h:56
            max_width  : signed_int;                        -- Xutil.h:57
            max_height : signed_int;                        -- Xutil.h:57
            width_inc  : signed_int;                        -- Xutil.h:58
            height_inc : signed_int;                        -- Xutil.h:58
            min_aspect : struct_anonymous0_t;               -- Xutil.h:62
            max_aspect : struct_anonymous0_t;               -- Xutil.h:62
            base_width : signed_int;                        -- Xutil.h:63
            base_height: signed_int;                        -- Xutil.h:63
            win_gravity: signed_int;                        -- Xutil.h:64
        end record;

    type XWMHints is                                        -- Xutil.h:101
        record
            flags        : X.long;                          -- Xutil.h:91
            input        : X.Xlib.Bool;                     -- Xutil.h:92
            initial_state: X.signed_int;                    -- Xutil.h:94
            icon_pixmap  : X.Pixmap;                        -- Xutil.h:95
            icon_window  : X.Window;                        -- Xutil.h:96
            icon_x       : X.signed_int;                    -- Xutil.h:97
            icon_y       : X.signed_int;                    -- Xutil.h:97
            icon_mask    : X.Pixmap;                        -- Xutil.h:98
            window_group : X.XID;                           -- Xutil.h:99
        end record;

    type unsigned_char_access is access all X.unsigned_char;

    type XTextProperty is                                   -- Xutil.h:138
        record
            value   : unsigned_char_access;                 -- Xutil.h:134
            encoding: X.Atom;                               -- Xutil.h:135
            format  : X.signed_int;                         -- Xutil.h:136
            nitems  : X.unsigned_long;                      -- Xutil.h:137
        end record;

    type XIconSize is                                       -- Xutil.h:155
        record
            min_width : X.signed_int;                       -- Xutil.h:152
            min_height: X.signed_int;                       -- Xutil.h:152
            max_width : X.signed_int;                       -- Xutil.h:153
            max_height: X.signed_int;                       -- Xutil.h:153
            width_inc : X.signed_int;                       -- Xutil.h:154
            height_inc: X.signed_int;                       -- Xutil.h:154
        end record;

    type XClassHint is                                      -- Xutil.h:160
        record
            res_name : X.Strings.charp;                     -- Xutil.h:158
            res_class: X.Strings.charp;                     -- Xutil.h:159
        end record;

    type XComposeStatus is                                  -- Xutil.h:180
        record
            compose_ptr  : X.Xlib.XPointer;                 -- Xutil.h:181
            chars_matched: X.signed_int;                    -- Xutil.h:182
        end record;

    type XVisualInfo is                                     -- Xutil.h:239
        record
            visual       : X.Xlib.Visual_access;            -- Xutil.h:225
            visualid     : X.VisualID;                      -- Xutil.h:226
            screen       : X.signed_int;                    -- Xutil.h:227
            depth        : X.signed_int;                    -- Xutil.h:228
            class        : X.signed_int;                    -- Xutil.h:232
            red_mask     : X.unsigned_long;                 -- Xutil.h:234
            green_mask   : X.unsigned_long;                 -- Xutil.h:235
            blue_mask    : X.unsigned_long;                 -- Xutil.h:236
            colormap_size: X.signed_int;                    -- Xutil.h:237
            bits_per_rgb : X.signed_int;                    -- Xutil.h:238
        end record;

    type XStandardColormap is                               -- Xutil.h:268
        record
            colormap  : X.Colormap;                         -- Xutil.h:258
            red_max   : X.unsigned_long;                    -- Xutil.h:259
            red_mult  : X.unsigned_long;                    -- Xutil.h:260
            green_max : X.unsigned_long;                    -- Xutil.h:261
            green_mult: X.unsigned_long;                    -- Xutil.h:262
            blue_max  : X.unsigned_long;                    -- Xutil.h:263
            blue_mult : X.unsigned_long;                    -- Xutil.h:264
            base_pixel: X.unsigned_long;                    -- Xutil.h:265
            visualid  : X.VisualID;                         -- Xutil.h:266
            killid    : X.XID;                              -- Xutil.h:267
        end record;

    function XAllocClassHint return XClassHint_access;      -- Xutil.h:303

    function XAllocIconSize return XIconSize_access;        -- Xutil.h:309

    function XAllocSizeHints return XSizeHints_access;      -- Xutil.h:315

    function XAllocStandardColormap return XStandardColormap_access;
                                                            -- Xutil.h:321

    function XAllocWMHints return XWMHints_access;          -- Xutil.h:327

    procedure XClipBox(
                r          : Region;
                rect_return: access X.Xlib.XRectangle);     -- Xutil.h:333

    function XCreateRegion return Region;                   -- Xutil.h:340

    function XDefaultString return X.Strings.charp;         -- Xutil.h:346

    function XDeleteContext(
                display: access X.Xlib.Display;
                rid    : X.XID;
                context: XContext)
               return X.signed_int;                         -- Xutil.h:352

    procedure XDestroyRegion(
                r: Region);                                 -- Xutil.h:360

    procedure XEmptyRegion(
                r: Region);                                 -- Xutil.h:366

    procedure XEqualRegion(
                r1: Region;
                r2: Region);                                -- Xutil.h:372

    function XFindContext(
                display    : access X.Xlib.Display;
                rid        : X.XID;
                context    : XContext;
                data_return: access X.Xlib.XPointer)
               return X.signed_int;                         -- Xutil.h:379

    function XGetClassHint(
                display           : access X.Xlib.Display;
                w                 : X.Window;
                class_hints_return: access XClassHint)
               return X.Xlib.Status;                        -- Xutil.h:388

    function XGetIconSizes(
                display         : access X.Xlib.Display;
                w               : X.Window;
                size_list_return: access XIconSize_access;
                count_return    : access X.signed_int)
               return X.Xlib.Status;                        -- Xutil.h:396

    function XGetNormalHints(
                display     : access X.Xlib.Display;
                w           : X.Window;
                hints_return: access XSizeHints)
               return X.Xlib.Status;                        -- Xutil.h:405

    function XGetRGBColormaps(
                display       : access X.Xlib.Display;
                w             : X.Window;
                stdcmap_return: access XStandardColormap_access;
                count_return  : access X.signed_int;
                property      : X.Atom)
               return X.Xlib.Status;                        -- Xutil.h:413

    function XGetSizeHints(
                display     : access X.Xlib.Display;
                w           : X.Window;
                hints_return: access XSizeHints;
                property    : X.Atom)
               return X.Xlib.Status;                        -- Xutil.h:423

    function XGetStandardColormap(
                display        : access X.Xlib.Display;
                w              : X.Window;
                colormap_return: access XStandardColormap;
                property       : X.Atom)
               return X.Xlib.Status;                        -- Xutil.h:432

    function XGetTextProperty(
                display         : access X.Xlib.Display;
                window          : X.Window;
                text_prop_return: access XTextProperty;
                property        : X.Atom)
               return X.Xlib.Status;                        -- Xutil.h:441

    function XGetVisualInfo(
                display       : access X.Xlib.Display;
                vinfo_mask    : X.long;
                vinfo_template: access XVisualInfo;
                nitems_return : access X.signed_int)
               return XVisualInfo_access;                   -- Xutil.h:450

    function XGetWMClientMachine(
                display         : access X.Xlib.Display;
                w               : X.Window;
                text_prop_return: access XTextProperty)
               return X.Xlib.Status;                        -- Xutil.h:459

    function XGetWMHints(
                display: access X.Xlib.Display;
                w      : X.Window)
               return XWMHints_access;                      -- Xutil.h:467

    function XGetWMIconName(
                display         : access X.Xlib.Display;
                w               : X.Window;
                text_prop_return: access XTextProperty)
               return X.Xlib.Status;                        -- Xutil.h:474

    function XGetWMName(
                display         : access X.Xlib.Display;
                w               : X.Window;
                text_prop_return: access XTextProperty)
               return X.Xlib.Status;                        -- Xutil.h:482

    function XGetWMNormalHints(
                display        : access X.Xlib.Display;
                w              : X.Window;
                hints_return   : access XSizeHints;
                supplied_return: access X.long)
               return X.Xlib.Status;                        -- Xutil.h:490

    function XGetWMSizeHints(
                display        : access X.Xlib.Display;
                w              : X.Window;
                hints_return   : access XSizeHints;
                supplied_return: access X.long;
                property       : X.Atom)
               return X.Xlib.Status;                        -- Xutil.h:499

    function XGetZoomHints(
                display      : access X.Xlib.Display;
                w            : X.Window;
                zhints_return: access XSizeHints)
               return X.Xlib.Status;                        -- Xutil.h:509

    procedure XIntersectRegion(
                sra      : Region;
                srb      : Region;
                dr_return: Region);                         -- Xutil.h:517

    function XLookupString(
                event_struct : access X.Xlib.XKeyEvent;
                buffer_return: X.Strings.charp;
                bytes_buffer : X.signed_int;
                keysym_return: access KeySym;
                status_in_out: access XComposeStatus)
               return X.signed_int;                         -- Xutil.h:525

    pragma Import(C, XLookupString, "XLookupString");

    procedure XLookupString(
                event_struct : access X.Xlib.XKeyEvent;
                buffer_return: in out Interfaces.C.Char_Array;
                bytes_buffer : X.signed_int;
                keysym_return: access KeySym;
                status_in_out: access XComposeStatus;
                result       : out X.signed_int);           -- Xutil.h:525

    pragma Inline(XLookupString);

    function XMatchVisualInfo(
                display     : access X.Xlib.Display;
                screen      : X.signed_int;
                depth       : X.signed_int;
                class       : X.signed_int;
                vinfo_return: access XVisualInfo)
               return X.Xlib.Status;                        -- Xutil.h:535

    procedure XOffsetRegion(
                r : Region;
                dx: X.signed_int;
                dy: X.signed_int);                          -- Xutil.h:545

    function XPointInRegion(
                r : Region;
                xx: signed_int;
                y : signed_int)
               return X.Xlib.Bool;                          -- Xutil.h:553

    function XPolygonRegion(
                points   : access X.Xlib.XPoint;
                n        : X.signed_int;
                fill_rule: X.signed_int)
               return Region;                               -- Xutil.h:561

    function XRectInRegion(
                r     : Region;
                x     : signed_int;
                y     : signed_int;
                width : unsigned_int;
                height: unsigned_int)
               return signed_int;                           -- Xutil.h:569

    function XSaveContext(
                display: access Xlib.Display;
                rid    : XID;
                context: XContext;
                data   : Strings.const_charp)
               return signed_int;                           -- Xutil.h:579

    pragma Import(C, XSaveContext, "XSaveContext");

    function XSaveContext(
                display: access X.Xlib.Display;
                rid    : X.XID;
                context: XContext;
                data   : Interfaces.C.Char_Array)
               return X.signed_int;                         -- Xutil.h:579

    pragma Inline(XSaveContext);

    procedure XSetClassHint(
                display    : access X.Xlib.Display;
                w          : X.Window;
                class_hints: access XClassHint);            -- Xutil.h:588

    procedure XSetIconSizes(
                display  : access X.Xlib.Display;
                w        : X.Window;
                size_list: access XIconSize;
                count    : X.signed_int);                   -- Xutil.h:596

    procedure XSetNormalHints(
                display: access X.Xlib.Display;
                w      : X.Window;
                hints  : access XSizeHints);                -- Xutil.h:605

    procedure XSetRGBColormaps(
                display : access X.Xlib.Display;
                w       : X.Window;
                stdcmaps: access XStandardColormap;
                count   : X.signed_int;
                property: X.Atom);                          -- Xutil.h:613

    procedure XSetSizeHints(
                display : access X.Xlib.Display;
                w       : X.Window;
                hints   : access XSizeHints;
                property: X.Atom);                          -- Xutil.h:623

    procedure XSetStandardProperties(
                display    : access X.Xlib.Display;
                w          : X.Window;
                window_name: X.Strings.const_charp;
                icon_name  : X.Strings.const_charp;
                icon_pixmap: X.Pixmap;
                argv       : X.Strings.charp_vector;
                argc       : X.signed_int;
                hints      : access XSizeHints);            -- Xutil.h:632

    pragma Import(C, XSetStandardProperties, "XSetStandardProperties");

    procedure XSetStandardProperties(
                display    : access X.Xlib.Display;
                w          : X.Window;
                window_name: Interfaces.C.Char_Array;
                icon_name  : Interfaces.C.Char_Array;
                icon_pixmap: X.Pixmap;
                argv       : X.Strings.charp_vector;
                argc       : X.signed_int;
                hints      : XSizeHints_access);            -- Xutil.h:632

    pragma Inline(XSetStandardProperties);

    procedure XSetTextProperty(
                display  : access X.Xlib.Display;
                w        : X.Window;
                text_prop: access XTextProperty;
                property : X.Atom);                         -- Xutil.h:645

    procedure XSetWMClientMachine(
                display  : access X.Xlib.Display;
                w        : X.Window;
                text_prop: access XTextProperty);           -- Xutil.h:654

    procedure XSetWMHints(
                display : access X.Xlib.Display;
                w       : X.Window;
                wm_hints: access XWMHints);                 -- Xutil.h:662

    procedure XSetWMIconName(
                display  : access X.Xlib.Display;
                w        : X.Window;
                text_prop: access XTextProperty);           -- Xutil.h:670

    procedure XSetWMName(
                display  : access X.Xlib.Display;
                w        : X.Window;
                text_prop: access XTextProperty);           -- Xutil.h:678

    procedure XSetWMNormalHints(
                display: access X.Xlib.Display;
                w      : X.Window;
                hints  : access XSizeHints);                -- Xutil.h:686

    procedure XSetWMProperties(
                display     : access X.Xlib.Display;
                w           : X.Window;
                window_name : access XTextProperty;
                icon_name   : access XTextProperty;
                argv        : X.Strings.charp_vector;
                argc        : X.signed_int;
                normal_hints: access XSizeHints;
                wm_hints    : access XWMHints;
                class_hints : access XClassHint);           -- Xutil.h:694

    procedure XmbSetWMProperties(
                display     : access X.Xlib.Display;
                w           : X.Window;
                window_name : X.Strings.const_charp;
                icon_name   : X.Strings.const_charp;
                argv        : X.Strings.charp_vector;
                argc        : X.signed_int;
                normal_hints: access XSizeHints;
                wm_hints    : access XWMHints;
                class_hints : access XClassHint);           -- Xutil.h:708

    pragma Import(C, XmbSetWMProperties, "XmbSetWMProperties");

    procedure XmbSetWMProperties(
                display     : access X.Xlib.Display;
                w           : X.Window;
                window_name : Interfaces.C.Char_Array;
                icon_name   : Interfaces.C.Char_Array;
                argv        : X.Strings.charp_vector;
                argc        : X.signed_int;
                normal_hints: XSizeHints_access;
                wm_hints    : XWMHints_access;
                class_hints : XClassHint_access);           -- Xutil.h:708

    pragma Inline(XmbSetWMProperties);

    procedure XSetWMSizeHints(
                display : access X.Xlib.Display;
                w       : X.Window;
                hints   : access XSizeHints;
                property: X.Atom);                          -- Xutil.h:722

    procedure XSetRegion(
                display: access X.Xlib.Display;
                gc     : X.Xlib.GC;
                r      : Region);                           -- Xutil.h:731

    procedure XSetStandardColormap(
                display : access X.Xlib.Display;
                w       : X.Window;
                colormap: access XStandardColormap;
                property: X.Atom);                          -- Xutil.h:739

    procedure XSetZoomHints(
                display: access X.Xlib.Display;
                w      : X.Window;
                zhints : access XSizeHints);                -- Xutil.h:748

    procedure XShrinkRegion(
                r : Region;
                dx: X.signed_int;
                dy: X.signed_int);                          -- Xutil.h:756

    function XStringListToTextProperty(
                list            : X.Strings.charp_vector;
                count           : X.signed_int;
                text_prop_return: access XTextProperty)
               return X.Xlib.Status;                        -- Xutil.h:764

    procedure XSubtractRegion(
                sra      : Region;
                srb      : Region;
                dr_return: Region);                         -- Xutil.h:772

    function XmbTextListToTextProperty(
                display         : access X.Xlib.Display;
                list            : X.Strings.charp_vector;
                count           : X.signed_int;
                style           : XICCEncodingStyle;
                text_prop_return: access XTextProperty)
               return X.signed_int;                         -- Xutil.h:780

    function XwcTextListToTextProperty(
                display         : access X.Xlib.Display;
                list            : access X.wchar_access;
                count           : X.signed_int;
                style           : XICCEncodingStyle;
                text_prop_return: access XTextProperty)
               return X.signed_int;                         -- Xutil.h:790

    procedure XwcFreeStringList(
                list: access X.wchar_access);               -- Xutil.h:800

    function XTextPropertyToStringList(
                text_prop   : access XTextProperty;
                list_return : access X.Strings.charp_vector;-- char ***
                count_return: access X.signed_int)
               return X.Xlib.Status;                        -- Xutil.h:806

    function XmbTextPropertyToTextList(
                display     : access X.Xlib.Display;
                text_prop   : access XTextProperty;
                list_return : access X.Strings.charp_vector;-- char ***
                count_return: access X.signed_int)
               return X.signed_int;                         -- Xutil.h:814

    function XwcTextPropertyToTextList(
                display     : access X.Xlib.Display;
                text_prop   : access XTextProperty;
                list_return : access X.wchar_access_access; -- wchar ***
                count_return: access X.signed_int)
               return X.signed_int;                         -- Xutil.h:823

    procedure XUnionRectWithRegion(
                rectangle         : access X.Xlib.XRectangle;
                src_region        : Region;
                dest_region_return: Region);                -- Xutil.h:832

    procedure XUnionRegion(
                sra      : Region;
                srb      : Region;
                dr_return: Region);                         -- Xutil.h:840

    procedure XWMGeometry(
                display         : access X.Xlib.Display;
                screen_number   : X.signed_int;
                user_geometry   : X.Strings.const_charp;
                default_geometry: X.Strings.const_charp;
                border_width    : X.unsigned_int;
                hints           : access XSizeHints;
                x_return        : access X.signed_int;
                y_return        : access X.signed_int;
                width_return    : access X.signed_int;
                height_return   : access X.signed_int;
                gravity_return  : access X.signed_int);     -- Xutil.h:848

    pragma Import(C, XWMGeometry, "XWMGeometry");

    procedure XWMGeometry(
                display         : access X.Xlib.Display;
                screen_number   : X.signed_int;
                user_geometry   : Interfaces.C.Char_Array;
                default_geometry: Interfaces.C.Char_Array;
                border_width    : X.unsigned_int;
                hints           : access XSizeHints;
                x_return        : access X.signed_int;
                y_return        : access X.signed_int;
                width_return    : access X.signed_int;
                height_return   : access X.signed_int;
                gravity_return  : access X.signed_int);     -- Xutil.h:848

    pragma Inline(XWMGeometry);

    procedure XXorRegion(
                sra      : Region;
                srb      : Region;
                dr_return: Region);                         -- Xutil.h:864

    -- **************
    -- Xutil.h macros
    -- **************

    function XDestroyImage (Image: access X.Xlib.Ximage) return X.signed_int;

    function XGetPixel (
                Image: access X.Xlib.XImage;
                XX   : signed_int;
                Y    : signed_int)
               return X.unsigned_long;

    function XPutPixel (
                Image: access X.Xlib.XImage;
                XX   : signed_int;
                Y    : signed_int;
                Pixel: unsigned_long)
               return X.signed_int;

    function XSubImage (
                Image : access X.Xlib.XImage;
                XX    : signed_int;
                Y     : signed_int;
                Width : unsigned_int;
                Height: unsigned_int)
               return X.Xlib.XImage_access;

    function XAddPixel (
                Image: access X.Xlib.XImage;
                Value: X.long)
               return X.signed_int;

    function IsKeypadKey (Key: X.KeySym) return Boolean;

    function IsCursorKey (Key: X.KeySym) return Boolean;

    function IsPFKey (Key: X.KeySym) return Boolean;

    function IsFunctionKey (Key: X.KeySym) return Boolean;

    function IsMiscFunctionKey (Key: X.KeySym) return Boolean;

    function IsModifierKey (Key: X.KeySym) return Boolean;

    function XUniqueContext return XContext;

    function XStringToContext (String: X.Strings.const_charp) return XContext;

private

    type XRegion is null record;                            -- Xutil.h:210
    pragma Import(C, XAllocClassHint, "XAllocClassHint");   -- Xutil.h:303
    pragma Import(C, XAllocIconSize, "XAllocIconSize");     -- Xutil.h:309
    pragma Import(C, XAllocSizeHints, "XAllocSizeHints");   -- Xutil.h:315
    pragma Import(C, XAllocStandardColormap, "XAllocStandardColormap");
                                                            -- Xutil.h:321
    pragma Import(C, XAllocWMHints, "XAllocWMHints");       -- Xutil.h:327
    pragma Import(C, XClipBox, "XClipBox");                 -- Xutil.h:333
    pragma Import(C, XCreateRegion, "XCreateRegion");       -- Xutil.h:340
    pragma Import(C, XDefaultString, "XDefaultString");     -- Xutil.h:346
    pragma Import(C, XDeleteContext, "XDeleteContext");     -- Xutil.h:352
    pragma Import(C, XDestroyRegion, "XDestroyRegion");     -- Xutil.h:360
    pragma Import(C, XEmptyRegion, "XEmptyRegion");         -- Xutil.h:366
    pragma Import(C, XEqualRegion, "XEqualRegion");         -- Xutil.h:372
    pragma Import(C, XFindContext, "XFindContext");         -- Xutil.h:379
    pragma Import(C, XGetClassHint, "XGetClassHint");       -- Xutil.h:388
    pragma Import(C, XGetIconSizes, "XGetIconSizes");       -- Xutil.h:396
    pragma Import(C, XGetNormalHints, "XGetNormalHints");   -- Xutil.h:405
    pragma Import(C, XGetRGBColormaps, "XGetRGBColormaps"); -- Xutil.h:413
    pragma Import(C, XGetSizeHints, "XGetSizeHints");       -- Xutil.h:423
    pragma Import(C, XGetStandardColormap, "XGetStandardColormap");
                                                            -- Xutil.h:432
    pragma Import(C, XGetTextProperty, "XGetTextProperty"); -- Xutil.h:441
    pragma Import(C, XGetVisualInfo, "XGetVisualInfo");     -- Xutil.h:450
    pragma Import(C, XGetWMClientMachine, "XGetWMClientMachine");
                                                            -- Xutil.h:459
    pragma Import(C, XGetWMHints, "XGetWMHints");           -- Xutil.h:467
    pragma Import(C, XGetWMIconName, "XGetWMIconName");     -- Xutil.h:474
    pragma Import(C, XGetWMName, "XGetWMName");             -- Xutil.h:482
    pragma Import(C, XGetWMNormalHints, "XGetWMNormalHints");
                                                            -- Xutil.h:490
    pragma Import(C, XGetWMSizeHints, "XGetWMSizeHints");   -- Xutil.h:499
    pragma Import(C, XGetZoomHints, "XGetZoomHints");       -- Xutil.h:509
    pragma Import(C, XIntersectRegion, "XIntersectRegion"); -- Xutil.h:517
    pragma Import(C, XMatchVisualInfo, "XMatchVisualInfo"); -- Xutil.h:535
    pragma Import(C, XOffsetRegion, "XOffsetRegion");       -- Xutil.h:545
    pragma Import(C, XPointInRegion, "XPointInRegion");     -- Xutil.h:553
    pragma Import(C, XPolygonRegion, "XPolygonRegion");     -- Xutil.h:561
    pragma Import(C, XRectInRegion, "XRectInRegion");       -- Xutil.h:569
    pragma Import(C, XSetClassHint, "XSetClassHint");       -- Xutil.h:588
    pragma Import(C, XSetIconSizes, "XSetIconSizes");       -- Xutil.h:596
    pragma Import(C, XSetNormalHints, "XSetNormalHints");   -- Xutil.h:605
    pragma Import(C, XSetRGBColormaps, "XSetRGBColormaps"); -- Xutil.h:613
    pragma Import(C, XSetSizeHints, "XSetSizeHints");       -- Xutil.h:623
    pragma Import(C, XSetTextProperty, "XSetTextProperty"); -- Xutil.h:645
    pragma Import(C, XSetWMClientMachine, "XSetWMClientMachine");
                                                            -- Xutil.h:654
    pragma Import(C, XSetWMHints, "XSetWMHints");           -- Xutil.h:662
    pragma Import(C, XSetWMIconName, "XSetWMIconName");     -- Xutil.h:670
    pragma Import(C, XSetWMName, "XSetWMName");             -- Xutil.h:678
    pragma Import(C, XSetWMNormalHints, "XSetWMNormalHints");
                                                            -- Xutil.h:686
    pragma Import(C, XSetWMProperties, "XSetWMProperties"); -- Xutil.h:694
    pragma Import(C, XSetWMSizeHints, "XSetWMSizeHints");   -- Xutil.h:722
    pragma Import(C, XSetRegion, "XSetRegion");             -- Xutil.h:731
    pragma Import(C, XSetStandardColormap, "XSetStandardColormap");
                                                            -- Xutil.h:739
    pragma Import(C, XSetZoomHints, "XSetZoomHints");       -- Xutil.h:748
    pragma Import(C, XShrinkRegion, "XShrinkRegion");       -- Xutil.h:756
    pragma Import(C, XStringListToTextProperty, "XStringListToTextProperty");
                                                            -- Xutil.h:764
    pragma Import(C, XSubtractRegion, "XSubtractRegion");   -- Xutil.h:772
    pragma Import(C, XmbTextListToTextProperty, "XmbTextListToTextProperty");
                                                            -- Xutil.h:780
    pragma Import(C, XwcTextListToTextProperty, "XwcTextListToTextProperty");
                                                            -- Xutil.h:790
    pragma Import(C, XwcFreeStringList, "XwcFreeStringList");
                                                            -- Xutil.h:800
    pragma Import(C, XTextPropertyToStringList, "XTextPropertyToStringList");
                                                            -- Xutil.h:806
    pragma Import(C, XmbTextPropertyToTextList, "XmbTextPropertyToTextList");
                                                            -- Xutil.h:814
    pragma Import(C, XwcTextPropertyToTextList, "XwcTextPropertyToTextList");
                                                            -- Xutil.h:823
    pragma Import(C, XUnionRectWithRegion, "XUnionRectWithRegion");
                                                            -- Xutil.h:832
    pragma Import(C, XUnionRegion, "XUnionRegion");         -- Xutil.h:840
    pragma Import(C, XXorRegion, "XXorRegion");             -- Xutil.h:864

    pragma Convention(C, struct_anonymous0_t);
    pragma Convention(C, XSizeHints);
    pragma Convention(C, XWMHints);
    pragma Convention(C, XTextProperty);
    pragma Convention(C, XIconSize);
    pragma Convention(C, XClassHint);
    pragma Convention(C, XComposeStatus);
    pragma Convention(C, XVisualInfo);
    pragma Convention(C, XStandardColormap);

    pragma Inline(XDestroyImage);
    pragma Inline(XGetPixel);
    pragma Inline(XPutPixel);
    pragma Inline(XSubImage);
    pragma Inline(XAddPixel);
    pragma Inline(IsKeypadKey);
    pragma Inline(IsCursorKey);
    pragma Inline(IsPFKey);
    pragma Inline(IsFunctionKey);
    pragma Inline(IsMiscFunctionKey);
    pragma Inline(IsModifierKey);
    pragma Inline(XUniqueContext);
    pragma Inline(XStringToContext);

end X.Xutil;

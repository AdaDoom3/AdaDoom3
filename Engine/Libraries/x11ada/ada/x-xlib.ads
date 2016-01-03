-- $Source: /home/harp/1/proto/monoBANK/xbind/x-xlib.ads,v $ 
-- $Revision: 1.19 $ $Date: 96/10/21 12:54:18 $ $Author: mg $ 

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
-- see the accompanying file Xlib.h.
-- --------------------------------------------------------------------------

with X;
with Interfaces.C;
with Stdarg;
with System;
with X.Strings;

package X.Xlib is

    subtype Char_Array is Interfaces.C.Char_Array;
    Nul: Interfaces.C.Char renames Interfaces.C.Nul;
    -- use type Interfaces.C.Char_Array;                       -- to get "&"
    function "&" (S: Interfaces.C.Char_Array; C: Interfaces.C.Char)
	return Interfaces.C.Char_Array renames Interfaces.C."&";

    use type Interfaces.C.Int;                              -- to get "+"

    XlibSpecificationRelease: constant := 5;                -- Xlib.h:31
    True                    : constant := 1;                -- Xlib.h:71
    False                   : constant := 0;                -- Xlib.h:72
    QueuedAlready           : constant := 0;                -- Xlib.h:74
    QueuedAfterReading      : constant := 1;                -- Xlib.h:75
    QueuedAfterFlush        : constant := 2;                -- Xlib.h:76
    AllPlanes               : constant Unsigned_Long := Unsigned_Long'Last;
                                                            -- Xlib.h:86
    XIMPreeditArea          : constant := 16#1#;            -- Xlib.h:1076
    XIMPreeditCallbacks     : constant := 16#2#;            -- Xlib.h:1077
    XIMPreeditPosition      : constant := 16#4#;            -- Xlib.h:1078
    XIMPreeditNothing       : constant := 16#8#;            -- Xlib.h:1079
    XIMPreeditNone          : constant := 16#10#;           -- Xlib.h:1080
    XIMStatusArea           : constant := 16#100#;          -- Xlib.h:1081
    XIMStatusCallbacks      : constant := 16#200#;          -- Xlib.h:1082
    XIMStatusNothing        : constant := 16#400#;          -- Xlib.h:1083
    XIMStatusNone           : constant := 16#800#;          -- Xlib.h:1084
    XNVaNestedList          : constant Char_Array := "XNVaNestedList" & Nul;
                                                            -- Xlib.h:1086
    XNQueryInputStyle       : constant Char_Array := "queryInputStyle" & Nul;
                                                            -- Xlib.h:1087
    XNClientWindow          : constant Char_Array := "clientWindow" & Nul;
                                                            -- Xlib.h:1088
    XNInputStyle            : constant Char_Array := "inputStyle" & Nul;
                                                            -- Xlib.h:1089
    XNFocusWindow           : constant Char_Array := "focusWindow" & Nul;
                                                            -- Xlib.h:1090
    XNResourceName          : constant Char_Array := "resourceName" & Nul;
                                                            -- Xlib.h:1091
    XNResourceClass         : constant Char_Array := "resourceClass" & Nul;
                                                            -- Xlib.h:1092
    XNGeometryCallback      : constant Char_Array := "geometryCallback" & Nul;
                                                            -- Xlib.h:1093
    XNFilterEvents          : constant Char_Array := "filterEvents" & Nul;
                                                            -- Xlib.h:1094
    XNPreeditStartCallback  : constant Char_Array := "preeditStartCallback" 
                                                     & Nul; -- Xlib.h:1095
    XNPreeditDoneCallback   : constant Char_Array := "preeditDoneCallback" 
                                                     & Nul; -- Xlib.h:1096
    XNPreeditDrawCallback   : constant Char_Array := "preeditDrawCallback" 
                                                     & Nul; -- Xlib.h:1097
    XNPreeditCaretCallback  : constant Char_Array := "preeditCaretCallback" 
                                                     & Nul; -- Xlib.h:1098
    XNPreeditAttributes     : constant Char_Array := "preeditAttributes" & Nul;
                                                            -- Xlib.h:1099
    XNStatusStartCallback   : constant Char_Array := "statusStartCallback" 
                                                     & Nul; -- Xlib.h:1100
    XNStatusDoneCallback    : constant Char_Array := "statusDoneCallback" & Nul;
                                                            -- Xlib.h:1101
    XNStatusDrawCallback    : constant Char_Array := "statusDrawCallback" & Nul;
                                                            -- Xlib.h:1102
    XNStatusAttributes      : constant Char_Array := "statusAttributes" & Nul;
                                                            -- Xlib.h:1103
    XNArea                  : constant Char_Array := "area" & Nul;
                                                            -- Xlib.h:1104
    XNAreaNeeded            : constant Char_Array := "areaNeeded" & Nul;
                                                            -- Xlib.h:1105
    XNSpotLocation          : constant Char_Array := "spotLocation" & Nul;
                                                            -- Xlib.h:1106
    XNColormap              : constant Char_Array := "colorMap" & Nul;
                                                            -- Xlib.h:1107
    XNStdColormap           : constant Char_Array := "stdColorMap" & Nul;
                                                            -- Xlib.h:1108
    XNForeground            : constant Char_Array := "foreground" & Nul;
                                                            -- Xlib.h:1109
    XNBackground            : constant Char_Array := "background" & Nul;
                                                            -- Xlib.h:1110
    XNBackgroundPixmap      : constant Char_Array := "backgroundPixmap" & Nul;
                                                            -- Xlib.h:1111
    XNFontSet               : constant Char_Array := "fontSet" & Nul;
                                                            -- Xlib.h:1112
    XNLineSpace             : constant Char_Array := "lineSpace" & Nul;
                                                            -- Xlib.h:1113
    XNCursor                : constant Char_Array := "cursor" & Nul;
                                                            -- Xlib.h:1114
    XBufferOverflow         : constant := -1;               -- Xlib.h:1116
    XLookupNone             : constant := 1;                -- Xlib.h:1117
    XLookupChars            : constant := 2;                -- Xlib.h:1118
    XLookupKeySym           : constant := 3;                -- Xlib.h:1119
    XLookupBoth             : constant := 4;                -- Xlib.h:1120
    XIMReverse              : constant := 1;                -- Xlib.h:1135
    XIMUnderline            : constant := 2;                -- Xlib.h:1136
    XIMHighlight            : constant := 4;                -- Xlib.h:1137
    XIMPrimary              : constant := 32;               -- Xlib.h:1138
    XIMSecondary            : constant := 64;               -- Xlib.h:1139
    XIMTertiary             : constant := 128;              -- Xlib.h:1140

    subtype Bool is X.Signed_Int;                           -- Xlib.h:69
    subtype Status is Interfaces.C.Int;                     -- Xlib.h:70

    type XIMStyle is new X.unsigned_long;                   -- Xlib.h:1069
    type XIMStyle_access is access all XIMStyle;            -- Xlib.h:1069
    type XIMFeedback is new X.unsigned_long;                -- Xlib.h:1133
    type XIMFeedback_access is access all XIMFeedback;      -- Xlib.h:1133

    type XIMCaretDirection is (                             -- Xlib.h:1167
        XIMForwardChar,                                     -- Xlib.h:1160
        XIMBackwardChar,                                    -- Xlib.h:1160
        XIMForwardWord,                                     -- Xlib.h:1161
        XIMBackwardWord,                                    -- Xlib.h:1161
        XIMCaretUp,                                         -- Xlib.h:1162
        XIMCaretDown,                                       -- Xlib.h:1162
        XIMNextLine,                                        -- Xlib.h:1163
        XIMPreviousLine,                                    -- Xlib.h:1163
        XIMLineStart,                                       -- Xlib.h:1164
        XIMLineEnd,                                         -- Xlib.h:1164
        XIMAbsolutePosition,                                -- Xlib.h:1165
        XIMDontChange                                       -- Xlib.h:1167
    );
    for XIMCaretDirection'Size use X.Int'Size;              -- Xlib.h:1167

    type XIMCaretStyle is (                                 -- Xlib.h:1173
        XIMIsInvisible,                                     -- Xlib.h:1170
        XIMIsPrimary,                                       -- Xlib.h:1171
        XIMIsSecondary                                      -- Xlib.h:1173
    );
    for XIMCaretStyle'Size use X.Int'Size;                  -- Xlib.h:1173

    type XIMStatusDataType is (                             -- Xlib.h:1184
        XIMTextType,                                        -- Xlib.h:1182
        XIMBitmapType                                       -- Xlib.h:1184
    );
    for XIMStatusDataType'Size use X.Int'Size;              -- Xlib.h:1184

    type XPointer is access all X.signed_char;              -- Xlib.h:67
    type KeyCode_access is access all X.KeyCode;            -- Xlib.h:447
    type KeySym_access is access all X.KeySym;              -- Xlib.h:495
    type Atom_access is access all X.Atom;
    type Window_access is access all X.Window;

    type Char32 is                                          -- Xlib.h:433
        array(integer range 0..31)
        of aliased X.signed_char;

    type Char20 is                                          -- Xlib.h:894
        array(integer range 0..19)
        of aliased X.signed_char;

    type Short10 is                                         -- Xlib.h:895
        array(integer range 0..9)
        of aliased X.signed_short;

    type Long5 is                                           -- Xlib.h:896
        array(integer range 0..4)
        of aliased X.long;

    type XKeytrans is private;                              -- Xlib.h:514
    type XSQEvent is private;                               -- Xlib.h:476
    type XrmHashBucketRec is private;                       -- Xlib.h:485
    type XIMFilter is private;                              -- Xlib.h:536
    type XFreeFuncs is private;                             -- Xlib.h:457
    type XExten is private;                                 -- Xlib.h:502
    type XDisplayAtoms is private;                          -- Xlib.h:516
    type XContextDB is private;                             -- Xlib.h:524
    type XFontSet_rec is private;                           -- Xlib.h:1048
    type XIM_rec is private;                                -- Xlib.h:1066
    type XIC_rec is private;                                -- Xlib.h:1067


    type XExtData;                                          -- Xlib.h:137
    type XExtCodes;                                         -- Xlib.h:150
    type XPixmapFormatValues;                               -- Xlib.h:160
    type XGC;                                               -- Xlib.h:199
    type Visual;                                            -- Xlib.h:223
    type Depth;                                             -- Xlib.h:232
    type Screen;                                            -- Xlib.h:257
    type ScreenFormat;                                      -- Xlib.h:267
    type XHostAddress;                                      -- Xlib.h:327
    type XTimeCoord;                                        -- Xlib.h:441
    type XModifierKeymap;                                   -- Xlib.h:448
    type XCharStruct;                                       -- Xlib.h:983
    type XFontProp;                                         -- Xlib.h:992
    type XFontStruct;                                       -- Xlib.h:1011
    type XChar2b;                                           -- Xlib.h:1026
    type XFontSetExtents;                                   -- Xlib.h:1046
    type XIMText;                                           -- Xlib.h:1142
    type XDisplay;                                          -- Xlib.h:241
    type XImage;                                            -- Xlib.h:349

    type GC is access all XGC;                              -- Xlib.h:206
    type Visual_access is access all Visual;                -- Xlib.h:231
    type XDisplay_access is access all XDisplay;            -- Xlib.h:241
    type Depth_access is access all Depth;                  -- Xlib.h:246
    type Screen_access is access all Screen;                -- Xlib.h:315
    type ScreenFormat_access is access all ScreenFormat;    -- Xlib.h:473
    type XModifierKeymap_access is access all XModifierKeymap;
                                                            -- Xlib.h:496
    type XFontProp_access is access all XFontProp;          -- Xlib.h:1005
    type XKeytrans_access is access all XKeytrans;          -- Xlib.h:514
    type XSQEvent_access is access all XSQEvent;            -- Xlib.h:476
    type XrmHashBucketRec_access is access all XrmHashBucketRec;
                                                            -- Xlib.h:485
    type XIMFilter_access is access all XIMFilter;          -- Xlib.h:536
    type XFreeFuncs_access is access all XFreeFuncs;        -- Xlib.h:457
    type XExten_access is access all XExten;                -- Xlib.h:502
    type XDisplayAtoms_access is access all XDisplayAtoms;  -- Xlib.h:516
    type XContextDB_access is access all XContextDB;        -- Xlib.h:524
    type XCharStruct_access is access all XCharStruct;      -- Xlib.h:1008
    type XChar2b_access is access all XChar2b;              -- Xlib.h:1029
    type XFontStruct_access is access all XFontStruct;      -- Xlib.h:1041
    type XFontStruct_access_access is access all XFontStruct_access;
                                                            -- Xlib.h:1529
    type XFontSet is access all XFontSet_rec;               -- Xlib.h:1048
    type XIM is access all XIM_rec;                         -- Xlib.h:1066
    type XIC is access all XIC_rec;                         -- Xlib.h:1067
    type XIMText_access is access all XIMText;              -- Xlib.h:1156
    type XTimeCoord_access is access all XTimeCoord;        -- Xlib.h:1219
    type Ximage_access is access all XImage;                -- Xlib.h:1270
    type XHostAddress_access is access all XHostAddress;    -- Xlib.h:1555
    type XExtCodes_access is access all XExtCodes;          -- Xlib.h:1623
    type XExtData_access is access all XExtData;            -- Xlib.h:137
    type XExtData_access_access is access all XExtData_access;
                                                            -- Xlib.h:1635
    type XPixmapFormatValues_access is access all XPixmapFormatValues;
                                                            -- Xlib.h:1799
    type XFontSetExtents_access is access all XFontSetExtents;
                                                            -- Xlib.h:4046


    type af_138_free_private is access function  return X.signed_int;
                                                            -- Xlib.h:138

    type af_349_create_image is access function  return Ximage_access;
                                                            -- Xlib.h:349


    type af_351_destroy_image is access function (
                Image: access XImage)
               return X.signed_int;                         -- Xlib.h:351


    type af_352_get_pixel is access function (
                Image: access XImage;
                XX   : X.signed_int;
                Y    : X.signed_int)
               return X.unsigned_long;                      -- Xlib.h:352


    type af_353_put_pixel is access function (
                Image: access XImage;
                XX   : X.signed_int;
                Y    : X.signed_int;
                Pixel: X.unsigned_long)
               return X.signed_int;                         -- Xlib.h:353


    type af_354_sub_image is access function (
                Image : access XImage;
                XX    : X.signed_int;
                Y     : X.signed_int;
                Width : X.unsigned_int;
                Height: X.unsigned_int)
               return Ximage_access;                        -- Xlib.h:354


    type af_355_add_pixel is access function (
                Image: access XImage;
                Value: X.long)
               return X.signed_int;                         -- Xlib.h:355


    type af_519_old_handler is access function  return X.signed_int;
                                                            -- Xlib.h:519


    type af_467_resource_alloc is access function (
                Dpy: access XDisplay) 
               return X.XID;                                -- Xlib.h:467


    type af_486_synchandler is access function  return X.signed_int;
                                                            -- Xlib.h:486


    type XExtData is                                        -- Xlib.h:137
        record
            number      : X.signed_int;                     -- Xlib.h:136
            next        : XExtData_access;                  -- Xlib.h:137
            free_private: af_138_free_private;              -- Xlib.h:138
            private_data: XPointer;                         -- Xlib.h:139
        end record;

    type XExtCodes is                                       -- Xlib.h:150
        record
            extension   : X.signed_int;                     -- Xlib.h:146
            major_opcode: X.signed_int;                     -- Xlib.h:147
            first_event : X.signed_int;                     -- Xlib.h:148
            first_error : X.signed_int;                     -- Xlib.h:149
        end record;

    type XPixmapFormatValues is                             -- Xlib.h:160
        record
            depth         : X.signed_int;                   -- Xlib.h:157
            bits_per_pixel: X.signed_int;                   -- Xlib.h:158
            scanline_pad  : X.signed_int;                   -- Xlib.h:159
        end record;

    type XGCValues is                                       -- Xlib.h:192
        record
            c_function        : X.signed_int;               -- Xlib.h:167
            plane_mask        : X.unsigned_long;            -- Xlib.h:168
            foreground        : X.unsigned_long;            -- Xlib.h:169
            background        : X.unsigned_long;            -- Xlib.h:170
            line_width        : X.signed_int;               -- Xlib.h:171
            line_style        : X.signed_int;               -- Xlib.h:172
            cap_style         : X.signed_int;               -- Xlib.h:173
            join_style        : X.signed_int;               -- Xlib.h:175
            fill_style        : X.signed_int;               -- Xlib.h:176
            fill_rule         : X.signed_int;               -- Xlib.h:178
            arc_mode          : X.signed_int;               -- Xlib.h:179
            tile              : X.Pixmap;                   -- Xlib.h:180
            stipple           : X.Pixmap;                   -- Xlib.h:181
            ts_x_origin       : X.signed_int;               -- Xlib.h:182
            ts_y_origin       : X.signed_int;               -- Xlib.h:183
            font              : X.Font;                     -- Xlib.h:184
            subwindow_mode    : X.signed_int;               -- Xlib.h:185
            graphics_exposures: Bool;                       -- Xlib.h:186
            clip_x_origin     : X.signed_int;               -- Xlib.h:187
            clip_y_origin     : X.signed_int;               -- Xlib.h:188
            clip_mask         : X.Pixmap;                   -- Xlib.h:189
            dash_offset       : X.signed_int;               -- Xlib.h:190
            dashes            : X.signed_char;              -- Xlib.h:191
        end record;

    type XGC is                                             -- Xlib.h:199
        record
            ext_data: XExtData_access;                      -- Xlib.h:200
            gid     : X.GContext;                           -- Xlib.h:201
            rects   : Bool;                                 -- Xlib.h:202
            dashes  : Bool;                                 -- Xlib.h:203
            dirty   : X.unsigned_long;                      -- Xlib.h:204
            values  : XGCValues;                            -- Xlib.h:205
        end record;

    type Visual is                                          -- Xlib.h:223
        record
            ext_data    : XExtData_access;                  -- Xlib.h:213
            visualid    : X.VisualID;                       -- Xlib.h:214
            class       : X.signed_int;                     -- Xlib.h:218
            red_mask    : X.unsigned_long;                  -- Xlib.h:220
            green_mask  : X.unsigned_int;                   -- Xlib.h:220
            blue_mask   : X.unsigned_int;                   -- Xlib.h:220
            bits_per_rgb: X.signed_int;                     -- Xlib.h:221
            map_entries : X.signed_int;                     -- Xlib.h:222
        end record;

    type Depth is                                           -- Xlib.h:232
        record
            depth   : X.signed_int;                         -- Xlib.h:229
            nvisuals: X.signed_int;                         -- Xlib.h:230
            visuals : Visual_access;                        -- Xlib.h:231
        end record;

    type Screen is                                          -- Xlib.h:257
        record
            ext_data       : XExtData_access;               -- Xlib.h:240
            display        : XDisplay_access;               -- Xlib.h:241
            root           : X.Window;                      -- Xlib.h:242
            width          : X.signed_int;                  -- Xlib.h:243
            height         : X.signed_int;                  -- Xlib.h:243
            mwidth         : X.signed_int;                  -- Xlib.h:244
            mheight        : X.signed_int;                  -- Xlib.h:244
            ndepths        : X.signed_int;                  -- Xlib.h:245
            depths         : Depth_access;                  -- Xlib.h:246
            root_depth     : X.signed_int;                  -- Xlib.h:247
            root_visual    : Visual_access;                 -- Xlib.h:248
            default_gc     : GC;                            -- Xlib.h:249
            cmap           : X.Colormap;                    -- Xlib.h:250
            white_pixel    : X.unsigned_long;               -- Xlib.h:251
            black_pixel    : X.unsigned_long;               -- Xlib.h:252
            max_maps       : X.signed_int;                  -- Xlib.h:253
            min_maps       : X.signed_int;                  -- Xlib.h:253
            backing_store  : X.signed_int;                  -- Xlib.h:254
            save_unders    : Bool;                          -- Xlib.h:255
            root_input_mask: X.long;                        -- Xlib.h:256
        end record;

    type ScreenFormat is                                    -- Xlib.h:267
        record
            ext_data      : XExtData_access;                -- Xlib.h:263
            depth         : X.signed_int;                   -- Xlib.h:264
            bits_per_pixel: X.signed_int;                   -- Xlib.h:265
            scanline_pad  : X.signed_int;                   -- Xlib.h:266
        end record;

    type XSetWindowAttributes is                            -- Xlib.h:288
        record
            background_pixmap    : X.Pixmap;                -- Xlib.h:273
            background_pixel     : X.unsigned_long;         -- Xlib.h:274
            border_pixmap        : X.Pixmap;                -- Xlib.h:275
            border_pixel         : X.unsigned_int;          -- Xlib.h:276
            bit_gravity          : X.signed_int;            -- Xlib.h:277
            win_gravity          : X.signed_int;            -- Xlib.h:278
            backing_store        : X.signed_int;            -- Xlib.h:279
            backing_planes       : X.unsigned_long;         -- Xlib.h:280
            backing_pixel        : X.unsigned_long;         -- Xlib.h:281
            save_under           : Bool;                    -- Xlib.h:282
            event_mask           : X.long;                  -- Xlib.h:283
            do_not_propagate_mask: X.long;                  -- Xlib.h:284
            override_redirect    : Bool;                    -- Xlib.h:285
            colormap             : X.Colormap;              -- Xlib.h:286
            cursor               : X.Cursor;                -- Xlib.h:287
        end record;

    type XWindowAttributes is                               -- Xlib.h:316
        record
            xx                   : X.signed_int;            -- Xlib.h:291
            y                    : X.signed_int;            -- Xlib.h:291
            width                : X.signed_int;            -- Xlib.h:292
            height               : X.signed_int;            -- Xlib.h:292
            border_width         : X.signed_int;            -- Xlib.h:293
            depth                : X.signed_int;            -- Xlib.h:294
            visual               : Visual_access;           -- Xlib.h:295
            root                 : X.Window;                -- Xlib.h:296
            class                : X.signed_int;            -- Xlib.h:300
            bit_gravity          : X.signed_int;            -- Xlib.h:302
            win_gravity          : X.signed_int;            -- Xlib.h:303
            backing_store        : X.signed_int;            -- Xlib.h:304
            backing_planes       : X.unsigned_long;         -- Xlib.h:305
            backing_pixel        : X.unsigned_long;         -- Xlib.h:306
            save_under           : Bool;                    -- Xlib.h:307
            colormap             : X.Colormap;              -- Xlib.h:308
            map_installed        : Bool;                    -- Xlib.h:309
            map_state            : X.signed_int;            -- Xlib.h:310
            all_event_masks      : X.long;                  -- Xlib.h:311
            your_event_mask      : X.long;                  -- Xlib.h:312
            do_not_propagate_mask: X.long;                  -- Xlib.h:313
            override_redirect    : Bool;                    -- Xlib.h:314
            screen               : Screen_access;           -- Xlib.h:315
        end record;

    type XHostAddress is                                    -- Xlib.h:327
        record
            family : X.signed_int;                          -- Xlib.h:324
            length : X.signed_int;                          -- Xlib.h:325
            address: X.Strings.charp;                       -- Xlib.h:326
        end record;

    type funcs is                                           -- Xlib.h:348
        record
            create_image : af_349_create_image;             -- Xlib.h:349
            destroy_image: af_351_destroy_image;            -- Xlib.h:351
            get_pixel    : af_352_get_pixel;                -- Xlib.h:352
            put_pixel    : af_353_put_pixel;                -- Xlib.h:353
            sub_image    : af_354_sub_image;                -- Xlib.h:354
            add_pixel    : af_355_add_pixel;                -- Xlib.h:355
        end record;

    type XWindowChanges is                                  -- Xlib.h:375
        record
            xx          : X.signed_int;                     -- Xlib.h:370
            y           : X.signed_int;                     -- Xlib.h:370
            width       : X.signed_int;                     -- Xlib.h:371
            height      : X.signed_int;                     -- Xlib.h:371
            border_width: X.signed_int;                     -- Xlib.h:372
            sibling     : X.Window;                         -- Xlib.h:373
            stack_mode  : X.signed_int;                     -- Xlib.h:374
        end record;

    type XColor is                                          -- Xlib.h:385
        record
            pixel: X.unsigned_long;                         -- Xlib.h:381
            red  : X.unsigned_short;                        -- Xlib.h:382
            green: X.unsigned_short;                        -- Xlib.h:382
            blue : X.unsigned_short;                        -- Xlib.h:382
            flags: X.signed_char;                           -- Xlib.h:383
            pad  : X.signed_char;                           -- Xlib.h:384
        end record;

    type XSegment is                                        -- Xlib.h:394
        record
            x1: X.signed_short;                             -- Xlib.h:393
            y1: X.signed_short;                             -- Xlib.h:393
            x2: X.signed_short;                             -- Xlib.h:393
            y2: X.signed_short;                             -- Xlib.h:393
        end record;

    type XPoint is                                          -- Xlib.h:398
        record
            XX: X.signed_short;                             -- Xlib.h:397
            y : X.signed_short;                             -- Xlib.h:397
        end record;

    type XRectangle is                                      -- Xlib.h:403
        record
            xx    : X.signed_short;                         -- Xlib.h:401
            y     : X.signed_short;                         -- Xlib.h:401
            width : X.unsigned_short;                       -- Xlib.h:402
            height: X.unsigned_short;                       -- Xlib.h:402
        end record;

    type XArc is                                            -- Xlib.h:409
        record
            xx    : X.signed_short;                         -- Xlib.h:406
            y     : X.signed_short;                         -- Xlib.h:406
            width : X.unsigned_short;                       -- Xlib.h:407
            height: X.unsigned_short;                       -- Xlib.h:407
            angle1: X.signed_short;                         -- Xlib.h:408
            angle2: X.signed_short;                         -- Xlib.h:408
        end record;

    type XKeyboardControl is                                -- Xlib.h:423
        record
            key_click_percent: X.signed_int;                -- Xlib.h:415
            bell_percent     : X.signed_int;                -- Xlib.h:416
            bell_pitch       : X.signed_int;                -- Xlib.h:417
            bell_duration    : X.signed_int;                -- Xlib.h:418
            led              : X.signed_int;                -- Xlib.h:419
            led_mode         : X.signed_int;                -- Xlib.h:420
            key              : X.signed_int;                -- Xlib.h:421
            auto_repeat_mode : X.signed_int;                -- Xlib.h:422
        end record;

    type XKeyboardState is                                  -- Xlib.h:434
        record
            key_click_percent : X.signed_int;               -- Xlib.h:428
            bell_percent      : X.signed_int;               -- Xlib.h:429
            bell_pitch        : X.unsigned_int;             -- Xlib.h:430
            bell_duration     : X.unsigned_int;             -- Xlib.h:430
            led_mask          : X.unsigned_long;            -- Xlib.h:431
            global_auto_repeat: X.signed_int;               -- Xlib.h:432
            auto_repeats      : Char32;                     -- Xlib.h:433
        end record;

    type XTimeCoord is                                      -- Xlib.h:441
        record
            time: X.Time;                                   -- Xlib.h:439
            xx  : X.signed_short;                           -- Xlib.h:440
            y   : X.signed_short;                           -- Xlib.h:440
        end record;

    type XModifierKeymap is                                 -- Xlib.h:448
        record
            max_keypermod: X.signed_int;                    -- Xlib.h:446
            modifiermap  : KeyCode_access;                  -- Xlib.h:447
        end record;

    -- ********
    -- X events
    -- ********

    type XKeyEvent is                                       -- Xlib.h:560
        record
            window     : X.Window;                          -- Xlib.h:551
            root       : X.Window;                          -- Xlib.h:552
            subwindow  : X.Window;                          -- Xlib.h:553
            time       : X.Time;                            -- Xlib.h:554
            xx         : X.signed_int;                      -- Xlib.h:555
            y          : X.signed_int;                      -- Xlib.h:555
            x_root     : X.signed_int;                      -- Xlib.h:556
            y_root     : X.signed_int;                      -- Xlib.h:556
            state      : X.unsigned_int;                    -- Xlib.h:557
            keycode    : X.unsigned_int;                    -- Xlib.h:558
            same_screen: Bool;                              -- Xlib.h:559
        end record;

    subtype XKeyPressedEvent is XKeyEvent;                  -- Xlib.h:561

    subtype XKeyReleasedEvent is XKeyEvent;                 -- Xlib.h:562

    type XButtonEvent is                                    -- Xlib.h:578
        record
            window     : X.Window;                          -- Xlib.h:569
            root       : X.Window;                          -- Xlib.h:570
            subwindow  : X.Window;                          -- Xlib.h:571
            time       : X.Time;                            -- Xlib.h:572
            xx         : X.signed_int;                      -- Xlib.h:573
            y          : X.signed_int;                      -- Xlib.h:573
            x_root     : X.signed_int;                      -- Xlib.h:574
            y_root     : X.signed_int;                      -- Xlib.h:574
            state      : X.unsigned_int;                    -- Xlib.h:575
            button     : X.unsigned_int;                    -- Xlib.h:576
            same_screen: Bool;                              -- Xlib.h:577
        end record;

    subtype XButtonPressedEvent is XButtonEvent;            -- Xlib.h:579

    subtype XButtonReleasedEvent is XButtonEvent;           -- Xlib.h:580

    type XMotionEvent is                                    -- Xlib.h:596
        record
            window     : X.Window;                          -- Xlib.h:587
            root       : X.Window;                          -- Xlib.h:588
            subwindow  : X.Window;                          -- Xlib.h:589
            time       : X.Time;                            -- Xlib.h:590
            xx         : X.signed_int;                      -- Xlib.h:591
            y          : X.signed_int;                      -- Xlib.h:591
            x_root     : X.signed_int;                      -- Xlib.h:592
            y_root     : X.signed_int;                      -- Xlib.h:592
            state      : X.unsigned_int;                    -- Xlib.h:593
            is_hint    : X.signed_char;                     -- Xlib.h:594
            same_screen: Bool;                              -- Xlib.h:595
        end record;

    subtype XPointerMovedEvent is XMotionEvent;             -- Xlib.h:597

    type XCrossingEvent is                                  -- Xlib.h:619
        record
            window     : X.Window;                          -- Xlib.h:604
            root       : X.Window;                          -- Xlib.h:605
            subwindow  : X.Window;                          -- Xlib.h:606
            time       : X.Time;                            -- Xlib.h:607
            xx         : X.signed_int;                      -- Xlib.h:608
            y          : X.signed_int;                      -- Xlib.h:608
            x_root     : X.signed_int;                      -- Xlib.h:609
            y_root     : X.signed_int;                      -- Xlib.h:609
            mode       : X.signed_int;                      -- Xlib.h:610
            detail     : X.signed_int;                      -- Xlib.h:611
            same_screen: Bool;                              -- Xlib.h:616
            focus      : Bool;                              -- Xlib.h:617
            state      : X.unsigned_int;                    -- Xlib.h:618
        end record;

    subtype XEnterWindowEvent is XCrossingEvent;            -- Xlib.h:620

    subtype XLeaveWindowEvent is XCrossingEvent;            -- Xlib.h:621

    type XFocusChangeEvent is                               -- Xlib.h:636
        record
            window    : X.Window;                           -- Xlib.h:628
            mode      : X.signed_int;                       -- Xlib.h:629
            detail    : X.signed_int;                       -- Xlib.h:630
        end record;

    subtype XFocusInEvent is XFocusChangeEvent;             -- Xlib.h:637

    subtype XFocusOutEvent is XFocusChangeEvent;            -- Xlib.h:638

    type XKeymapEvent is                                    -- Xlib.h:648
        record
            window    : X.Window;                           -- Xlib.h:646
            key_vector: Char32;                             -- Xlib.h:647
        end record;

    type XExposeEvent is                                    -- Xlib.h:659
        record
            window    : X.Window;                           -- Xlib.h:655
            xx        : X.signed_int;                       -- Xlib.h:656
            y         : X.signed_int;                       -- Xlib.h:656
            width     : X.signed_int;                       -- Xlib.h:657
            height    : X.signed_int;                       -- Xlib.h:657
            count     : X.signed_int;                       -- Xlib.h:658
        end record;

    type XGraphicsExposeEvent is                            -- Xlib.h:672
        record
            drawable  : X.Drawable;                         -- Xlib.h:666
            xx        : X.signed_int;                       -- Xlib.h:667
            y         : X.signed_int;                       -- Xlib.h:667
            width     : X.signed_int;                       -- Xlib.h:668
            height    : X.signed_int;                       -- Xlib.h:668
            count     : X.signed_int;                       -- Xlib.h:669
            major_code: X.signed_int;                       -- Xlib.h:670
            minor_code: X.signed_int;                       -- Xlib.h:671
        end record;

    type XNoExposeEvent is                                  -- Xlib.h:682
        record
            drawable  : X.Drawable;                         -- Xlib.h:679
            major_code: X.signed_int;                       -- Xlib.h:680
            minor_code: X.signed_int;                       -- Xlib.h:681
        end record;

    type XVisibilityEvent is                                -- Xlib.h:691
        record
            window    : X.Window;                           -- Xlib.h:689
            state     : X.signed_int;                       -- Xlib.h:690
        end record;

    type XCreateWindowEvent is                              -- Xlib.h:704
        record
            parent           : X.Window;                    -- Xlib.h:698
            window           : X.Window;                    -- Xlib.h:699
            xx               : X.signed_int;                -- Xlib.h:700
            y                : X.signed_int;                -- Xlib.h:700
            width            : X.signed_int;                -- Xlib.h:701
            height           : X.signed_int;                -- Xlib.h:701
            border_width     : X.signed_int;                -- Xlib.h:702
            override_redirect: Bool;                        -- Xlib.h:703
        end record;

    type XDestroyWindowEvent is                             -- Xlib.h:713
        record
            event     : X.Window;                           -- Xlib.h:711
            window    : X.Window;                           -- Xlib.h:712
        end record;

    type XUnmapEvent is                                     -- Xlib.h:723
        record
            event         : X.Window;                       -- Xlib.h:720
            window        : X.Window;                       -- Xlib.h:721
            from_configure: Bool;                           -- Xlib.h:722
        end record;

    type XMapEvent is                                       -- Xlib.h:733
        record
            event            : X.Window;                    -- Xlib.h:730
            window           : X.Window;                    -- Xlib.h:731
            override_redirect: Bool;                        -- Xlib.h:732
        end record;

    type XMapRequestEvent is                                -- Xlib.h:742
        record
            parent    : X.Window;                           -- Xlib.h:740
            window    : X.Window;                           -- Xlib.h:741
        end record;

    type XReparentEvent is                                  -- Xlib.h:754
        record
            event            : X.Window;                    -- Xlib.h:749
            window           : X.Window;                    -- Xlib.h:750
            parent           : X.Window;                    -- Xlib.h:751
            xx               : X.signed_int;                -- Xlib.h:752
            y                : X.signed_int;                -- Xlib.h:752
            override_redirect: Bool;                        -- Xlib.h:753
        end record;

    type XConfigureEvent is                                 -- Xlib.h:768
        record
            event            : X.Window;                    -- Xlib.h:761
            window           : X.Window;                    -- Xlib.h:762
            xx               : X.signed_int;                -- Xlib.h:763
            y                : X.signed_int;                -- Xlib.h:763
            width            : X.signed_int;                -- Xlib.h:764
            height           : X.signed_int;                -- Xlib.h:764
            border_width     : X.signed_int;                -- Xlib.h:765
            above            : X.Window;                    -- Xlib.h:766
            override_redirect: Bool;                        -- Xlib.h:767
        end record;

    type XGravityEvent is                                   -- Xlib.h:778
        record
            event     : X.Window;                           -- Xlib.h:775
            window    : X.Window;                           -- Xlib.h:776
            xx        : X.signed_int;                       -- Xlib.h:777
            y         : X.signed_int;                       -- Xlib.h:777
        end record;

    type XResizeRequestEvent is                             -- Xlib.h:787
        record
            window    : X.Window;                           -- Xlib.h:785
            width     : X.signed_int;                       -- Xlib.h:786
            height    : X.signed_int;                       -- Xlib.h:786
        end record;

    type XConfigureRequestEvent is                          -- Xlib.h:802
        record
            parent      : X.Window;                         -- Xlib.h:794
            window      : X.Window;                         -- Xlib.h:795
            xx          : X.signed_int;                     -- Xlib.h:796
            y           : X.signed_int;                     -- Xlib.h:796
            width       : X.signed_int;                     -- Xlib.h:797
            height      : X.signed_int;                     -- Xlib.h:797
            border_width: X.signed_int;                     -- Xlib.h:798
            above       : X.Window;                         -- Xlib.h:799
            detail      : X.signed_int;                     -- Xlib.h:800
            value_mask  : X.unsigned_long;                  -- Xlib.h:801
        end record;

    type XCirculateEvent is                                 -- Xlib.h:812
        record
            event     : X.Window;                           -- Xlib.h:809
            window    : X.Window;                           -- Xlib.h:810
            place     : X.signed_int;                       -- Xlib.h:811
        end record;

    type XCirculateRequestEvent is                          -- Xlib.h:822
        record
            parent    : X.Window;                           -- Xlib.h:819
            window    : X.Window;                           -- Xlib.h:820
            place     : X.signed_int;                       -- Xlib.h:821
        end record;

    type XPropertyEvent is                                  -- Xlib.h:833
        record
            window    : X.Window;                           -- Xlib.h:829
            atom      : X.Atom;                             -- Xlib.h:830
            time      : X.Time;                             -- Xlib.h:831
            state     : X.signed_int;                       -- Xlib.h:832
        end record;

    type XSelectionClearEvent is                            -- Xlib.h:843
        record
            window    : X.Window;                           -- Xlib.h:840
            selection : X.Atom;                             -- Xlib.h:841
            time      : X.Time;                             -- Xlib.h:842
        end record;

    type XSelectionRequestEvent is                          -- Xlib.h:856
        record
            owner     : X.Window;                           -- Xlib.h:850
            requestor : X.Window;                           -- Xlib.h:851
            selection : X.Atom;                             -- Xlib.h:852
            target    : X.Atom;                             -- Xlib.h:853
            property  : X.Atom;                             -- Xlib.h:854
            time      : X.Time;                             -- Xlib.h:855
        end record;

    type XSelectionEvent is                                 -- Xlib.h:868
        record
            requestor : X.Window;                           -- Xlib.h:863
            selection : X.Atom;                             -- Xlib.h:864
            target    : X.Atom;                             -- Xlib.h:865
            property  : X.Atom;                             -- Xlib.h:866
            time      : X.Time;                             -- Xlib.h:867
        end record;

    type XColormapEvent is                                  -- Xlib.h:883
        record
            window    : X.Window;                           -- Xlib.h:875
            colormap  : X.Colormap;                         -- Xlib.h:876
            c_new     : Bool;                               -- Xlib.h:880
            state     : X.signed_int;                       -- Xlib.h:882
        end record;

    type MessageEventFormats is (F8, F16, F32);
    for MessageEventFormats use (F8 => 8, F16 => 16, F32 => 32);
    for MessageEventFormats'Size use X.Int'Size;

    type XClientMessageEvent_Inner(format: MessageEventFormats := F8) is 
        record
            case format is
                when F8 =>
                    b: Char20;                              -- Xlib.h:894
                when F16 =>
                    s: Short10;                             -- Xlib.h:895
                when F32 =>
                    l: Long5;                               -- Xlib.h:896
            end case;
        end record;

    -- Not allowed by GNAT, but on the other hand GNAT puts the
    -- discriminant first, which is all that is needed.
    -- for XClientMessageEvent_Inner use                       
        -- record at mod 4;
            -- format       at 0 range 0 .. X.Int'Size-1;
        -- end record;

    type XClientMessageEvent is 
	record
            window      : X.Window;                         -- Xlib.h:890
            message_type: X.Atom;                           -- Xlib.h:891
	    Inner       : XClientMessageEvent_Inner;
        end record;

    type XMappingEvent is                                   -- Xlib.h:910
        record
            window       : X.Window;                        -- Xlib.h:905
            request      : X.signed_int;                    -- Xlib.h:906
            first_keycode: X.signed_int;                    -- Xlib.h:908
            count        : X.signed_int;                    -- Xlib.h:909
        end record;

    type XErrorEvent is                                     -- Xlib.h:920
        record
            Event_Type  : X.signed_int;                     -- Xlib.h:913
            display     : XDisplay_access;                  -- Xlib.h:914
            resourceid  : X.XID;                            -- Xlib.h:915
            serial      : X.unsigned_long;                  -- Xlib.h:916
            error_code  : X.unsigned_char;                  -- Xlib.h:917
            request_code: X.unsigned_char;                  -- Xlib.h:918
            minor_code  : X.unsigned_char;                  -- Xlib.h:919
        end record;

    type XAnyEvent is                                       -- Xlib.h:928
        record
            window    : X.Window;                           -- Xlib.h:927
        end record;

    type Pad20 is array(Integer range 0..19) of aliased X.long;
							    -- Xlib.h:967

    type XEvent (Event_Type: X.signed_int := X.LASTEvent+1) is record
							    -- Xlib.h:934
        serial       : X.unsigned_long;                     -- Xlib.h:902
        send_event   : Bool;                                -- Xlib.h:903
        display      : XDisplay_access;                     -- Xlib.h:904
	case Event_Type is
	    when X.KeyPress | X.KeyRelease =>
		xkey     : XKeyEvent;                       -- Xlib.h:937
	    when X.ButtonPress | X.ButtonRelease =>
		xbutton  : XButtonEvent;                    -- Xlib.h:938
	    when X.MotionNotify =>
		xmotion  : XMotionEvent;                    -- Xlib.h:939
	    when X.EnterNotify | X.LeaveNotify =>
		xcrossing: XCrossingEvent;                  -- Xlib.h:940
	    when X.FocusIn | X.FocusOut =>
		xfocus   : XFocusChangeEvent;               -- Xlib.h:941
	    when X.KeymapNotify =>
		xkeymap  : XKeymapEvent;                    -- Xlib.h:966
	    when X.Expose =>
		xexpose  : XExposeEvent;                    -- Xlib.h:942
	    when X.GraphicsExpose =>
		xgraphicsexpose: XGraphicsExposeEvent;      -- Xlib.h:943
	    when X.NoExpose =>
		xnoexpose: XNoExposeEvent;                  -- Xlib.h:944
	    when X.VisibilityNotify =>
		xvisibility: XVisibilityEvent;              -- Xlib.h:945
	    when X.CreateNotify =>
		xcreatewindow: XCreateWindowEvent;          -- Xlib.h:946
	    when X.DestroyNotify =>
		xdestroywindow: XDestroyWindowEvent;        -- Xlib.h:947
	    when X.UnmapNotify =>
		xunmap   : XUnmapEvent;                     -- Xlib.h:948
	    when X.MapNotify =>
		xmap     : XMapEvent;                       -- Xlib.h:949
	    when X.MapRequest =>
		xmaprequest: XMapRequestEvent;              -- Xlib.h:950
	    when X.ReparentNotify =>
		xreparent: XReparentEvent;                  -- Xlib.h:951
	    when X.ConfigureNotify =>
		xconfigure: XConfigureEvent;                -- Xlib.h:952
	    when X.GravityNotify =>
		xgravity : XGravityEvent;                   -- Xlib.h:953
	    when X.ResizeRequest =>
		xresizerequest: XResizeRequestEvent;        -- Xlib.h:954
	    when X.ConfigureRequest =>
		xconfigurerequest: XConfigureRequestEvent; -- Xlib.h:955
	    when X.CirculateNotify =>
		xcirculate: XCirculateEvent;                -- Xlib.h:956
	    when X.CirculateRequest =>
		xcirculaterequest: XCirculateRequestEvent; -- Xlib.h:957
	    when X.PropertyNotify =>
		xproperty: XPropertyEvent;                  -- Xlib.h:958
	    when X.SelectionClear =>
		xselectionclear: XSelectionClearEvent;      -- Xlib.h:959
	    when X.SelectionRequest =>
		xselectionrequest: XSelectionRequestEvent;   -- Xlib.h:960
	    when X.SelectionNotify =>
		xselection: XSelectionEvent;                -- Xlib.h:961
	    when X.ColormapNotify =>
		xcolormap: XColormapEvent;                  -- Xlib.h:962
	    when X.ClientMessage =>
		xclient  : XClientMessageEvent;             -- Xlib.h:963
	    when X.MappingNotify =>
		xmapping : XMappingEvent;                   -- Xlib.h:964
	    when X.LASTEvent =>
		pad      : pad20;			    -- Xlib.h:967
	    when others =>
		xany     : XAnyEvent;                       -- Xlib.h:936
	end case;
    end record;

    type XEvent_access is access all XEvent;

    -- ***************
    -- end of X events
    -- ***************

    type XCharStruct is                                     -- Xlib.h:983
        record
            lbearing  : X.signed_short;                     -- Xlib.h:977
            rbearing  : X.signed_short;                     -- Xlib.h:978
            width     : X.signed_short;                     -- Xlib.h:979
            ascent    : X.signed_short;                     -- Xlib.h:980
            descent   : X.signed_short;                     -- Xlib.h:981
            attributes: X.unsigned_short;                   -- Xlib.h:982
        end record;

    type XFontProp is                                       -- Xlib.h:992
        record
            name  : X.Atom;                                 -- Xlib.h:990
            card32: X.unsigned_long;                        -- Xlib.h:991
        end record;

    type XFontStruct is                                     -- Xlib.h:1011
        record
            ext_data         : XExtData_access;             -- Xlib.h:995
            fid              : X.Font;                      -- Xlib.h:996
            direction        : X.unsigned_int;              -- Xlib.h:997
            min_char_or_byte2: X.unsigned_int;              -- Xlib.h:998
            max_char_or_byte2: X.unsigned_int;              -- Xlib.h:999
            min_byte1        : X.unsigned_int;              -- Xlib.h:1000
            max_byte1        : X.unsigned_int;              -- Xlib.h:1001
            all_chars_exist  : Bool;                        -- Xlib.h:1002
            default_char     : X.unsigned_int;              -- Xlib.h:1003
            n_properties     : X.signed_int;                -- Xlib.h:1004
            properties       : XFontProp_access;            -- Xlib.h:1005
            min_bounds       : XCharStruct;                 -- Xlib.h:1006
            max_bounds       : XCharStruct;                 -- Xlib.h:1007
            per_char         : XCharStruct_access;          -- Xlib.h:1008
            ascent           : X.signed_int;                -- Xlib.h:1009
            descent          : X.signed_int;                -- Xlib.h:1010
        end record;

    type XTextItem is                                       -- Xlib.h:1021
        record
            chars  : X.Strings.charp;                       -- Xlib.h:1017
            nchars : X.signed_int;                          -- Xlib.h:1018
            c_delta: X.signed_int;                          -- Xlib.h:1019
            font   : X.Font;                                -- Xlib.h:1020
        end record;

    type XChar2b is                                         -- Xlib.h:1026
        record
            byte1: X.unsigned_char;                         -- Xlib.h:1024
            byte2: X.unsigned_char;                         -- Xlib.h:1025
        end record;

    type XTextItem16 is                                     -- Xlib.h:1033
        record
            chars  : XChar2b_access;                        -- Xlib.h:1029
            nchars : X.signed_int;                          -- Xlib.h:1030
            c_delta: X.signed_int;                          -- Xlib.h:1031
            font   : X.Font;                                -- Xlib.h:1032
        end record;

--     type XEDataObject_kind is (                             -- Xlib.h:1041
--         display_kind,
--         gc_kind,
--         visual_kind,
--         screen_kind,
--         pixmap_format_kind,
--         font_kind
--     );
-- 
--     type XEDataObject (Which: XEDataObject_kind := display_kind) is
--                                                             -- Xlib.h:1041
--         record
--             case Which is
--                 when display_kind =>
--                     display : XDisplay_access;                  -- Xlib.h:1036
--                 when gc_kind =>
--                     gc_field: GC;                           -- Xlib.h:1037
--                 when visual_kind =>
--                     visual  : Visual_access;                   -- Xlib.h:1038
--                 when screen_kind =>
--                     screen  : Screen_access;                   -- Xlib.h:1039
--                 when pixmap_format_kind =>
--                     pixmap_format: ScreenFormat_access;        -- Xlib.h:1040
--                 when font_kind =>
--                     font    : XFontStruct_access;           -- Xlib.h:1041
--             end case;
--         end record;
-- 
--     pragma Unchecked_Union(XEDataObject);
--     pragma Convention(C, XEDataObject);

    -- until Unchecked_Union is working:
    type XEDataObject is new System.Address;

    type XFontSetExtents is                                 -- Xlib.h:1046
        record
            max_ink_extent    : XRectangle;                 -- Xlib.h:1044
            max_logical_extent: XRectangle;                 -- Xlib.h:1045
        end record;

    type XmbTextItem is                                     -- Xlib.h:1055
        record
            chars   : X.Strings.charp;                      -- Xlib.h:1051
            nchars  : X.signed_int;                         -- Xlib.h:1052
            c_delta : X.signed_int;                         -- Xlib.h:1053
            font_set: XFontSet;                             -- Xlib.h:1054
        end record;

    type XwcTextItem is                                     -- Xlib.h:1062
        record
            chars   : X.wchar_access;                       -- Xlib.h:1058
            nchars  : X.signed_int;                         -- Xlib.h:1059
            c_delta : X.signed_int;                         -- Xlib.h:1060
            font_set: XFontSet;                             -- Xlib.h:1061
        end record;

    type XIMProc is access procedure ;                      -- Xlib.h:1064


    type XIMStyles is                                       -- Xlib.h:1074
        record
            count_styles    : X.unsigned_short;             -- Xlib.h:1072
            supported_styles: XIMStyle_access;              -- Xlib.h:1073
        end record;

    type XIMCallback is                                     -- Xlib.h:1131
        record
            client_data: XPointer;                          -- Xlib.h:1129
            callback   : XIMProc;                           -- Xlib.h:1130
        end record;

    type XIMText_Inner(encoding_is_wchar: Bool := False) is       
        record
            case encoding_is_wchar is
                when False =>
                    multi_byte: X.Strings.charp;            -- Xlib.h:1147
                when others =>
                    wide_char: X.wchar_access;              -- Xlib.h:1148
            end case;
        end record;

    -- Not allowed by GNAT, but on the other hand GNAT puts the
    -- discriminant first, which is all that is needed.
    -- for XIMText_Inner use                       
        -- record at mod 4;
            -- encoding_is_wchar       at 0 range 0 .. X.Int'Size-1;
        -- end record;

    type XIMText(encoding_is_wchar: Bool := False) is       -- Xlib.h:1142
        record
            length   : X.unsigned_short;                    -- Xlib.h:1143
            feedback : XIMFeedback_access;                  -- Xlib.h:1144
            Inner    : XIMText_Inner;
        end record;

    type XIMPreeditDrawCallbackStruct is                    -- Xlib.h:1152
        record
            caret     : X.signed_int;                       -- Xlib.h:1153
            chg_first : X.signed_int;                       -- Xlib.h:1154
            chg_length: X.signed_int;                       -- Xlib.h:1155
            text      : XIMText_access;                     -- Xlib.h:1156
        end record;

    type XIMPreeditCaretCallbackStruct is                   -- Xlib.h:1175
        record
            position : X.signed_int;                        -- Xlib.h:1176
            direction: XIMCaretDirection;                   -- Xlib.h:1177
            style    : XIMCaretStyle;                       -- Xlib.h:1178
        end record;

    type XIMStatusDrawCallbackStruct(c_type: XIMStatusDataType := XIMTextType) 
        is record                                           -- Xlib.h:1186
            case c_type is
                when XIMTextType =>
                    text: XIMText_access;                   -- Xlib.h:1189
                when XIMBitmapType =>
                    bitmap: X.Pixmap;                       -- Xlib.h:1190
            end case;
        end record;

    -- Not allowed by GNAT, but on the other hand GNAT puts the
    -- discriminant first, which is all that is needed.
    -- for XIMStatusDrawCallbackStruct use                       
        -- record at mod 4;
            -- c_type at 0 range 0 .. X.Int'Size-1;
        -- end record;

    type XErrorHandler is access function (
                display    : access XDisplay;
                error_event: access XErrorEvent)
               return X.signed_int;                         -- Xlib.h:1767


    type XIOErrorHandler is access function (
                display: access XDisplay)
               return X.signed_int;                         -- Xlib.h:1781


    -- *********************
    -- XDisplay declarations
    -- *********************

    type event_func is access function return Bool;


    type event_vector is                                    -- Xlib.h:511
        array(integer range 0..127)
        of aliased event_func;

    type wire_func is access function return Status;


    type wire_vector is                                     -- Xlib.h:512
        array(integer range 0..127)
        of aliased wire_func;

    type struct_anonymous24_t is                            -- Xlib.h:521
        record
            sequence_number: X.long;                        -- Xlib.h:518
            old_handler    : af_519_old_handler;            -- Xlib.h:519
            succeeded      : Bool;                          -- Xlib.h:520
        end record;

    type error_vector is access all event_vector;           -- Xlib.h:525

    type struct_anonymous26_t is                            -- Xlib.h:534
        record
            defaultCCCs           : XPointer;               -- Xlib.h:530
            clientCmaps           : XPointer;               -- Xlib.h:531
            perVisualIntensityMaps: XPointer;               -- Xlib.h:532
        end record;

    type XDisplay is                                        -- Xlib.h:241
        record
            ext_data             : XExtData_access;         -- Xlib.h:456
            free_funcs           : XFreeFuncs_access;       -- Xlib.h:457
            fd                   : X.signed_int;            -- Xlib.h:458
            lock                 : X.signed_int;            -- Xlib.h:459
            proto_major_version  : X.signed_int;            -- Xlib.h:460
            proto_minor_version  : X.signed_int;            -- Xlib.h:461
            vendor               : X.Strings.charp;         -- Xlib.h:462
            resource_base        : X.XID;                   -- Xlib.h:463
            resource_mask        : X.XID;                   -- Xlib.h:464
            resource_id          : X.XID;                   -- Xlib.h:465
            resource_shift       : X.signed_int;            -- Xlib.h:466
            resource_alloc       : af_467_resource_alloc;   -- Xlib.h:467
            byte_order           : X.signed_int;            -- Xlib.h:468
            bitmap_unit          : X.signed_int;            -- Xlib.h:469
            bitmap_pad           : X.signed_int;            -- Xlib.h:470
            bitmap_bit_order     : X.signed_int;            -- Xlib.h:471
            nformats             : X.signed_int;            -- Xlib.h:472
            pixmap_format        : ScreenFormat_access;     -- Xlib.h:473
            vnumber              : X.signed_int;            -- Xlib.h:474
            release              : X.signed_int;            -- Xlib.h:475
            head                 : XSQEvent_access;         -- Xlib.h:476
            tail                 : XSQEvent_access;         -- Xlib.h:476
            qlen                 : X.signed_int;            -- Xlib.h:477
            last_request_read    : X.unsigned_long;         -- Xlib.h:478
            request              : X.unsigned_long;         -- Xlib.h:479
            last_req             : X.Strings.charp;         -- Xlib.h:480
            buffer               : X.Strings.charp;         -- Xlib.h:481
            bufptr               : X.Strings.charp;         -- Xlib.h:482
            bufmax               : X.Strings.charp;         -- Xlib.h:483
            max_request_size     : X.unsigned_int;          -- Xlib.h:484
            db                   : XrmHashBucketRec_access; -- Xlib.h:485
            synchandler          : af_486_synchandler;      -- Xlib.h:486
            display_name         : X.Strings.charp;         -- Xlib.h:487
            default_screen       : X.signed_int;            -- Xlib.h:488
            nscreens             : X.signed_int;            -- Xlib.h:489
            screens              : Screen_access;           -- Xlib.h:490
            motion_buffer        : X.unsigned_long;         -- Xlib.h:491
            current              : X.Window;                -- Xlib.h:492
            min_keycode          : X.signed_int;            -- Xlib.h:493
            max_keycode          : X.signed_int;            -- Xlib.h:494
            keysyms              : KeySym_access;           -- Xlib.h:495
            modifiermap          : XModifierKeymap_access;  -- Xlib.h:496
            keysyms_per_keycode  : X.signed_int;            -- Xlib.h:497
            xdefaults            : X.Strings.charp;         -- Xlib.h:498
            scratch_buffer       : X.Strings.charp;         -- Xlib.h:499
            scratch_length       : X.unsigned_long;         -- Xlib.h:500
            ext_number           : X.signed_int;            -- Xlib.h:501
            ext_procs            : XExten_access;           -- Xlib.h:502
            event_vec            : aliased event_vector;    -- Xlib.h:511
            wire_vec             : aliased wire_vector;     -- Xlib.h:512
            lock_meaning         : X.KeySym;                -- Xlib.h:513
            key_bindings         : XKeytrans_access;        -- Xlib.h:514
            cursor_font          : X.Font;                  -- Xlib.h:515
            atoms                : XDisplayAtoms_access;    -- Xlib.h:516
            reconfigure_wm_window: struct_anonymous24_t;    -- Xlib.h:521
            flags                : X.unsigned_long;         -- Xlib.h:522
            mode_switch          : X.unsigned_int;          -- Xlib.h:523
            context_db           : XContextDB_access;       -- Xlib.h:524
            error_vec            : error_vector;            -- Xlib.h:525
            cms                  : struct_anonymous26_t;    -- Xlib.h:534
            conn_checker         : X.signed_int;            -- Xlib.h:535
            im_filters           : XIMFilter_access;        -- Xlib.h:536
        end record;

    -- ****************************
    -- end of XDisplay declarations
    -- ****************************

    type XImage is                                          -- Xlib.h:349
        record
            width           : X.signed_int;                 -- Xlib.h:333
            height          : X.signed_int;                 -- Xlib.h:333
            xoffset         : X.signed_int;                 -- Xlib.h:334
            format          : X.signed_int;                 -- Xlib.h:335
            data            : X.Strings.charp;              -- Xlib.h:336
            byte_order      : X.signed_int;                 -- Xlib.h:337
            bitmap_unit     : X.signed_int;                 -- Xlib.h:338
            bitmap_bit_order: X.signed_int;                 -- Xlib.h:339
            bitmap_pad      : X.signed_int;                 -- Xlib.h:340
            depth           : X.signed_int;                 -- Xlib.h:341
            bytes_per_line  : X.signed_int;                 -- Xlib.h:342
            bits_per_pixel  : X.signed_int;                 -- Xlib.h:343
            red_mask        : X.unsigned_long;              -- Xlib.h:344
            green_mask      : X.unsigned_long;              -- Xlib.h:345
            blue_mask       : X.unsigned_long;              -- Xlib.h:346
            obdata          : XPointer;                     -- Xlib.h:347
            f               : funcs;                        -- Xlib.h:363
        end record;

    subtype Display is XDisplay;                            -- Xlib.h:537

    function XLoadQueryFont(
                display: access XDisplay;
                name   : X.Strings.const_charp)
               return XFontStruct_access;                   -- Xlib.h:1196

    pragma Import(C, XLoadQueryFont, "XLoadQueryFont");

    function XLoadQueryFont(
                display: access XDisplay;
                name   : Interfaces.C.Char_Array)
               return XFontStruct_access;                   -- Xlib.h:1196

    pragma Inline(XLoadQueryFont);

    function XQueryFont(
                display: access XDisplay;
                font_ID: X.XID)
               return XFontStruct_access;                   -- Xlib.h:1203

    function XGetMotionEvents(
                display       : access XDisplay;
                w             : X.Window;
                start         : X.Time;
                stop          : X.Time;
                nevents_return: access X.signed_int)
               return XTimeCoord_access;                    -- Xlib.h:1211

    function XDeleteModifiermapEntry(
                modmap       : access XModifierKeymap;
                keycode_entry: X.unsigned_int;
                modifier     : X.signed_int)
               return XModifierKeymap_access;               -- Xlib.h:1221

    function XGetModifierMapping(
                display: access XDisplay)
               return XModifierKeymap_access;               -- Xlib.h:1233

    function XInsertModifiermapEntry(
                modmap       : access XModifierKeymap;
                keycode_entry: X.unsigned_int;
                modifier     : X.signed_int)
               return XModifierKeymap_access;               -- Xlib.h:1239

    function XNewModifiermap(
                max_keys_per_mod: X.signed_int)
               return XModifierKeymap_access;               -- Xlib.h:1251

    function XCreateImage(
                display       : access XDisplay;
                visual        : Visual_access;
                depth         : X.unsigned_int;
                format        : X.signed_int;
                offset        : X.signed_int;
                data          : X.Strings.charp;
                width         : X.unsigned_int;
                height        : X.unsigned_int;
                bitmap_pad    : X.signed_int;
                bytes_per_line: X.signed_int)
               return Ximage_access;                        -- Xlib.h:1257

    pragma Import(C, XCreateImage, "XCreateImage");

    procedure XCreateImage(
                display       : access XDisplay;
                visual        : Visual_access;
                depth         : X.unsigned_int;
                format        : X.signed_int;
                offset        : X.signed_int;
                data          : in out Interfaces.C.Char_Array;
                width         : X.unsigned_int;
                height        : X.unsigned_int;
                bitmap_pad    : X.signed_int;
                bytes_per_line: X.signed_int;
                result        : out XImage_access);         -- Xlib.h:1257

    pragma Inline(XCreateImage);

    function XGetImage(
                display   : access XDisplay;
                d         : X.Drawable;
                xx        : X.signed_int;
                y         : X.signed_int;
                width     : X.unsigned_int;
                height    : X.unsigned_int;
                plane_mask: X.unsigned_long;
                format    : X.signed_int)
               return Ximage_access;                        -- Xlib.h:1271

    function XGetSubImage(
                display   : access XDisplay;
                d         : X.Drawable;
                xx        : X.signed_int;
                y         : X.signed_int;
                width     : X.unsigned_int;
                height    : X.unsigned_int;
                plane_mask: X.unsigned_long;
                format    : X.signed_int;
                dest_image: access XImage;
                dest_x    : X.signed_int;
                dest_y    : X.signed_int)
               return Ximage_access;                        -- Xlib.h:1283

    function XOpenDisplay(
                display_name: X.Strings.const_charp)
               return XDisplay_access;                      -- Xlib.h:1302

    pragma Import(C, XOpenDisplay, "XOpenDisplay");

    function XOpenDisplay(
                display_name: Interfaces.C.Char_Array)
               return XDisplay_access;                      -- Xlib.h:1302

    pragma Inline(XOpenDisplay);

    procedure XrmInitialize;                                -- Xlib.h:1308

    function XFetchBytes(
                display      : access XDisplay;
                nbytes_return: access X.signed_int)
               return X.Strings.charp;                      -- Xlib.h:1314

    function XFetchBuffer(
                display      : access XDisplay;
                nbytes_return: access X.signed_int;
                buffer       : X.signed_int)
               return X.Strings.charp;                      -- Xlib.h:1320

    function XGetAtomName(
                display: access XDisplay;
                atom   : X.Atom)
               return X.Strings.charp;                      -- Xlib.h:1327

    function XGetDefault(
                display: access XDisplay;
                program: X.Strings.const_charp;
                option : X.Strings.const_charp)
               return X.Strings.charp;                      -- Xlib.h:1333

    pragma Import(C, XGetDefault, "XGetDefault");

    function XGetDefault(
                display: access XDisplay;
                program: Interfaces.C.Char_Array;
                option : Interfaces.C.Char_Array)
               return X.Strings.charp;                      -- Xlib.h:1333

    pragma Inline(XGetDefault);

    function XDisplayName(
                string: X.Strings.const_charp)
               return X.Strings.charp;                      -- Xlib.h:1340

    pragma Import(C, XDisplayName, "XDisplayName");

    function XDisplayName(
                string: Interfaces.C.Char_Array)
               return X.Strings.charp;                      -- Xlib.h:1340

    pragma Inline(XDisplayName);

    function XKeysymToString(
                keysym: X.KeySym)
               return X.Strings.charp;                      -- Xlib.h:1345

    type XSynchronize_func is access function (
                display: access XDisplay;
                onoff  : Bool)
               return X.Signed_Int;                         -- Xlib.h:1351

    pragma Convention(C, XSynchronize_func);


    XSynchronize: XSynchronize_func;

    type af_1360_func is access function  
        (display : access XDisplay) return X.signed_int;    -- Xlib.h:1360


    type XSetAfter_func is access function (
                display: access XDisplay;
                func   : af_1360_func)
               return X.signed_int;                         -- Xlib.h:1357

    pragma Convention(C, XSetAfter_func);


    type af_predicate is access function (
                display: access XDisplay;
                event  : access XEvent;
                arg    : XPointer)
               return Bool;                                 -- Xlib.h:2102


    XSetAfterFunction : XSetAfter_func;

    function XInternAtom(
                display       : access XDisplay;
                atom_name     : X.Strings.const_charp;
                only_if_exists: Bool)
               return X.Atom;                               -- Xlib.h:1367

    pragma Import(C, XInternAtom, "XInternAtom");

    function XInternAtom(
                display       : access XDisplay;
                atom_name     : Interfaces.C.Char_Array;
                only_if_exists: X.signed_int)
               return X.Atom;                               -- Xlib.h:1367

    pragma Inline(XInternAtom);

    function XCopyColormapAndFree(
                display : access XDisplay;
                colormap: X.Colormap)
               return X.Colormap;                           -- Xlib.h:1374

    function XCreateColormap(
                display: access XDisplay;
                w      : X.Window;
                visual : Visual_access;
                alloc  : X.signed_int)
               return X.Colormap;                           -- Xlib.h:1380

    function XCreatePixmapCursor(
                display         : access XDisplay;
                source          : X.Pixmap;
                mask            : X.Pixmap;
                foreground_color: access XColor;
                background_color: access XColor;
                xx              : X.unsigned_int;
                y               : X.unsigned_int)
               return X.Cursor;                             -- Xlib.h:1388

    function XCreateGlyphCursor(
                display         : access XDisplay;
                source_font     : X.Font;
                mask_font       : X.Font;
                source_char     : X.unsigned_int;
                mask_char       : X.unsigned_int;
                foreground_color: access XColor;
                background_color: access XColor)
               return X.Cursor;                             -- Xlib.h:1399

    function XCreateFontCursor(
                display: access XDisplay;
                shape  : X.unsigned_int)
               return X.Cursor;                             -- Xlib.h:1410

    function XLoadFont(
                display: access XDisplay;
                name   : X.Strings.const_charp)
               return X.Font;                               -- Xlib.h:1416

    pragma Import(C, XLoadFont, "XLoadFont");

    function XLoadFont(
                display: access XDisplay;
                name   : Interfaces.C.Char_Array)
               return X.Font;                               -- Xlib.h:1416

    pragma Inline(XLoadFont);

    function XCreateGC(
                display  : access XDisplay;
                d        : X.Drawable;
                valuemask: X.unsigned_long;
                values   : access XGCValues)
               return GC;                                   -- Xlib.h:1422

    function XGContextFromGC(
                gc: access XGC)
               return X.GContext;                           -- Xlib.h:1430

    procedure XFlushGC(
                display: access XDisplay;
                gc     : access XGC);                       -- Xlib.h:1435

    function XCreatePixmap(
                display: access XDisplay;
                d      : X.Drawable;
                width  : X.unsigned_int;
                height : X.unsigned_int;
                depth  : X.unsigned_int)
               return X.Pixmap;                             -- Xlib.h:1441

    function XCreateBitmapFromData(
                display: access XDisplay;
                d      : X.Drawable;
                data   : X.Strings.const_charp;
                width  : X.unsigned_int;
                height : X.unsigned_int)
               return X.Pixmap;                             -- Xlib.h:1450

    pragma Import(C, XCreateBitmapFromData, "XCreateBitmapFromData");

    function XCreateBitmapFromData(
                display: access XDisplay;
                d      : X.Drawable;
                data   : Interfaces.C.Char_Array;
                width  : X.unsigned_int;
                height : X.unsigned_int)
               return X.Pixmap;                             -- Xlib.h:1450

    pragma Inline(XCreateBitmapFromData);

    function XCreatePixmapFromBitmapData(
                display: access XDisplay;
                d      : X.Drawable;
                data   : X.Strings.charp;
                width  : X.unsigned_int;
                height : X.unsigned_int;
                fg     : X.unsigned_long;
                bg     : X.unsigned_long;
                depth  : X.unsigned_int)
               return X.Pixmap;                             -- Xlib.h:1459

    pragma Import(C, XCreatePixmapFromBitmapData, 
                     "XCreatePixmapFromBitmapData");

    procedure XCreatePixmapFromBitmapData(
                display: access XDisplay;
                d      : X.Drawable;
                data   : in out Interfaces.C.Char_Array;
                width  : X.unsigned_int;
                height : X.unsigned_int;
                fg     : X.unsigned_long;
                bg     : X.unsigned_long;
                depth  : X.unsigned_int;
                result : out X.Pixmap);                     -- Xlib.h:1459

    pragma Inline(XCreatePixmapFromBitmapData);

    function XCreateSimpleWindow(
                display     : access XDisplay;
                parent      : X.Window;
                xx          : X.signed_int;
                y           : X.signed_int;
                width       : X.unsigned_int;
                height      : X.unsigned_int;
                border_width: X.unsigned_int;
                border      : X.unsigned_long;
                background  : X.unsigned_long)
               return X.Window;                             -- Xlib.h:1471

    function XGetSelectionOwner(
                display  : access XDisplay;
                selection: X.Atom)
               return X.Window;                             -- Xlib.h:1484

    function XCreateWindow(
                display     : access XDisplay;
                parent      : X.Window;
                xx          : X.signed_int;
                y           : X.signed_int;
                width       : X.unsigned_int;
                height      : X.unsigned_int;
                border_width: X.unsigned_int;
                depth       : X.signed_int;
                class       : X.unsigned_int;
                visual      : Visual_access;
                valuemask   : X.unsigned_long;
                attributes  : access XSetWindowAttributes)
               return X.Window;                             -- Xlib.h:1490

    type Colormap_access is access all X.Colormap;

    function XListInstalledColormaps(
                display   : access XDisplay;
                w         : X.Window;
                num_return: access X.signed_int)
               return Colormap_access;                      -- Xlib.h:1506

    function XListFonts(
                display            : access XDisplay;
                pattern            : X.Strings.const_charp;
                maxnames           : X.signed_int;
                actual_count_return: access X.signed_int)
               return X.Strings.charp_vector;               -- Xlib.h:1513

    pragma Import(C, XListFonts, "XListFonts");

    function XListFonts(
                display            : access XDisplay;
                pattern            : Interfaces.C.Char_Array;
                maxnames           : X.signed_int;
                actual_count_return: access X.signed_int)
               return X.Strings.charp_vector;               -- Xlib.h:1513

    pragma Inline(XListFonts);

    function XListFontsWithInfo(
                display     : access XDisplay;
                pattern     : X.Strings.const_charp;
                maxnames    : X.signed_int;
                count_return: access X.signed_int;
                info_return : XFontStruct_access_access)
               return X.Strings.charp_vector;               -- Xlib.h:1521

    pragma Import(C, XListFontsWithInfo, "XListFontsWithInfo");

    function XListFontsWithInfo(
                display     : access XDisplay;
                pattern     : Interfaces.C.Char_Array;
                maxnames    : X.signed_int;
                count_return: access X.signed_int;
                info_return : XFontStruct_access_access)
               return X.Strings.charp_vector;               -- Xlib.h:1521

    pragma Inline(XListFontsWithInfo);

    function XGetFontPath(
                display      : access XDisplay;
                npaths_return: access X.signed_int)
               return X.Strings.charp_vector;               -- Xlib.h:1530

    function XListExtensions(
                display           : access XDisplay;
                nextensions_return: access X.signed_int)
               return X.Strings.charp_vector;               -- Xlib.h:1536

    function XListProperties(
                display        : access XDisplay;
                w              : X.Window;
                num_prop_return: access X.signed_int)
               return Atom_access;                          -- Xlib.h:1542

    function XListHosts(
                display      : access XDisplay;
                nhosts_return: access X.signed_int;
                state_return : access Bool)
               return XHostAddress_access;                  -- Xlib.h:1549

    function XKeycodeToKeysym(
                display: access XDisplay;
                keycode: X.unsigned_int;
                index  : X.signed_int)
               return X.KeySym;                             -- Xlib.h:1556

    function XLookupKeysym_func(
                key_event: access XKeyEvent;
                index    : X.signed_int)
               return X.KeySym;                             -- Xlib.h:1567

    function XGetKeyboardMapping(
                display                   : access XDisplay;
                first_keycode             : X.unsigned_int;
                keycode_count             : X.signed_int;
                keysyms_per_keycode_return: access X.signed_int)
               return KeySym_access;                        -- Xlib.h:1573

    function XStringToKeysym(
                string: X.Strings.const_charp)
               return X.KeySym;                             -- Xlib.h:1585

    pragma Import(C, XStringToKeysym, "XStringToKeysym");

    function XStringToKeysym(
                string: Interfaces.C.Char_Array)
               return X.KeySym;                             -- Xlib.h:1585

    pragma Inline(XStringToKeysym);

    function XMaxRequestSize(
                display: access XDisplay)
               return X.long;                               -- Xlib.h:1590

    function XResourceManagerString(
                display: access XDisplay)
               return X.Strings.charp;                      -- Xlib.h:1595

    function XScreenResourceString(
                screen: Screen_access)
               return X.Strings.charp;                      -- Xlib.h:1600

    function XDisplayMotionBufferSize(
                display: access XDisplay)
               return X.unsigned_long;                      -- Xlib.h:1605

    function XVisualIDFromVisual(
                visual: Visual_access)
               return X.VisualID;                           -- Xlib.h:1610

    function XInitExtension(
                display: access XDisplay;
                name   : X.Strings.const_charp)
               return XExtCodes_access;                     -- Xlib.h:1618

    pragma Import(C, XInitExtension, "XInitExtension");

    function XInitExtension(
                display: access XDisplay;
                name   : Interfaces.C.Char_Array)
               return XExtCodes_access;                     -- Xlib.h:1618

    pragma Inline(XInitExtension);

    function XAddExtension(
                display: access XDisplay)
               return XExtCodes_access;                     -- Xlib.h:1625

    function XFindOnExtensionList(
                structure: access XExtData_access;
                number   : X.signed_int)
               return XExtData_access;                      -- Xlib.h:1630

    function XEHeadOfExtensionList(
                object: XEDataObject)
               return XExtData_access_access;               -- Xlib.h:1636

    function XRootWindow(
                display      : access XDisplay;
                screen_number: X.signed_int)
               return X.Window;                             -- Xlib.h:1643

    function XDefaultRootWindow(
                display: access XDisplay)
               return X.Window;                             -- Xlib.h:1649

    function XRootWindowOfScreen(
                screen: Screen_access)
               return X.Window;                             -- Xlib.h:1654

    function XDefaultVisual(
                display      : access XDisplay;
                screen_number: X.signed_int)
               return Visual_access;                        -- Xlib.h:1659

    function XDefaultVisualOfScreen(
                screen: Screen_access)
               return Visual_access;                        -- Xlib.h:1665

    function XDefaultGC(
                display      : access XDisplay;
                screen_number: X.signed_int)
               return GC;                                   -- Xlib.h:1670

    function XDefaultGCOfScreen(
                screen: Screen_access)
               return GC;                                   -- Xlib.h:1676

    function XBlackPixel(
                display      : access XDisplay;
                screen_number: X.signed_int)
               return X.unsigned_long;                      -- Xlib.h:1681

    function XWhitePixel(
                display      : access XDisplay;
                screen_number: X.signed_int)
               return X.unsigned_long;                      -- Xlib.h:1687

    function XAllPlanes return X.unsigned_long;             -- Xlib.h:1693

    function XBlackPixelOfScreen(
                screen: Screen_access)
               return X.unsigned_long;                      -- Xlib.h:1698

    function XWhitePixelOfScreen(
                screen: Screen_access)
               return X.unsigned_long;                      -- Xlib.h:1703

    function XNextRequest(
                display: access XDisplay)
               return X.unsigned_long;                      -- Xlib.h:1708

    function XLastKnownRequestProcessed(
                display: access XDisplay)
               return X.unsigned_long;                      -- Xlib.h:1713

    function XServerVendor(
                display: access XDisplay)
               return X.Strings.charp;                      -- Xlib.h:1718

    function XDisplayString(
                display: access XDisplay)
               return X.Strings.charp;                      -- Xlib.h:1723

    function XDefaultColormap(
                display      : access XDisplay;
                screen_number: X.signed_int)
               return X.Colormap;                           -- Xlib.h:1728

    function XDefaultColormapOfScreen(
                screen: Screen_access)
               return X.Colormap;                           -- Xlib.h:1734

    function XDisplayOfScreen(
                screen: Screen_access)
               return XDisplay_access;                      -- Xlib.h:1739

    function XScreenOfDisplay(
                display      : access XDisplay;
                screen_number: X.signed_int)
               return Screen_access;                        -- Xlib.h:1744

    function XDefaultScreenOfDisplay(
                display: access XDisplay)
               return Screen_access;                        -- Xlib.h:1750

    function XEventMaskOfScreen(
                screen: Screen_access)
               return X.long;                               -- Xlib.h:1755

    function XScreenNumberOfScreen(
                screen: Screen_access)
               return X.signed_int;                         -- Xlib.h:1761

    function XSetErrorHandler(
                handler: XErrorHandler)
               return XErrorHandler;                        -- Xlib.h:1774

    function XSetIOErrorHandler(
                handler: XIOErrorHandler)
               return XIOErrorHandler;                      -- Xlib.h:1787

    function XListPixmapFormats(
                display     : access XDisplay;
                count_return: access X.signed_int)
               return XPixmapFormatValues_access;           -- Xlib.h:1794

    function XListDepths(
                display      : access XDisplay;
                screen_number: X.signed_int;
                count_return : access X.signed_int)
               return X.int_access;                         -- Xlib.h:1800

    function XReconfigureWMWindow(
                display      : access XDisplay;
                w            : X.Window;
                screen_number: X.signed_int;
                mask         : X.unsigned_int;
                changes      : access XWindowChanges)
               return Status;                               -- Xlib.h:1810

    function XGetWMProtocols(
                display         : access XDisplay;
                w               : X.Window;
                protocols_return: access Atom_access;
                count_return    : access X.signed_int)
               return Status;                               -- Xlib.h:1820

    function XSetWMProtocols(
                display  : access XDisplay;
                w        : X.Window;
                protocols: access X.Atom;
                count    : X.signed_int)
               return Status;                               -- Xlib.h:1828

    function XIconifyWindow(
                display      : access XDisplay;
                w            : X.Window;
                screen_number: X.signed_int)
               return Status;                               -- Xlib.h:1836

    function XWithdrawWindow(
                display      : access XDisplay;
                w            : X.Window;
                screen_number: X.signed_int)
               return Status;                               -- Xlib.h:1843

    function XGetCommand(
                display    : access XDisplay;
                w          : X.Window;
                argv_return: access X.Strings.charp_vector;
                argc_return: access X.signed_int)
               return Status;                               -- Xlib.h:1850

    function XGetWMColormapWindows(
                display       : access XDisplay;
                w             : X.Window;
                windows_return: access Window_access;
                count_return  : access X.signed_int)
               return Status;                               -- Xlib.h:1858

    function XSetWMColormapWindows(
                display         : access XDisplay;
                w               : X.Window;
                colormap_windows: access X.Window;
                count           : X.signed_int)
               return Status;                               -- Xlib.h:1866

    procedure XFreeStringList(
                list: X.Strings.charp_vector);              -- Xlib.h:1874

    procedure XSetTransientForHint(
                display    : access XDisplay;
                w          : X.Window;
                prop_window: X.Window);                     -- Xlib.h:1879

    procedure XActivateScreenSaver(
                display: access XDisplay);                  -- Xlib.h:1889

    procedure XAddHost(
                display: access XDisplay;
                host   : access XHostAddress);              -- Xlib.h:1895

    procedure XAddHosts(
                display  : access XDisplay;
                hosts    : access XHostAddress;
                num_hosts: X.signed_int);                   -- Xlib.h:1902

    function XAddToExtensionList(
                structure: access XExtData_access;
                ext_data : XExtData_access)
               return X.signed_int;                         -- Xlib.h:1910

    procedure XAddToSaveSet(
                display: access XDisplay;
                w      : X.Window);                         -- Xlib.h:1917

    function XAllocColor(
                display      : access XDisplay;
                colormap     : X.Colormap;
                screen_in_out: access XColor)
               return Status;                               -- Xlib.h:1924

    function XAllocColorCells(
                display           : access XDisplay;
                colormap          : X.Colormap;
                contig            : Bool;
                plane_masks_return: access X.unsigned_long;
                nplanes           : X.unsigned_int;
                pixels_return     : access X.unsigned_long;
                npixels           : X.unsigned_int)
               return Status;                               -- Xlib.h:1932

    function XAllocColorPlanes(
                display      : access XDisplay;
                colormap     : X.Colormap;
                contig       : Bool;
                pixels_return: access X.unsigned_long;
                ncolors      : X.signed_int;
                nreds        : X.signed_int;
                ngreens      : X.signed_int;
                nblues       : X.signed_int;
                rmask_return : access X.unsigned_long;
                gmask_return : access X.unsigned_long;
                bmask_return : access X.unsigned_long)
               return Status;                               -- Xlib.h:1944

    function XAllocNamedColor(
                display          : access XDisplay;
                colormap         : X.Colormap;
                color_name       : X.Strings.const_charp;
                screen_def_return: access XColor;
                exact_def_return : access XColor)
               return Status;                               -- Xlib.h:1960

    pragma Import(C, XAllocNamedColor, "XAllocNamedColor");

    function XAllocNamedColor(
                display          : access XDisplay;
                colormap         : X.Colormap;
                color_name       : Interfaces.C.Char_Array;
                screen_def_return: access XColor;
                exact_def_return : access XColor)
               return X.signed_int;                         -- Xlib.h:1960

    pragma Inline(XAllocNamedColor);

    procedure XAllowEvents(
                display   : access XDisplay;
                event_mode: X.signed_int;
                time      : X.Time);                        -- Xlib.h:1970

    procedure XAutoRepeatOff(
                display: access XDisplay);                  -- Xlib.h:1978

    procedure XAutoRepeatOn(
                display: access XDisplay);                  -- Xlib.h:1984

    procedure XBell(
                display: access XDisplay;
                percent: X.signed_int);                     -- Xlib.h:1990

    function XBitmapBitOrder(
                display: access XDisplay)
               return X.signed_int;                         -- Xlib.h:1997

    function XBitmapPad(
                display: access XDisplay)
               return X.signed_int;                         -- Xlib.h:2003

    function XBitmapUnit(
                display: access XDisplay)
               return X.signed_int;                         -- Xlib.h:2009

    function XCellsOfScreen(
                screen: Screen_access)
               return X.signed_int;                         -- Xlib.h:2015

    procedure XChangeActivePointerGrab(
                display   : access XDisplay;
                event_mask: X.unsigned_int;
                cursor    : X.Cursor;
                time      : X.Time);                        -- Xlib.h:2021

    procedure XChangeGC(
                display  : access XDisplay;
                gc       : access XGC;
                valuemask: X.unsigned_long;
                values   : access XGCValues);               -- Xlib.h:2030

    procedure XChangeKeyboardControl(
                display   : access XDisplay;
                value_mask: X.unsigned_long;
                values    : access XKeyboardControl);       -- Xlib.h:2039

    procedure XChangeKeyboardMapping(
                display            : access XDisplay;
                first_keycode      : X.signed_int;
                keysyms_per_keycode: X.signed_int;
                keysyms            : access X.KeySym;
                num_codes          : X.signed_int);         -- Xlib.h:2047

    procedure XChangePointerControl(
                display          : access XDisplay;
                do_accel         : Bool;
                do_threshold     : Bool;
                accel_numerator  : X.signed_int;
                accel_denominator: X.signed_int;
                threshold        : X.signed_int);           -- Xlib.h:2057

    procedure XChangeProperty(
                display  : access XDisplay;
                w        : X.Window;
                property : X.Atom;
                c_type   : X.Atom;
                format   : X.signed_int;
                mode     : X.signed_int;
                data     : X.const_unsigned_char_access;
                nelements: X.signed_int);                   -- Xlib.h:2068

    procedure XChangeSaveSet(
                display    : access XDisplay;
                w          : X.Window;
                change_mode: X.signed_int);                 -- Xlib.h:2081

    procedure XChangeWindowAttributes(
                display   : access XDisplay;
                w         : X.Window;
                valuemask : X.unsigned_long;
                attributes: access XSetWindowAttributes);   -- Xlib.h:2089

    function XCheckIfEvent(
                display     : access XDisplay;
                event_return: access Xevent;
                predicate   : af_predicate;
                arg         : XPointer)
               return Bool;                                 -- Xlib.h:2098

    function XCheckMaskEvent(
                display     : access XDisplay;
                event_mask  : X.long;
                event_return: access Xevent)
               return Bool;                                 -- Xlib.h:2113

    function XCheckTypedEvent(
                display     : access XDisplay;
                event_type  : X.signed_int;
                event_return: access Xevent)
               return Bool;                                 -- Xlib.h:2121

    function XCheckTypedWindowEvent(
                display     : access XDisplay;
                w           : X.Window;
                event_type  : X.signed_int;
                event_return: access Xevent)
               return Bool;                                 -- Xlib.h:2129

    function XCheckWindowEvent(
                display     : access XDisplay;
                w           : X.Window;
                event_mask  : X.long;
                event_return: access Xevent)
               return Bool;                                 -- Xlib.h:2138

    procedure XCirculateSubwindows(
                display  : access XDisplay;
                w        : X.Window;
                direction: X.signed_int);                   -- Xlib.h:2147

    procedure XCirculateSubwindowsDown(
                display: access XDisplay;
                w      : X.Window);                         -- Xlib.h:2155

    procedure XCirculateSubwindowsUp(
                display: access XDisplay;
                w      : X.Window);                         -- Xlib.h:2162

    procedure XClearArea(
                display  : access XDisplay;
                w        : X.Window;
                xx       : X.signed_int;
                y        : X.signed_int;
                width    : X.unsigned_int;
                height   : X.unsigned_int;
                exposures: Bool);                           -- Xlib.h:2169

    procedure XClearWindow(
                display: access XDisplay;
                w      : X.Window);                         -- Xlib.h:2181

    procedure XCloseDisplay(
                display: access XDisplay);                  -- Xlib.h:2188

    procedure XConfigureWindow(
                display   : access XDisplay;
                w         : X.Window;
                value_mask: X.unsigned_int;
                values    : access XWindowChanges);         -- Xlib.h:2194

    function XConnectionNumber(
                display: access XDisplay)
               return X.signed_int;                         -- Xlib.h:2203

    procedure XConvertSelection(
                display  : access XDisplay;
                selection: X.Atom;
                target   : X.Atom;
                property : X.Atom;
                requestor: X.Window;
                time     : X.Time);                         -- Xlib.h:2209

    procedure XCopyArea(
                display: access XDisplay;
                src    : X.Drawable;
                dest   : X.Drawable;
                gc     : access XGC;
                src_x  : X.signed_int;
                src_y  : X.signed_int;
                width  : X.unsigned_int;
                height : X.unsigned_int;
                dest_x : X.signed_int;
                dest_y : X.signed_int);                     -- Xlib.h:2220

    procedure XCopyGC(
                display  : access XDisplay;
                src      : access XGC;
                valuemask: X.unsigned_long;
                dest     : access XGC);                     -- Xlib.h:2235

    procedure XCopyPlane(
                display: access XDisplay;
                src    : X.Drawable;
                dest   : X.Drawable;
                gc     : access XGC;
                src_x  : X.signed_int;
                src_y  : X.signed_int;
                width  : X.unsigned_int;
                height : X.unsigned_int;
                dest_x : X.signed_int;
                dest_y : X.signed_int;
                plane  : X.unsigned_long);                  -- Xlib.h:2244

    function XDefaultDepth(
                display      : access XDisplay;
                screen_number: X.signed_int)
               return X.signed_int;                         -- Xlib.h:2260

    function XDefaultDepthOfScreen(
                screen: Screen_access)
               return X.signed_int;                         -- Xlib.h:2267

    function XDefaultScreen(
                display: access XDisplay)
               return X.signed_int;                         -- Xlib.h:2273

    procedure XDefineCursor(
                display: access XDisplay;
                w      : X.Window;
                cursor : X.Cursor);                         -- Xlib.h:2279

    procedure XDeleteProperty(
                display : access XDisplay;
                w       : X.Window;
                property: X.Atom);                          -- Xlib.h:2287

    procedure XDestroyWindow(
                display: access XDisplay;
                w      : X.Window);                         -- Xlib.h:2295

    procedure XDestroySubwindows(
                display: access XDisplay;
                w      : X.Window);                         -- Xlib.h:2302

    function XDoesBackingStore(
                screen: Screen_access)
               return X.signed_int;                         -- Xlib.h:2309

    function XDoesSaveUnders(
                screen: Screen_access)
               return Bool;                                 -- Xlib.h:2315

    procedure XDisableAccessControl(
                display: access XDisplay);                  -- Xlib.h:2321

    function XDisplayCells(
                display      : access XDisplay;
                screen_number: X.signed_int)
               return X.signed_int;                         -- Xlib.h:2328

    function XDisplayHeight(
                display      : access XDisplay;
                screen_number: X.signed_int)
               return X.signed_int;                         -- Xlib.h:2335

    function XDisplayHeightMM(
                display      : access XDisplay;
                screen_number: X.signed_int)
               return X.signed_int;                         -- Xlib.h:2342

    procedure XDisplayKeycodes(
                display            : access XDisplay;
                min_keycodes_return: access X.signed_int;
                max_keycodes_return: access X.signed_int);  -- Xlib.h:2349

    function XDisplayPlanes(
                display      : access XDisplay;
                screen_number: X.signed_int)
               return X.signed_int;                         -- Xlib.h:2357

    function XDisplayWidth(
                display      : access XDisplay;
                screen_number: X.signed_int)
               return X.signed_int;                         -- Xlib.h:2364

    function XDisplayWidthMM(
                display      : access XDisplay;
                screen_number: X.signed_int)
               return X.signed_int;                         -- Xlib.h:2371

    procedure XDrawArc(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                xx     : X.signed_int;
                y      : X.signed_int;
                width  : X.unsigned_int;
                height : X.unsigned_int;
                angle1 : X.signed_int;
                angle2 : X.signed_int);                     -- Xlib.h:2378

    procedure XDrawArcs(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                arcs   : access XArc;
                narcs  : X.signed_int);                     -- Xlib.h:2392

    procedure XDrawImageString(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                xx     : X.signed_int;
                y      : X.signed_int;
                string : X.Strings.const_charp;
                length : X.signed_int);                     -- Xlib.h:2402

    pragma Import(C, XDrawImageString, "XDrawImageString");

    procedure XDrawImageString(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                xx     : X.signed_int;
                y      : X.signed_int;
                string : Interfaces.C.Char_Array;
                length : X.signed_int);                     -- Xlib.h:2402

    pragma Inline(XDrawImageString);

    procedure XDrawImageString16(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                xx     : X.signed_int;
                y      : X.signed_int;
                string : access XChar2b;
                length : X.signed_int);                     -- Xlib.h:2414

    procedure XDrawLine(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                x1     : X.signed_int;
                x2     : X.signed_int;
                y1     : X.signed_int;
                y2     : X.signed_int);                     -- Xlib.h:2426

    procedure XDrawLines(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                points : access XPoint;
                npoints: X.signed_int;
                mode   : X.signed_int);                     -- Xlib.h:2438

    procedure XDrawPoint(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                xx     : X.signed_int;
                y      : X.signed_int);                     -- Xlib.h:2449

    procedure XDrawPoints(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                points : access XPoint;
                npoints: X.signed_int;
                mode   : X.signed_int);                     -- Xlib.h:2459

    procedure XDrawRectangle(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                xx     : X.signed_int;
                y      : X.signed_int;
                width  : X.unsigned_int;
                height : X.unsigned_int);                   -- Xlib.h:2470

    procedure XDrawRectangles(
                display    : access XDisplay;
                d          : X.Drawable;
                gc         : access XGC;
                rectangles : access XRectangle;
                nrectangles: X.signed_int);                 -- Xlib.h:2482

    procedure XDrawSegments(
                display  : access XDisplay;
                d        : X.Drawable;
                gc       : access XGC;
                segments : access XSegment;
                nsegments: X.signed_int);                   -- Xlib.h:2492

    procedure XDrawString(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                xx     : X.signed_int;
                y      : X.signed_int;
                string : X.Strings.const_charp;
                length : X.signed_int);                     -- Xlib.h:2502

    pragma Import(C, XDrawString, "XDrawString");

    procedure XDrawString(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                xx     : X.signed_int;
                y      : X.signed_int;
                string : Interfaces.C.Char_Array;
                length : X.signed_int);                     -- Xlib.h:2502

    pragma Inline(XDrawString);

    procedure XDrawString16(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                xx     : X.signed_int;
                y      : X.signed_int;
                string : access XChar2b;
                length : X.signed_int);                     -- Xlib.h:2514

    procedure XDrawText(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                xx     : X.signed_int;
                y      : X.signed_int;
                items  : access XTextItem;
                nitems : X.signed_int);                     -- Xlib.h:2526

    procedure XDrawText16(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                xx     : X.signed_int;
                y      : X.signed_int;
                items  : access XTextItem16;
                nitems : X.signed_int);                     -- Xlib.h:2538

    procedure XEnableAccessControl(
                display: access XDisplay);                  -- Xlib.h:2550

    function XEventsQueued(
                display: access XDisplay;
                mode   : X.signed_int)
               return X.signed_int;                         -- Xlib.h:2556

    function XFetchName(
                display           : access XDisplay;
                w                 : X.Window;
                window_name_return: X.Strings.charp_vector)
               return Status;                               -- Xlib.h:2563

    procedure XFillArc(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                xx     : X.signed_int;
                y      : X.signed_int;
                width  : X.unsigned_int;
                height : X.unsigned_int;
                angle1 : X.signed_int;
                angle2 : X.signed_int);                     -- Xlib.h:2571

    procedure XFillArcs(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                arcs   : access XArc;
                narcs  : X.signed_int);                     -- Xlib.h:2585

    procedure XFillPolygon(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                points : access XPoint;
                npoints: X.signed_int;
                shape  : X.signed_int;
                mode   : X.signed_int);                     -- Xlib.h:2595

    procedure XFillRectangle(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                xx     : X.signed_int;
                y      : X.signed_int;
                width  : X.unsigned_int;
                height : X.unsigned_int);                   -- Xlib.h:2607

    procedure XFillRectangles(
                display    : access XDisplay;
                d          : X.Drawable;
                gc         : access XGC;
                rectangles : access XRectangle;
                nrectangles: X.signed_int);                 -- Xlib.h:2619

    procedure XFlush(
                display: access XDisplay);                  -- Xlib.h:2629

    procedure XForceScreenSaver(
                display: access XDisplay;
                mode   : X.signed_int);                     -- Xlib.h:2635

    procedure XFree(
                data: system.address);                      -- Xlib.h:2642

    procedure XFreeColormap(
                display : access XDisplay;
                colormap: X.Colormap);                      -- Xlib.h:2648

    procedure XFreeColors(
                display : access XDisplay;
                colormap: X.Colormap;
                pixels  : access X.unsigned_long;
                npixels : X.signed_int;
                planes  : X.unsigned_long);                 -- Xlib.h:2655

    procedure XFreeCursor(
                display: access XDisplay;
                cursor : X.Cursor);                         -- Xlib.h:2665

    procedure XFreeExtensionList(
                list: X.Strings.charp_vector);              -- Xlib.h:2672

    procedure XFreeFont(
                display    : access XDisplay;
                font_struct: access XFontStruct);           -- Xlib.h:2678

    procedure XFreeFontInfo(
                names       : X.Strings.charp_vector;
                free_info   : access XFontStruct;
                actual_count: X.signed_int);                -- Xlib.h:2685

    function XFreeFontNames(
                list: X.Strings.charp_vector)
               return X.signed_int;                         -- Xlib.h:2693

    procedure XFreeFontPath(
                list: X.Strings.charp_vector);              -- Xlib.h:2699

    procedure XFreeGC(
                display: access XDisplay;
                gc     : access XGC);                       -- Xlib.h:2705

    procedure XFreeModifiermap(
                modmap: access XModifierKeymap);            -- Xlib.h:2712

    procedure XFreePixmap(
                display: access XDisplay;
                pixmap : X.Pixmap);                         -- Xlib.h:2718

    function XGeometry(
                display         : access XDisplay;
                screen          : X.signed_int;
                position        : X.Strings.const_charp;
                default_position: X.Strings.const_charp;
                bwidth          : X.unsigned_int;
                fwidth          : X.unsigned_int;
                fheight         : X.unsigned_int;
                xadder          : X.signed_int;
                yadder          : X.signed_int;
                x_return        : access X.signed_int;
                y_return        : access X.signed_int;
                width_return    : access X.signed_int;
                height_return   : access X.signed_int)
               return X.signed_int;                         -- Xlib.h:2725

    pragma Import(C, XGeometry, "XGeometry");

    function XGeometry(
                display         : access XDisplay;
                screen          : X.signed_int;
                position        : Interfaces.C.Char_Array;
                default_position: Interfaces.C.Char_Array;
                bwidth          : X.unsigned_int;
                fwidth          : X.unsigned_int;
                fheight         : X.unsigned_int;
                xadder          : X.signed_int;
                yadder          : X.signed_int;
                x_return        : access X.signed_int;
                y_return        : access X.signed_int;
                width_return    : access X.signed_int;
                height_return   : access X.signed_int)
               return X.signed_int;                         -- Xlib.h:2725

    pragma Inline(XGeometry);

    procedure XGetErrorDatabaseText(
                display       : access XDisplay;
                name          : X.Strings.const_charp;
                message       : X.Strings.const_charp;
                default_string: X.Strings.const_charp;
                buffer_return : X.Strings.charp;
                length        : X.signed_int);              -- Xlib.h:2743

    pragma Import(C, XGetErrorDatabaseText, "XGetErrorDatabaseText");

    procedure XGetErrorDatabaseText(
                display       : access XDisplay;
                name          : Interfaces.C.Char_Array;
                message       : Interfaces.C.Char_Array;
                default_string: Interfaces.C.Char_Array;
                buffer_return : in out Interfaces.C.Char_Array;
                length        : X.signed_int);              -- Xlib.h:2743

    pragma Inline(XGetErrorDatabaseText);

    procedure XGetErrorText(
                display      : access XDisplay;
                code         : X.signed_int;
                buffer_return: X.Strings.charp;
                length       : X.signed_int);               -- Xlib.h:2754

    pragma Import(C, XGetErrorText, "XGetErrorText");

    procedure XGetErrorText(
                display      : access XDisplay;
                code         : X.signed_int;
                buffer_return: in out Interfaces.C.Char_Array;
                length       : X.signed_int);               -- Xlib.h:2754

    pragma Inline(XGetErrorText);

    function XGetFontProperty(
                font_struct : access XFontStruct;
                atom        : X.Atom;
                value_return: access X.unsigned_long)
               return Bool;                                 -- Xlib.h:2763

    function XGetGCValues(
                display      : access XDisplay;
                gc           : access XGC;
                valuemask    : X.unsigned_long;
                values_return: access XGCValues)
               return Status;                               -- Xlib.h:2771

    function XGetGeometry(
                display            : access XDisplay;
                d                  : X.Drawable;
                root_return        : access X.Window;
                x_return           : access X.signed_int;
                y_return           : access X.signed_int;
                width_return       : access X.unsigned_int;
                height_return      : access X.unsigned_int;
                border_width_return: access X.unsigned_int;
                depth_return       : access X.unsigned_int)
               return Status;                               -- Xlib.h:2780

    function XGetIconName(
                display         : access XDisplay;
                w               : X.Window;
                icon_name_return: X.Strings.charp_vector)
               return Status;                               -- Xlib.h:2794

    procedure XGetInputFocus(
                display         : access XDisplay;
                focus_return    : access X.Window;
                revert_to_return: access X.signed_int);     -- Xlib.h:2802

    procedure XGetKeyboardControl(
                display      : access XDisplay;
                values_return: access XKeyboardState);      -- Xlib.h:2810

    procedure XGetPointerControl(
                display                 : access XDisplay;
                accel_numerator_return  : access X.signed_int;
                accel_denominator_return: access X.signed_int;
                threshold_return        : access X.signed_int);
                                                            -- Xlib.h:2817

    function XGetPointerMapping(
                display   : access XDisplay;
                map_return: X.unsigned_char_access;
                nmap      : X.signed_int)
               return X.signed_int;                         -- Xlib.h:2826

    procedure XGetScreenSaver(
                display               : access XDisplay;
                timeout_return        : access X.signed_int;
                interval_return       : access X.signed_int;
                prefer_blanking_return: access X.signed_int;
                allow_exposures_return: access X.signed_int);
                                                            -- Xlib.h:2834

    function XGetTransientForHint(
                display           : access XDisplay;
                w                 : X.Window;
                prop_window_return: access X.Window)
               return Status;                               -- Xlib.h:2844

    function XGetWindowProperty(
                display             : access XDisplay;
                w                   : X.Window;
                property            : X.Atom;
                long_offset         : X.long;
                long_length         : X.long;
                delete              : Bool;
                req_type            : X.Atom;
                actual_type_return  : access X.Atom;
                actual_format_return: access X.signed_int;
                nitems_return       : access X.unsigned_long;
                bytes_after_return  : access X.unsigned_long;
                prop_return         : access X.unsigned_char_access)
               return X.signed_int;                         -- Xlib.h:2852

    function XGetWindowAttributes(
                display                 : access XDisplay;
                w                       : X.Window;
                window_attributes_return: access XWindowAttributes)
               return Status;                               -- Xlib.h:2869

    procedure XGrabButton(
                display      : access XDisplay;
                button       : X.unsigned_int;
                modifiers    : X.unsigned_int;
                grab_window  : X.Window;
                owner_events : Bool;
                event_mask   : X.unsigned_int;
                pointer_mode : X.signed_int;
                keyboard_mode: X.signed_int;
                confine_to   : X.Window;
                cursor       : X.Cursor);                   -- Xlib.h:2877

    procedure XGrabKey(
                display      : access XDisplay;
                keycode      : X.signed_int;
                modifiers    : X.unsigned_int;
                grab_window  : X.Window;
                owner_events : Bool;
                pointer_mode : X.signed_int;
                keyboard_mode: X.signed_int);               -- Xlib.h:2892

    function XGrabKeyboard(
                display      : access XDisplay;
                grab_window  : X.Window;
                owner_events : Bool;
                pointer_mode : X.signed_int;
                keyboard_mode: X.signed_int;
                time         : X.Time)
               return X.signed_int;                         -- Xlib.h:2904

    function XGrabPointer(
                display      : access XDisplay;
                grab_window  : X.Window;
                owner_events : Bool;
                event_mask   : X.unsigned_int;
                pointer_mode : X.signed_int;
                keyboard_mode: X.signed_int;
                confine_to   : X.Window;
                cursor       : X.Cursor;
                time         : X.Time)
               return X.signed_int;                         -- Xlib.h:2915

    procedure XGrabServer(
                display: access XDisplay);                  -- Xlib.h:2929

    function XHeightMMOfScreen(
                screen: Screen_access)
               return X.signed_int;                         -- Xlib.h:2935

    function XHeightOfScreen(
                screen: Screen_access)
               return X.signed_int;                         -- Xlib.h:2941

    procedure XIfEvent(
                display     : access XDisplay;
                event_return: access Xevent;
                predicate   : af_predicate;
                arg         : XPointer);                    -- Xlib.h:2947

    function XImageByteOrder(
                display: access XDisplay)
               return X.signed_int;                         -- Xlib.h:2962

    procedure XInstallColormap(
                display : access XDisplay;
                colormap: X.Colormap);                      -- Xlib.h:2968

    function XKeysymToKeycode(
                display: access XDisplay;
                keysym : X.KeySym)
               return X.KeyCode;                            -- Xlib.h:2975

    procedure XKillClient(
                display : access XDisplay;
                resource: X.XID);                           -- Xlib.h:2982

    function XLookupColor(
                display          : access XDisplay;
                colormap         : X.Colormap;
                color_name       : X.Strings.const_charp;
                exact_def_return : access XColor;
                screen_def_return: access XColor)
               return Status;                               -- Xlib.h:2995

    pragma Import(C, XLookupColor, "XLookupColor");

    function XLookupColor(
                display          : access XDisplay;
                colormap         : X.Colormap;
                color_name       : Interfaces.C.Char_Array;
                exact_def_return : access XColor;
                screen_def_return: access XColor)
               return X.signed_int;                         -- Xlib.h:2995

    pragma Inline(XLookupColor);

    procedure XLowerWindow(
                display: access XDisplay;
                w      : X.Window);                         -- Xlib.h:3005

    procedure XMapRaised(
                display: access XDisplay;
                w      : X.Window);                         -- Xlib.h:3012

    procedure XMapSubwindows(
                display: access XDisplay;
                w      : X.Window);                         -- Xlib.h:3019

    procedure XMapWindow(
                display: access XDisplay;
                w      : X.Window);                         -- Xlib.h:3026

    procedure XMaskEvent(
                display     : access XDisplay;
                event_mask  : X.long;
                event_return: access Xevent);               -- Xlib.h:3033

    function XMaxCmapsOfScreen(
                screen: Screen_access)
               return X.signed_int;                         -- Xlib.h:3041

    function XMinCmapsOfScreen(
                screen: Screen_access)
               return X.signed_int;                         -- Xlib.h:3047

    procedure XMoveResizeWindow(
                display: access XDisplay;
                w      : X.Window;
                xx     : X.signed_int;
                y      : X.signed_int;
                width  : X.unsigned_int;
                height : X.unsigned_int);                   -- Xlib.h:3053

    procedure XMoveWindow(
                display: access XDisplay;
                w      : X.Window;
                xx     : X.signed_int;
                y      : X.signed_int);                     -- Xlib.h:3064

    procedure XNextEvent(
                display     : access XDisplay;
                event_return: access Xevent);               -- Xlib.h:3073

    procedure XNoOp(
                display: access XDisplay);                  -- Xlib.h:3080

    function XParseColor(
                display         : access XDisplay;
                colormap        : X.Colormap;
                spec            : X.Strings.const_charp;
                exact_def_return: access XColor)
               return Status;                               -- Xlib.h:3086

    pragma Import(C, XParseColor, "XParseColor");

    function XParseColor(
                display         : access XDisplay;
                colormap        : X.Colormap;
                spec            : Interfaces.C.Char_Array;
                exact_def_return: access XColor)
               return X.signed_int;                         -- Xlib.h:3086

    pragma Inline(XParseColor);

    function XParseGeometry(
                parsestring  : X.Strings.const_charp;
                x_return     : access X.signed_int;
                y_return     : access X.signed_int;
                width_return : access X.unsigned_int;
                height_return: access X.unsigned_int)
               return X.signed_int;                         -- Xlib.h:3095

    pragma Import(C, XParseGeometry, "XParseGeometry");

    function XParseGeometry(
                parsestring  : Interfaces.C.Char_Array;
                x_return     : access X.signed_int;
                y_return     : access X.signed_int;
                width_return : access X.unsigned_int;
                height_return: access X.unsigned_int)
               return X.signed_int;                         -- Xlib.h:3095

    pragma Inline(XParseGeometry);

    procedure XPeekEvent(
                display     : access XDisplay;
                event_return: access Xevent);               -- Xlib.h:3105

    procedure XPeekIfEvent(
                display     : access XDisplay;
                event_return: access Xevent;
                predicate   : af_predicate;
                arg         : XPointer);                    -- Xlib.h:3112

    function XPending(
                display: access XDisplay)
               return X.signed_int;                         -- Xlib.h:3127

    function XPlanesOfScreen(
                screen: Screen_access)
               return X.signed_int;                         -- Xlib.h:3133

    function XProtocolRevision(
                display: access XDisplay)
               return X.signed_int;                         -- Xlib.h:3140

    function XProtocolVersion(
                display: access XDisplay)
               return X.signed_int;                         -- Xlib.h:3146

    procedure XPutBackEvent(
                display: access XDisplay;
                event  : access Xevent);                    -- Xlib.h:3153

    procedure XPutImage(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                image  : access XImage;
                src_x  : X.signed_int;
                src_y  : X.signed_int;
                dest_x : X.signed_int;
                dest_y : X.signed_int;
                width  : X.unsigned_int;
                height : X.unsigned_int);                   -- Xlib.h:3160

    function XQLength(
                display: access XDisplay)
               return X.signed_int;                         -- Xlib.h:3175

    function XQueryBestCursor(
                display      : access XDisplay;
                d            : X.Drawable;
                width        : X.unsigned_int;
                height       : X.unsigned_int;
                width_return : access X.unsigned_int;
                height_return: access X.unsigned_int)
               return Status;                               -- Xlib.h:3181

    function XQueryBestSize(
                display      : access XDisplay;
                class        : X.signed_int;
                which_screen : X.Drawable;
                width        : X.unsigned_int;
                height       : X.unsigned_int;
                width_return : access X.unsigned_int;
                height_return: access X.unsigned_int)
               return Status;                               -- Xlib.h:3192

    function XQueryBestStipple(
                display      : access XDisplay;
                which_screen : X.Drawable;
                width        : X.unsigned_int;
                height       : X.unsigned_int;
                width_return : access X.unsigned_int;
                height_return: access X.unsigned_int)
               return Status;                               -- Xlib.h:3204

    function XQueryBestTile(
                display      : access XDisplay;
                which_screen : X.Drawable;
                width        : X.unsigned_int;
                height       : X.unsigned_int;
                width_return : access X.unsigned_int;
                height_return: access X.unsigned_int)
               return Status;                               -- Xlib.h:3215

    procedure XQueryColor(
                display   : access XDisplay;
                colormap  : X.Colormap;
                def_in_out: access XColor);                 -- Xlib.h:3226

    procedure XQueryColors(
                display    : access XDisplay;
                colormap   : X.Colormap;
                defs_in_out: access XColor;
                ncolors    : X.signed_int);                 -- Xlib.h:3234

    function XQueryExtension(
                display            : access XDisplay;
                name               : X.Strings.const_charp;
                major_opcode_return: access X.signed_int;
                first_event_return : access X.signed_int;
                first_error_return : access X.signed_int)
               return Bool;                                 -- Xlib.h:3243

    pragma Import(C, XQueryExtension, "XQueryExtension");

    function XQueryExtension(
                display            : access XDisplay;
                name               : Interfaces.C.Char_Array;
                major_opcode_return: access X.signed_int;
                first_event_return : access X.signed_int;
                first_error_return : access X.signed_int)
               return X.signed_int;                         -- Xlib.h:3243

    pragma Inline(XQueryExtension);

    procedure XQueryKeymap(
                display    : access XDisplay;
                keys_return: access Char32);                -- Xlib.h:3253

    function XQueryPointer(
                display      : access XDisplay;
                w            : X.Window;
                root_return  : access X.Window;
                child_return : access X.Window;
                root_x_return: access X.signed_int;
                root_y_return: access X.signed_int;
                win_x_return : access X.signed_int;
                win_y_return : access X.signed_int;
                mask_return  : access X.unsigned_int)
               return Bool;                                 -- Xlib.h:3260

    procedure XQueryTextExtents(
                display            : access XDisplay;
                font_ID            : X.XID;
                string             : X.Strings.const_charp;
                nchars             : X.signed_int;
                direction_return   : access X.signed_int;
                font_ascent_return : access X.signed_int;
                font_descent_return: access X.signed_int;
                overall_return     : access XCharStruct);   -- Xlib.h:3274

    pragma Import(C, XQueryTextExtents, "XQueryTextExtents");

    procedure XQueryTextExtents(
                display            : access XDisplay;
                font_ID            : X.XID;
                string             : Interfaces.C.Char_Array;
                nchars             : X.signed_int;
                direction_return   : access X.signed_int;
                font_ascent_return : access X.signed_int;
                font_descent_return: access X.signed_int;
                overall_return     : XCharStruct_access);   -- Xlib.h:3274

    pragma Inline(XQueryTextExtents);

    procedure XQueryTextExtents16(
                display            : access XDisplay;
                font_ID            : X.XID;
                string             : access XChar2b;
                nchars             : X.signed_int;
                direction_return   : access X.signed_int;
                font_ascent_return : access X.signed_int;
                font_descent_return: access X.signed_int;
                overall_return     : access XCharStruct);   -- Xlib.h:3287

    function XQueryTree(
                display         : access XDisplay;
                w               : X.Window;
                root_return     : access X.Window;
                parent_return   : access X.Window;
                children_return : access Window_access;
                nchildren_return: access X.unsigned_int)
               return Status;                               -- Xlib.h:3300

    procedure XRaiseWindow(
                display: access XDisplay;
                w      : X.Window);                         -- Xlib.h:3311

    function XReadBitmapFile(
                display      : access XDisplay;
                d            : X.Drawable;
                filename     : X.Strings.const_charp;
                width_return : access X.unsigned_int;
                height_return: access X.unsigned_int;
                bitmap_return: access X.Pixmap;
                x_hot_return : access X.signed_int;
                y_hot_return : access X.signed_int)
               return X.signed_int;                         -- Xlib.h:3318

    pragma Import(C, XReadBitmapFile, "XReadBitmapFile");

    function XReadBitmapFile(
                display      : access XDisplay;
                d            : X.Drawable;
                filename     : Interfaces.C.Char_Array;
                width_return : access X.unsigned_int;
                height_return: access X.unsigned_int;
                bitmap_return: access X.Pixmap;
                x_hot_return : access X.signed_int;
                y_hot_return : access X.signed_int)
               return X.signed_int;                         -- Xlib.h:3318

    pragma Inline(XReadBitmapFile);

    procedure XRebindKeysym(
                display     : access XDisplay;
                keysym      : X.KeySym;
                list        : access X.KeySym;
                mod_count   : X.signed_int;
                string      : X.const_unsigned_char_access;
                bytes_string: X.signed_int);                -- Xlib.h:3331

    procedure XRecolorCursor(
                display         : access XDisplay;
                cursor          : X.Cursor;
                foreground_color: access XColor;
                background_color: access XColor);           -- Xlib.h:3342

    procedure XRefreshKeyboardMapping(
                event_map: access XMappingEvent);           -- Xlib.h:3351

    procedure XRemoveFromSaveSet(
                display: access XDisplay;
                w      : X.Window);                         -- Xlib.h:3357

    procedure XRemoveHost(
                display: access XDisplay;
                host   : access XHostAddress);              -- Xlib.h:3364

    procedure XRemoveHosts(
                display  : access XDisplay;
                hosts    : access XHostAddress;
                num_hosts: X.signed_int);                   -- Xlib.h:3371

    procedure XReparentWindow(
                display: access XDisplay;
                w      : X.Window;
                parent : X.Window;
                xx     : X.signed_int;
                y      : X.signed_int);                     -- Xlib.h:3379

    procedure XResetScreenSaver(
                display: access XDisplay);                  -- Xlib.h:3389

    procedure XResizeWindow(
                display: access XDisplay;
                w      : X.Window;
                width  : X.unsigned_int;
                height : X.unsigned_int);                   -- Xlib.h:3395

    procedure XRestackWindows(
                display : access XDisplay;
                windows : access X.Window;
                nwindows: X.signed_int);                    -- Xlib.h:3404

    procedure XRotateBuffers(
                display: access XDisplay;
                rotate : X.signed_int);                     -- Xlib.h:3412

    procedure XRotateWindowProperties(
                display   : access XDisplay;
                w         : X.Window;
                properties: access X.Atom;
                num_prop  : X.signed_int;
                npositions: X.signed_int);                  -- Xlib.h:3419

    function XScreenCount(
                display: access XDisplay)
               return X.signed_int;                         -- Xlib.h:3429

    procedure XSelectInput(
                display   : access XDisplay;
                w         : X.Window;
                event_mask: X.long);                        -- Xlib.h:3435

    function XSendEvent(
                display   : access XDisplay;
                w         : X.Window;
                propagate : Bool;
                event_mask: X.long;
                event_send: access Xevent)
               return Status;                               -- Xlib.h:3443

    procedure XSetAccessControl(
                display: access XDisplay;
                mode   : X.signed_int);                     -- Xlib.h:3453

    procedure XSetArcMode(
                display : access XDisplay;
                gc      : access XGC;
                arc_mode: X.signed_int);                    -- Xlib.h:3460

    procedure XSetBackground(
                display   : access XDisplay;
                gc        : access XGC;
                background: X.unsigned_long);               -- Xlib.h:3468

    procedure XSetClipMask(
                display: access XDisplay;
                gc     : access XGC;
                pixmap : X.Pixmap);                         -- Xlib.h:3476

    procedure XSetClipOrigin(
                display      : access XDisplay;
                gc           : access XGC;
                clip_x_origin: X.signed_int;
                clip_y_origin: X.signed_int);               -- Xlib.h:3484

    procedure XSetClipRectangles(
                display      : access XDisplay;
                gc           : access XGC;
                clip_x_origin: X.signed_int;
                clip_y_origin: X.signed_int;
                rectangles   : access XRectangle;
                n            : X.signed_int;
                ordering     : X.signed_int);               -- Xlib.h:3493

    procedure XSetCloseDownMode(
                display   : access XDisplay;
                close_mode: X.signed_int);                  -- Xlib.h:3505

    procedure XSetCommand(
                display: access XDisplay;
                w      : X.Window;
                argv   : X.Strings.charp_vector;
                argc   : X.signed_int);                     -- Xlib.h:3512

    procedure XSetDashes(
                display    : access XDisplay;
                gc         : access XGC;
                dash_offset: X.signed_int;
                dash_list  : X.Strings.const_charp;
                n          : X.signed_int);                 -- Xlib.h:3521

    pragma Import(C, XSetDashes, "XSetDashes");

    procedure XSetDashes(
                display    : access XDisplay;
                gc         : access XGC;
                dash_offset: X.signed_int;
                dash_list  : Interfaces.C.Char_Array;
                n          : X.signed_int);                 -- Xlib.h:3521

    pragma Inline(XSetDashes);

    procedure XSetFillRule(
                display  : access XDisplay;
                gc       : access XGC;
                fill_rule: X.signed_int);                   -- Xlib.h:3531

    procedure XSetFillStyle(
                display   : access XDisplay;
                gc        : access XGC;
                fill_style: X.signed_int);                  -- Xlib.h:3539

    procedure XSetFont(
                display: access XDisplay;
                gc     : access XGC;
                font   : X.Font);                           -- Xlib.h:3547

    procedure XSetFontPath(
                display    : access XDisplay;
                directories: X.Strings.charp_vector;
                ndirs      : X.signed_int);                 -- Xlib.h:3555

    procedure XSetForeground(
                display   : access XDisplay;
                gc        : access XGC;
                foreground: X.unsigned_long);               -- Xlib.h:3563

    procedure XSetFunction(
                display   : access XDisplay;
                gc        : access XGC;
                c_procedure: X.signed_int);                 -- Xlib.h:3571

    procedure XSetGraphicsExposures(
                display           : access XDisplay;
                gc                : access XGC;
                graphics_exposures: Bool);                  -- Xlib.h:3579

    procedure XSetIconName(
                display  : access XDisplay;
                w        : X.Window;
                icon_name: X.Strings.const_charp);          -- Xlib.h:3587

    pragma Import(C, XSetIconName, "XSetIconName");

    procedure XSetIconName(
                display  : access XDisplay;
                w        : X.Window;
                icon_name: Interfaces.C.Char_Array);        -- Xlib.h:3587

    pragma Inline(XSetIconName);

    procedure XSetInputFocus(
                display  : access XDisplay;
                focus    : X.Window;
                revert_to: X.signed_int;
                time     : X.Time);                         -- Xlib.h:3595

    procedure XSetLineAttributes(
                display   : access XDisplay;
                gc        : access XGC;
                line_width: X.unsigned_int;
                line_style: X.signed_int;
                cap_style : X.signed_int;
                join_style: X.signed_int);                  -- Xlib.h:3604

    function XSetModifierMapping(
                display: access XDisplay;
                modmap : access XModifierKeymap)
               return X.signed_int;                         -- Xlib.h:3615

    procedure XSetPlaneMask(
                display   : access XDisplay;
                gc        : access XGC;
                plane_mask: X.unsigned_long);               -- Xlib.h:3622

    function XSetPointerMapping(
                display: access XDisplay;
                map    : X.const_unsigned_char_access;
                nmap   : X.signed_int)
               return X.signed_int;                         -- Xlib.h:3630

    procedure XSetScreenSaver(
                display        : access XDisplay;
                timeout        : X.signed_int;
                interval       : X.signed_int;
                prefer_blanking: X.signed_int;
                allow_exposures: X.signed_int);             -- Xlib.h:3638

    procedure XSetSelectionOwner(
                display  : access XDisplay;
                selection: X.Atom;
                owner    : X.Window;
                time     : X.Time);                         -- Xlib.h:3648

    procedure XSetState(
                display   : access XDisplay;
                gc        : access XGC;
                foreground: X.unsigned_long;
                background: X.unsigned_long;
                c_procedure: X.signed_int;
                plane_mask: X.unsigned_long);               -- Xlib.h:3657

    procedure XSetStipple(
                display: access XDisplay;
                gc     : access XGC;
                stipple: X.Pixmap);                         -- Xlib.h:3668

    procedure XSetSubwindowMode(
                display       : access XDisplay;
                gc            : access XGC;
                subwindow_mode: X.signed_int);              -- Xlib.h:3676

    procedure XSetTSOrigin(
                display    : access XDisplay;
                gc         : access XGC;
                ts_x_origin: X.signed_int;
                ts_y_origin: X.signed_int);                 -- Xlib.h:3684

    procedure XSetTile(
                display: access XDisplay;
                gc     : access XGC;
                tile   : X.Pixmap);                         -- Xlib.h:3693

    procedure XSetWindowBackground(
                display         : access XDisplay;
                w               : X.Window;
                background_pixel: X.unsigned_long);         -- Xlib.h:3701

    procedure XSetWindowBackgroundPixmap(
                display          : access XDisplay;
                w                : X.Window;
                background_pixmap: X.Pixmap);               -- Xlib.h:3709

    procedure XSetWindowBorder(
                display     : access XDisplay;
                w           : X.Window;
                border_pixel: X.unsigned_long);             -- Xlib.h:3717

    procedure XSetWindowBorderPixmap(
                display      : access XDisplay;
                w            : X.Window;
                border_pixmap: X.Pixmap);                   -- Xlib.h:3725

    procedure XSetWindowBorderWidth(
                display: access XDisplay;
                w      : X.Window;
                width  : X.unsigned_int);                   -- Xlib.h:3733

    procedure XSetWindowColormap(
                display : access XDisplay;
                w       : X.Window;
                colormap: X.Colormap);                      -- Xlib.h:3741

    procedure XStoreBuffer(
                display: access XDisplay;
                bytes  : X.Strings.const_charp;
                nbytes : X.signed_int;
                buffer : X.signed_int);                     -- Xlib.h:3749

    pragma Import(C, XStoreBuffer, "XStoreBuffer");

    procedure XStoreBuffer(
                display: access XDisplay;
                bytes  : Interfaces.C.Char_Array;
                nbytes : X.signed_int;
                buffer : X.signed_int);                     -- Xlib.h:3749

    pragma Inline(XStoreBuffer);

    procedure XStoreBytes(
                display: access XDisplay;
                bytes  : X.Strings.const_charp;
                nbytes : X.signed_int);                     -- Xlib.h:3758

    pragma Import(C, XStoreBytes, "XStoreBytes");

    procedure XStoreBytes(
                display: access XDisplay;
                bytes  : Interfaces.C.Char_Array;
                nbytes : X.signed_int);                     -- Xlib.h:3758

    pragma Inline(XStoreBytes);

    procedure XStoreColor(
                display : access XDisplay;
                colormap: X.Colormap;
                color   : access XColor);                   -- Xlib.h:3766

    procedure XStoreColors(
                display : access XDisplay;
                colormap: X.Colormap;
                color   : access XColor;
                ncolors : X.signed_int);                    -- Xlib.h:3774

    procedure XStoreName(
                display    : access XDisplay;
                w          : X.Window;
                window_name: X.Strings.const_charp);        -- Xlib.h:3783

    pragma Import(C, XStoreName, "XStoreName");

    procedure XStoreName(
                display    : access XDisplay;
                w          : X.Window;
                window_name: Interfaces.C.Char_Array);      -- Xlib.h:3783

    pragma Inline(XStoreName);

    procedure XStoreNamedColor(
                display : access XDisplay;
                colormap: X.Colormap;
                color   : X.Strings.const_charp;
                pixel   : X.unsigned_long;
                flags   : X.signed_int);                    -- Xlib.h:3791

    pragma Import(C, XStoreNamedColor, "XStoreNamedColor");

    procedure XStoreNamedColor(
                display : access XDisplay;
                colormap: X.Colormap;
                color   : Interfaces.C.Char_Array;
                pixel   : X.unsigned_long;
                flags   : X.signed_int);                    -- Xlib.h:3791

    pragma Inline(XStoreNamedColor);

    procedure XSync(
                display: access XDisplay;
                discard: Bool);                             -- Xlib.h:3801

    procedure XTextExtents(
                font_struct        : access XFontStruct;
                string             : X.Strings.const_charp;
                nchars             : X.signed_int;
                direction_return   : access X.signed_int;
                font_ascent_return : access X.signed_int;
                font_descent_return: access X.signed_int;
                overall_return     : access XCharStruct);   -- Xlib.h:3808

    pragma Import(C, XTextExtents, "XTextExtents");

    procedure XTextExtents(
                font_struct        : XFontStruct_access;
                string             : Interfaces.C.Char_Array;
                nchars             : X.signed_int;
                direction_return   : access X.signed_int;
                font_ascent_return : access X.signed_int;
                font_descent_return: access X.signed_int;
                overall_return     : XCharStruct_access);   -- Xlib.h:3808

    pragma Inline(XTextExtents);

    procedure XTextExtents16(
                font_struct        : access XFontStruct;
                string             : access XChar2b;
                nchars             : X.signed_int;
                direction_return   : access X.signed_int;
                font_ascent_return : access X.signed_int;
                font_descent_return: access X.signed_int;
                overall_return     : access XCharStruct);   -- Xlib.h:3820

    function XTextWidth(
                font_struct: access XFontStruct;
                string     : X.Strings.const_charp;
                count      : X.signed_int)
               return X.signed_int;                         -- Xlib.h:3832

    pragma Import(C, XTextWidth, "XTextWidth");

    function XTextWidth(
                font_struct: XFontStruct_access;
                string     : Interfaces.C.Char_Array;
                count      : X.signed_int)
               return X.signed_int;                         -- Xlib.h:3832

    pragma Inline(XTextWidth);

    function XTextWidth16(
                font_struct: access XFontStruct;
                string     : access XChar2b;
                count      : X.signed_int)
               return X.signed_int;                         -- Xlib.h:3840

    function XTranslateCoordinates(
                display      : access XDisplay;
                src_w        : X.Window;
                dest_w       : X.Window;
                src_x        : X.signed_int;
                src_y        : X.signed_int;
                dest_x_return: access X.signed_int;
                dest_y_return: access X.signed_int;
                child_return : access X.Window)
               return Bool;                                 -- Xlib.h:3848

    procedure XUndefineCursor(
                display: access XDisplay;
                w      : X.Window);                         -- Xlib.h:3861

    procedure XUngrabButton(
                display    : access XDisplay;
                button     : X.unsigned_int;
                modifiers  : X.unsigned_int;
                grab_window: X.Window);                     -- Xlib.h:3868

    procedure XUngrabKey(
                display    : access XDisplay;
                keycode    : X.signed_int;
                modifiers  : X.unsigned_int;
                grab_window: X.Window);                     -- Xlib.h:3877

    procedure XUngrabKeyboard(
                display: access XDisplay;
                time   : X.Time);                           -- Xlib.h:3886

    procedure XUngrabPointer(
                display: access XDisplay;
                time   : X.Time);                           -- Xlib.h:3893

    procedure XUngrabServer(
                display: access XDisplay);                  -- Xlib.h:3900

    procedure XUninstallColormap(
                display : access XDisplay;
                colormap: X.Colormap);                      -- Xlib.h:3906

    procedure XUnloadFont(
                display: access XDisplay;
                font   : X.Font);                           -- Xlib.h:3913

    procedure XUnmapSubwindows(
                display: access XDisplay;
                w      : X.Window);                         -- Xlib.h:3920

    procedure XUnmapWindow(
                display: access XDisplay;
                w      : X.Window);                         -- Xlib.h:3927

    function XVendorRelease(
                display: access XDisplay)
               return X.signed_int;                         -- Xlib.h:3934

    procedure XWarpPointer(
                display   : access XDisplay;
                src_w     : X.Window;
                dest_w    : X.Window;
                src_x     : X.signed_int;
                src_y     : X.signed_int;
                src_width : X.unsigned_int;
                src_height: X.unsigned_int;
                dest_x    : X.signed_int;
                dest_y    : X.signed_int);                  -- Xlib.h:3940

    function XWidthMMOfScreen(
                screen: Screen_access)
               return X.signed_int;                         -- Xlib.h:3954

    function XWidthOfScreen(
                screen: Screen_access)
               return X.signed_int;                         -- Xlib.h:3960

    procedure XWindowEvent(
                display     : access XDisplay;
                w           : X.Window;
                event_mask  : X.long;
                event_return: access Xevent);               -- Xlib.h:3966

    function XWriteBitmapFile(
                display : access XDisplay;
                filename: X.Strings.const_charp;
                bitmap  : X.Pixmap;
                width   : X.unsigned_int;
                height  : X.unsigned_int;
                x_hot   : X.signed_int;
                y_hot   : X.signed_int)
               return X.signed_int;                         -- Xlib.h:3975

    pragma Import(C, XWriteBitmapFile, "XWriteBitmapFile");

    function XWriteBitmapFile(
                display : access XDisplay;
                filename: Interfaces.C.Char_Array;
                bitmap  : X.Pixmap;
                width   : X.unsigned_int;
                height  : X.unsigned_int;
                x_hot   : X.signed_int;
                y_hot   : X.signed_int)
               return X.signed_int;                         -- Xlib.h:3975

    pragma Inline(XWriteBitmapFile);

    function XSupportsLocale return Bool;                   -- Xlib.h:3987

    function XSetLocaleModifiers(
                modifier_list: X.Strings.const_charp)
               return X.Strings.charp;                      -- Xlib.h:3993

    pragma Import(C, XSetLocaleModifiers, "XSetLocaleModifiers");

    function XSetLocaleModifiers(
                modifier_list: Interfaces.C.Char_Array)
               return X.Strings.charp;                      -- Xlib.h:3993

    pragma Inline(XSetLocaleModifiers);

    function XCreateFontSet(
                display              : access XDisplay;
                base_font_name_list  : X.Strings.const_charp;
                missing_charset_list : access X.Strings.charp_vector;
                missing_charset_count: access X.signed_int;
                def_string           : X.Strings.charp_vector)
               return XFontSet;                             -- Xlib.h:3999

    pragma Import(C, XCreateFontSet, "XCreateFontSet");

    function XCreateFontSet(
                display              : access XDisplay;
                base_font_name_list  : Interfaces.C.Char_Array;
                missing_charset_list : access X.Strings.charp_vector;
                missing_charset_count: access X.signed_int;
                def_string           : X.Strings.charp_vector)
               return XFontSet;                             -- Xlib.h:3999

    pragma Inline(XCreateFontSet);

    procedure XFreeFontSet(
                display : access XDisplay;
                font_set: XFontSet);                        -- Xlib.h:4009

    function XFontsOfFontSet(
                font_set        : XFontSet;
                font_struct_list: access XFontStruct_access_access;
                                                            -- ***
                font_name_list  : access X.Strings.charp_vector)
                                                            -- ***
               return X.signed_int;                         -- Xlib.h:4016

    function XBaseFontNameListOfFontSet(
                font_set: XFontSet)
               return X.Strings.charp;                      -- Xlib.h:4024

    function XLocaleOfFontSet(
                font_set: XFontSet)
               return X.Strings.charp;                      -- Xlib.h:4030

    function XContextDependentDrawing(
                font_set: XFontSet)
               return Bool;                                 -- Xlib.h:4036

    function XExtentsOfFontSet(
                font_set: XFontSet)
               return XFontSetExtents_access;               -- Xlib.h:4042

    function XmbTextEscapement(
                font_set  : XFontSet;
                text      : X.Strings.const_charp;
                bytes_text: X.signed_int)
               return X.signed_int;                         -- Xlib.h:4048

    pragma Import(C, XmbTextEscapement, "XmbTextEscapement");

    function XmbTextEscapement(
                font_set  : XFontSet;
                text      : Interfaces.C.Char_Array;
                bytes_text: X.signed_int)
               return X.signed_int;                         -- Xlib.h:4048

    pragma Inline(XmbTextEscapement);

    function XwcTextEscapement(
                font_set  : XFontSet;
                text      : X.wchar_access;
                num_wchars: X.signed_int)
               return X.signed_int;                         -- Xlib.h:4056

    function XmbTextExtents(
                font_set              : XFontSet;
                text                  : X.Strings.const_charp;
                bytes_text            : X.signed_int;
                overall_ink_return    : access XRectangle;
                overall_logical_return: access XRectangle)
               return X.signed_int;                         -- Xlib.h:4064

    pragma Import(C, XmbTextExtents, "XmbTextExtents");

    function XmbTextExtents(
                font_set              : XFontSet;
                text                  : Interfaces.C.Char_Array;
                bytes_text            : X.signed_int;
                overall_ink_return    : access XRectangle;
                overall_logical_return: access XRectangle)
               return X.signed_int;                         -- Xlib.h:4064

    pragma Inline(XmbTextExtents);

    function XwcTextExtents(
                font_set              : XFontSet;
                text                  : X.wchar_access;
                num_wchars            : X.signed_int;
                overall_ink_return    : access XRectangle;
                overall_logical_return: access XRectangle)
               return X.signed_int;                         -- Xlib.h:4074

    function XmbTextPerCharExtents(
                font_set              : XFontSet;
                text                  : X.Strings.const_charp;
                bytes_text            : X.signed_int;
                ink_extents_buffer    : access XRectangle;
                logical_extents_buffer: access XRectangle;
                buffer_size           : X.signed_int;
                num_chars             : access X.signed_int;
                overall_ink_return    : access XRectangle;
                overall_logical_return: access XRectangle)
               return Status;                               -- Xlib.h:4084

    pragma Import(C, XmbTextPerCharExtents, "XmbTextPerCharExtents");

    function XmbTextPerCharExtents(
                font_set              : XFontSet;
                text                  : Interfaces.C.Char_Array;
                bytes_text            : X.signed_int;
                ink_extents_buffer    : access XRectangle;
                logical_extents_buffer: access XRectangle;
                buffer_size           : X.signed_int;
                num_chars             : access X.signed_int;
                overall_ink_return    : access XRectangle;
                overall_logical_return: access XRectangle)
               return X.signed_int;                         -- Xlib.h:4084

    pragma Inline(XmbTextPerCharExtents);

    function XwcTextPerCharExtents(
                font_set              : XFontSet;
                text                  : X.wchar_access;
                num_wchars            : X.signed_int;
                ink_extents_buffer    : access XRectangle;
                logical_extents_buffer: access XRectangle;
                buffer_size           : X.signed_int;
                num_chars             : access X.signed_int;
                overall_ink_return    : access XRectangle;
                overall_logical_return: access XRectangle)
               return Status;                               -- Xlib.h:4098

    procedure XmbDrawText(
                display   : access XDisplay;
                d         : X.Drawable;
                gc        : access XGC;
                xx        : X.signed_int;
                y         : X.signed_int;
                text_items: access XmbTextItem;
                nitems    : X.signed_int);                  -- Xlib.h:4112

    procedure XwcDrawText(
                display   : access XDisplay;
                d         : X.Drawable;
                gc        : access XGC;
                xx        : X.signed_int;
                y         : X.signed_int;
                text_items: access XwcTextItem;
                nitems    : X.signed_int);                  -- Xlib.h:4124

    procedure XmbDrawString(
                display   : access XDisplay;
                d         : X.Drawable;
                font_set  : XFontSet;
                gc        : access XGC;
                xx        : X.signed_int;
                y         : X.signed_int;
                text      : X.Strings.const_charp;
                bytes_text: X.signed_int);                  -- Xlib.h:4136

    pragma Import(C, XmbDrawString, "XmbDrawString");

    procedure XmbDrawString(
                display   : access XDisplay;
                d         : X.Drawable;
                font_set  : XFontSet;
                gc        : access XGC;
                xx        : X.signed_int;
                y         : X.signed_int;
                text      : Interfaces.C.Char_Array;
                bytes_text: X.signed_int);                  -- Xlib.h:4136

    pragma Inline(XmbDrawString);

    procedure XwcDrawString(
                display   : access XDisplay;
                d         : X.Drawable;
                font_set  : XFontSet;
                gc        : access XGC;
                xx        : X.signed_int;
                y         : X.signed_int;
                text      : X.wchar_access;
                num_wchars: X.signed_int);                  -- Xlib.h:4149

    procedure XmbDrawImageString(
                display   : access XDisplay;
                d         : X.Drawable;
                font_set  : XFontSet;
                gc        : access XGC;
                xx        : X.signed_int;
                y         : X.signed_int;
                text      : X.Strings.const_charp;
                bytes_text: X.signed_int);                  -- Xlib.h:4162

    pragma Import(C, XmbDrawImageString, "XmbDrawImageString");

    procedure XmbDrawImageString(
                display   : access XDisplay;
                d         : X.Drawable;
                font_set  : XFontSet;
                gc        : access XGC;
                xx        : X.signed_int;
                y         : X.signed_int;
                text      : Interfaces.C.Char_Array;
                bytes_text: X.signed_int);                  -- Xlib.h:4162

    pragma Inline(XmbDrawImageString);

    procedure XwcDrawImageString(
                display   : access XDisplay;
                d         : X.Drawable;
                font_set  : XFontSet;
                gc        : access XGC;
                xx        : X.signed_int;
                y         : X.signed_int;
                text      : X.wchar_access;
                num_wchars: X.signed_int);                  -- Xlib.h:4175

    function XOpenIM(
                dpy      : access XDisplay;
                rdb      : XrmHashBucketRec_access;
                res_name : X.Strings.const_charp;
                res_class: X.Strings.const_charp)
               return XIM;                                  -- Xlib.h:4188

    pragma Import(C, XOpenIM, "XOpenIM");

    function XOpenIM(
                dpy      : access XDisplay;
                rdb      : XrmHashBucketRec_access;
                res_name : Interfaces.C.Char_Array;
                res_class: Interfaces.C.Char_Array)
               return XIM;                                  -- Xlib.h:4188

    pragma Inline(XOpenIM);

    function XCloseIM(
                im: XIM)
               return Status;                               -- Xlib.h:4197

    function XGetIMValues(
                im  : XIM;
                args: Stdarg.ArgList := Stdarg.Empty)
               return X.Strings.charp;                      -- Xlib.h:4203

    function XDisplayOfIM(
                im: XIM)
               return XDisplay_access;                      -- Xlib.h:4209

    function XLocaleOfIM(
                im: XIM)
               return X.Strings.charp;                      -- Xlib.h:4215

    function XCreateIC(
                im  : XIM;
                args: Stdarg.ArgList := Stdarg.Empty)
               return XIC;                                  -- Xlib.h:4221

    procedure XDestroyIC(
                ic: XIC);                                   -- Xlib.h:4227

    procedure XSetICFocus(
                ic: XIC);                                   -- Xlib.h:4233

    procedure XUnsetICFocus(
                ic: XIC);                                   -- Xlib.h:4239

    function XwcResetIC(
                ic: XIC)
               return X.wchar_access;                       -- Xlib.h:4245

    function XmbResetIC(
                ic: XIC)
               return X.Strings.charp;                      -- Xlib.h:4251

    function XSetICValues(
                ic  : XIC;
                args: Stdarg.ArgList := Stdarg.Empty)
               return X.Strings.charp;                      -- Xlib.h:4257

    function XGetICValues(
                ic  : XIC;
                args: Stdarg.ArgList := Stdarg.Empty)
               return X.Strings.charp;                      -- Xlib.h:4263

    function XIMOfIC(
                ic: XIC)
               return XIM;                                  -- Xlib.h:4269

    function XFilterEvent(
                event : access Xevent;
                window: X.Window)
               return Bool;                                 -- Xlib.h:4275

    function XmbLookupString(
                ic           : XIC;
                event        : access XKeyEvent;
                buffer_return: X.Strings.charp;
                bytes_buffer : X.signed_int;
                keysym_return: access KeySym;
                status_return: access Status)
               return X.signed_int;                         -- Xlib.h:4282

    pragma Import(C, XmbLookupString, "XmbLookupString");

    procedure XmbLookupString(
                ic           : XIC;
                event        : access XKeyEvent;
                buffer_return: in out Interfaces.C.Char_Array;
                bytes_buffer : X.signed_int;
                keysym_return: access KeySym;
                status_return: access Status;
                result       : out X.signed_int);           -- Xlib.h:4282

    pragma Inline(XmbLookupString);

    function XwcLookupString(
                ic           : XIC;
                event        : access XKeyEvent;
                buffer_return: X.wchar_access;
                wchars_buffer: X.signed_int;
                keysym_return: access KeySym;
                status_return: access Status)
               return X.signed_int;                         -- Xlib.h:4293

    -- -----------------------------------------------------------------------
    -- Use Ada Stdarg package instead.
    --
    -- type XVaNestedList is access all c.void;                 -- Xlib.h:1123
    -- function XVaCreateNestedList(
    --             unused: X.signed_int)
    --            return XVaNestedList;                         -- Xlib.h:4304
    -- -----------------------------------------------------------------------

    -- ***********
    -- Xlib macros 
    -- ***********

    function ConnectionNumber(Dpy: access XDisplay) return X.signed_int;

    function RootWindow(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.Window;

    function DefaultScreen(Dpy: access XDisplay) return X.signed_int;

    function DefaultRootWindow(Dpy: access XDisplay) return X.Window;

    function DefaultVisual(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return Visual_access;

    function DefaultGC(Dpy: access XDisplay; Scr: X.Signed_Int) return GC;

    function BlackPixel(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.unsigned_long;

    function WhitePixel(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.unsigned_long;

    function QLength(Dpy: access XDisplay) return X.signed_int;

    function DisplayWidth(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.signed_int;

    function DisplayHeight(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.signed_int;

    function DisplayWidthMM(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.signed_int;

    function DisplayHeightMM(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.signed_int;

    function DisplayPlanes(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.signed_int;

    function DisplayCells(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.signed_int;

    function ScreenCount(Dpy: access XDisplay) return X.signed_int;

    function ServerVendor(Dpy: access XDisplay) return X.Strings.charp;

    function ProtocolVersion(Dpy: access XDisplay) return X.signed_int;

    function ProtocolRevision(Dpy: access XDisplay) return X.signed_int;

    function VendorRelease(Dpy: access XDisplay) return X.signed_int;

    function DisplayString(Dpy: access XDisplay) return X.Strings.charp;

    function DefaultDepth(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.signed_int;

    function DefaultColormap(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.Colormap;

    function BitmapUnit(Dpy: access XDisplay) return X.signed_int;

    function BitmapBitOrder(Dpy: access XDisplay) return X.signed_int;

    function BitmapPad(Dpy: access XDisplay) return X.signed_int;

    function ImageByteOrder(Dpy: access XDisplay) return X.signed_int;

    function NextRequest(Dpy: access XDisplay) return X.unsigned_long;

    function LastKnownRequestProcessed(Dpy: access XDisplay) 
        return X.unsigned_long;

    function ScreenOfDisplay(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return Screen_access;

    function DefaultScreenOfDisplay(Dpy: access XDisplay) return Screen_access;

    function DisplayOfScreen(S: access Screen) return XDisplay_access;

    function RootWindowOfScreen(S: access Screen) return X.Window;

    function BlackPixelOfScreen(S: access Screen) return X.unsigned_long;

    function WhitePixelOfScreen(S: access Screen) return X.unsigned_long;

    function DefaultColormapOfScreen(S: access Screen) return X.Colormap;

    function DefaultDepthOfScreen(S: access Screen) return X.signed_int;

    function DefaultGCOfScreen(S: access Screen) return GC;

    function DefaultVisualOfScreen(S: access Screen) return Visual_access;

    function WidthOfScreen(S: access Screen) return X.signed_int;

    function HeightOfScreen(S: access Screen) return X.signed_int;

    function WidthMMOfScreen(S: access Screen) return X.signed_int;

    function HeightMMOfScreen(S: access Screen) return X.signed_int;

    function PlanesOfScreen(S: access Screen) return X.signed_int;

    function CellsOfScreen(S: access Screen) return X.signed_int;

    function MinCmapsOfScreen(S: access Screen) return X.signed_int;

    function MaxCmapsOfScreen(S: access Screen) return X.signed_int;

    function DoesSaveUnders(S: access Screen) return Bool;

    function DoesBackingStore(S: access Screen) return X.signed_int;

    function EventMaskOfScreen(S: access Screen) return X.long;

    function XAllocID(Dpy: access XDisplay) return XID;

private

    type XKeytrans is null record;                          -- Xlib.h:514
    type XSQEvent is null record;                           -- Xlib.h:476
    type XrmHashBucketRec is null record;                   -- Xlib.h:485
    type XIMFilter is null record;                          -- Xlib.h:536
    type XFreeFuncs is null record;                         -- Xlib.h:457
    type XExten is null record;                             -- Xlib.h:502
    type XDisplayAtoms is null record;                      -- Xlib.h:516
    type XContextDB is null record;                         -- Xlib.h:524
    type XIC_rec is null record;                            -- Xlib.h:1067
    type XIM_rec is null record;                            -- Xlib.h:1066
    type XFontSet_rec is null record;                       -- Xlib.h:1048

    pragma Import(C, XQueryFont, "XQueryFont");             -- Xlib.h:1203
    pragma Import(C, XGetMotionEvents, "XGetMotionEvents"); -- Xlib.h:1211
    pragma Import(C, XDeleteModifiermapEntry, "XDeleteModifiermapEntry");
                                                            -- Xlib.h:1221
    pragma Import(C, XGetModifierMapping, "XGetModifierMapping");
                                                            -- Xlib.h:1233
    pragma Import(C, XInsertModifiermapEntry, "XInsertModifiermapEntry");
                                                            -- Xlib.h:1239
    pragma Import(C, XNewModifiermap, "XNewModifiermap");   -- Xlib.h:1251
    pragma Import(C, XGetImage, "XGetImage");               -- Xlib.h:1271
    pragma Import(C, XGetSubImage, "XGetSubImage");         -- Xlib.h:1283
    pragma Import(C, XrmInitialize, "XrmInitialize");       -- Xlib.h:1308
    pragma Import(C, XFetchBytes, "XFetchBytes");           -- Xlib.h:1314
    pragma Import(C, XFetchBuffer, "XFetchBuffer");         -- Xlib.h:1320
    pragma Import(C, XGetAtomName, "XGetAtomName");         -- Xlib.h:1327
    pragma Import(C, XKeysymToString, "XKeysymToString");   -- Xlib.h:1345
    pragma Import(C, XSynchronize, "XSynchronize");         -- Xlib.h:1351
    pragma Import(C, XSetAfterFunction, "XSetAfterFunction");
                                                            -- Xlib.h:1357
    pragma Import(C, XCopyColormapAndFree, "XCopyColormapAndFree");
                                                            -- Xlib.h:1374
    pragma Import(C, XCreateColormap, "XCreateColormap");   -- Xlib.h:1380
    pragma Import(C, XCreatePixmapCursor, "XCreatePixmapCursor");
                                                            -- Xlib.h:1388
    pragma Import(C, XCreateGlyphCursor, "XCreateGlyphCursor");
                                                            -- Xlib.h:1399
    pragma Import(C, XCreateFontCursor, "XCreateFontCursor");
                                                            -- Xlib.h:1410
    pragma Import(C, XCreateGC, "XCreateGC");               -- Xlib.h:1422
    pragma Import(C, XGContextFromGC, "XGContextFromGC");   -- Xlib.h:1430
    pragma Import(C, XFlushGC, "XFlushGC");                 -- Xlib.h:1435
    pragma Import(C, XCreatePixmap, "XCreatePixmap");       -- Xlib.h:1441
    pragma Import(C, XCreateSimpleWindow, "XCreateSimpleWindow");
                                                            -- Xlib.h:1471
    pragma Import(C, XGetSelectionOwner, "XGetSelectionOwner");
                                                            -- Xlib.h:1484
    pragma Import(C, XCreateWindow, "XCreateWindow");       -- Xlib.h:1490
    pragma Import(C, XListInstalledColormaps, "XListInstalledColormaps");
                                                            -- Xlib.h:1506
    pragma Import(C, XGetFontPath, "XGetFontPath");         -- Xlib.h:1530
    pragma Import(C, XListExtensions, "XListExtensions");   -- Xlib.h:1536
    pragma Import(C, XListProperties, "XListProperties");   -- Xlib.h:1542
    pragma Import(C, XListHosts, "XListHosts");             -- Xlib.h:1549
    pragma Import(C, XKeycodeToKeysym, "XKeycodeToKeysym"); -- Xlib.h:1556
    pragma Import(C, XLookupKeysym_func, "XLookupKeysym");  -- Xlib.h:1567
    pragma Import(C, XGetKeyboardMapping, "XGetKeyboardMapping");
                                                            -- Xlib.h:1573
    pragma Import(C, XMaxRequestSize, "XMaxRequestSize");   -- Xlib.h:1590
    pragma Import(C, XResourceManagerString, "XResourceManagerString");
                                                            -- Xlib.h:1595
    pragma Import(C, XScreenResourceString, "XScreenResourceString");
                                                            -- Xlib.h:1600
    pragma Import(C, XDisplayMotionBufferSize, "XDisplayMotionBufferSize");
                                                            -- Xlib.h:1605
    pragma Import(C, XVisualIDFromVisual, "XVisualIDFromVisual");
                                                            -- Xlib.h:1610
    pragma Import(C, XAddExtension, "XAddExtension");       -- Xlib.h:1625
    pragma Import(C, XFindOnExtensionList, "XFindOnExtensionList");
                                                            -- Xlib.h:1630
    pragma Import(C, XEHeadOfExtensionList, "XEHeadOfExtensionList");
                                                            -- Xlib.h:1636
    pragma Import(C, XRootWindow, "XRootWindow");           -- Xlib.h:1643
    pragma Import(C, XDefaultRootWindow, "XDefaultRootWindow");
                                                            -- Xlib.h:1649
    pragma Import(C, XRootWindowOfScreen, "XRootWindowOfScreen");
                                                            -- Xlib.h:1654
    pragma Import(C, XDefaultVisual, "XDefaultVisual");     -- Xlib.h:1659
    pragma Import(C, XDefaultVisualOfScreen, "XDefaultVisualOfScreen");
                                                            -- Xlib.h:1665
    pragma Import(C, XDefaultGC, "XDefaultGC");             -- Xlib.h:1670
    pragma Import(C, XDefaultGCOfScreen, "XDefaultGCOfScreen");
                                                            -- Xlib.h:1676
    pragma Import(C, XBlackPixel, "XBlackPixel");           -- Xlib.h:1681
    pragma Import(C, XWhitePixel, "XWhitePixel");           -- Xlib.h:1687
    pragma Import(C, XAllPlanes, "XAllPlanes");             -- Xlib.h:1693
    pragma Import(C, XBlackPixelOfScreen, "XBlackPixelOfScreen");
                                                            -- Xlib.h:1698
    pragma Import(C, XWhitePixelOfScreen, "XWhitePixelOfScreen");
                                                            -- Xlib.h:1703
    pragma Import(C, XNextRequest, "XNextRequest");         -- Xlib.h:1708
    pragma Import(C, XLastKnownRequestProcessed, "XLastKnownRequestProcessed");
                                                            -- Xlib.h:1713
    pragma Import(C, XServerVendor, "XServerVendor");       -- Xlib.h:1718
    pragma Import(C, XDisplayString, "XDisplayString");     -- Xlib.h:1723
    pragma Import(C, XDefaultColormap, "XDefaultColormap"); -- Xlib.h:1728
    pragma Import(C, XDefaultColormapOfScreen, "XDefaultColormapOfScreen");
                                                            -- Xlib.h:1734
    pragma Import(C, XDisplayOfScreen, "XDisplayOfScreen"); -- Xlib.h:1739
    pragma Import(C, XScreenOfDisplay, "XScreenOfDisplay"); -- Xlib.h:1744
    pragma Import(C, XDefaultScreenOfDisplay, "XDefaultScreenOfDisplay");
                                                            -- Xlib.h:1750
    pragma Import(C, XEventMaskOfScreen, "XEventMaskOfScreen");
                                                            -- Xlib.h:1755
    pragma Import(C, XScreenNumberOfScreen, "XScreenNumberOfScreen");
                                                            -- Xlib.h:1761
    pragma Import(C, XSetErrorHandler, "XSetErrorHandler"); -- Xlib.h:1774
    pragma Import(C, XSetIOErrorHandler, "XSetIOErrorHandler");
                                                            -- Xlib.h:1787
    pragma Import(C, XListPixmapFormats, "XListPixmapFormats");
                                                            -- Xlib.h:1794
    pragma Import(C, XListDepths, "XListDepths");           -- Xlib.h:1800
    pragma Import(C, XReconfigureWMWindow, "XReconfigureWMWindow");
                                                            -- Xlib.h:1810
    pragma Import(C, XGetWMProtocols, "XGetWMProtocols");   -- Xlib.h:1820
    pragma Import(C, XSetWMProtocols, "XSetWMProtocols");   -- Xlib.h:1828
    pragma Import(C, XIconifyWindow, "XIconifyWindow");     -- Xlib.h:1836
    pragma Import(C, XWithdrawWindow, "XWithdrawWindow");   -- Xlib.h:1843
    pragma Import(C, XGetCommand, "XGetCommand");           -- Xlib.h:1850
    pragma Import(C, XGetWMColormapWindows, "XGetWMColormapWindows");
                                                            -- Xlib.h:1858
    pragma Import(C, XSetWMColormapWindows, "XSetWMColormapWindows");
                                                            -- Xlib.h:1866
    pragma Import(C, XFreeStringList, "XFreeStringList");   -- Xlib.h:1874
    pragma Import(C, XSetTransientForHint, "XSetTransientForHint");
                                                            -- Xlib.h:1879
    pragma Import(C, XActivateScreenSaver, "XActivateScreenSaver");
                                                            -- Xlib.h:1889
    pragma Import(C, XAddHost, "XAddHost");                 -- Xlib.h:1895
    pragma Import(C, XAddHosts, "XAddHosts");               -- Xlib.h:1902
    pragma Import(C, XAddToExtensionList, "XAddToExtensionList");
                                                            -- Xlib.h:1910
    pragma Import(C, XAddToSaveSet, "XAddToSaveSet");       -- Xlib.h:1917
    pragma Import(C, XAllocColor, "XAllocColor");           -- Xlib.h:1924
    pragma Import(C, XAllocColorCells, "XAllocColorCells"); -- Xlib.h:1932
    pragma Import(C, XAllocColorPlanes, "XAllocColorPlanes");
                                                            -- Xlib.h:1944
    pragma Import(C, XAllowEvents, "XAllowEvents");         -- Xlib.h:1970
    pragma Import(C, XAutoRepeatOff, "XAutoRepeatOff");     -- Xlib.h:1978
    pragma Import(C, XAutoRepeatOn, "XAutoRepeatOn");       -- Xlib.h:1984
    pragma Import(C, XBell, "XBell");                       -- Xlib.h:1990
    pragma Import(C, XBitmapBitOrder, "XBitmapBitOrder");   -- Xlib.h:1997
    pragma Import(C, XBitmapPad, "XBitmapPad");             -- Xlib.h:2003
    pragma Import(C, XBitmapUnit, "XBitmapUnit");           -- Xlib.h:2009
    pragma Import(C, XCellsOfScreen, "XCellsOfScreen");     -- Xlib.h:2015
    pragma Import(C, XChangeActivePointerGrab, "XChangeActivePointerGrab");
                                                            -- Xlib.h:2021
    pragma Import(C, XChangeGC, "XChangeGC");               -- Xlib.h:2030
    pragma Import(C, XChangeKeyboardControl, "XChangeKeyboardControl");
                                                            -- Xlib.h:2039
    pragma Import(C, XChangeKeyboardMapping, "XChangeKeyboardMapping");
                                                            -- Xlib.h:2047
    pragma Import(C, XChangePointerControl, "XChangePointerControl");
                                                            -- Xlib.h:2057
    pragma Import(C, XChangeProperty, "XChangeProperty");   -- Xlib.h:2068

    pragma Import(C, XChangeSaveSet, "XChangeSaveSet");     -- Xlib.h:2081
    pragma Import(C, XChangeWindowAttributes, "XChangeWindowAttributes");
                                                            -- Xlib.h:2089
    pragma Import(C, XCheckIfEvent, "XCheckIfEvent");       -- Xlib.h:2098
    pragma Import(C, XCheckMaskEvent, "XCheckMaskEvent");   -- Xlib.h:2113
    pragma Import(C, XCheckTypedEvent, "XCheckTypedEvent"); -- Xlib.h:2121
    pragma Import(C, XCheckTypedWindowEvent, "XCheckTypedWindowEvent");
                                                            -- Xlib.h:2129
    pragma Import(C, XCheckWindowEvent, "XCheckWindowEvent");
                                                            -- Xlib.h:2138
    pragma Import(C, XCirculateSubwindows, "XCirculateSubwindows");
                                                            -- Xlib.h:2147
    pragma Import(C, XCirculateSubwindowsDown, "XCirculateSubwindowsDown");
                                                            -- Xlib.h:2155
    pragma Import(C, XCirculateSubwindowsUp, "XCirculateSubwindowsUp");
                                                            -- Xlib.h:2162
    pragma Import(C, XClearArea, "XClearArea");             -- Xlib.h:2169
    pragma Import(C, XClearWindow, "XClearWindow");         -- Xlib.h:2181
    pragma Import(C, XCloseDisplay, "XCloseDisplay");       -- Xlib.h:2188
    pragma Import(C, XConfigureWindow, "XConfigureWindow"); -- Xlib.h:2194
    pragma Import(C, XConnectionNumber, "XConnectionNumber");
                                                            -- Xlib.h:2203
    pragma Import(C, XConvertSelection, "XConvertSelection");
                                                            -- Xlib.h:2209
    pragma Import(C, XCopyArea, "XCopyArea");               -- Xlib.h:2220
    pragma Import(C, XCopyGC, "XCopyGC");                   -- Xlib.h:2235
    pragma Import(C, XCopyPlane, "XCopyPlane");             -- Xlib.h:2244
    pragma Import(C, XDefaultDepth, "XDefaultDepth");       -- Xlib.h:2260
    pragma Import(C, XDefaultDepthOfScreen, "XDefaultDepthOfScreen");
                                                            -- Xlib.h:2267
    pragma Import(C, XDefaultScreen, "XDefaultScreen");     -- Xlib.h:2273
    pragma Import(C, XDefineCursor, "XDefineCursor");       -- Xlib.h:2279
    pragma Import(C, XDeleteProperty, "XDeleteProperty");   -- Xlib.h:2287
    pragma Import(C, XDestroyWindow, "XDestroyWindow");     -- Xlib.h:2295
    pragma Import(C, XDestroySubwindows, "XDestroySubwindows");
                                                            -- Xlib.h:2302
    pragma Import(C, XDoesBackingStore, "XDoesBackingStore");
                                                            -- Xlib.h:2309
    pragma Import(C, XDoesSaveUnders, "XDoesSaveUnders");   -- Xlib.h:2315
    pragma Import(C, XDisableAccessControl, "XDisableAccessControl");
                                                            -- Xlib.h:2321
    pragma Import(C, XDisplayCells, "XDisplayCells");       -- Xlib.h:2328
    pragma Import(C, XDisplayHeight, "XDisplayHeight");     -- Xlib.h:2335
    pragma Import(C, XDisplayHeightMM, "XDisplayHeightMM"); -- Xlib.h:2342
    pragma Import(C, XDisplayKeycodes, "XDisplayKeycodes"); -- Xlib.h:2349
    pragma Import(C, XDisplayPlanes, "XDisplayPlanes");     -- Xlib.h:2357
    pragma Import(C, XDisplayWidth, "XDisplayWidth");       -- Xlib.h:2364
    pragma Import(C, XDisplayWidthMM, "XDisplayWidthMM");   -- Xlib.h:2371
    pragma Import(C, XDrawArc, "XDrawArc");                 -- Xlib.h:2378
    pragma Import(C, XDrawArcs, "XDrawArcs");               -- Xlib.h:2392
    pragma Import(C, XDrawImageString16, "XDrawImageString16");
                                                            -- Xlib.h:2414
    pragma Import(C, XDrawLine, "XDrawLine");               -- Xlib.h:2426
    pragma Import(C, XDrawLines, "XDrawLines");             -- Xlib.h:2438
    pragma Import(C, XDrawPoint, "XDrawPoint");             -- Xlib.h:2449
    pragma Import(C, XDrawPoints, "XDrawPoints");           -- Xlib.h:2459
    pragma Import(C, XDrawRectangle, "XDrawRectangle");     -- Xlib.h:2470
    pragma Import(C, XDrawRectangles, "XDrawRectangles");   -- Xlib.h:2482
    pragma Import(C, XDrawSegments, "XDrawSegments");       -- Xlib.h:2492
    pragma Import(C, XDrawString16, "XDrawString16");       -- Xlib.h:2514
    pragma Import(C, XDrawText, "XDrawText");               -- Xlib.h:2526
    pragma Import(C, XDrawText16, "XDrawText16");           -- Xlib.h:2538
    pragma Import(C, XEnableAccessControl, "XEnableAccessControl");
                                                            -- Xlib.h:2550
    pragma Import(C, XEventsQueued, "XEventsQueued");       -- Xlib.h:2556
    pragma Import(C, XFetchName, "XFetchName");             -- Xlib.h:2563
    pragma Import(C, XFillArc, "XFillArc");                 -- Xlib.h:2571
    pragma Import(C, XFillArcs, "XFillArcs");               -- Xlib.h:2585
    pragma Import(C, XFillPolygon, "XFillPolygon");         -- Xlib.h:2595
    pragma Import(C, XFillRectangle, "XFillRectangle");     -- Xlib.h:2607
    pragma Import(C, XFillRectangles, "XFillRectangles");   -- Xlib.h:2619
    pragma Import(C, XFlush, "XFlush");                     -- Xlib.h:2629
    pragma Import(C, XForceScreenSaver, "XForceScreenSaver");
                                                            -- Xlib.h:2635
    pragma Import(C, XFree, "XFree");                       -- Xlib.h:2642
    pragma Import(C, XFreeColormap, "XFreeColormap");       -- Xlib.h:2648
    pragma Import(C, XFreeColors, "XFreeColors");           -- Xlib.h:2655
    pragma Import(C, XFreeCursor, "XFreeCursor");           -- Xlib.h:2665
    pragma Import(C, XFreeExtensionList, "XFreeExtensionList");
                                                            -- Xlib.h:2672
    pragma Import(C, XFreeFont, "XFreeFont");               -- Xlib.h:2678
    pragma Import(C, XFreeFontInfo, "XFreeFontInfo");       -- Xlib.h:2685
    pragma Import(C, XFreeFontNames, "XFreeFontNames");     -- Xlib.h:2693
    pragma Import(C, XFreeFontPath, "XFreeFontPath");       -- Xlib.h:2699
    pragma Import(C, XFreeGC, "XFreeGC");                   -- Xlib.h:2705
    pragma Import(C, XFreeModifiermap, "XFreeModifiermap"); -- Xlib.h:2712
    pragma Import(C, XFreePixmap, "XFreePixmap");           -- Xlib.h:2718
    pragma Import(C, XGetFontProperty, "XGetFontProperty"); -- Xlib.h:2763
    pragma Import(C, XGetGCValues, "XGetGCValues");         -- Xlib.h:2771
    pragma Import(C, XGetGeometry, "XGetGeometry");         -- Xlib.h:2780
    pragma Import(C, XGetIconName, "XGetIconName");         -- Xlib.h:2794
    pragma Import(C, XGetInputFocus, "XGetInputFocus");     -- Xlib.h:2802
    pragma Import(C, XGetKeyboardControl, "XGetKeyboardControl");
                                                            -- Xlib.h:2810
    pragma Import(C, XGetPointerControl, "XGetPointerControl");
                                                            -- Xlib.h:2817
    pragma Import(C, XGetPointerMapping, "XGetPointerMapping");
                                                            -- Xlib.h:2826
    pragma Import(C, XGetScreenSaver, "XGetScreenSaver");   -- Xlib.h:2834
    pragma Import(C, XGetTransientForHint, "XGetTransientForHint");
                                                            -- Xlib.h:2844
    pragma Import(C, XGetWindowProperty, "XGetWindowProperty");
                                                            -- Xlib.h:2852
    pragma Import(C, XGetWindowAttributes, "XGetWindowAttributes");
                                                            -- Xlib.h:2869
    pragma Import(C, XGrabButton, "XGrabButton");           -- Xlib.h:2877
    pragma Import(C, XGrabKey, "XGrabKey");                 -- Xlib.h:2892
    pragma Import(C, XGrabKeyboard, "XGrabKeyboard");       -- Xlib.h:2904
    pragma Import(C, XGrabPointer, "XGrabPointer");         -- Xlib.h:2915
    pragma Import(C, XGrabServer, "XGrabServer");           -- Xlib.h:2929
    pragma Import(C, XHeightMMOfScreen, "XHeightMMOfScreen");
                                                            -- Xlib.h:2935
    pragma Import(C, XHeightOfScreen, "XHeightOfScreen");   -- Xlib.h:2941
    pragma Import(C, XIfEvent, "XIfEvent");                 -- Xlib.h:2947
    pragma Import(C, XImageByteOrder, "XImageByteOrder");   -- Xlib.h:2962
    pragma Import(C, XInstallColormap, "XInstallColormap"); -- Xlib.h:2968
    pragma Import(C, XKeysymToKeycode, "XKeysymToKeycode"); -- Xlib.h:2975
    pragma Import(C, XKillClient, "XKillClient");           -- Xlib.h:2982
    pragma Import(C, XLowerWindow, "XLowerWindow");         -- Xlib.h:3005
    pragma Import(C, XMapRaised, "XMapRaised");             -- Xlib.h:3012
    pragma Import(C, XMapSubwindows, "XMapSubwindows");     -- Xlib.h:3019
    pragma Import(C, XMapWindow, "XMapWindow");             -- Xlib.h:3026
    pragma Import(C, XMaskEvent, "XMaskEvent");             -- Xlib.h:3033
    pragma Import(C, XMaxCmapsOfScreen, "XMaxCmapsOfScreen");
                                                            -- Xlib.h:3041
    pragma Import(C, XMinCmapsOfScreen, "XMinCmapsOfScreen");
                                                            -- Xlib.h:3047
    pragma Import(C, XMoveResizeWindow, "XMoveResizeWindow");
                                                            -- Xlib.h:3053
    pragma Import(C, XMoveWindow, "XMoveWindow");           -- Xlib.h:3064
    pragma Import(C, XNextEvent, "XNextEvent");             -- Xlib.h:3073
    pragma Import(C, XNoOp, "XNoOp");                       -- Xlib.h:3080
    pragma Import(C, XPeekEvent, "XPeekEvent");             -- Xlib.h:3105
    pragma Import(C, XPeekIfEvent, "XPeekIfEvent");         -- Xlib.h:3112
    pragma Import(C, XPending, "XPending");                 -- Xlib.h:3127
    pragma Import(C, XPlanesOfScreen, "XPlanesOfScreen");   -- Xlib.h:3133
    pragma Import(C, XProtocolRevision, "XProtocolRevision");
                                                            -- Xlib.h:3140
    pragma Import(C, XProtocolVersion, "XProtocolVersion"); -- Xlib.h:3146
    pragma Import(C, XPutBackEvent, "XPutBackEvent");       -- Xlib.h:3153
    pragma Import(C, XPutImage, "XPutImage");               -- Xlib.h:3160
    pragma Import(C, XQLength, "XQLength");                 -- Xlib.h:3175
    pragma Import(C, XQueryBestCursor, "XQueryBestCursor"); -- Xlib.h:3181
    pragma Import(C, XQueryBestSize, "XQueryBestSize");     -- Xlib.h:3192
    pragma Import(C, XQueryBestStipple, "XQueryBestStipple");
                                                            -- Xlib.h:3204
    pragma Import(C, XQueryBestTile, "XQueryBestTile");     -- Xlib.h:3215
    pragma Import(C, XQueryColor, "XQueryColor");           -- Xlib.h:3226
    pragma Import(C, XQueryColors, "XQueryColors");         -- Xlib.h:3234
    pragma Import(C, XQueryKeymap, "XQueryKeymap");         -- Xlib.h:3260
    pragma Import(C, XQueryPointer, "XQueryPointer");       -- Xlib.h:3260
    pragma Import(C, XQueryTextExtents16, "XQueryTextExtents16");
                                                            -- Xlib.h:3287
    pragma Import(C, XQueryTree, "XQueryTree");             -- Xlib.h:3300
    pragma Import(C, XRaiseWindow, "XRaiseWindow");         -- Xlib.h:3311
    pragma Import(C, XRebindKeysym, "XRebindKeysym");       -- Xlib.h:3331
    pragma Import(C, XRecolorCursor, "XRecolorCursor");     -- Xlib.h:3342
    pragma Import(C, XRefreshKeyboardMapping, "XRefreshKeyboardMapping");
                                                            -- Xlib.h:3351
    pragma Import(C, XRemoveFromSaveSet, "XRemoveFromSaveSet");
                                                            -- Xlib.h:3357
    pragma Import(C, XRemoveHost, "XRemoveHost");           -- Xlib.h:3364
    pragma Import(C, XRemoveHosts, "XRemoveHosts");         -- Xlib.h:3371
    pragma Import(C, XReparentWindow, "XReparentWindow");   -- Xlib.h:3379
    pragma Import(C, XResetScreenSaver, "XResetScreenSaver");
                                                            -- Xlib.h:3389
    pragma Import(C, XResizeWindow, "XResizeWindow");       -- Xlib.h:3395
    pragma Import(C, XRestackWindows, "XRestackWindows");   -- Xlib.h:3404
    pragma Import(C, XRotateBuffers, "XRotateBuffers");     -- Xlib.h:3412
    pragma Import(C, XRotateWindowProperties, "XRotateWindowProperties");
                                                            -- Xlib.h:3419
    pragma Import(C, XScreenCount, "XScreenCount");         -- Xlib.h:3429
    pragma Import(C, XSelectInput, "XSelectInput");         -- Xlib.h:3435
    pragma Import(C, XSendEvent, "XSendEvent");             -- Xlib.h:3443
    pragma Import(C, XSetAccessControl, "XSetAccessControl");
                                                            -- Xlib.h:3453
    pragma Import(C, XSetArcMode, "XSetArcMode");           -- Xlib.h:3460
    pragma Import(C, XSetBackground, "XSetBackground");     -- Xlib.h:3468
    pragma Import(C, XSetClipMask, "XSetClipMask");         -- Xlib.h:3476
    pragma Import(C, XSetClipOrigin, "XSetClipOrigin");     -- Xlib.h:3484
    pragma Import(C, XSetClipRectangles, "XSetClipRectangles");
                                                            -- Xlib.h:3493
    pragma Import(C, XSetCloseDownMode, "XSetCloseDownMode");
                                                            -- Xlib.h:3505
    pragma Import(C, XSetCommand, "XSetCommand");           -- Xlib.h:3512
    pragma Import(C, XSetFillRule, "XSetFillRule");         -- Xlib.h:3531
    pragma Import(C, XSetFillStyle, "XSetFillStyle");       -- Xlib.h:3539
    pragma Import(C, XSetFont, "XSetFont");                 -- Xlib.h:3547
    pragma Import(C, XSetFontPath, "XSetFontPath");         -- Xlib.h:3555
    pragma Import(C, XSetForeground, "XSetForeground");     -- Xlib.h:3563
    pragma Import(C, XSetFunction, "XSetFunction");         -- Xlib.h:3571
    pragma Import(C, XSetGraphicsExposures, "XSetGraphicsExposures");
                                                            -- Xlib.h:3579
    pragma Import(C, XSetInputFocus, "XSetInputFocus");     -- Xlib.h:3595
    pragma Import(C, XSetLineAttributes, "XSetLineAttributes");
                                                            -- Xlib.h:3604
    pragma Import(C, XSetModifierMapping, "XSetModifierMapping");
                                                            -- Xlib.h:3615
    pragma Import(C, XSetPlaneMask, "XSetPlaneMask");       -- Xlib.h:3622
    pragma Import(C, XSetPointerMapping, "XSetPointerMapping");
                                                            -- Xlib.h:3630

    pragma Import(C, XSetScreenSaver, "XSetScreenSaver");   -- Xlib.h:3638
    pragma Import(C, XSetSelectionOwner, "XSetSelectionOwner");
                                                            -- Xlib.h:3648
    pragma Import(C, XSetState, "XSetState");               -- Xlib.h:3657
    pragma Import(C, XSetStipple, "XSetStipple");           -- Xlib.h:3668
    pragma Import(C, XSetSubwindowMode, "XSetSubwindowMode");
                                                            -- Xlib.h:3676
    pragma Import(C, XSetTSOrigin, "XSetTSOrigin");         -- Xlib.h:3684
    pragma Import(C, XSetTile, "XSetTile");                 -- Xlib.h:3693
    pragma Import(C, XSetWindowBackground, "XSetWindowBackground");
                                                            -- Xlib.h:3701
    pragma Import(C, XSetWindowBackgroundPixmap, "XSetWindowBackgroundPixmap");
                                                            -- Xlib.h:3709
    pragma Import(C, XSetWindowBorder, "XSetWindowBorder"); -- Xlib.h:3717
    pragma Import(C, XSetWindowBorderPixmap, "XSetWindowBorderPixmap");
                                                            -- Xlib.h:3725
    pragma Import(C, XSetWindowBorderWidth, "XSetWindowBorderWidth");
                                                            -- Xlib.h:3733
    pragma Import(C, XSetWindowColormap, "XSetWindowColormap");
                                                            -- Xlib.h:3741
    pragma Import(C, XStoreColor, "XStoreColor");           -- Xlib.h:3766
    pragma Import(C, XStoreColors, "XStoreColors");         -- Xlib.h:3774
    pragma Import(C, XSync, "XSync");                       -- Xlib.h:3801
    pragma Import(C, XTextExtents16, "XTextExtents16");     -- Xlib.h:3820
    pragma Import(C, XTextWidth16, "XTextWidth16");         -- Xlib.h:3840
    pragma Import(C, XTranslateCoordinates, "XTranslateCoordinates");
                                                            -- Xlib.h:3848
    pragma Import(C, XUndefineCursor, "XUndefineCursor");   -- Xlib.h:3861
    pragma Import(C, XUngrabButton, "XUngrabButton");       -- Xlib.h:3868
    pragma Import(C, XUngrabKey, "XUngrabKey");             -- Xlib.h:3877
    pragma Import(C, XUngrabKeyboard, "XUngrabKeyboard");   -- Xlib.h:3886
    pragma Import(C, XUngrabPointer, "XUngrabPointer");     -- Xlib.h:3893
    pragma Import(C, XUngrabServer, "XUngrabServer");       -- Xlib.h:3900
    pragma Import(C, XUninstallColormap, "XUninstallColormap");
                                                            -- Xlib.h:3906
    pragma Import(C, XUnloadFont, "XUnloadFont");           -- Xlib.h:3913
    pragma Import(C, XUnmapSubwindows, "XUnmapSubwindows"); -- Xlib.h:3920
    pragma Import(C, XUnmapWindow, "XUnmapWindow");         -- Xlib.h:3927
    pragma Import(C, XVendorRelease, "XVendorRelease");     -- Xlib.h:3934
    pragma Import(C, XWarpPointer, "XWarpPointer");         -- Xlib.h:3940
    pragma Import(C, XWidthMMOfScreen, "XWidthMMOfScreen"); -- Xlib.h:3954
    pragma Import(C, XWidthOfScreen, "XWidthOfScreen");     -- Xlib.h:3960
    pragma Import(C, XWindowEvent, "XWindowEvent");         -- Xlib.h:3966
    pragma Import(C, XSupportsLocale, "XSupportsLocale");   -- Xlib.h:3987
    pragma Import(C, XFreeFontSet, "XFreeFontSet");         -- Xlib.h:4009
    pragma Import(C, XFontsOfFontSet, "XFontsOfFontSet");   -- Xlib.h:4016
    pragma Import(C, XBaseFontNameListOfFontSet, "XBaseFontNameListOfFontSet");
                                                            -- Xlib.h:4024
    pragma Import(C, XLocaleOfFontSet, "XLocaleOfFontSet"); -- Xlib.h:4030
    pragma Import(C, XContextDependentDrawing, "XContextDependentDrawing");
                                                            -- Xlib.h:4036
    pragma Import(C, XExtentsOfFontSet, "XExtentsOfFontSet");
                                                            -- Xlib.h:4042
    pragma Import(C, XwcTextEscapement, "XwcTextEscapement");
                                                            -- Xlib.h:4056
    pragma Import(C, XwcTextExtents, "XwcTextExtents");     -- Xlib.h:4074
    pragma Import(C, XwcTextPerCharExtents, "XwcTextPerCharExtents");
                                                            -- Xlib.h:4098
    pragma Import(C, XmbDrawText, "XmbDrawText");           -- Xlib.h:4112
    pragma Import(C, XwcDrawText, "XwcDrawText");           -- Xlib.h:4124
    pragma Import(C, XwcDrawString, "XwcDrawString");       -- Xlib.h:4149
    pragma Import(C, XwcDrawImageString, "XwcDrawImageString");
                                                            -- Xlib.h:4175
    pragma Import(C, XCloseIM, "XCloseIM");                 -- Xlib.h:4197
    pragma Import(C, XDisplayOfIM, "XDisplayOfIM");         -- Xlib.h:4209
    pragma Import(C, XLocaleOfIM, "XLocaleOfIM");           -- Xlib.h:4215
    pragma Import(C, XDestroyIC, "XDestroyIC");             -- Xlib.h:4227
    pragma Import(C, XSetICFocus, "XSetICFocus");           -- Xlib.h:4233
    pragma Import(C, XUnsetICFocus, "XUnsetICFocus");       -- Xlib.h:4239
    pragma Import(C, XwcResetIC, "XwcResetIC");             -- Xlib.h:4245
    pragma Import(C, XmbResetIC, "XmbResetIC");             -- Xlib.h:4251
    pragma Import(C, XIMOfIC, "XIMOfIC");                   -- Xlib.h:4269
    pragma Import(C, XFilterEvent, "XFilterEvent");         -- Xlib.h:4275
    pragma Import(C, XwcLookupString, "XwcLookupString");   -- Xlib.h:4293

    pragma Inline(ConnectionNumber);
    pragma Inline(RootWindow);
    pragma Inline(DefaultScreen);
    pragma Inline(DefaultRootWindow);
    pragma Inline(DefaultVisual);
    pragma Inline(DefaultGC);
    pragma Inline(BlackPixel);
    pragma Inline(WhitePixel);
    pragma Inline(QLength);
    pragma Inline(DisplayWidth);
    pragma Inline(DisplayHeight);
    pragma Inline(DisplayWidthMM);
    pragma Inline(DisplayHeightMM);
    pragma Inline(DisplayPlanes);
    pragma Inline(DisplayCells);
    pragma Inline(ScreenCount);
    pragma Inline(ServerVendor);
    pragma Inline(ProtocolVersion);
    pragma Inline(ProtocolRevision);
    pragma Inline(VendorRelease);
    pragma Inline(DisplayString);
    pragma Inline(DefaultDepth);
    pragma Inline(DefaultColormap);
    pragma Inline(BitmapUnit);
    pragma Inline(BitmapBitOrder);
    pragma Inline(BitmapPad);
    pragma Inline(ImageByteOrder);
    pragma Inline(NextRequest);
    pragma Inline(LastKnownRequestProcessed);
    pragma Inline(ScreenOfDisplay);
    pragma Inline(DefaultScreenOfDisplay);
    pragma Inline(DisplayOfScreen);
    pragma Inline(RootWindowOfScreen);
    pragma Inline(BlackPixelOfScreen);
    pragma Inline(WhitePixelOfScreen);
    pragma Inline(DefaultColormapOfScreen);
    pragma Inline(DefaultDepthOfScreen);
    pragma Inline(DefaultGCOfScreen);
    pragma Inline(DefaultVisualOfScreen);
    pragma Inline(WidthOfScreen);
    pragma Inline(HeightOfScreen);
    pragma Inline(WidthMMOfScreen);
    pragma Inline(HeightMMOfScreen);
    pragma Inline(PlanesOfScreen);
    pragma Inline(CellsOfScreen);
    pragma Inline(MinCmapsOfScreen);
    pragma Inline(MaxCmapsOfScreen);
    pragma Inline(DoesSaveUnders);
    pragma Inline(DoesBackingStore);
    pragma Inline(EventMaskOfScreen);
    pragma Inline(XAllocID);

    pragma Convention(C,  XExtData);
    pragma Convention(C,  XExtCodes);
    pragma Convention(C,  XPixmapFormatValues);
    pragma Convention(C,  XGCValues);
    pragma Convention(C,  XGC);
    pragma Convention(C,  Visual);
    pragma Convention(C,  Depth);
    pragma Convention(C,  Screen);
    pragma Convention(C,  ScreenFormat);
    pragma Convention(C,  XSetWindowAttributes);
    pragma Convention(C,  XWindowAttributes);
    pragma Convention(C,  XHostAddress);
    pragma Convention(C,  funcs);
    pragma Convention(C,  XWindowChanges);
    pragma Convention(C,  XColor);
    pragma Convention(C,  XSegment);
    pragma Convention(C,  XPoint);
    pragma Convention(C,  XRectangle);
    pragma Convention(C,  XArc);
    pragma Convention(C,  XKeyboardControl);
    pragma Convention(C,  XKeyboardState);
    pragma Convention(C,  XTimeCoord);
    pragma Convention(C,  XModifierKeymap);
    pragma Convention(C,  struct_anonymous24_t);
    pragma Convention(C,  struct_anonymous26_t);
    pragma Convention(C,  XKeyEvent);
    pragma Convention(C,  XButtonEvent);
    pragma Convention(C,  XMotionEvent);
    pragma Convention(C,  XCrossingEvent);
    pragma Convention(C,  XFocusChangeEvent);
    pragma Convention(C,  XKeymapEvent);
    pragma Convention(C,  XExposeEvent);
    pragma Convention(C,  XGraphicsExposeEvent);
    pragma Convention(C,  XNoExposeEvent);
    pragma Convention(C,  XVisibilityEvent);
    pragma Convention(C,  XCreateWindowEvent);
    pragma Convention(C,  XDestroyWindowEvent);
    pragma Convention(C,  XUnmapEvent);
    pragma Convention(C,  XMapEvent);
    pragma Convention(C,  XMapRequestEvent);
    pragma Convention(C,  XReparentEvent);
    pragma Convention(C,  XConfigureEvent);
    pragma Convention(C,  XGravityEvent);
    pragma Convention(C,  XResizeRequestEvent);
    pragma Convention(C,  XConfigureRequestEvent);
    pragma Convention(C,  XCirculateEvent);
    pragma Convention(C,  XCirculateRequestEvent);
    pragma Convention(C,  XPropertyEvent);
    pragma Convention(C,  XSelectionClearEvent);
    pragma Convention(C,  XSelectionRequestEvent);
    pragma Convention(C,  XSelectionEvent);
    pragma Convention(C,  XColormapEvent);
    pragma Convention(C,  XMappingEvent);
    pragma Convention(C,  XErrorEvent);
    pragma Convention(C,  XAnyEvent);
    pragma Convention(C,  XCharStruct);
    pragma Convention(C,  XFontProp);
    pragma Convention(C,  XFontStruct);
    pragma Convention(C,  XTextItem);
    pragma Convention(C,  XChar2b);
    pragma Convention(C,  XTextItem16);
    pragma Convention(C,  XFontSetExtents);
    pragma Convention(C,  XmbTextItem);
    pragma Convention(C,  XwcTextItem);
    pragma Convention(C,  XIMStyles);
    pragma Convention(C,  XIMCallback);
    pragma Convention(C,  XIMPreeditDrawCallbackStruct);
    pragma Convention(C,  XIMPreeditCaretCallbackStruct);
    pragma Convention(C,  XDisplay);
    pragma Convention(C,  XImage);
    pragma Convention(C,  XClientMessageEvent);
    pragma Convention(C,  XClientMessageEvent_Inner);
    pragma Convention(C,  XIMText);
    pragma Convention(C,  XIMText_Inner);
    pragma Convention(C,  XIMStatusDrawCallbackStruct);

    pragma Convention(C, af_138_free_private);

    pragma Convention(C, af_349_create_image);
    pragma Convention(C, af_351_destroy_image);
    pragma Convention(C, af_352_get_pixel);
    pragma Convention(C, af_353_put_pixel);
    pragma Convention(C, af_354_sub_image);
    pragma Convention(C, af_355_add_pixel);
    pragma Convention(C, af_519_old_handler);
    pragma Convention(C, af_467_resource_alloc);
    pragma Convention(C, af_486_synchandler);
    pragma Convention(C, XIMProc);
    pragma Convention(C, XErrorHandler);
    pragma Convention(C, XIOErrorHandler);
    pragma Convention(C, event_func);
    pragma Convention(C, wire_func);
    pragma Convention(C, af_1360_func);
    pragma Convention(C, af_predicate);
end X.Xlib;

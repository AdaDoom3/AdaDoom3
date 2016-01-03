-- $Source: /home/harp/1/proto/monoBANK/xbind/x-xatom.ads,v $ 
-- $Revision: 1.6 $ $Date: 95/12/05 08:53:36 $ $Author: mg $ 

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
-- see the accompanying file Xatom.h.
-- --------------------------------------------------------------------------

package X.Xatom is

    XATOM_H                       : constant := 1;          -- Xatom.h:5
    XA_PRIMARY                    : constant Atom := 1;     -- Xatom.h:12
    XA_SECONDARY                  : constant Atom := 2;     -- Xatom.h:13
    XA_ARC                        : constant Atom := 3;     -- Xatom.h:14
    XA_ATOM                       : constant Atom := 4;     -- Xatom.h:15
    XA_BITMAP                     : constant Atom := 5;     -- Xatom.h:16
    XA_CARDINAL                   : constant Atom := 6;     -- Xatom.h:17
    XA_COLORMAP                   : constant Atom := 7;     -- Xatom.h:18
    XA_CURSOR                     : constant Atom := 8;     -- Xatom.h:19
    XA_CUT_BUFFER0                : constant Atom := 9;     -- Xatom.h:20
    XA_CUT_BUFFER1                : constant Atom := 10;    -- Xatom.h:21
    XA_CUT_BUFFER2                : constant Atom := 11;    -- Xatom.h:22
    XA_CUT_BUFFER3                : constant Atom := 12;    -- Xatom.h:23
    XA_CUT_BUFFER4                : constant Atom := 13;    -- Xatom.h:24
    XA_CUT_BUFFER5                : constant Atom := 14;    -- Xatom.h:25
    XA_CUT_BUFFER6                : constant Atom := 15;    -- Xatom.h:26
    XA_CUT_BUFFER7                : constant Atom := 16;    -- Xatom.h:27
    XA_DRAWABLE                   : constant Atom := 17;    -- Xatom.h:28
    XA_FONT                       : constant Atom := 18;    -- Xatom.h:29
    XA_INTEGER                    : constant Atom := 19;    -- Xatom.h:30
    XA_PIXMAP                     : constant Atom := 20;    -- Xatom.h:31
    XA_POINT                      : constant Atom := 21;    -- Xatom.h:32
    XA_RECTANGLE                  : constant Atom := 22;    -- Xatom.h:33
    XA_RESOURCE_MANAGER           : constant Atom := 23;    -- Xatom.h:34
    XA_RGB_COLOR_MAP              : constant Atom := 24;    -- Xatom.h:35
    XA_RGB_BEST_MAP               : constant Atom := 25;    -- Xatom.h:36
    XA_RGB_BLUE_MAP               : constant Atom := 26;    -- Xatom.h:37
    XA_RGB_DEFAULT_MAP            : constant Atom := 27;    -- Xatom.h:38
    XA_RGB_GRAY_MAP               : constant Atom := 28;    -- Xatom.h:39
    XA_RGB_GREEN_MAP              : constant Atom := 29;    -- Xatom.h:40
    XA_RGB_RED_MAP                : constant Atom := 30;    -- Xatom.h:41
    XA_STRING                     : constant Atom := 31;    -- Xatom.h:42
    XA_VISUALID                   : constant Atom := 32;    -- Xatom.h:43
    XA_WINDOW                     : constant Atom := 33;    -- Xatom.h:44
    XA_WM_COMMAND                 : constant Atom := 34;    -- Xatom.h:45
    XA_WM_HINTS                   : constant Atom := 35;    -- Xatom.h:46
    XA_WM_CLIENT_MACHINE          : constant Atom := 36;    -- Xatom.h:47
    XA_WM_ICON_NAME               : constant Atom := 37;    -- Xatom.h:48
    XA_WM_ICON_SIZE               : constant Atom := 38;    -- Xatom.h:49
    XA_WM_NAME                    : constant Atom := 39;    -- Xatom.h:50
    XA_WM_NORMAL_HINTS            : constant Atom := 40;    -- Xatom.h:51
    XA_WM_SIZE_HINTS              : constant Atom := 41;    -- Xatom.h:52
    XA_WM_ZOOM_HINTS              : constant Atom := 42;    -- Xatom.h:53
    XA_MIN_SPACE                  : constant Atom := 43;    -- Xatom.h:54
    XA_NORM_SPACE                 : constant Atom := 44;    -- Xatom.h:55
    XA_MAX_SPACE                  : constant Atom := 45;    -- Xatom.h:56
    XA_END_SPACE                  : constant Atom := 46;    -- Xatom.h:57
    XA_SUPERSCRIPT_X              : constant Atom := 47;    -- Xatom.h:58
    XA_SUPERSCRIPT_Y              : constant Atom := 48;    -- Xatom.h:59
    XA_SUBSCRIPT_X                : constant Atom := 49;    -- Xatom.h:60
    XA_SUBSCRIPT_Y                : constant Atom := 50;    -- Xatom.h:61
    XA_UNDERLINE_POSITION         : constant Atom := 51;    -- Xatom.h:62
    XA_UNDERLINE_THICKNESS        : constant Atom := 52;    -- Xatom.h:63
    XA_STRIKEOUT_ASCENT           : constant Atom := 53;    -- Xatom.h:64
    XA_STRIKEOUT_DESCENT          : constant Atom := 54;    -- Xatom.h:65
    XA_ITALIC_ANGLE               : constant Atom := 55;    -- Xatom.h:66
    XA_X_HEIGHT                   : constant Atom := 56;    -- Xatom.h:67
    XA_QUAD_WIDTH                 : constant Atom := 57;    -- Xatom.h:68
    XA_WEIGHT                     : constant Atom := 58;    -- Xatom.h:69
    XA_POINT_SIZE                 : constant Atom := 59;    -- Xatom.h:70
    XA_RESOLUTION                 : constant Atom := 60;    -- Xatom.h:71
    XA_COPYRIGHT                  : constant Atom := 61;    -- Xatom.h:72
    XA_NOTICE                     : constant Atom := 62;    -- Xatom.h:73
    XA_FONT_NAME                  : constant Atom := 63;    -- Xatom.h:74
    XA_FAMILY_NAME                : constant Atom := 64;    -- Xatom.h:75
    XA_FULL_NAME                  : constant Atom := 65;    -- Xatom.h:76
    XA_CAP_HEIGHT                 : constant Atom := 66;    -- Xatom.h:77
    XA_WM_CLASS                   : constant Atom := 67;    -- Xatom.h:78
    XA_WM_TRANSIENT_FOR           : constant Atom := 68;    -- Xatom.h:79
    XA_LAST_PREDEFINED            : constant Atom := 68;    -- Xatom.h:81

end X.Xatom;

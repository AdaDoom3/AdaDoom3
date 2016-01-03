-- $Source: /home/harp/1/proto/monoBANK/xbind/x-xlib.adb,v $ 
-- $Revision: 1.14 $ $Date: 95/12/05 09:07:49 $ $Author: mg $ 

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
with Interfaces.C.Pointers;
with Stdarg;
with Stdarg.Impl;

package body X.Xlib is

    -- *************************
    -- Functions that use Stdarg
    -- *************************

    function "&" is new Stdarg.Concat(XIM);
    function "&" is new Stdarg.Concat(XIC);

    function To_charp is new Ada.Unchecked_Conversion(
        Stdarg.C_Param, X.Strings.charp);

    function To_XIC is new Ada.Unchecked_Conversion(
	Stdarg.C_Param, XIC);

    function XGetIMValues(
                im  : XIM;
                args: Stdarg.ArgList := Stdarg.Empty)
               return X.Strings.charp is                    -- Xlib.h:4203

        use Stdarg, Stdarg.Impl;

        Complete_Args: Stdarg.ArgList :=
            Stdarg.Empty & im & args;

        function C_XGetIMValues return Integer;
        pragma Import(C, C_XGetIMValues, "XGetIMValues");

    begin
        return To_charp(F_Varargs(
            C_XGetIMValues'Address,
            ArgCount(Complete_Args),
            Address_of_First_Arg(Complete_Args)));

    end XGetIMValues;

    function XCreateIC(
                im  : XIM;
                args: Stdarg.ArgList := Stdarg.Empty)
               return XIC is                                -- Xlib.h:4221

        use Stdarg, Stdarg.Impl;

        Complete_Args: Stdarg.ArgList :=
            Stdarg.Empty & im & args;

        function C_XCreateIC return Integer;
        pragma Import(C, C_XCreateIC, "XCreateIC");

    begin
        return To_XIC(F_Varargs(
            C_XCreateIC'Address,
            ArgCount(Complete_Args),
            Address_of_First_Arg(Complete_Args)));

    end XCreateIC;

    function XSetICValues(
                ic  : XIC;
                args: Stdarg.ArgList := Stdarg.Empty)
               return X.Strings.charp is                    -- Xlib.h:4257

        use Stdarg, Stdarg.Impl;

        Complete_Args: Stdarg.ArgList :=
            Stdarg.Empty & ic & args;

        function C_XSetICValues return Integer;
        pragma Import(C, C_XSetICValues, "XSetICValues");

    begin
        return To_charp(F_Varargs(
            C_XSetICValues'Address,
            ArgCount(Complete_Args),
            Address_of_First_Arg(Complete_Args)));

    end XSetICValues;

    function XGetICValues(
                ic  : XIC;
                args: Stdarg.ArgList := Stdarg.Empty)
               return X.Strings.charp is                    -- Xlib.h:4263

        use Stdarg, Stdarg.Impl;

        Complete_Args: Stdarg.ArgList :=
            Stdarg.Empty & ic & args;

        function C_XGetICValues return Integer;
        pragma Import(C, C_XGetICValues, "XGetICValues");

    begin
        return To_charp(F_Varargs(
            C_XGetICValues'Address,
            ArgCount(Complete_Args),
            Address_of_First_Arg(Complete_Args)));

    end XGetICValues;

    -- ******
    -- Macros
    -- ******

    function Elem (P: Screen_access; Index: X.Signed_Int) 
        return Screen_access is
        -- Interfaces.C.Pointers still not working as of GNAT 2.03
        type Addr is mod System.Memory_Size;
        function To_Addr is new Ada.Unchecked_Conversion (Screen_access, Addr);
        function To_Screen is new Ada.Unchecked_Conversion 
            (addr, Screen_access);
	use type Interfaces.C.Int;
        Siz: constant X.Signed_Int := 
            (Screen'Size + System.Storage_Unit - 1) / System.Storage_Unit;
        use type Interfaces.C.Int;
    begin
        return To_Screen(To_Addr(P) + Addr(Index * Siz));
    end Elem;
    pragma Inline(Elem);

    function ConnectionNumber(Dpy: access XDisplay) return X.signed_int is
    begin
        return Dpy.fd;
    end;

    function RootWindow(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.Window is
    begin
        return Elem(Dpy.Screens, Scr).root;
    end;

    function DefaultScreen(Dpy: access XDisplay) return X.signed_int is
    begin
        return Dpy.default_screen;
    end;

    function DefaultRootWindow(Dpy: access XDisplay) return X.Window is
    begin
        return Elem(Dpy.Screens, Dpy.default_screen).root;
    end;

    function DefaultVisual(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return Visual_access is
    begin
        return Elem(Dpy.Screens, Scr).root_visual;
    end;

    function DefaultGC(Dpy: access XDisplay; Scr: X.Signed_Int) return GC is
    begin
        return Elem(Dpy.Screens, Scr).default_gc;
    end;

    function BlackPixel(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.unsigned_long is
    begin
        return Elem(Dpy.Screens, Scr).black_pixel;
    end;

    function WhitePixel(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.unsigned_long is
    begin
        return Elem(Dpy.Screens, Scr).white_pixel;
    end;

    function QLength(Dpy: access XDisplay) return X.signed_int is
    begin
        return Dpy.qlen;
    end;

    function DisplayWidth(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.signed_int is
    begin
        return Elem(Dpy.Screens, Scr).width;
    end;

    function DisplayHeight(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.signed_int is
    begin
        return Elem(Dpy.Screens, Scr).height;
    end;

    function DisplayWidthMM(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.signed_int is
    begin
        return Elem(Dpy.Screens, Scr).mwidth;
    end;

    function DisplayHeightMM(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.signed_int is
    begin
        return Elem(Dpy.Screens, Scr).mheight;
    end;

    function DisplayPlanes(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.signed_int is
    begin
        return Elem(Dpy.Screens, Scr).root_depth;
    end;

    function DisplayCells(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.signed_int is
    begin
        return DefaultVisual(dpy, Scr).map_entries;
    end;

    function ScreenCount(Dpy: access XDisplay) return X.signed_int is
    begin
        return Dpy.nscreens;
    end;

    function ServerVendor(Dpy: access XDisplay) return X.Strings.charp is
    begin
        return Dpy.vendor;
    end;

    function ProtocolVersion(Dpy: access XDisplay) return X.signed_int is
    begin
        return Dpy.proto_major_version;
    end;

    function ProtocolRevision(Dpy: access XDisplay) return X.signed_int is
    begin
        return Dpy.proto_minor_version;
    end;

    function VendorRelease(Dpy: access XDisplay) return X.signed_int is
    begin
        return Dpy.release;
    end;

    function DisplayString(Dpy: access XDisplay) return X.Strings.charp is
    begin
        return Dpy.display_name;
    end;

    function DefaultDepth(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.signed_int is
    begin
        return Elem(Dpy.Screens, Scr).root_depth;
    end;

    function DefaultColormap(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return X.Colormap is
    begin
        return Elem(Dpy.Screens, Scr).cmap;
    end;

    function BitmapUnit(Dpy: access XDisplay) return X.signed_int is
    begin
        return Dpy.bitmap_unit;
    end;

    function BitmapBitOrder(Dpy: access XDisplay) return X.signed_int is
    begin
        return Dpy.bitmap_bit_order;
    end;

    function BitmapPad(Dpy: access XDisplay) return X.signed_int is
    begin
        return Dpy.bitmap_pad;
    end;

    function ImageByteOrder(Dpy: access XDisplay) return X.signed_int is
    begin
        return Dpy.byte_order;
    end;

    function NextRequest(Dpy: access XDisplay) return X.unsigned_long is
        use type Interfaces.C.unsigned_long;
    begin
        return Dpy.request + 1;
    end;

    function LastKnownRequestProcessed(Dpy: access XDisplay) 
        return X.unsigned_long is
    begin
        return Dpy.last_request_read;
    end;

    function ScreenOfDisplay(Dpy: access XDisplay; Scr: X.Signed_Int) 
        return Screen_access is
    begin
        return Elem(Dpy.Screens, Scr);
    end;

    function DefaultScreenOfDisplay(Dpy: access XDisplay) 
        return Screen_access is
    begin
        return Elem(Dpy.Screens, Dpy.default_screen);
    end;

    function DisplayOfScreen(S: access Screen) return XDisplay_access is
    begin
        return s.display;
    end;

    function RootWindowOfScreen(S: access Screen) return X.Window is
    begin
        return s.root;
    end;

    function BlackPixelOfScreen(S: access Screen) return X.unsigned_long is
    begin
        return s.black_pixel;
    end;

    function WhitePixelOfScreen(S: access Screen) return X.unsigned_long is
    begin
        return s.white_pixel;
    end;

    function DefaultColormapOfScreen(S: access Screen) return X.Colormap is
    begin
        return s.cmap;
    end;

    function DefaultDepthOfScreen(S: access Screen) return X.signed_int is
    begin
        return s.root_depth;
    end;

    function DefaultGCOfScreen(S: access Screen) return GC is
    begin
        return s.default_gc;
    end;

    function DefaultVisualOfScreen(S: access Screen) return Visual_access is
    begin
        return s.root_visual;
    end;

    function WidthOfScreen(S: access Screen) return X.signed_int is
    begin
        return s.width;
    end;

    function HeightOfScreen(S: access Screen) return X.signed_int is
    begin
        return s.height;
    end;

    function WidthMMOfScreen(S: access Screen) return X.signed_int is
    begin
        return s.mwidth;
    end;

    function HeightMMOfScreen(S: access Screen) return X.signed_int is
    begin
        return s.mheight;
    end;

    function PlanesOfScreen(S: access Screen) return X.signed_int is
    begin
        return s.root_depth;
    end;

    function CellsOfScreen(S: access Screen) return X.signed_int is
    begin
        return DefaultVisualOfScreen(S).map_entries;
    end;

    function MinCmapsOfScreen(S: access Screen) return X.signed_int is
    begin
        return s.min_maps;
    end;

    function MaxCmapsOfScreen(S: access Screen) return X.signed_int is
    begin
        return s.max_maps;
    end;

    function DoesSaveUnders(S: access Screen) return Bool is
    begin
        return s.save_unders;
    end;

    function DoesBackingStore(S: access Screen) return X.signed_int is
    begin
        return s.backing_store;
    end;

    function EventMaskOfScreen(S: access Screen) return X.long is
    begin
        return s.root_input_mask;
    end;

    function XAllocID(Dpy: access XDisplay) return XID is
    begin
        return Dpy.all.Resource_Alloc(Dpy);
    end;

    -- **************************************
    -- Extra functions with string parameters
    -- **************************************

    function XLoadQueryFont(
                display: access XDisplay;
                name   : Interfaces.C.Char_Array) return XFontStruct_access is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array := 
            name & Interfaces.C.Nul;
    begin
        return XLoadQueryFont(
                display,
                Tmp_name(Tmp_name'First)'unchecked_access);
    end XLoadQueryFont;                                     -- Xlib.h:1196

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
                result        : out XImage_access) is

    begin
        result := XCreateImage(
                display,
                visual,
                depth,
                format,
                offset,
                data(data'First)'unchecked_access,
                width,
                height,
                bitmap_pad,
                bytes_per_line);
    end XCreateImage;                                       -- Xlib.h:1257

    function XOpenDisplay(
                display_name: Interfaces.C.Char_Array) 
               return XDisplay_access is

        use type Interfaces.C.Char_Array;
        Tmp_display_name: constant Interfaces.C.Char_Array := 
            display_name & Interfaces.C.Nul;
    begin
        return XOpenDisplay(
                Tmp_display_name(Tmp_display_name'First)'unchecked_access);
    end XOpenDisplay;                                       -- Xlib.h:1302

    function XGetDefault(
                display: access XDisplay;
                program: Interfaces.C.Char_Array;
                option : Interfaces.C.Char_Array) return X.Strings.charp is

        use type Interfaces.C.Char_Array;
        Tmp_program: constant Interfaces.C.Char_Array := 
            program & Interfaces.C.Nul;
        Tmp_option: constant Interfaces.C.Char_Array := 
            option & Interfaces.C.Nul;
    begin
        return XGetDefault(
                display,
                Tmp_program(Tmp_program'First)'unchecked_access,
                Tmp_option(Tmp_option'First)'unchecked_access);
    end XGetDefault;                                        -- Xlib.h:1333

    function XDisplayName(
                string: Interfaces.C.Char_Array) return X.Strings.charp is

        use type Interfaces.C.Char_Array;
        Tmp_string: constant Interfaces.C.Char_Array := 
            string & Interfaces.C.Nul;
    begin
        return XDisplayName(
                Tmp_string(Tmp_string'First)'unchecked_access);
    end XDisplayName;                                       -- Xlib.h:1340

    function XInternAtom(
                display       : access XDisplay;
                atom_name     : Interfaces.C.Char_Array;
                only_if_exists: X.signed_int) return X.Atom is

        use type Interfaces.C.Char_Array;
        Tmp_atom_name: constant Interfaces.C.Char_Array := 
            atom_name & Interfaces.C.Nul;
    begin
        return XInternAtom(
                display,
                Tmp_atom_name(Tmp_atom_name'First)'unchecked_access,
                only_if_exists);
    end XInternAtom;                                        -- Xlib.h:1367

    function XLoadFont(
                display: access XDisplay;
                name   : Interfaces.C.Char_Array) return X.Font is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array := 
            name & Interfaces.C.Nul;
    begin
        return XLoadFont(
                display,
                Tmp_name(Tmp_name'First)'unchecked_access);
    end XLoadFont;                                          -- Xlib.h:1416

    function XCreateBitmapFromData(
                display: access XDisplay;
                d      : X.Drawable;
                data   : Interfaces.C.Char_Array;
                width  : X.unsigned_int;
                height : X.unsigned_int) return X.Pixmap is

        use type Interfaces.C.Char_Array;
        Tmp_data: constant Interfaces.C.Char_Array := 
            data & Interfaces.C.Nul;
    begin
        return XCreateBitmapFromData(
                display,
                d,
                Tmp_data(Tmp_data'First)'unchecked_access,
                width,
                height);
    end XCreateBitmapFromData;                              -- Xlib.h:1450

    procedure XCreatePixmapFromBitmapData(
                display: access XDisplay;
                d      : X.Drawable;
                data   : in out Interfaces.C.Char_Array;
                width  : X.unsigned_int;
                height : X.unsigned_int;
                fg     : X.unsigned_long;
                bg     : X.unsigned_long;
                depth  : X.unsigned_int;
                result : out X.Pixmap) is

    begin
        result := XCreatePixmapFromBitmapData(
                display,
                d,
                data(data'First)'unchecked_access,
                width,
                height,
                fg,
                bg,
                depth);
    end XCreatePixmapFromBitmapData;                        -- Xlib.h:1459

    function XListFonts(
                display            : access XDisplay;
                pattern            : Interfaces.C.Char_Array;
                maxnames           : X.signed_int;
                actual_count_return: access X.signed_int) 
            return X.Strings.charp_vector is

        use type Interfaces.C.Char_Array;
        Tmp_pattern: constant Interfaces.C.Char_Array := 
            pattern & Interfaces.C.Nul;
    begin
        return XListFonts(
                display,
                Tmp_pattern(Tmp_pattern'First)'unchecked_access,
                maxnames,
                actual_count_return);
    end XListFonts;                                         -- Xlib.h:1513

    function XListFontsWithInfo(
                display     : access XDisplay;
                pattern     : Interfaces.C.Char_Array;
                maxnames    : X.signed_int;
                count_return: access X.signed_int;
                info_return : XFontStruct_access_access) 
               return X.Strings.charp_vector is

        use type Interfaces.C.Char_Array;
        Tmp_pattern: constant Interfaces.C.Char_Array := 
            pattern & Interfaces.C.Nul;
    begin
        return XListFontsWithInfo(
                display,
                Tmp_pattern(Tmp_pattern'First)'unchecked_access,
                maxnames,
                count_return,
                info_return);
    end XListFontsWithInfo;                                 -- Xlib.h:1521

    function XStringToKeysym(
                string: Interfaces.C.Char_Array) return X.KeySym is

        use type Interfaces.C.Char_Array;
        Tmp_string: constant Interfaces.C.Char_Array := 
            string & Interfaces.C.Nul;
    begin
        return XStringToKeysym(
                Tmp_string(Tmp_string'First)'unchecked_access);
    end XStringToKeysym;                                    -- Xlib.h:1585

    function XInitExtension(
                display: access XDisplay;
                name   : Interfaces.C.Char_Array) return XExtCodes_access is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array := 
            name & Interfaces.C.Nul;
    begin
        return XInitExtension(
                display,
                Tmp_name(Tmp_name'First)'unchecked_access);
    end XInitExtension;                                     -- Xlib.h:1618

    function XAllocNamedColor(
                display          : access XDisplay;
                colormap         : X.Colormap;
                color_name       : Interfaces.C.Char_Array;
                screen_def_return: access XColor;
                exact_def_return : access XColor) return X.signed_int is

        use type Interfaces.C.Char_Array;
        Tmp_color_name: constant Interfaces.C.Char_Array := 
            color_name & Interfaces.C.Nul;
    begin
        return XAllocNamedColor(
                display,
                colormap,
                Tmp_color_name(Tmp_color_name'First)'unchecked_access,
                screen_def_return,
                exact_def_return);
    end XAllocNamedColor;                                   -- Xlib.h:1960

    procedure XDrawImageString(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                xx     : X.signed_int;
                y      : X.signed_int;
                string : Interfaces.C.Char_Array;
                length : X.signed_int) is

        use type Interfaces.C.Char_Array;
        Tmp_string: constant Interfaces.C.Char_Array := 
            string & Interfaces.C.Nul;
    begin
        XDrawImageString(
                display,
                d,
                gc,
                xx,
                y,
                Tmp_string(Tmp_string'First)'unchecked_access,
                length);
    end XDrawImageString;                                   -- Xlib.h:2402

    procedure XDrawString(
                display: access XDisplay;
                d      : X.Drawable;
                gc     : access XGC;
                xx     : X.signed_int;
                y      : X.signed_int;
                string : Interfaces.C.Char_Array;
                length : X.signed_int) is

        use type Interfaces.C.Char_Array;
        Tmp_string: constant Interfaces.C.Char_Array := 
            string & Interfaces.C.Nul;
    begin
        XDrawString(
                display,
                d,
                gc,
                xx,
                y,
                Tmp_string(Tmp_string'First)'unchecked_access,
                length);
    end XDrawString;                                        -- Xlib.h:2502

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
                height_return   : access X.signed_int) return X.signed_int is

        use type Interfaces.C.Char_Array;
        Tmp_position: constant Interfaces.C.Char_Array := 
            position & Interfaces.C.Nul;
        Tmp_default_position: constant Interfaces.C.Char_Array := 
            default_position & Interfaces.C.Nul;
    begin
        return XGeometry(
                display,
                screen,
                Tmp_position(Tmp_position'First)'unchecked_access,
                Tmp_default_position(Tmp_default_position'First)'
		    unchecked_access,
                bwidth,
                fwidth,
                fheight,
                xadder,
                yadder,
                x_return,
                y_return,
                width_return,
                height_return);
    end XGeometry;                                          -- Xlib.h:2725

    procedure XGetErrorDatabaseText(
                display       : access XDisplay;
                name          : Interfaces.C.Char_Array;
                message       : Interfaces.C.Char_Array;
                default_string: Interfaces.C.Char_Array;
                buffer_return : in out Interfaces.C.Char_Array;
                length        : X.signed_int) is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array := 
            name & Interfaces.C.Nul;
        Tmp_message: constant Interfaces.C.Char_Array := 
            message & Interfaces.C.Nul;
        Tmp_default_string: constant Interfaces.C.Char_Array := 
            default_string & Interfaces.C.Nul;
    begin
        XGetErrorDatabaseText(
                display,
                Tmp_name(Tmp_name'First)'unchecked_access,
                Tmp_message(Tmp_message'First)'unchecked_access,
                Tmp_default_string(Tmp_default_string'First)'unchecked_access,
                buffer_return(buffer_return'First)'unchecked_access,
                length);
    end XGetErrorDatabaseText;                              -- Xlib.h:2743

    procedure XGetErrorText(
                display      : access XDisplay;
                code         : X.signed_int;
                buffer_return: in out Interfaces.C.Char_Array;
                length       : X.signed_int) is

    begin
        XGetErrorText(
                display,
                code,
                buffer_return(buffer_return'First)'unchecked_access,
                length);
    end XGetErrorText;                                      -- Xlib.h:2754

    function XLookupColor(
                display          : access XDisplay;
                colormap         : X.Colormap;
                color_name       : Interfaces.C.Char_Array;
                exact_def_return : access XColor;
                screen_def_return: access XColor) return X.signed_int is

        use type Interfaces.C.Char_Array;
        Tmp_color_name: constant Interfaces.C.Char_Array := 
            color_name & Interfaces.C.Nul;
    begin
        return XLookupColor(
                display,
                colormap,
                Tmp_color_name(Tmp_color_name'First)'unchecked_access,
                exact_def_return,
                screen_def_return);
    end XLookupColor;                                       -- Xlib.h:2995

    function XParseColor(
                display         : access XDisplay;
                colormap        : X.Colormap;
                spec            : Interfaces.C.Char_Array;
                exact_def_return: access XColor) return X.signed_int is

        use type Interfaces.C.Char_Array;
        Tmp_spec: constant Interfaces.C.Char_Array := 
            spec & Interfaces.C.Nul;
    begin
        return XParseColor(
                display,
                colormap,
                Tmp_spec(Tmp_spec'First)'unchecked_access,
                exact_def_return);
    end XParseColor;                                        -- Xlib.h:3086

    function XParseGeometry(
                parsestring  : Interfaces.C.Char_Array;
                x_return     : access X.signed_int;
                y_return     : access X.signed_int;
                width_return : access X.unsigned_int;
                height_return: access X.unsigned_int) return X.signed_int is

        use type Interfaces.C.Char_Array;
        Tmp_parsestring: constant Interfaces.C.Char_Array := 
            parsestring & Interfaces.C.Nul;
    begin
        return XParseGeometry(
                Tmp_parsestring(Tmp_parsestring'First)'unchecked_access,
                x_return,
                y_return,
                width_return,
                height_return);
    end XParseGeometry;                                     -- Xlib.h:3095

    function XQueryExtension(
                display            : access XDisplay;
                name               : Interfaces.C.Char_Array;
                major_opcode_return: access X.signed_int;
                first_event_return : access X.signed_int;
                first_error_return : access X.signed_int) 
            return X.signed_int is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array := 
            name & Interfaces.C.Nul;
    begin
        return XQueryExtension(
                display,
                Tmp_name(Tmp_name'First)'unchecked_access,
                major_opcode_return,
                first_event_return,
                first_error_return);
    end XQueryExtension;                                    -- Xlib.h:3243

    procedure XQueryTextExtents(
                display            : access XDisplay;
                font_ID            : X.XID;
                string             : Interfaces.C.Char_Array;
                nchars             : X.signed_int;
                direction_return   : access X.signed_int;
                font_ascent_return : access X.signed_int;
                font_descent_return: access X.signed_int;
                overall_return     : XCharStruct_access) is

        use type Interfaces.C.Char_Array;
        Tmp_string: constant Interfaces.C.Char_Array := 
            string & Interfaces.C.Nul;
    begin
        XQueryTextExtents(
                display,
                font_ID,
                Tmp_string(Tmp_string'First)'unchecked_access,
                nchars,
                direction_return,
                font_ascent_return,
                font_descent_return,
                overall_return);
    end XQueryTextExtents;                                  -- Xlib.h:3274

    function XReadBitmapFile(
                display      : access XDisplay;
                d            : X.Drawable;
                filename     : Interfaces.C.Char_Array;
                width_return : access X.unsigned_int;
                height_return: access X.unsigned_int;
                bitmap_return: access X.Pixmap;
                x_hot_return : access X.signed_int;
                y_hot_return : access X.signed_int) return X.signed_int is

        use type Interfaces.C.Char_Array;
        Tmp_filename: constant Interfaces.C.Char_Array := 
            filename & Interfaces.C.Nul;
    begin
        return XReadBitmapFile(
                display,
                d,
                Tmp_filename(Tmp_filename'First)'unchecked_access,
                width_return,
                height_return,
                bitmap_return,
                x_hot_return,
                y_hot_return);
    end XReadBitmapFile;                                    -- Xlib.h:3318

    procedure XSetDashes(
                display    : access XDisplay;
                gc         : access XGC;
                dash_offset: X.signed_int;
                dash_list  : Interfaces.C.Char_Array;
                n          : X.signed_int) is

        use type Interfaces.C.Char_Array;
        Tmp_dash_list: constant Interfaces.C.Char_Array := 
            dash_list & Interfaces.C.Nul;
    begin
        XSetDashes(
                display,
                gc,
                dash_offset,
                Tmp_dash_list(Tmp_dash_list'First)'unchecked_access,
                n);
    end XSetDashes;                                         -- Xlib.h:3521

    procedure XSetIconName(
                display  : access XDisplay;
                w        : X.Window;
                icon_name: Interfaces.C.Char_Array) is

        use type Interfaces.C.Char_Array;
        Tmp_icon_name: constant Interfaces.C.Char_Array := 
            icon_name & Interfaces.C.Nul;
    begin
        XSetIconName(
                display,
                w,
                Tmp_icon_name(Tmp_icon_name'First)'unchecked_access);
    end XSetIconName;                                       -- Xlib.h:3587

    procedure XStoreBuffer(
                display: access XDisplay;
                bytes  : Interfaces.C.Char_Array;
                nbytes : X.signed_int;
                buffer : X.signed_int) is

        use type Interfaces.C.Char_Array;
        Tmp_bytes: constant Interfaces.C.Char_Array := 
            bytes & Interfaces.C.Nul;
    begin
        XStoreBuffer(
                display,
                Tmp_bytes(Tmp_bytes'First)'unchecked_access,
                nbytes,
                buffer);
    end XStoreBuffer;                                       -- Xlib.h:3749

    procedure XStoreBytes(
                display: access XDisplay;
                bytes  : Interfaces.C.Char_Array;
                nbytes : X.signed_int) is

        use type Interfaces.C.Char_Array;
        Tmp_bytes: constant Interfaces.C.Char_Array := 
            bytes & Interfaces.C.Nul;
    begin
        XStoreBytes(
                display,
                Tmp_bytes(Tmp_bytes'First)'unchecked_access,
                nbytes);
    end XStoreBytes;                                        -- Xlib.h:3758

    procedure XStoreName(
                display    : access XDisplay;
                w          : X.Window;
                window_name: Interfaces.C.Char_Array) is

        use type Interfaces.C.Char_Array;
        Tmp_window_name: constant Interfaces.C.Char_Array := 
            window_name & Interfaces.C.Nul;
    begin
        XStoreName(
                display,
                w,
                Tmp_window_name(Tmp_window_name'First)'unchecked_access);
    end XStoreName;                                         -- Xlib.h:3783

    procedure XStoreNamedColor(
                display : access XDisplay;
                colormap: X.Colormap;
                color   : Interfaces.C.Char_Array;
                pixel   : X.unsigned_long;
                flags   : X.signed_int) is

        use type Interfaces.C.Char_Array;
        Tmp_color: constant Interfaces.C.Char_Array := 
            color & Interfaces.C.Nul;
    begin
        XStoreNamedColor(
                display,
                colormap,
                Tmp_color(Tmp_color'First)'unchecked_access,
                pixel,
                flags);
    end XStoreNamedColor;                                   -- Xlib.h:3791

    procedure XTextExtents(
                font_struct        : XFontStruct_access;
                string             : Interfaces.C.Char_Array;
                nchars             : X.signed_int;
                direction_return   : access X.signed_int;
                font_ascent_return : access X.signed_int;
                font_descent_return: access X.signed_int;
                overall_return     : XCharStruct_access) is

        use type Interfaces.C.Char_Array;
        Tmp_string: constant Interfaces.C.Char_Array := 
            string & Interfaces.C.Nul;
    begin
        XTextExtents(
                font_struct,
                Tmp_string(Tmp_string'First)'unchecked_access,
                nchars,
                direction_return,
                font_ascent_return,
                font_descent_return,
                overall_return);
    end XTextExtents;                                       -- Xlib.h:3808

    function XTextWidth(
                font_struct: XFontStruct_access;
                string     : Interfaces.C.Char_Array;
                count      : X.signed_int) return X.signed_int is

        use type Interfaces.C.Char_Array;
        Tmp_string: constant Interfaces.C.Char_Array := 
            string & Interfaces.C.Nul;
    begin
        return XTextWidth(
                font_struct,
                Tmp_string(Tmp_string'First)'unchecked_access,
                count);
    end XTextWidth;                                         -- Xlib.h:3832

    function XWriteBitmapFile(
                display : access XDisplay;
                filename: Interfaces.C.Char_Array;
                bitmap  : X.Pixmap;
                width   : X.unsigned_int;
                height  : X.unsigned_int;
                x_hot   : X.signed_int;
                y_hot   : X.signed_int) return X.signed_int is

        use type Interfaces.C.Char_Array;
        Tmp_filename: constant Interfaces.C.Char_Array := 
            filename & Interfaces.C.Nul;
    begin
        return XWriteBitmapFile(
                display,
                Tmp_filename(Tmp_filename'First)'unchecked_access,
                bitmap,
                width,
                height,
                x_hot,
                y_hot);
    end XWriteBitmapFile;                                   -- Xlib.h:3975

    function XSetLocaleModifiers(
                modifier_list: Interfaces.C.Char_Array) 
            return X.Strings.charp is

        use type Interfaces.C.Char_Array;
        Tmp_modifier_list: constant Interfaces.C.Char_Array := 
            modifier_list & Interfaces.C.Nul;
    begin
        return XSetLocaleModifiers(
                Tmp_modifier_list(Tmp_modifier_list'First)'unchecked_access);
    end XSetLocaleModifiers;                                -- Xlib.h:3993

    function XCreateFontSet(
                display              : access XDisplay;
                base_font_name_list  : Interfaces.C.Char_Array;
                missing_charset_list : access X.Strings.charp_vector;
                missing_charset_count: access X.signed_int;
                def_string           : X.Strings.charp_vector) 
            return XFontSet is

        use type Interfaces.C.Char_Array;
        Tmp_base_font_name_list: constant Interfaces.C.Char_Array := 
            base_font_name_list & Interfaces.C.Nul;
    begin
        return XCreateFontSet(
                display,
                Tmp_base_font_name_list(Tmp_base_font_name_list'First)'unchecked_access,
                missing_charset_list,
                missing_charset_count,
                def_string);
    end XCreateFontSet;                                     -- Xlib.h:3999

    function XmbTextEscapement(
                font_set  : XFontSet;
                text      : Interfaces.C.Char_Array;
                bytes_text: X.signed_int) return X.signed_int is

        use type Interfaces.C.Char_Array;
        Tmp_text: constant Interfaces.C.Char_Array := 
            text & Interfaces.C.Nul;
    begin
        return XmbTextEscapement(
                font_set,
                Tmp_text(Tmp_text'First)'unchecked_access,
                bytes_text);
    end XmbTextEscapement;                                  -- Xlib.h:4048

    function XmbTextExtents(
                font_set              : XFontSet;
                text                  : Interfaces.C.Char_Array;
                bytes_text            : X.signed_int;
                overall_ink_return    : access XRectangle;
                overall_logical_return: access XRectangle) 
            return X.signed_int is

        use type Interfaces.C.Char_Array;
        Tmp_text: constant Interfaces.C.Char_Array := 
            text & Interfaces.C.Nul;
    begin
        return XmbTextExtents(
                font_set,
                Tmp_text(Tmp_text'First)'unchecked_access,
                bytes_text,
                overall_ink_return,
                overall_logical_return);
    end XmbTextExtents;                                     -- Xlib.h:4064

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
            return X.signed_int is

        use type Interfaces.C.Char_Array;
        Tmp_text: constant Interfaces.C.Char_Array := 
            text & Interfaces.C.Nul;
    begin
        return XmbTextPerCharExtents(
                font_set,
                Tmp_text(Tmp_text'First)'unchecked_access,
                bytes_text,
                ink_extents_buffer,
                logical_extents_buffer,
                buffer_size,
                num_chars,
                overall_ink_return,
                overall_logical_return);
    end XmbTextPerCharExtents;                              -- Xlib.h:4084

    procedure XmbDrawString(
                display   : access XDisplay;
                d         : X.Drawable;
                font_set  : XFontSet;
                gc        : access XGC;
                xx        : X.signed_int;
                y         : X.signed_int;
                text      : Interfaces.C.Char_Array;
                bytes_text: X.signed_int) is

        use type Interfaces.C.Char_Array;
        Tmp_text: constant Interfaces.C.Char_Array := 
            text & Interfaces.C.Nul;
    begin
        XmbDrawString(
                display,
                d,
                font_set,
                gc,
                xx,
                y,
                Tmp_text(Tmp_text'First)'unchecked_access,
                bytes_text);
    end XmbDrawString;                                      -- Xlib.h:4136

    procedure XmbDrawImageString(
                display   : access XDisplay;
                d         : X.Drawable;
                font_set  : XFontSet;
                gc        : access XGC;
                xx        : X.signed_int;
                y         : X.signed_int;
                text      : Interfaces.C.Char_Array;
                bytes_text: X.signed_int) is

        use type Interfaces.C.Char_Array;
        Tmp_text: constant Interfaces.C.Char_Array := 
            text & Interfaces.C.Nul;
    begin
        XmbDrawImageString(
                display,
                d,
                font_set,
                gc,
                xx,
                y,
                Tmp_text(Tmp_text'First)'unchecked_access,
                bytes_text);
    end XmbDrawImageString;                                 -- Xlib.h:4162

    function XOpenIM(
                dpy      : access XDisplay;
                rdb      : XrmHashBucketRec_access;
                res_name : Interfaces.C.Char_Array;
                res_class: Interfaces.C.Char_Array) return XIM is

        use type Interfaces.C.Char_Array;
        Tmp_res_name: constant Interfaces.C.Char_Array := 
            res_name & Interfaces.C.Nul;
        Tmp_res_class: constant Interfaces.C.Char_Array := 
            res_class & Interfaces.C.Nul;
    begin
        return XOpenIM(
                dpy,
                rdb,
                Tmp_res_name(Tmp_res_name'First)'unchecked_access,
                Tmp_res_class(Tmp_res_class'First)'unchecked_access);
    end XOpenIM;                                            -- Xlib.h:4188

    procedure XmbLookupString(
                ic           : XIC;
                event        : access XKeyEvent;
                buffer_return: in out Interfaces.C.Char_Array;
                bytes_buffer : X.signed_int;
                keysym_return: access KeySym;
                status_return: access Status;
                result       : out X.signed_int) is

    begin
        result := XmbLookupString(
                ic,
                event,
                buffer_return(buffer_return'First)'unchecked_access,
                bytes_buffer,
                keysym_return,
                status_return);
    end XmbLookupString;                                    -- Xlib.h:4282

end X.Xlib;

-- $Source: /home/harp/1/proto/monoBANK/xbind/x-xutil.adb,v $ 
-- $Revision: 1.10 $ $Date: 95/12/05 09:07:51 $ $Author: mg $ 

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

with Interfaces.C;
with X.Keysymdef;
with X.Xresource;

package body X.Xutil is

    -- ************
    -- Xutil macros
    -- ************

    function XDestroyImage (Image: access X.Xlib.Ximage) return X.signed_int is
    begin
        return Image.F.Destroy_Image(Image);
    end XDestroyImage;

    function XGetPixel (
                Image: access X.Xlib.XImage;
                XX   : signed_int;
                Y    : signed_int)
               return X.unsigned_long is
    begin
        return Image.F.get_pixel(Image, XX, Y);
    end XGetPixel;

    function XPutPixel (
                Image: access X.Xlib.XImage;
                XX   : signed_int;
                Y    : signed_int;
                Pixel: unsigned_long)
               return X.signed_int is
    begin
        return Image.F.put_pixel(Image, XX, Y, Pixel);
    end XPutPixel;

    function XSubImage (
                Image : access X.Xlib.XImage;
                XX    : signed_int;
                Y     : signed_int;
                Width : unsigned_int;
                Height: unsigned_int)
               return X.Xlib.XImage_access is
    begin
        return Image.F.sub_image(Image, XX, Y, Width, Height);
    end XSubImage;

    function XAddPixel (
                Image: access X.Xlib.XImage;
                Value: X.long)
               return X.signed_int is
    begin
        return Image.F.add_pixel(Image, Value);
    end XAddPixel;

    function IsKeypadKey (Key: X.KeySym) return Boolean is
    begin
        return (Key >= X.Keysymdef.XK_KP_Space) and then
               (Key <= X.Keysymdef.XK_KP_Equal);
    end IsKeypadKey;

    function IsCursorKey (Key: X.KeySym) return Boolean is
    begin
        return (Key >= X.Keysymdef.XK_Home) and then
               (Key <= X.Keysymdef.XK_Select);
    end IsCursorKey;

    function IsPFKey (Key: X.KeySym) return Boolean is
    begin
        return (Key >= X.Keysymdef.XK_KP_F1) and then
               (Key <= X.Keysymdef.XK_KP_F4);
    end IsPFKey;

    function IsFunctionKey (Key: X.KeySym) return Boolean is
    begin
        return (Key >= X.Keysymdef.XK_F1) and then
               (Key <= X.Keysymdef.XK_F35);
    end IsFunctionKey;

    function IsMiscFunctionKey (Key: X.KeySym) return Boolean is
    begin
        return (Key >= X.Keysymdef.XK_Select) and then
               (Key <= X.Keysymdef.XK_Break);
    end IsMiscFunctionKey;

    function IsModifierKey (Key: X.KeySym) return Boolean is
    begin
        return ((Key >= X.Keysymdef.XK_Shift_L) and then
                (Key <= X.Keysymdef.XK_Hyper_R)) or else
                (Key = X.Keysymdef.XK_Mode_switch) or else
                (Key = X.Keysymdef.XK_Num_Lock);
    end IsModifierKey;

    function XUniqueContext return XContext is
    begin
        return XContext(X.Xresource.XrmUniqueQuark);
    end XUniqueContext;

    function XStringToContext (String: X.Strings.const_charp) return XContext is
    begin
        return Xcontext(X.Xresource.XrmStringToQuark(String));
    end XStringToContext;

    -- ************************************
    -- Functions with Char_Array parameters
    -- ************************************

    procedure XLookupString(
                event_struct : access X.Xlib.XKeyEvent;
                buffer_return: in out Interfaces.C.Char_Array;
                bytes_buffer : X.signed_int;
                keysym_return: access KeySym;
                status_in_out: access XComposeStatus;
                result       : out X.signed_int) is

    begin
        result := XLookupString(
                event_struct,
                buffer_return(buffer_return'First)'unchecked_access,
                bytes_buffer,
                keysym_return,
                status_in_out);
    end XLookupString;                                      -- Xutil.h:525

    function XSaveContext(
                display: access X.Xlib.Display;
                rid    : X.XID;
                context: XContext;
                data   : Interfaces.C.Char_Array) return X.signed_int is

        use type Interfaces.C.Char_Array;
        Tmp_data: constant Interfaces.C.Char_Array :=
            data & Interfaces.C.Nul;
    begin
        return XSaveContext(
                display,
                rid,
                context,
                Tmp_data(Tmp_data'First)'unchecked_access);
    end XSaveContext;                                       -- Xutil.h:579

    procedure XSetStandardProperties(
                display    : access X.Xlib.Display;
                w          : X.Window;
                window_name: Interfaces.C.Char_Array;
                icon_name  : Interfaces.C.Char_Array;
                icon_pixmap: X.Pixmap;
                argv       : X.Strings.charp_vector;
                argc       : X.signed_int;
                hints      : XSizeHints_access) is

        use type Interfaces.C.Char_Array;
        Tmp_window_name: constant Interfaces.C.Char_Array :=
            window_name & Interfaces.C.Nul;
        Tmp_icon_name: constant Interfaces.C.Char_Array :=
            icon_name & Interfaces.C.Nul;
    begin
        XSetStandardProperties(
                display,
                w,
                Tmp_window_name(Tmp_window_name'First)'unchecked_access,
                Tmp_icon_name(Tmp_icon_name'First)'unchecked_access,
                icon_pixmap,
                argv,
                argc,
                hints);
    end XSetStandardProperties;                             -- Xutil.h:632

    procedure XmbSetWMProperties(
                display     : access X.Xlib.Display;
                w           : X.Window;
                window_name : Interfaces.C.Char_Array;
                icon_name   : Interfaces.C.Char_Array;
                argv        : X.Strings.charp_vector;
                argc        : X.signed_int;
                normal_hints: XSizeHints_access;
                wm_hints    : XWMHints_access;
                class_hints : XClassHint_access) is

        use type Interfaces.C.Char_Array;
        Tmp_window_name: constant Interfaces.C.Char_Array :=
            window_name & Interfaces.C.Nul;
        Tmp_icon_name: constant Interfaces.C.Char_Array :=
            icon_name & Interfaces.C.Nul;
    begin
        XmbSetWMProperties(
                display,
                w,
                Tmp_window_name(Tmp_window_name'First)'unchecked_access,
                Tmp_icon_name(Tmp_icon_name'First)'unchecked_access,
                argv,
                argc,
                normal_hints,
                wm_hints,
                class_hints);
    end XmbSetWMProperties;                                 -- Xutil.h:708

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
                gravity_return  : access X.signed_int) is

        use type Interfaces.C.Char_Array;
        Tmp_user_geometry: constant Interfaces.C.Char_Array :=
            user_geometry & Interfaces.C.Nul;
        Tmp_default_geometry: constant Interfaces.C.Char_Array :=
            default_geometry & Interfaces.C.Nul;
    begin
        XWMGeometry(
                display,
                screen_number,
                Tmp_user_geometry(Tmp_user_geometry'First)'unchecked_access,
                Tmp_default_geometry(Tmp_default_geometry'First)'unchecked_access,
                border_width,
                hints,
                x_return,
                y_return,
                width_return,
                height_return,
                gravity_return);
    end XWMGeometry;                                        -- Xutil.h:848

end X.Xutil;

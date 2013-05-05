-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  This packages implements a new widget, a double-buffer.
--  It should be used exactly like a drawing_area (i.e you can draw
--  anything on it), but it implictly provides a double-buffer.
--
--  Whereas with a Gtk_Drawing_Area you would get the drawable to draw on
--  with a call to Gtk.Widget.Get_Window, for a Gtk_Double_Buffer you should
--  call Double_Buffer.Get_Pixmap. Otherwise, all other operations are
--  equivalent.
--
--  Once you are done drawing anything you want on the pixmap, you can
--  force it to be updated on screen with a call to Gtk.Widget.Draw.
--
--  While doing long operations, you can freeze the content of the
--  double-buffer (which is then no longer updated on the screen), and
--  reactivate it once you're done.
--
--  This widget handles the resizing automatically. Two modes exist: either
--  the content of the window is preserved when the window is resized, or it
--  is not. If Back_Store is False, the content of the new pixmap is random.
--
--  The exact semantic is the following:
--
--  * The user draws to the Pixmap (through a call to Get_Pixmap).
--    If the widget needs to be updated on screen (ie some part of it gets
--    an expose event), the following happens:
--     - If the user is using Triple_Buffer, the triple_buffer is copied to
--       the screen, but it does not include the latest changes from the user.
--       This happens independently on the frozen state of the widget.
--     - Else, if the widget is not frozen, then the Pixmap is copied to the
--       screen, including partial drawings that the user was doing.
--       If the widget is frozen, the screen is not updated.
--
--  * The user is done with his changes, and calls Gtk.Widget.Draw to
--    commit them to the screen
--     - If the widget is not frozen, then the Pixmap is copied to the
--       screen. If trip_buffer is used, the pixmap is also copied to it.
--     - Else, the update of the screen (i.e first point above) is done
--       as soon as the widget is thawed.
--
--
--  If you want to call you own function each time the buffer is resized,
--  connect a callback to the event "configure" (see the body of this package
--  for an example).

with Gtk.Drawing_Area;
with Gdk.Pixmap;
with Gdk.Drawable;

package Double_Buffer is

   type Gtk_Double_Buffer_Record is new
     Gtk.Drawing_Area.Gtk_Drawing_Area_Record with private;
   type Gtk_Double_Buffer is access all Gtk_Double_Buffer_Record'Class;

   procedure Gtk_New (Buffer : out Gtk_Double_Buffer);
   procedure Initialize (Buffer : access Gtk_Double_Buffer_Record'Class);

   procedure Freeze (Buffer : access Gtk_Double_Buffer_Record);
   --  The double-buffer is no longer update on the screen

   procedure Thaw (Buffer : access Gtk_Double_Buffer_Record);
   --  The screen update is re-enabled.
   --  This is equivalent to doing a Gtk.Widget.Draw if you are using
   --  Triple-Buffers

   function Get_Pixmap (Buffer : access Gtk_Double_Buffer_Record)
                       return Gdk.Drawable.Gdk_Drawable;
   --  Gives access to the buffer. This is the only widget on which you
   --  should draw (any drawing done directly on Buffer will be lost).

   procedure Set_Back_Store (Buffer : access Gtk_Double_Buffer_Record;
                             Back_Store : Boolean := True);
   --  If BACK_STORE is set to true, then the content of the buffer is
   --  kept when the buffer is resized. Otherwise, it is the responsability
   --  of the user to restore it

   procedure Set_Triple_Buffer (Buffer : access Gtk_Double_Buffer_Record;
                                Use_Triple_Buffer : Boolean := True);
   --  If USE_TRIPLE_BUFFER is True, then the screen is updated from a third
   --  pixmap. Thus the current modifications from the user to the pixmap are
   --  not commited to the screen immediatly.

private

   type Gtk_Double_Buffer_Record is new
     Gtk.Drawing_Area.Gtk_Drawing_Area_Record
   with record
      Pixmap                  : Gdk.Pixmap.Gdk_Pixmap :=
        Gdk.Pixmap.Null_Pixmap;
      Triple_Buffer           : Gdk.Pixmap.Gdk_Pixmap :=
        Gdk.Pixmap.Null_Pixmap;
      Is_Frozen               : Boolean := False;
      Back_Store              : Boolean := False;
      Should_Update_On_Screen : Boolean := False;
      Use_Triple_Buffer       : Boolean := False;
   end record;

   --  PIXMAP is the actual double-buffer pixmap on which the user is
   --  drawing.
   --
   --  TRIPLE_BUFFER is not initialized by default, only when the user
   --  calls Set_Triple_Buffer with 'True'.

end Double_Buffer;

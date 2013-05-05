-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2001-2013, AdaCore                  --
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

--  <description>
--  This widget organizes its children into resizable panes. Within each
--  pane, multiple children can be put, and they will be accessible through
--  a notebook.
--  </description>
--  <group>Layout containers</group>

with Ada.Tags;
with GNAT.Strings;
with Glib;        use Glib;
with Glib.Main;
with Glib.Xml_Int;
with Gdk.Color;
with Gdk.Cursor;
with Gdk.Event;
with Gdk.Pixbuf;
with Gdk.Rectangle;
with Gtk.Accel_Group;
with Gtk.Box;
with Gtk.Enums;
with Gtk.Event_Box;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Menu;
with Gtk.Menu_Item;
with Gtk.Notebook;
with Gtk.Style;
with Gtk.Check_Menu_Item;
with Gtk.Radio_Menu_Item;
with Gtk.Widget;
with Gtk.Window;
with Gtkada.Handlers;
with Gtkada.Multi_Paned;
with Pango.Font;
with Pango.Layout;

package Gtkada.MDI is

   type MDI_Window_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type MDI_Window is access all MDI_Window_Record'Class;
   --  Although this widget is implemented as a gtk_layout, you shouldn't
   --  use the standard Gtk_Layout functions like Put and Move yourself.

   type MDI_Child_Record is new Gtk.Event_Box.Gtk_Event_Box_Record
     with private;
   type MDI_Child is access all MDI_Child_Record'Class;
   pragma No_Strict_Aliasing (MDI_Child);
   --  A child of the MDI, that encapsulates the widgets you have put in the
   --  MDI window.
   --  You can easily convert from this to the initial widget using the
   --  functions Find_MDI_Child and Get_Widget.

   type MDI_Child_Array is array (Natural range <>) of MDI_Child;
   No_Children : constant MDI_Child_Array := (1 .. 0 => null);

   type State_Type is (Normal, Floating, Invisible);
   --  This type indicates the state of an item in the MDI:
   --  - Normal: the item can be manipulated (moved and resized) by the user.
   --      It is found either in the middle notebook (maximized items), or
   --      in the layout.
   --  - Floating: the item has its own toplevel window, and is thus managed
   --      by the window manager.
   --  - Invisible: the child was part of a previously displayed perspective,
   --      but is no longer in the current perspective. We still keep it to
   --      reuse it when switching back to the previous perspective.

   procedure Gtk_New
     (MDI   : out MDI_Window;
      Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class;
      Independent_Perspectives : Boolean := False);
   --  Create a new MDI window.
   --  Note that it is recommended that you modify the style (Set_Background
   --  in State_Normal) to have a different color.
   --  You should call Setup_Toplevel_Window once you have added the MDI to a
   --  toplevel widget, so that focus is correctly handled when the toplevel
   --  window gains the focus
   --  When Independent_Perspectives is True, switching perspectives will not
   --  preserve any window. Otherwise, the windows that are in the central
   --  area will be preserved in the new perspective.

   procedure Initialize
     (MDI   : access MDI_Window_Record'Class;
      Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class;
      Independent_Perspectives : Boolean := False);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Setup_Toplevel_Window
     (MDI    : access MDI_Window_Record;
      Parent : access Gtk.Window.Gtk_Window_Record'Class);
   --  Setup Parent to properly handle focus when the window manager changes
   --  the window that currently has the focus.
   --  Parent must be the toplevel window that contains the MDI.

   type Show_Tabs_Policy_Enum is (Always, Never, Automatic);
   type Title_Bars_Policy     is (Always, Never, Central_Only);

   procedure Configure
     (MDI                       : access MDI_Window_Record;
      Opaque_Resize             : Boolean := False;
      Close_Floating_Is_Unfloat : Boolean := True;
      Title_Font         : Pango.Font.Pango_Font_Description := null;
      Background_Color   : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Title_Bar_Color    : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Focus_Title_Color  : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Draw_Title_Bars    : Title_Bars_Policy   := Always;
      Tabs_Position      : Gtk.Enums.Gtk_Position_Type := Gtk.Enums.Pos_Bottom;
      Show_Tabs_Policy   : Show_Tabs_Policy_Enum := Automatic);
   --  Change the setup of the MDI.
   --  Close_Floating_Is_Unfloat, if True, means that closing a floating child
   --  will put it back in the MDI instead of destroying it (unless its flag
   --  Always_Destroy_Float is set).
   --  Title_Font is the font used in the title bars (if null, "sans 8"
   --  is used).
   --  The colors, when Null_Color, will not change the current setup.
   --  If Draw_Title_Bars is False, then no extra title bar will be displayed
   --  for the MDI children when they are maximized. This saves space on the
   --  screen. However, the notebook tabs will be highlighted with
   --  Title_Bar_Color in exchange.
   --  Tabs_Position indicates where the notebook tabs should be put.
   --  Show_Tabs_Policy indicates when the notebook tabs should be displayed.

   function Independent_Perspectives
     (MDI : access MDI_Window_Record) return Boolean;
   --  Whether the MDI is using independent perspectives

   -------------
   -- Windows --
   -------------

   type Child_Flags is mod 2 ** 5;
   Destroy_Button       : constant Child_Flags := 2 ** 2;
   Float_As_Transient   : constant Child_Flags := 2 ** 3;
   Always_Destroy_Float : constant Child_Flags := 2 ** 4;
   All_Buttons          : constant Child_Flags := Destroy_Button;
   --  Special flags to set up the widgets:
   --  The first is the buttons that should be displayed in the title
   --  bar of the MDI children.
   --  If Float_As_Transient is set, then the child will be set up as a
   --  transient window when floating: on most window managers, it will stay on
   --  top of the MDI, but the window will have less decorations in its title
   --  bar, in particular no destroy button. In such a case, <Esc> will close
   --  the window, or unfloat it depending on the MDI's setup, as is the case
   --  for all dialogs in GtkAda. The MDI's setup will be ignored (and the
   --  child always destroyed when Esc is pressed) if Always_Destroy_Float is
   --  true.

   type Child_Group is new Positive;
   Group_Default : constant Child_Group := 1;
   Group_Any     : constant Child_Group := Child_Group'Last;
   --  This type can be used to help group windows by type within the MDI.
   --  Group_Default as a special status when computing the initial position
   --  for a window. But you can create your own groups as needed, so that for
   --  instance editors tend to be grouped with other editors, graphs with
   --  other graphs,... depending on your application.
   --  The group has an impact when the last window from a notebook is closed:
   --  If the window belongs to Group_Default, and it is the last of its group,
   --  then the space currently occupied by that window is not reclaimed, and
   --  therefore an empty area will exist in the MDI. The idea is that for
   --  instance editors typically play a special role in an integrated
   --  development environment, and the users like to have them in the center
   --  of the window. When closing the last editor, they do not want the side
   --  windows (browsers, consoles,...) to take up that space that should
   --  really only be used for editors.
   --  To get such a behavior, editors should belong to Group_Default, and all
   --  other windows to custom groups.
   --
   --  Do not use Group_Any, it is used internally with special meanings.

   procedure Gtk_New
     (Child        : out MDI_Child;
      Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags        : Child_Flags := All_Buttons;
      Group        : Child_Group := Group_Default;
      Focus_Widget : Gtk.Widget.Gtk_Widget := null);
   --  Create a new MDI child that contains widget.
   --  Widget mustn't be of type Gtk_Window.
   --
   --  You shouldn't access Widget directly afterwards, but should manipulate
   --  Child only. However, as a special exception, you can still pass Widget
   --  as a parameter to the subprograms in this package to manipulate it (e.g.
   --  in Raise_Child, ...)
   --
   --  Note: You might have to call Set_Size_Request on Widget to set its
   --  initial size. This won't prevent it from being resized by the user.
   --
   --  If Focus_Widget is not null, this is the widget that gets the keyboard
   --  focus when the child is selected.

   procedure Initialize
     (Child        : access MDI_Child_Record'Class;
      Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags        : Child_Flags := All_Buttons;
      Group        : Child_Group := Group_Default;
      Focus_Widget : Gtk.Widget.Gtk_Widget := null);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   type Child_Position is
     (Position_Automatic,
      Position_Bottom,
      Position_Top,
      Position_Left,
      Position_Right);
   subtype Side_Position is Child_Position
      range Position_Bottom .. Position_Right;
   --  The initial position of windows within the MDI.
   --  In all cases, the initial location for a window is computed with the
   --  following algorithm. This algorithm is designed with the notion of
   --  groups of windows in mind, so that some windows (typically editors) have
   --  a special status.
   --     - If another window with the same Group is already in the MDI, the
   --       new window is put on top of it.
   --     - Otherwise, if Position_Automatic, if an empty area exists within
   --       the MDI, the new window is put in that area.
   --     - Else if the Position is Bottom .. Right, the new window is put
   --       below all others (resp. to the top, left or right)
   --     - Else the window is put on top of the currently selected window

   procedure Put
     (MDI              : access MDI_Window_Record;
      Child            : access MDI_Child_Record'Class;
      Initial_Position : Child_Position := Position_Automatic);
   --  Add a new child to the MDI window, and return its embedding widget.
   --  Calling Put does not give the focus to the newly inserted widget.
   --  To do that, you should call Set_Focus_Child.

   procedure Set_Size
     (MDI        : access MDI_Window_Record;
      Child      : access MDI_Child_Record'Class;
      Width      : Glib.Gint;
      Height     : Glib.Gint;
      Fixed_Size : Boolean := False);
   --  Forces a new size for a child. If Width or Height is left to -1, the
   --  matching size will be computed from the child's requisition. If they are
   --  left to 0, the corresponding length is left to its current value.
   --  If Fixed_Size is True, then the widget will not be resized when the MDI
   --  itself is resized (unless the user has first moved one of the handles to
   --  manually resize it). Otherwise, it will grow proportionally with the
   --  rest of the MDI.

   procedure Close
     (MDI   : access MDI_Window_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class;
      Force : Boolean := False);
   --  Close the child that contains Child, and remove its window from the
   --  MDI. See also Close_Child if you need to close a MDI_Child itself.
   --  This first checks through a delete_event callback whether the child
   --  accepts to be closed.
   --  "delete_event" is not sent, and the child is automatically closed, if
   --  Force is set to True.

   procedure Set_Title
     (Child       : access MDI_Child_Record;
      Title       : UTF8_String;
      Short_Title : UTF8_String := "");
   --  Set the title for a child. Title is the title put in titlebar of
   --  the children, whereas Short_Title is the name of the notebook tab when
   --  children are maximized. By default, it is the same as Title.
   --
   --  The default title is the empty string.
   --  This title will be the one used for the window when the child is set to
   --  floating state.

   function Get_MDI (Child : access MDI_Child_Record) return MDI_Window;
   --  Return the MDI to which Child is associated. In Child is a floating
   --  child, it might not be in the MDI window itself.

   function Get_Title (Child : access MDI_Child_Record) return UTF8_String;
   --  Return the title for a specific child

   function Get_Short_Title
     (Child : access MDI_Child_Record) return UTF8_String;
   --  Return the name of the notebook tab used when children are maximized.

   function Has_Title_Bar (Child : access MDI_Child_Record) return Boolean;
   --  Whether a title bar is currently displayed for Child

   function Get_State (Child : access MDI_Child_Record) return State_Type;
   --  Return the current state of the child

   procedure Set_Icon
     (Child : access MDI_Child_Record;
      Icon  : Gdk.Pixbuf.Gdk_Pixbuf);
   --  Associate an icon with Child. This icon is visible in the title bar, the
   --  notebook tabs, the Window menu and the interactive selection dialog.
   --  The icon is updated dynamically on the screen.

   function Get_Icon
     (Child : access MDI_Child_Record) return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Returns the icon associated with Child

   ---------------------------
   -- Drag and Drop support --
   ---------------------------

   function Dnd_Data
     (Child : access MDI_Child_Record; Copy : Boolean) return MDI_Child;
   --  When a drag-and-drop operation took place to move a child from one
   --  position to the next, this function is called to know what child should
   --  be moved.
   --  As a result, the implementor can choose whether a copy of the child
   --  should be returned (creating a new view for an editor for instance), or
   --  if the child itself should be moved (the default).
   --  The returned MDI_Child must have been added to the MDI before it is
   --  returned.
   --  Copy is set to true if a copy operation was requested, to False if a
   --  simple move operation was requested. It can be ignored if Child doesn't
   --  know how to create a copy of itself for instance.

   procedure Set_Dnd_Message
     (MDI     : access MDI_Window_Record;
      Message : String);
   --  Override the message that is displayed in the popup window while
   --  performing a drag. By default, this message mentions:
   --     "... will be (preserved|hidden) when changing perspective"
   --     "Use shift to create a new view for editors"
   --  so might not be suitable for all applications.
   --  Through this function you can override the message. If you insert the
   --  special sequence "(#)" in the message, it will be replaced either with
   --  "preserved" or "hidden" depending on where the drop occurs (the central
   --  area or the perspectives). It could also be replaced by an empty string
   --  if the MDI was configured with independent perspectives.
   --  You can use markup like "<b>...</b>" to put keywords in bold.
   --
   --  Passing an empty string for Message will restore the default message.

   procedure Child_Drag_Begin
     (Child  : access MDI_Child_Record'Class;
      Event  : Gdk.Event.Gdk_Event);
   --  Starts a drag-and-drop operation for the child, so that it can be put in
   --  some other place on the desktop. This should only be called when a
   --  handler for the "button_press_event" signal, passing the event itself in
   --  parameter.
   --  The Child is immediately raised and gains the focus.

   procedure Cancel_Child_Drag (Child : access MDI_Child_Record'Class);
   --  Cancel a drag operation started by Child_Drag_Begin.
   --  It doesn't call Child_Drag_Finished.

   procedure Child_Drag_Finished (Child  : access MDI_Child_Record);
   --  Called when a drag operation is either aborted or completed. It should
   --  be overriden if special cleanup should be done.

   -----------
   -- Menus --
   -----------

   type Tab_Contextual_Menu_Factory is access procedure
     (Child : access MDI_Child_Record'Class;
      Menu  : access Gtk.Menu.Gtk_Menu_Record'Class);

   procedure Set_Tab_Contextual_Menu_Factory
     (MDI     : access MDI_Window_Record;
      Factory : Tab_Contextual_Menu_Factory);
   --  Set (or unset if Factory is null) the callback to create the contextual
   --  menu entries when the user clicks on a notebook tab.
   --  Factory should add entries to Menu (which already contains the default
   --  entries, but you can remove them if needed).

   ------------------------
   -- Selecting children --
   ------------------------

   procedure Highlight_Child
     (Child : access MDI_Child_Record; Highlight : Boolean := True);
   --  Highlight the child until it is selected by the user.
   --  The color of its menu label and of the text in the notebook tabs is
   --  changed.
   --  Nothing is done if the child is already fully visible (either in the
   --  active page in one of the notebooks, or the child that has the selection
   --  in the layout).
   --  This is meant to be used as a graphical note to the user that the child
   --  has been updated and the user should look at it.

   function Get_Focus_Child
     (MDI : access MDI_Window_Record) return MDI_Child;
   --  Return the child that currently has the MDI focus.
   --  null is returned if no child has the focus.

   procedure Set_Focus_Child
     (MDI        : access MDI_Window_Record;
      Containing : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Give the focus to the child containing Containing. This will not
   --  Grab_Focus for the child in all cases, since you might want to give the
   --  focus to some specific part of your widget (an entry field,...) in some
   --  cases.

   procedure Set_Focus_Child (Child : access MDI_Child_Record);
   --  Make Child the active widget, and raise it at the top.

   procedure Check_Interactive_Selection_Dialog
     (MDI          : access MDI_Window_Record;
      Event        : Gdk.Event.Gdk_Event;
      Move_To_Next : Boolean;
      Only_Group   : Child_Group := Group_Any);
   --  Open the interactive dialog for selecting windows.
   --  This dialog should be open as a result of a key press event.
   --  Move_To_Next indicates whether we want to select the next child (True)
   --  or the previous child (False).
   --  This dialog will be closed only when the key that opened it is fully
   --  released. For instance, if the dialog was opened as a result of
   --  pressing Ctrl-Tab, the dialog will only be closed when Ctrl itself is
   --  released.
   --  You can call this procedure even if a dialog is currently open. This
   --  simply forces a move to the next or previous child. In fact, it is
   --  your responsability to call this procedure when the user presses
   --  the keys to move between children.
   --
   --  If Event is null, then no dialog is displayed. Instead, the next or
   --  previous visible child is immediately selected. In such a mode, windows
   --  that are not on top of their respective notebook are ignored. This can
   --  be used to emulate Emacs's behavior for goto-other-window.
   --
   --  If Only_Group is specified, then only the windows from that group will
   --  be shown in the dialog.

   --  This function is not internal to the MDI since connecting to the
   --  key_press_event and key_release_event should be done in the gtk_window
   --  that contains the MDI. Otherwise, some events are intercepted by gtk+,
   --  for instance the key_release_events, and the key_press_events for some
   --  specified keys.
   --  It also gives the choice to the application of whether this feature is
   --  wanted or not.

   -----------------------------------------
   -- MDI_Child and encapsulated children --
   -----------------------------------------

   function Get_Widget
     (Child : access MDI_Child_Record) return Gtk.Widget.Gtk_Widget;
   --  Return the widget that Child encapsulates. This is the widget you
   --  initially Put() in MDI.

   function Find_MDI_Child
     (MDI    : access MDI_Window_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return MDI_Child;
   --  Return the MDI_Child that encapsulates Widget.
   --  Widget must be the exact same one you gave in argument to Put.
   --  If the child is currently not visible in the perspective (for instance
   --  it was created for another perspective, but is not present in the
   --  current one), it is inserted automatically back in the MDI.

   function Find_MDI_Child_From_Widget
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return MDI_Child;
   --  Return the MDI child that encapsulate the parent of Widget.
   --  As opposed to Find_MDI_Child, Widget can be anywhere within the
   --  widget tree. This function properly handles floating children
   --  If the child is currently not visible in the perspective (for instance
   --  it was created for another perspective, but is not present in the
   --  current one), it is inserted automatically back in the MDI.

   function Find_MDI_Child_By_Tag
     (MDI : access MDI_Window_Record;
      Tag : Ada.Tags.Tag;
      Visible_Only : Boolean := False) return MDI_Child;
   --  Return the first child matching Tag
   --  If the child is currently not visible in the perspective (for instance
   --  it was created for another perspective, but is not present in the
   --  current one), it is inserted automatically back in the MDI.
   --  If Visible_Only is True, an invisible child is not returned. This is
   --  useful to check whether a child is currently visible.

   function Find_MDI_Child_By_Name
     (MDI  : access MDI_Window_Record;
      Name : String) return MDI_Child;
   --  Return the first child matching Name.
   --  If the child is currently not visible in the perspective (for instance
   --  it was created for another perspective, but is not present in the
   --  current one), it is inserted automatically back in the MDI.

   type Child_Iterator is private;

   function First_Child
     (MDI               : access MDI_Window_Record;
      Group_By_Notebook : Boolean := False;
      Visible_Only      : Boolean := True) return Child_Iterator;
   --  Return an access to the first child of the MDI.
   --
   --  If Group_By_Notebook is True, then the children are reported one after
   --  the other, but all the widget from the same notebook are reported in the
   --  same order as the notebook pages. Floating children do not belong to a
   --  notebook, and are also reported together. To find out to which notebook
   --  a child belongs, use Get_Notebook below.
   --
   --  If Group_By_Notebook is False, it is garanteed that the first child is
   --  the one that currently has the focus in the MDI. The children are
   --  returned in the order in which they last had the focus.
   --
   --  If Visible_Only is true, then only those children currently visible in
   --  the perspective are returned. The children that were part of a former
   --  perspective are not returned.

   procedure Next (Iterator : in out Child_Iterator);
   --  Move to the next child in the MDI

   function Get_Notebook
     (Iterator : Child_Iterator) return Gtk.Notebook.Gtk_Notebook;
   --  Return the notebook to which the current child belongs. null is returned
   --  for floating children

   function Get (Iterator : Child_Iterator) return MDI_Child;
   --  Return the child pointed to by Iterator.
   --  If Iterator is no longer valid, null is returned.

   -----------------------------------
   -- Floating and closing children --
   -----------------------------------

   procedure Float_Child
     (Child : access MDI_Child_Record'Class; Float : Boolean);
   --  Change the floating state of a child

   function Is_Floating
     (Child : access MDI_Child_Record'Class) return Boolean;
   --  Return True if Child is currently in a separate window

   procedure Close_Child
     (Child : access MDI_Child_Record'Class;
      Force : Boolean := False);
   --  Same as Close, but applies directly to a MDI_Child.

   procedure Set_All_Floating_Mode
     (MDI : access MDI_Window_Record; All_Floating : Boolean);
   --  If All_Floating is set to true, the MDI will have a size of 0x0, and all
   --  children are set to floating. This can be used if you wish to let the
   --  window manager handle the windows. If All_Floating is True, children
   --  can no longer be maximized.

   procedure Use_Short_Titles_For_Floats
     (MDI : access MDI_Window_Record; Short_Titles : Boolean);
   --  If Short_Titles is set to true, only short titles will ever be used
   --  either in the title bars (in notebooks) or as the title for floating
   --  windows.

   ---------------------------
   -- Reorganizing children --
   ---------------------------

   procedure Raise_Child
     (Child : access MDI_Child_Record'Class; Give_Focus : Boolean := True);
   --  Put Child in the foreground.
   --  Note that this does not give the focus to this child, unless
   --  Give_Focus is set to True. If Child and the current focus child are in
   --  the same notebook, Child will always gain the focus, so that the focus
   --  is not left on an invisible window.

   function Is_Raised (Child : access MDI_Child_Record'Class) return Boolean;
   --  Whether the child is currently raised, ie fully visible to the user

   procedure Lower_Child (Child : access MDI_Child_Record'Class);
   --  Put Child in the background.
   --  If the children are maximized, this selected the next page from the
   --  notebook.

   type Split_Mode is
     (Before, Before_Reuse,
      After,  After_Reuse,
      Any_Side_Reuse);
   --  How a child should be split:
   --  If "Before", the child is put above or to the left of its current
   --  position. A new window is created to containing it. If the "_Reuse"
   --  version is used, and a window already exist at that position, the child
   --  will be put in it instead of creating a new one.
   --  Any_Side_Reuse indicates that the child will be put on either side,
   --  depending on where a window already exists. If there is no window on the
   --  side, a new one is created.

   procedure Split
     (MDI           : access MDI_Window_Record;
      Orientation   : Gtk.Enums.Gtk_Orientation;
      Child         : MDI_Child := null;
      Mode          : Split_Mode := Before;
      Width, Height : Glib.Gint := 0);
   --  Split the notebook containing Child (by default, the current focus
   --  child).
   --  Mode indicates in which direction the splitting should occur. If you
   --  are splitting a child in the central area, splitting will never reuse
   --  a window outside of the central area.
   --  Width and Height indicate the desired geometry for the splitted area,
   --  0 indicate a 50/50 split.

   ----------------------
   -- Desktop Handling --
   ----------------------
   --  The MDI provides a way to save desktops, i.e the list of children
   --  currently open in the MDI and their location. It can then restore the
   --  desktop at some later point.
   --
   --  Desktops require support from the widgets that are put in the MDI. They
   --  need to register a function to save them and a function to recreate
   --  them. Using Ada streams for this didn't prove workable since some
   --  children might need extra parameters not available to them through
   --  streams. This is why the following subprograms are in a generic package,
   --  so that you can pass whatever parameter(s) is needed in your
   --  application.
   --
   --  Desktops are saved and restored in XML trees.
   --
   --  If you need your application to load a "default desktop" when the user
   --  hasn't defined one, it is recommended that you distribute an actual
   --  file containing this desktop. You could also create the XML tree in
   --  memory yourself, and thus hard-code the default desktop if need be.

   generic
      type User_Data is private;
      --  Generic type of parameter that is passed to all the children's save
      --  and restore functions.

      --  This package needs to be instantiated at library level

   package Desktop is

      type Menu_Registration_Procedure is access procedure
        (User       : User_Data;
         Item_Name  : String;
         Accel_Path : String);
      --  Function used to register in the application a static menu
      --  created by the MDI.

      function Create_Menu
        (MDI               : access MDI_Window_Record'Class;
         Accel_Path_Prefix : String := "<gtkada>";
         User              : User_Data;
         Registration      : Menu_Registration_Procedure := null)
         return Gtk.Menu.Gtk_Menu;
      --  Create a dynamic menu that can then be inserted into a menu bar. This
      --  menu is dynamic, ie its content will changed based on the focus
      --  child.
      --  If this function is called several times, the same menu is returned
      --  every time. Accel_Path_Prefix must be the same for every call.
      --  Accel_Path_Prefix is used so that the key shortcuts associated with
      --  these menu items can be changed dynamically by the user (see
      --  gtk-accel_map.ads). The prefix must start with "<" and end with ">".
      --  User is used for the callbacks on perspective changes, and passed to
      --  Load_Perspective
      --  If Registration is specified, call it for

      type Save_Desktop_Function is access function
        (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
         User   : User_Data) return Glib.Xml_Int.Node_Ptr;
      --  A general function that dumps the parameters of a widget into an XML
      --  tree.
      --
      --  Note: you should register one such function for all the widget types
      --  you will put in the MDI and that need to be saved when a desktop is
      --  saved. The MDI will call all the registered functions one after the
      --  other. Therefore, your function should return null if Widget is not
      --  of a type that is it can handle.

      type Load_Desktop_Function is access function
        (MDI : MDI_Window; Node : Glib.Xml_Int.Node_Ptr; User : User_Data)
         return MDI_Child;
      --  A general function that loads a widget from an XML tree.
      --
      --  As for Save_Desktop_Function, this function should return null if it
      --  doesn't know how to handle Node or if Node doesn't describe a widget
      --  type that it can handle.
      --
      --  This function returns an MDI_Widget that has been put in the MDI.

      procedure Register_Desktop_Functions
        (Save : Save_Desktop_Function;
         Load : Load_Desktop_Function);
      --  Register a set of functions to save and load desktops for some
      --  specific widget types. This can be called multiple times.
      --  Neither Save nor Load can be null.

      function Restore_Desktop
        (MDI          : access MDI_Window_Record'Class;
         Perspectives : Glib.Xml_Int.Node_Ptr;
         From_Tree    : Glib.Xml_Int.Node_Ptr;
         User         : User_Data) return Boolean;
      --  Restore the contents of the MDI from its saved XML tree.
      --  Perspectives is the list of perspectives. It is cloned as needed, so
      --  the caller is still responsible for freeing it. The first perspective
      --  is loaded.
      --  From_Tree is the part of the desktop that describes the editor area.
      --  User is passed as a parameter to all of the Load_Desktop_Function
      --  registered by the widgets.
      --  Return False if the desktop couldn't be loaded
      --  It also restores the size and position of the toplevel window that
      --  contains the MDI

      procedure Load_Perspective
        (MDI          : access MDI_Window_Record'Class;
         Name         : String;
         User         : User_Data);
      --  Replace the current perspective by another one. This preserves the
      --  editor area.
      --  If the perspective does not exist, nothing is done, unless no
      --  perspective is currently loaded (in which case we load the first
      --  on in the list).

      procedure Create_Perspective
        (MDI          : access MDI_Window_Record'Class;
         Name         : String;
         User         : User_Data);
      --  Create a new perspective with the current desktop layout. If another
      --  perspective with the same name exists, it is replaced.

      procedure Define_Perspective
        (MDI          : access MDI_Window_Record'Class;
         XML          : Glib.Xml_Int.Node_Ptr;
         User         : User_Data);
      --  Define a new perspective (in the same format as returned by
      --  Save_Desktop, the central area is under control of the user so you
      --  cannot change it).
      --  If such a perspective already exists, nothing is done (since the user
      --  might have modified it already).
      --  XML's root node is the <perspective> node, including its "name"
      --  attribute.
      --  XML must be freed by the caller.

      procedure Save_Desktop
        (MDI          : access MDI_Window_Record'Class;
         User         : User_Data;
         Perspectives : out Glib.Xml_Int.Node_Ptr;
         Central      : out Glib.Xml_Int.Node_Ptr);
      --  Return XML representations of the perspectives and central area. Both
      --  nodes need to be freed by the caller, and can be saved in a file (to
      --  be passed to Restore_Desktop later on).
      --  This function calls each of the registered function for the children
      --  of the MDI.
      --  It also saves the size and position of the toplevel window that
      --  contains the MDI

      function Get_XML_Content
        (MDI : access MDI_Window_Record'Class;
         Tag : String) return Glib.Xml_Int.Node_Ptr;
      --  Return the first XML subtree starting with 'Tag'. This allows a
      --  module to retrieve its content after the 'Load_Desktop' call.

      procedure Free_Registered_Desktop_Functions;
      --  Free the memory allocated for the registered functions.

   private
      type Register_Node_Record;
      type Register_Node is access Register_Node_Record;
      type Register_Node_Record is record
         Save : Save_Desktop_Function;
         Load : Load_Desktop_Function;
         Next : Register_Node;
      end record;

      type Perspective_Menu_Item_Record
        is new Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item_Record
      with record
         MDI  : MDI_Window;
         Name : Natural;
         User : User_Data;
      end record;
      type Perspective_Menu_Item
        is access all Perspective_Menu_Item_Record'Class;

      procedure Change_Perspective
        (Item : access Gtk.Widget.Gtk_Widget_Record'Class);
      CP_Access : constant
        Gtkada.Handlers.Widget_Callback.Marshallers.Marshaller :=
        Gtkada.Handlers.Widget_Callback.To_Marshaller
          (Change_Perspective'Access);
      --  Internal, but needed so that we can have a 'Access on a callback

      procedure Create_Perspective_CB
        (Item : access Gtk.Widget.Gtk_Widget_Record'Class);
      CreateP_Access : constant
        Gtkada.Handlers.Widget_Callback.Marshallers.Marshaller :=
        Gtkada.Handlers.Widget_Callback.To_Marshaller
          (Create_Perspective_CB'Access);

      Registers : Register_Node;
      --  Global variable that contains the list of functions that have been
      --  registered.
   end Desktop;

   function Desktop_Was_Loaded (MDI : access MDI_Window_Record) return Boolean;
   --  Return True if a desktop was loaded, False if the MDI is only the result
   --  of calls to Gtk_New and Put.

   function List_Of_Perspectives
     (MDI : access MDI_Window_Record)
      return GNAT.Strings.String_List_Access;
   --  Return the list of perspectives known to the MDI. The caller must not
   --  free the list

   function Current_Perspective
     (MDI : access MDI_Window_Record'Class) return String;
   --  Return the name of the currently displayed perspective

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "child_selected"
   --    procedure Handler
   --       (MDI : access MDI_Window_Record'Class; Child : System.Address);
   --
   --    This signal is emitted when a new child has gained the focus. Convert
   --    Child to a MDI_Child by calling Gtk.Arguments.To_Object. This can be
   --    used to change some global information at the MDI level. You should
   --    connect to "selected" (see below) instead if you want to change some
   --    information at the child level.
   --    Child might be null if no child has the focus anymore
   --
   --  - "float_child"
   --    procedure Handler
   --       (MDI : access MDI_Window_Record'Class; Child : System.Address);
   --
   --    A child was set as floating in the MDI. A similar signal is emitted on
   --    the child itself.
   --
   --  - "child_title_changed"
   --    procedure Handler
   --      (MDI : access MDI_Window_Record'Class; Child : System.Address);
   --
   --    Emitted when the title of a child is changed. This signal is not
   --    emitted if Set_Title is called for a child that hasn't been put in the
   --    MDI yet.
   --
   --  - "child_added"
   --     procedure Handler
   --       (MDI : access MDI_Window_Record'Class; Child : System.Address);
   --     Emitted when a new child is added. You cannot use the "add" signal
   --     since in fact the children are added to notebooks that are part of
   --     the MDI, and thus "add" is only emitted when a new notebook is
   --     created.
   --
   --  - "child_removed"
   --     procedure Handler
   --       (MDI : access MDI_Window_Record'Class; Child : System.Address);
   --     Emitted when a new child is removed. You cannot use the "remove"
   --     signal since in fact the children are removed from notebooks that are
   --     part of the MDI, and thus "remove" is only emitted when a new
   --     notebook is destroyed.
   --     When this signal is emitted, Child no longer contains a widget, and
   --     is no longer part of the children, although you can still access its
   --     titles.
   --
   --  - "child_icon_changed"
   --     procedure Handler
   --       (MDI : access MDI_Window_Record'Class; Child : System.Address);
   --     Emitted when the icon for Child has changed
   --
   --  - "children_reorganized"
   --     procedure Handler (MDI : access MDI_Window_Record'Class);
   --     Emitted when the children have been reorganized: either a split
   --     occurred, or a window was dropped into another position
   --
   --  - "perspective_changed"
   --     procedure Handler (MDI : access MDI_Window_Record'Class);
   --     Called when the user has selected a new perspective. One use is to
   --     save the new desktop to a file.
   --
   --  </signals>
   --
   --  <signals>
   --  The following new signals are defined for the MDI_Child_Record object:
   --
   --  - "delete_event"
   --    function Handler (Child : access Gtk_Widget_Record'Class)
   --                     return Boolean;
   --
   --    This signal is emitted for each item in the MDI window before it is
   --    actually deleted. The child is destroyed only if the handler returns
   --    False.
   --    Note that the Child passed in argument is exactly the one you passed
   --    to Put to insert it in the MDI window.
   --    Note that this is also the signal to use to prevent top level
   --    Gtk_Window from being destroyed.
   --
   --  - "selected"
   --    procedure Handler (Child : access MDI_Child_Record'Class);
   --
   --    This is emitted when the child is selected, ie gains the
   --    MDI focus. You should probably also connect to the "grab_focus" signal
   --    to be informed when the child gets the keyboard focus. This can be
   --    used to transfer the focus to some specific part of the
   --    widget. Connecting to "grab_focus" should be done with the After
   --    parameter set to True.
   --
   --  - "float_child"
   --    procedure Handler (Child : access MDI_Child_Record'Class);
   --
   --    Emitted when a child is set as floating
   --
   --  - "unfloat_child"
   --    procedure Handler (Child : access MDI_Child_Record'Class);
   --
   --    Emitted when a child is put back in the main MDI window
   --
   --  - "child_state_changed"
   --    procedure Handler (Child : access MDI_Child_Record'Class);
   --
   --    Emitted when the state of the child has changed. See the function
   --    Get_State. In particular, this signal can be detected when a child is
   --    removed from the current perspective (the new state is "invisible"),
   --    and when it is put back (the new state is "normal" or "floating").
   --
   --  </signals>

   Signal_Child_Selected       : constant Signal_Name := "child_selected";
   Signal_Float_Child          : constant Signal_Name := "float_child";
   Signal_Child_Title_Changed  : constant Signal_Name := "child_title_changed";
   Signal_Child_Added          : constant Signal_Name := "child_added";
   Signal_Child_Removed        : constant Signal_Name := "child_removed";
   Signal_Child_Icon_Changed   : constant Signal_Name := "child_icon_changed";
   Signal_Delete_Event         : constant Signal_Name := "delete_event";
   Signal_Selected             : constant Signal_Name := "selected";
   Signal_Unfloat_Child        : constant Signal_Name := "unfloat_child";
   Signal_Perspective_Changed  : constant Signal_Name := "perspective_changed";
   Signal_Children_Reorganized : constant Signal_Name :=
                                   "children_reorganized";
   Signal_Child_State_Changed  : constant Signal_Name := "child_state_changed";

private
   type String_Access is access all UTF8_String;

   type MDI_Child_Record is new Gtk.Event_Box.Gtk_Event_Box_Record with record
      Initial       : Gtk.Widget.Gtk_Widget;
      --  The widget we use to build this child.

      Main_Box      : Gtk.Box.Gtk_Box;
      --  The main container.

      State         : State_Type := Normal;

      Group         : Child_Group := Group_Default;

      Title         : String_Access;
      Short_Title   : String_Access;
      --  Title of the item, as it appears in the title bar.
      --  These are UTF8-Encoded

      XML_Node_Name : String_Access;
      --  The name of the XML node when this child is saved in a desktop (if
      --  we know it). This is used to reuse a child when switching
      --  perspectives.

      MDI           : MDI_Window;
      --  The MDI to which the child belongs. We cannot get this information
      --  directly from Get_Parent since some children are actually floating
      --  and do not belong to the MDI anymore.

      Menu_Item     : Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item;
      --  The item in the dynamic menu that represents this child.

      Flags         : Child_Flags;

      Focus_Widget  : Gtk.Widget.Gtk_Widget;
      --  The widget which should actually get the keyboard focus

      Icon          : Gdk.Pixbuf.Gdk_Pixbuf;

      Title_Box     : Gtk.Box.Gtk_Box;
      --  Box that contains the title. It will be resized whenever the title
      --  font changes.

      Tab_Label     : Gtk.Label.Gtk_Label;
      --  label used when child is in a notebook, null if not in a notebook
   end record;

   type Child_Iterator (Group_By_Notebook : Boolean := False) is record
      Visible_Only : Boolean;

      case Group_By_Notebook is
         when False =>
            Iter : Gtk.Widget.Widget_List.Glist;

         when True =>
            MDI                 : MDI_Window;

            --  While iterating children
            Paned_Iter          : Gtkada.Multi_Paned.Child_Iterator;

            --  Whether we have already visited the children of the central
            --  area. This is True while iterating them, False afterward
            In_Central          : Boolean;

            --  While iterating the pages of a specific notebook (notebook is
            --  set to null when returning floating children)
            Notebook            : Gtk.Notebook.Gtk_Notebook;
            Notebook_Page       : Glib.Gint;

            --  While iterating the floating children
            Floating_Iter       : Gtk.Widget.Widget_List.Glist;
      end case;
   end record;

   type Drag_Status is (No_Drag, In_Pre_Drag, In_Drag);

   type MDI_Window_Record is new Gtkada.Multi_Paned.Gtkada_Multi_Paned_Record
   with record
      Items : Gtk.Widget.Widget_List.Glist := Gtk.Widget.Widget_List.Null_List;
      --  The list of all MDI children. This includes children in the editor
      --  area, even though they are technically in a separate multi_paned.
      --  Warning: this list might contain items which are in fact invisible in
      --  the MDI (in fact that are not even children of the MDI), if they
      --  existed in a previous perspective but no longer in the current one.

      Desktop_Was_Loaded : Boolean := False;
      --  True if a desktop was loaded

      Loading_Desktop : Boolean := False;
      --  Whether we are currently loading the desktop. This impacts a number
      --  of focus and sizing parameters, so that the desktop can be restored
      --  as accurately as possible.

      Delay_Before_Focus_Id : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      Delay_Before_Focus : Glib.Guint := 700;
      --  Delay in ms before a floating window gains the GPS focus, after the
      --  "focus_in" event. In all floating mode, this ensures that when the
      --  user is passing briefly over floating windows they do not gain the
      --  focus, thus potentially leaving the focus to the window that had it
      --  at the beginning of the move.
      --  Set to 0 to remove any delay.

      Focus_Child : MDI_Child := null;
      --  The child that currently has the focus. Some default actions will
      --  apply to this child only.

      Dnd_Message : String_Access;
      --  The message displayed during a dnd operation (see Set_Dnd_Message)

      Accel_Path_Prefix  : String_Access;
      --  The Accel path used for the dynamic menu

      Menu               : Gtk.Menu.Gtk_Menu;
      Float_Menu_Item    : Gtk.Check_Menu_Item.Gtk_Check_Menu_Item;
      Float_Menu_Item_Id : Gtk.Handlers.Handler_Id;
      Close_Menu_Item    : Gtk.Menu_Item.Gtk_Menu_Item;
      --  The dynamic menu used to provide access to the most common
      --  functions of MDI.

      Tab_Factory : Tab_Contextual_Menu_Factory;
      --  Build the contextual menu when right-clicking on tabs

      Title_Layout        : Pango.Layout.Pango_Layout;
      --  Layout used to draw titles in the MDI children

      Title_Bar_Height    : Glib.Gint;
      --  Height of the title bar for all the children

      Close_Floating_Is_Unfloat : Boolean;
      --  True if destroying a floating window will put the child back in the
      --  MDI instead of destroying it. False if the child should be destroyed
      --  (provided it accepts so in its delete_event handler).

      Highlight_Style : Gtk.Style.Gtk_Style;
      --  Style to use to highlight the tabs and menus for the highlighted
      --  children.

      Background_Color  : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Title_Bar_Color   : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Focus_Title_Color : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Default_Title_Color : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;

      Cursor_Cross      : Gdk.Cursor.Gdk_Cursor;
      Cursor_Fleur      : Gdk.Cursor.Gdk_Cursor;
      --  Cached cursors

      Draw_Title_Bars   : Title_Bars_Policy := Always;
      Tabs_Position     : Gtk.Enums.Gtk_Position_Type := Gtk.Enums.Pos_Bottom;
      Show_Tabs_Policy  : Show_Tabs_Policy_Enum := Automatic;

      Selection_Dialog : Gtk.Widget.Gtk_Widget;
      --  The interactive dialog for selecting new children.

      Dnd_Window       : Gtk.Window.Gtk_Window;
      Dnd_Window_Label : Gtk.Label.Gtk_Label;
      --  The small window displayed while a drag-and-drop operation is
      --  taking place.

      Group : Gtk.Accel_Group.Gtk_Accel_Group;

      All_Floating_Mode : Boolean := False;
      --  Set to true if all windows should be set to floating

      Independent_Perspectives : Boolean := False;
      --  See documentation for Configure.

      Use_Short_Titles_For_Floats : Boolean := False;
      --  Set to true if all floating children should use their short titles

      --  Handling of Dnd
      Drag_Start_X, Drag_Start_Y : Gint;
      In_Drag           : Drag_Status := No_Drag;
      Dnd_Rectangle     : Gdk.Rectangle.Gdk_Rectangle;  --  Highlighted area
      Dnd_Target        : Gdk.Gdk_Window;     --  The current target for DND
      Dnd_Target_Window : Gtk.Window.Gtk_Window;  --  The overlay window

      --  Loaded perspectives
      Perspective_Menu_Item  : Gtk.Menu_Item.Gtk_Menu_Item;
      Perspectives           : Glib.Xml_Int.Node_Ptr;
      View_Contents          : Glib.Xml_Int.Node_Ptr;
      Perspective_Names      : GNAT.Strings.String_List_Access;
      Central                : Gtkada.Multi_Paned.Gtkada_Multi_Paned;

      Current_Perspective    : Glib.Xml_Int.Node_Ptr;
      --  pointer into Perspectives
   end record;

   pragma Inline (Get_Widget);
   pragma Inline (Get_Focus_Child);
   pragma Inline (Get);
   pragma Inline (Next);
   pragma Inline (First_Child);

end Gtkada.MDI;

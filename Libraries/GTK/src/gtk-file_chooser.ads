-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006-2013, AdaCore                   --
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
--  This package provides an interface implemented by Gtk.File_Chooser_Widget
--  and Gtk.File_Chooser_Button, and by your own file selection widgets if you
--  wish to expose a standard interface.
--
--  Gtk_File_Chooser allows for shortcuts to various places in the filesystem.
--  In the default implementation these are displayed in the left pane. It may
--  be a bit confusing at first that these shortcuts come from various sources
--  and in various flavours, so lets explain the terminology here:
--    - Bookmarks
--      are created by the user, by dragging folders from the right pane to the
--      left pane, or by using the "Add". Bookmarks can be renamed and deleted
--      by the user.
--    - Shortcuts
--      can be provided by the application or by the underlying filesystem
--      abstraction (e.g. both the gnome-vfs and the Windows filesystems
--      provide "Desktop" shortcuts). Shortcuts cannot be modified by the user.
--    - Volumes
--      are provided by the underlying filesystem abstraction. They are the
--      "roots" of the filesystem.
--
--  File Names and Encodings
--
--  When the user is finished selecting files in a Gtk_File_Chooser, your
--  program can get the selected names either as filenames or as URIs. For
--  URIs, the normal escaping rules are applied if the URI contains non-ASCII
--  characters. However, filenames are always returned in the character set
--  specified by the G_FILENAME_ENCODING environment variable. Please see the
--  Glib documentation for more details about this variable.
--
--  Important: This means that while you can pass the result of
--  Get_Filename to low-level file system primitives , you will need to convert
--  it to UTF-8 before using it in a Gtk_Label for instance. Conversion is done
--  through Glib.Convert.Filename_To_UTF8.
--
--  You can add a custom preview widget to a file chooser and then get
--  notification about when the preview needs to be updated. To install a
--  preview widget, use gtk_file_chooser_set_preview_widget(). Then, connect to
--  the GtkFileChooser::update-preview signal to get notified when you need to
--  update the contents of the preview.
--
--  Preview widgets
--
--  You can add a custom preview widget to a file chooser and then get
--  notification about when the preview needs to be updated. To install a
--  preview widget, use Set_Preview_Widget. Then, connect to the
--  GtkFileChooser::update-preview signal to get notified when you need to
--  update the contents of the preview.
--
--  Your callback should use Get_Preview_Filename to see what needs previewing.
--  Once you have generated the preview for the corresponding file, you must
--  call Set_Preview_Widget_Active with a boolean flag that indicates whether
--  your callback could successfully generate a preview.
--
--  Adding Extra Widgets

--  You can add extra widgets to a file chooser to provide options that are not
--  present in the default design. For example, you can add a toggle button to
--  give the user the option to open a file in read-only mode. You can use
--  Set_Extra_Widget to insert additional widgets in a file chooser.
--
--  If you want to set more than one extra widget in the file chooser, you can
--  a container such as a GtkVBox or a GtkTable and include your widgets in it.
--  Then, set the container as the whole extra widget.
--
--  Key bindings
--
--  The following default key bindings are provided, but you can use a gtkrc
--  file to override them if you need (see gtk-rc.ads).
--     Signal name    | Key binding
--     location-popup | Control-L  (empty path)
--                    | /          (path of "/")
--                    | ~          (home directory)
--     up-folder      | Alt-Up  or backspace
--     down-folder    | Alt-Down
--     home-folder    | Alt-Home
--     desktop-folder | Alt-D
--     quick-bookmark | Alt-1 through Alt-0
--
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Selectors</group>

with Glib.Error;
with Glib.Properties;
with Glib.Types;
with Gtk.Enums;
with Gtk.File_Filter;
with Gtk.Widget;

package Gtk.File_Chooser is

   type Gtk_File_Chooser is new Glib.Types.GType_Interface;

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_File_Chooser

   type File_Chooser_Action is
     (Action_Open,
      Action_Save,
      Action_Select_Folder,
      Action_Create_Folder);
   --  Describes whether a Gtk_File_Chooser is being used to open existing
   --  files or to save to a possibly new file.
   --    Action_Open:          Will only let the user select existing file
   --    Action_Save:          Select existing file or enter new filename
   --    Action_Select_Folder: Only pick an existing folder
   --    Action_Create_Folder: Select existing folder or enter new name

   type File_Chooser_Confirmation is
     (Confirmation_Confirm,
      Confirmation_Accept_Filename,
      Confirmation_Select_Again);
   --  Used as a return value of handlers for the confirm-overwrite signal of a
   --  Gtk_File_Chooser. This value determines whether the file chooser will
   --  present the stock confirmation dialog, accept the user's choice of a
   --  filename, or let the user choose another filename.
   --    Confirmation_Confirm: Ask confirmation about overwriting existing file
   --    Confirmation_Accept_Filename: Accept the user's choice
   --    Confirmation_Select_Again: Let the user select another file

   type File_Chooser_Error is
     (Error_Non_Existent,
      Error_Bad_Filename);
   --  Identify the various errors that can occur while calling
   --  Gtk_File_Chooser functions

   -------------------
   -- Configuration --
   -------------------

   procedure Set_Action
     (Chooser : Gtk_File_Chooser; Action : File_Chooser_Action);
   function Get_Action (Chooser : Gtk_File_Chooser) return File_Chooser_Action;
   --  Sets the type of operation that the chooser is performing; the
   --  user interface is adapted to suit the selected action. For example,
   --  an option to create a new folder might be shown if the action is
   --  Action_Save, but not if the action is Action_Open.

   procedure Set_Local_Only
     (Chooser : Gtk_File_Chooser; Local_Only : Boolean := True);
   function Get_Local_Only (Chooser : Gtk_File_Chooser) return Boolean;
   --  Sets whether only local files can be selected in the file selector. If
   --  Local_Only is True (the default), then the selected file are files are
   --  guaranteed to be accessible through the operating systems native file
   --  file system and therefore the application only needs to worry about the
   --  filename functions in Gtk_File_Chooser, like Get_Filename, rather than
   --  the URI functions like Get_Uri,

   procedure Set_Select_Multiple
     (Chooser : Gtk_File_Chooser; Select_Multiple : Boolean);
   function Get_Select_Multiple (Chooser : Gtk_File_Chooser) return Boolean;
   --  Sets whether multiple files can be selected in the file selector. This
   --  is only relevant if the action is set to be Action_Open or Action_Save.
   --  It cannot be set with either of the folder actions.

   procedure Set_Show_Hidden
     (Chooser : Gtk_File_Chooser; Show_Hidden : Boolean);
   function Get_Show_Hidden (Chooser : Gtk_File_Chooser) return Boolean;
   --  Sets whether hidden files and folders are displayed in the file selector

   procedure Set_Do_Overwrite_Confirmation
     (Chooser                   : Gtk_File_Chooser;
      Do_Overwrite_Confirmation : Boolean);
   function Get_Do_Overwrite_Confirmation
     (Chooser : Gtk_File_Chooser) return Boolean;
   --  Sets whether a file chooser in Action_Save mode will present a
   --  confirmation dialog if the user types a file name that already exists.
   --  This is False by default.
   --  Regardless of this setting, Chooser will emit the "confirm-overwrite"
   --  signal when appropriate.
   --  If all you need is the stock confirmation dialog, set this property to
   --  True You can override the way confirmation is done by actually handling
   --  the "confirm-overwrite" signal; please refer to its documentation for
   --  the details.

   procedure Set_Current_Name (Chooser : Gtk_File_Chooser; Name : UTF8_String);
   --  Sets the current name in the file selector, as if entered by the user.
   --  Note that the name passed in here is a UTF-8 string rather than a
   --  filename. This function is meant for such uses as a suggested name in a
   --  "Save As..." dialog.
   --  If you want to preselect a particular existing file, you should use
   --  Set_Filename or Set_Uri instead.

   ---------------------------
   -- Filename manipulation --
   ---------------------------

   function Get_Filename (Chooser : Gtk_File_Chooser) return String;
   --  Gets the filename for the currently selected file in the file selector.
   --  If multiple files are selected, one of the filenames will be returned at
   --  random.
   --  If the file chooser is in folder mode, this function returns the
   --  selected folder.
   --  Return value: The currently selected filename, or "" if no file is
   --  selected, or the selected file can't be represented with a local
   --  filename.

   function Get_Filenames
     (Chooser : Gtk_File_Chooser) return Gtk.Enums.String_SList.GSlist;
   --  Lists all the selected files and subfolders in the current folder of
   --  Chooser. The returned names are full absolute paths. If files in the
   --  current folder cannot be represented as local filenames they will be
   --  ignored. (See Get_Uris).
   --  The returned value must be freed by the caller, through
   --  Gtk.Enums.Free_String_List.

   function Set_Filename
     (Chooser : Gtk_File_Chooser; Filename : String) return Boolean;
   --  Sets Filename as the current filename for the file chooser, by changing
   --  to the file's parent folder and actually selecting the file in list. If
   --  the Chooser is in Action_Save mode, the file's base name will also
   --  appear in the dialog's file name entry.
   --  If the file name isn't in the current folder of Chooser, then the
   --  current folder of Chooser will be changed to the folder containing
   --  Filename. This is equivalent to a sequence of Unselect_All followed by
   --  Select_Filename.
   --  Note that the file must exist, or nothing will be done except for the
   --  directory change.
   --  If you are implementing a "File/Save As..." dialog, you should use this
   --  function if you already have a file name to which the user may save; for
   --  example, when the user opens an existing file and then does "File/Save
   --  As..." on it. If you don't have a file name already &mdash; for example,
   --  if the user just created a new file and is saving it for the first time,
   --  do not call this function. Instead, use Set_Current_Name.
   --
   --  Returns True if both the folder could be changed and the file was
   --  selected successfully.

   function Select_Filename
     (Chooser : Gtk_File_Chooser; Filename : String) return Boolean;
   procedure Unselect_Filename
     (Chooser  : Gtk_File_Chooser; Filename : String);
   --  Selects a filename. If the file name isn't in the current
   --  folder of Chooser, then the current folder of Chooser will
   --  be changed to the folder containing Filename.
   --  Returns True if both the folder could be changed and the file was
   --  selected successfully.

   procedure Select_All (Chooser : Gtk_File_Chooser);
   procedure Unselect_All (Chooser : Gtk_File_Chooser);
   --  Selects all the files in the current folder of a file chooser.

   function Set_Current_Folder
     (Chooser : Gtk_File_Chooser; Filename : String) return Boolean;
   --  Sets the current folder for Chooser from a local filename. The user will
   --  be shown the full contents of the current folder, plus user interface
   --  elements for navigating to other folders.
   --  Filename is an absolute file name.
   --  Returns True if the folder could be changed successfully.

   function Get_Current_Folder (Chooser : Gtk_File_Chooser) return String;
   --  Gets the current folder of Chooser as a local filename.
   --  Returns "" if the file chooser was unable to load the last folder that
   --  was requested from it, for instance with a call to Set_Current_Folder
   --  with an invalid directory.
   --  Also returns "" if the selected path cannot be represented as a local
   --  filename.

   ----------------------
   -- URI manipulation --
   ----------------------

   function Set_Uri
     (Chooser : Gtk_File_Chooser; Uri : String) return Boolean;
   --  Sets the file referred to by Uri as the current file for the file
   --  chooser, by changing to the URI's parent folder and actually selecting
   --  the URI in the list. If the Chooser is Action_Save mode, the URI's base
   --  name will also appear in the dialog's file name entry.
   --  If the URI isn't in the current folder of Chooser, then the current
   --  folder of Chooser will be changed to the folder containing Uri. This is
   --  equivalent to a sequence of Unselect_All followed by Select_Uri.
   --  Note that the URI must exist, or nothing will be done except for the
   --  directory change.
   --  See also Set_Filename
   --  Return True if both the folder could be changed and the URI was
   --  selected successfully.

   function Get_Uri (Chooser : Gtk_File_Chooser) return String;
   --  Gets the URI for the currently selected file in the file selector. If
   --  multiple files are selected, one of the filenames will be returned at
   --  random.
   --  If the file chooser is in folder mode, this function returns the
   --  selected folder.

   function Get_Uris
     (Chooser : Gtk_File_Chooser) return Gtk.Enums.String_SList.GSlist;
   --  Lists all the selected files and subfolders in the current folder of
   --  Chooser. The returned names are full absolute URIs.
   --  The returned value must be freed by the caller

   function Select_Uri
     (Chooser : Gtk_File_Chooser; Uri : String) return Boolean;
   procedure Unselect_Uri (Chooser : Gtk_File_Chooser; Uri : String);
   --  Selects the file by Uri. If the URI doesn't refer to a file in the
   --  current folder of Chooser, then the current folder of Chooser will be
   --  changed to the folder containing Uri.
   --  Return True if both the folder could be changed and the URI was
   --  selected successfully.

   function Set_Current_Folder_Uri
     (Chooser : Gtk_File_Chooser; Uri : String) return Boolean;
   --  Sets the current folder for Chooser from an URI. The user will be shown
   --  the full contents of the current folder, plus user interface elements
   --  for navigating to other folders.
   --  Return True if the folder could be changed successfully.

   function Get_Current_Folder_Uri (Chooser : Gtk_File_Chooser) return String;
   --  Gets the current folder of Chooser as an URI, or "" if the chooser was
   --  unable to load the last folder set by Set_Current_Folder_Uri.

   --------------------
   -- Preview widget --
   --------------------

   procedure Set_Preview_Widget
     (Chooser        : Gtk_File_Chooser;
      Preview_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   function Get_Preview_Widget
     (Chooser : Gtk_File_Chooser) return Gtk.Widget.Gtk_Widget;
   --  Sets an application-supplied widget to use to display a custom preview
   --  of the currently selected file. To implement a preview, after setting
   --  the preview widget, you connect to the "update-preview" signal, and call
   --  Get_Preview_Filename() or Get_Preview_Uri on each change. If you can
   --  display a preview of the new file, update your widget and set the
   --  preview active using Set_Preview_Widget_Active. Otherwise, set the
   --  preview inactive.
   --  When there is no application-supplied preview widget, or the
   --  application-supplied preview widget is not active, the file chooser may
   --  display an internally generated preview of the current file or it may
   --  display no preview at all.

   procedure Set_Preview_Widget_Active
     (Chooser : Gtk_File_Chooser;
      Active  : Boolean);
   function Get_Preview_Widget_Active
     (Chooser : Gtk_File_Chooser) return Boolean;
   --  Sets whether the preview widget set by Set_Preview_Widget should be
   --  shown for the current filename. When Active is set to false, the file
   --  chooser may display an internally generated preview of the current file
   --  or it may display no preview at all.

   procedure Set_Use_Preview_Label
     (Chooser   : Gtk_File_Chooser;
      Use_Label : Boolean);
   function Get_Use_Preview_Label (Chooser : Gtk_File_Chooser) return Boolean;
   --  Sets whether the file chooser should display a stock label with the name
   --  of the file that is being previewed; the default is True. Applications
   --  that want to draw the whole preview area themselves should set this to
   --  False and display the name themselves in their preview widget.

   function Get_Preview_Filename (Chooser : Gtk_File_Chooser) return String;
   --  Gets the filename that should be previewed in a custom preview
   --  widget. See Set_Preview_Widget.
   --  Return "" if no file is selected or it can't be represented as a local
   --  filename.

   function Get_Preview_Uri (Chooser : Gtk_File_Chooser) return String;
   --  Gets the URI that should be previewed in a custom preview
   --  widget. See Set_Preview_Widget.
   --  Return "" if no file is selected.

   ------------------
   -- Extra widget --
   ------------------

   procedure Set_Extra_Widget
     (Chooser      : Gtk_File_Chooser;
      Extra_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   function Get_Extra_Widget
     (Chooser : Gtk_File_Chooser) return Gtk.Widget.Gtk_Widget;
   --  Sets an application-supplied widget to provide extra options to the user

   -------------
   -- Filters --
   -------------

   procedure Add_Filter
     (Chooser : Gtk_File_Chooser;
      Filter  : access Gtk.File_Filter.Gtk_File_Filter_Record'Class);
   procedure Remove_Filter
     (Chooser : Gtk_File_Chooser;
      Filter  : access Gtk.File_Filter.Gtk_File_Filter_Record'Class);
   --  Adds or Removes Filter to the list of filters that the user can select
   --  between.
   --  When a filter is selected, only files that are passed by that
   --  filter are displayed.
   --  Note that the Chooser takes ownership of the filter, so you have to
   --  ref and sink it if you want to keep a reference.

   procedure Set_Filter
     (Chooser : Gtk_File_Chooser;
      Filter  : access Gtk.File_Filter.Gtk_File_Filter_Record'Class);
   function Get_Filter
     (Chooser : Gtk_File_Chooser) return Gtk.File_Filter.Gtk_File_Filter;
   --  Sets the current filter; only the files that pass the filter will be
   --  displayed. If the user-selectable list of filters is non-empty, then the
   --  filter should be one of the filters in that list. Setting the current
   --  filter when the list of filters is empty is useful if you want to
   --  restrict the displayed set of files without letting the user change it.

   function List_Filters
     (Chooser : Gtk_File_Chooser)
      return Glib.Object.Object_List.GSlist;
   --  Lists the current set of user-selectable filters. The list contains
   --  Gtk_File_Filter instances.
   --  Do not free the contents of list, still owned by gtk+, but you must free
   --  the list itself with Glib.Object.Object_List.Free.

   ---------------------
   -- Shorcut folders --
   ---------------------

   function Add_Shortcut_Folder
     (Chooser : Gtk_File_Chooser;
      Folder  : String) return Glib.Error.GError;
   function Remove_Shortcut_Folder
     (Chooser : Gtk_File_Chooser; Folder : String) return Glib.Error.GError;
   --  Adds a folder to be displayed with the shortcut folders in a file
   --  chooser. Note that shortcut folders do not get saved, as they are
   --  provided by the application. For example, you can use this to add a
   --  "/usr/share/mydrawprogram/Clipart" folder to the volume list.
   --  Return null if the folder could be added successfully, or the code
   --  of the error otherwise.

   function List_Shortcut_Folders
     (Chooser : Gtk_File_Chooser) return Gtk.Enums.String_SList.GSlist;
   --  Queries the list of shortcut folders in the file chooser, as set by
   --  Add_Shortcut_Folder.
   --  The returned value must be freed by the user.

   function Add_Shortcut_Folder_Uri
     (Chooser : Gtk_File_Chooser;
      Uri     : String) return Glib.Error.GError;
   function Remove_Shortcut_Folder_Uri
     (Chooser : Gtk_File_Chooser; Uri : String) return Glib.Error.GError;
   --  Adds a folder URI to be displayed with the shortcut folders in a file
   --  chooser. Note that shortcut folders do not get saved, as they are
   --  provided by the application. For example, you can use this to add a
   --  "file:///usr/share/mydrawprogram/Clipart" folder to the volume list.
   --  Return null if the folder could be added successfully, or the code of
   --  the error otherwise.

   function List_Shortcut_Folder_Uris
     (Chooser : Gtk_File_Chooser) return Gtk.Enums.String_SList.GSlist;
   --  Queries the list of shortcut folders in the file chooser, as set by
   --  Add_Shortcut_Folder_Uri.
   --  The returned value must be freed by the user.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:   action
   --    Type:   File_Chooser_Action
   --    See also: Set_Action / Get_Action
   --
   --  - Name:   do-overwrite-confirmation
   --    Type:   Boolean
   --    See also: Set_Do_Overwrite_Confirmation/Get_Do_Overwrite_Confirmation
   --
   --  - Name:   extra-widget
   --    Type:   Gtk_Widget
   --    See also: Set_Extra_Widget / Get_Extra_Widget
   --
   --  - Name:   file-system-backend
   --    Type:   String
   --
   --  - Name:   filter
   --    Type:   Gtk_File_Filter
   --
   --  - Name:   local-only
   --    Type:   Boolean
   --    See also: Set_Local_Only / Get_Local_Only
   --
   --  - Name:   preview-widget
   --    Type:   Gtk_Widget
   --    See also: Set_Preview_Widget / Get_Preview_Widget
   --
   --  - Name:   preview-widget-active
   --    Type:   Boolean
   --    See also: Set_Preview_Widget_Active / Get_Preview_Widget_Active
   --
   --  - Name:   select-multiple
   --    Type:   Boolean
   --    See also: Set_Select_Multiple / Get_Select_Multiple
   --
   --  - Name:   show-hidden
   --    Type:   Boolean
   --    See also: Set_Show_Hidden / Get_Show_Hidden
   --
   --  - Name:   use-preview-label
   --    Type:   Boolean
   ---   See also: Set_Use_Preview_Label / Get_Use_Preview_Label
   --
   --  </properties>

   --  Action_Property : constant Glib.Properties.Property_Enum;
   Do_Overwrite_Confirmation_Property
                                  : constant Glib.Properties.Property_Boolean;
   Extra_Widget_Property          : constant Glib.Properties.Property_Object;
   File_System_Backend_Property   : constant Glib.Properties.Property_String;
   Filter_Property                : constant Glib.Properties.Property_Object;
   Local_Only_Property            : constant Glib.Properties.Property_Boolean;
   Preview_Widget_Property        : constant Glib.Properties.Property_Object;
   Preview_Widget_Active_Property : constant Glib.Properties.Property_Boolean;
   Select_Multiple_Property       : constant Glib.Properties.Property_Boolean;
   Show_Hidden_Property           : constant Glib.Properties.Property_Boolean;
   Use_Preview_Label_Property     : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "confirm-overwrite"
   --    function Handler
   --        (Chooser : Gtk_File_Chooser) return File_Chooser_Confirmation;
   --    This signal gets emitted whenever it is appropriate to present a
   --    confirmation dialog when the user has selected a file name that
   --    already exists. The signal only gets emitted when the file chooser is
   --    in Action_Save mode.
   --    Most applications just need to turn on the do-overwrite-confirmation
   --    property (or call the Set_Do_Overwrite_Confirmation function), and
   --    they will automatically get a stock confirmation dialog. Applications
   --    which need to customize this behavior should do that, and also connect
   --    to the confirm-overwrite signal.
   --    A signal handler for this signal must return a value which indicates
   --    the action to take. If the handler determines that the user wants to
   --    select a different filename, it should return
   --    Confirmation_Select_Again. If it determines that the user is satisfied
   --    with his choice of file name, it should return
   --    Confirmation_Accept_Filename. On the other hand, if it determines that
   --    the stock confirmation dialog should be used, it should return
   --    Confirmation_Confir.
   --
   --  - "current-folder-changed"
   --    procedure Handler (Chooser : Gtk_File_Chooser);
   --    This signal is emitted when the current folder in a Gtk_File_Chooser
   --    changes. This can happen due to the user performing some action that
   --    changes folders, such as selecting a bookmark or visiting a folder on
   --    the file list. It can also happen as a result of calling a function to
   --    explicitly change the current folder in a file chooser.
   --    Normally you do not need to connect to this signal, unless you need to
   --    keep track of which folder a file chooser is showing.
   --
   --  - "file-activated"
   --    procedure Handler (Chooser : Gtk_File_Chooser);
   --    This signal is emitted when the user "activates" a file in the file
   --    chooser. This can happen by double-clicking on a file in the file
   --    list, or by pressing Enter.
   --    Normally you do not need to connect to this signal. It is used
   --    internally by Gtk_File_Chooser_Dialog to know when to activate the
   --    default button in the dialog.
   --
   --  - "selection-changed"
   --    procedure Handler (Chooser : Gtk_File_Chooser);
   --    This signal is emitted when there is a change in the set of selected
   --    files in a Gtk_File_Chooser. This can happen when the user modifies
   --    the selection with the mouse or the keyboard, or when explicitly
   --    calling functions to change the selection.
   --    Normally you do not need to connect to this signal, as it is easier to
   --    wait for the file chooser to finish running, and then to get the list
   --    of selected files using the functions mentioned below.
   --
   --  - "update-preview"
   --    procedure Handler (Chooser : Gtk_File_Chooser);
   --    This signal is emitted when the preview in a file chooser should be
   --    regenerated. For example, this can happen when the currently selected
   --    file changes. You should use this signal if you want your file chooser
   --    to have a preview widget.
   --    Once you have installed a preview widget with Set_Preview_Widget, you
   --    should update it when this signal is emitted. You can use the
   --    functions Get_Preview_Filename or Get_Preview_Uri to get the name of
   --    the file to preview. Your widget may not be able to preview all kinds
   --    of files; your callback must call Set_Preview_Wiget_Active to inform
   --    the file chooser about whether the preview was generated successfully
   --    or not.
   --  </signals>

   Signal_Confirm_Overwrite      : constant Glib.Signal_Name :=
                                     "confirm-overwrite";
   Signal_Current_Folder_Changed : constant Glib.Signal_Name :=
                                     "current-folder-changed";
   Signal_File_Activated         : constant Glib.Signal_Name :=
                                     "file-activated";
   Signal_Selection_Changed      : constant Glib.Signal_Name :=
                                     "selection-changed";
   Signal_Update_Preview         : constant Glib.Signal_Name :=
                                     "update-preview";

private
   pragma Import (C, Get_Type, "gtk_file_chooser_get_type");
   pragma Import (C, Set_Action, "gtk_file_chooser_set_action");
   pragma Import (C, Get_Action, "gtk_file_chooser_get_action");
   pragma Import (C, Select_All, "gtk_file_chooser_select_all");
   pragma Import (C, Unselect_All, "gtk_file_chooser_unselect_all");

--     Action_Property : constant Glib.Properties.Property_Enum :=
--       Glib.Properties.Build ("action");
   Do_Overwrite_Confirmation_Property
     : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("do-overwrite-confirmation");
   Extra_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("extra-widget");
   File_System_Backend_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("file-system-backend");
   Filter_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("filter");
   Local_Only_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("local-only");
   Preview_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("preview-widget");
   Preview_Widget_Active_Property : constant Glib.Properties.Property_Boolean
     := Glib.Properties.Build ("preview-widget-active");
   Select_Multiple_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("select-multiple");
   Show_Hidden_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-hidden");
   Use_Preview_Label_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-preview-label");

end Gtk.File_Chooser;

--  No binding: gtk_file_chooser_error_quark

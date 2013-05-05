-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2013, AdaCore                   --
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
--  This widget provides completion functionality for Gtk.Gentry.Gtk_Entry.
--
--  "Completion functionality" means that when the user modifies the text in
--  the entry, GtkEntryCompletion checks which rows in the model match the
--  current content of the entry, and displays a list of matches. By default,
--  the matching is done by comparing the entry text case-insensitively against
--  the text column of the model (see Set_Text_Column), but this can be
--  overridden with a custom match function (see Set_Match_Func).
--
--  When the user selects a completion, the content of the entry is updated.
--  By default, the content of the entry is replaced by the text column of the
--  model, but this can be overridden by connecting to the ::match-selected
--  signal and updating the entry in the signal handler. Note that you should
--  return TRUE from the signal handler to suppress the default behaviour.
--
--  To add completion functionality to an entry, use Gtk.Entry.Set_Completion.
--
--  In addition to regular completion matches, which will be inserted into the
--  entry when they are selected, GtkEntryCompletion also allows to display
--  "actions" in the popup window. Their appearance is similar to menuitems, to
--  differentiate them clearly from completion strings. When an action is
--  selected, the ::action-activated signal is emitted.
--
--  </description>
--  <group>Numeric/Text Data Entry</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Glib.Properties;      use Glib.Properties;
with Glib.Types;           use Glib.Types;
with Gtk.Buildable;        use Gtk.Buildable;
with Gtk.Cell_Layout;      use Gtk.Cell_Layout;
with Gtk.Tree_Model;       use Gtk.Tree_Model;
with Gtk.Widget;           use Gtk.Widget;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package Gtk.Entry_Completion is

   type Gtk_Entry_Completion_Record is new GObject_Record with null record;
   type Gtk_Entry_Completion is access all Gtk_Entry_Completion_Record'Class;

   type C_Gtk_Entry_Completion_Match_Func is access function
     (Completion    : System.Address;
      Key           : Interfaces.C.Strings.chars_ptr;
      Iter          : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data          : System.Address) return Boolean;
   pragma Convention (C, C_Gtk_Entry_Completion_Match_Func);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Completion : out Gtk_Entry_Completion);
   procedure Initialize
      (Completion : access Gtk_Entry_Completion_Record'Class);
   --  Creates a new Gtk.Entry_Completion.Gtk_Entry_Completion object.
   --  Since: gtk+ 2.4

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_entry_completion_get_type");

   -------------
   -- Methods --
   -------------

   procedure Complete (Completion : access Gtk_Entry_Completion_Record);
   --  Requests a completion operation, or in other words a refiltering of the
   --  current list with completions, using the current key. The completion
   --  list view will be updated accordingly.
   --  Since: gtk+ 2.4

   procedure Delete_Action
      (Completion : access Gtk_Entry_Completion_Record;
       Index      : Gint);
   --  Deletes the action at Index_ from Completion's action list.
   --  Since: gtk+ 2.4
   --  "index_": The index of the item to Delete.

   function Get_Completion_Prefix
      (Completion : access Gtk_Entry_Completion_Record) return UTF8_String;
   --  Get the original text entered by the user that triggered the completion
   --  or null if there's no completion ongoing.
   --  Since: gtk+ 2.12

   function Get_Entry
      (Completion : access Gtk_Entry_Completion_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the entry Completion has been attached to.
   --  Since: gtk+ 2.4

   function Get_Inline_Completion
      (Completion : access Gtk_Entry_Completion_Record) return Boolean;
   procedure Set_Inline_Completion
      (Completion        : access Gtk_Entry_Completion_Record;
       Inline_Completion : Boolean);
   --  Sets whether the common prefix of the possible completions should be
   --  automatically inserted in the entry.
   --  Since: gtk+ 2.6
   --  "inline_completion": True to do inline completion

   function Get_Inline_Selection
      (Completion : access Gtk_Entry_Completion_Record) return Boolean;
   procedure Set_Inline_Selection
      (Completion       : access Gtk_Entry_Completion_Record;
       Inline_Selection : Boolean);
   --  Sets whether it is possible to cycle through the possible completions
   --  inside the entry.
   --  Since: gtk+ 2.12
   --  "inline_selection": True to do inline selection

   function Get_Minimum_Key_Length
      (Completion : access Gtk_Entry_Completion_Record) return Gint;
   procedure Set_Minimum_Key_Length
      (Completion : access Gtk_Entry_Completion_Record;
       Length     : Gint);
   --  Requires the length of the search key for Completion to be at least
   --  length. This is useful for long lists, where completing using a small
   --  key takes a lot of time and will come up with meaningless results anyway
   --  (ie, a too large dataset).
   --  Since: gtk+ 2.4
   --  "length": The minimum length of the key in order to start completing.

   function Get_Model
      (Completion : access Gtk_Entry_Completion_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model;
   procedure Set_Model
      (Completion : access Gtk_Entry_Completion_Record;
       Model      : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class);
   --  Sets the model for a Gtk.Entry_Completion.Gtk_Entry_Completion. If
   --  Completion already has a model set, it will remove it before setting the
   --  new model. If model is null, then it will unset the model.
   --  Since: gtk+ 2.4
   --  "model": The Gtk.Tree_Model.Gtk_Tree_Model.

   function Get_Popup_Completion
      (Completion : access Gtk_Entry_Completion_Record) return Boolean;
   procedure Set_Popup_Completion
      (Completion       : access Gtk_Entry_Completion_Record;
       Popup_Completion : Boolean);
   --  Sets whether the completions should be presented in a popup window.
   --  Since: gtk+ 2.6
   --  "popup_completion": True to do popup completion

   function Get_Popup_Set_Width
      (Completion : access Gtk_Entry_Completion_Record) return Boolean;
   procedure Set_Popup_Set_Width
      (Completion      : access Gtk_Entry_Completion_Record;
       Popup_Set_Width : Boolean);
   --  Sets whether the completion popup window will be resized to be the same
   --  width as the entry.
   --  Since: gtk+ 2.8
   --  "popup_set_width": True to make the width of the popup the same as the
   --  entry

   function Get_Popup_Single_Match
      (Completion : access Gtk_Entry_Completion_Record) return Boolean;
   procedure Set_Popup_Single_Match
      (Completion         : access Gtk_Entry_Completion_Record;
       Popup_Single_Match : Boolean);
   --  Sets whether the completion popup window will appear even if there is
   --  only a single match. You may want to set this to False if you are using
   --  <link linkend="GtkEntryCompletion--inline-completion">inline
   --  completion</link>.
   --  Since: gtk+ 2.8
   --  "popup_single_match": True if the popup should appear even for a single
   --  match

   function Get_Text_Column
      (Completion : access Gtk_Entry_Completion_Record) return Gint;
   procedure Set_Text_Column
      (Completion : access Gtk_Entry_Completion_Record;
       Column     : Gint);
   --  completion list with just strings. This function will set up Completion
   --  to have a list displaying all (and just) strings in the completion list,
   --  and to get those strings from Column in the model of Completion. This
   --  functions creates and adds a Gtk.Cellrenderertext.Gtk_Cellrenderertext
   --  for the selected column. If you need to set the text column, but don't
   --  want the cell renderer, use g_object_set to set the ::text_column
   --  property directly.
   --  Since: gtk+ 2.4
   --  "column": The column in the model of Completion to get strings from.

   procedure Insert_Action_Markup
      (Completion : access Gtk_Entry_Completion_Record;
       Index      : Gint;
       Markup     : UTF8_String);
   --  Inserts an action in Completion's action item list at position Index_
   --  with markup Markup.
   --  Since: gtk+ 2.4
   --  "index_": The index of the item to insert.
   --  "markup": Markup of the item to insert.

   procedure Insert_Action_Text
      (Completion : access Gtk_Entry_Completion_Record;
       Index      : Gint;
       Text       : UTF8_String);
   --  Inserts an action in Completion's action item list at position Index_
   --  with text Text. If you want the action item to have markup, use
   --  Gtk.Entry_Completion.Insert_Action_Markup.
   --  Since: gtk+ 2.4
   --  "index_": The index of the item to insert.
   --  "text": Text of the item to insert.

   procedure Insert_Prefix (Completion : access Gtk_Entry_Completion_Record);
   --  Requests a prefix insertion.
   --  Since: gtk+ 2.6

   procedure Set_Match_Func
      (Completion  : access Gtk_Entry_Completion_Record;
       Func        : C_Gtk_Entry_Completion_Match_Func;
       Func_Data   : System.Address;
       Func_Notify : Glib.G_Destroy_Notify_Address);
   --  Sets the match function for Completion to be Func. The match function
   --  is used to determine if a row should or should not be in the completion
   --  list.
   --  Since: gtk+ 2.4
   --  "func": The C_Gtk_Entry_Completion_Match_Func to use.
   --  "func_data": The user data for Func.
   --  "func_notify": Destroy notifier for Func_Data.

   ----------------------
   -- GtkAda additions --
   ----------------------

   generic
   type Data_Type (<>) is private;
   package Match_Functions is
      type Gtk_Entry_Completion_Match_Func is access
      function (Completion : access Gtk_Entry_Completion_Record'Class;
         Key        : String;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         User_Data  : Data_Type) return Boolean;

      type Destroy_Notify is access procedure (Data : in out Data_Type);

      procedure Set_Match_Func
        (Completion  : access Gtk_Entry_Completion_Record;
         Func        : Gtk_Entry_Completion_Match_Func;
         Func_Data   : Data_Type;
         Func_Notify : Destroy_Notify);
      --  Sets the match function for completion to be Func. The match function
      --  is used to determine if a row should or should not be in the
      --  completion list.
   end Match_Functions;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "CellLayout"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Entry_Completion_Record, Gtk_Entry_Completion);
   function "+"
     (Widget : access Gtk_Entry_Completion_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Entry_Completion
   renames Implements_Buildable.To_Object;

   package Implements_CellLayout is new Glib.Types.Implements
     (Gtk.Cell_Layout.Gtk_Cell_Layout, Gtk_Entry_Completion_Record, Gtk_Entry_Completion);
   function "+"
     (Widget : access Gtk_Entry_Completion_Record'Class)
   return Gtk.Cell_Layout.Gtk_Cell_Layout
   renames Implements_CellLayout.To_Interface;
   function "-"
     (Interf : Gtk.Cell_Layout.Gtk_Cell_Layout)
   return Gtk_Entry_Completion
   renames Implements_CellLayout.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Inline_Completion_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Determines whether the common prefix of the possible completions should
   --  be inserted automatically in the entry. Note that this requires
   --  text-column to be set, even if you are using a custom match function.
   --
   --  Name: Inline_Selection_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Determines whether the possible completions on the popup will appear in
   --  the entry as you navigate through them.
   --
   --  Name: Minimum_Key_Length_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Model_Property
   --  Type: Gtk.Tree_Model.Gtk_Tree_Model
   --  Flags: read-write
   --
   --  Name: Popup_Completion_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Determines whether the possible completions should be shown in a popup
   --  window.
   --
   --  Name: Popup_Set_Width_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Determines whether the completions popup window will be resized to the
   --  width of the entry.
   --
   --  Name: Popup_Single_Match_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Determines whether the completions popup window will shown for a single
   --  possible completion. You probably want to set this to False if you are
   --  using <link linkend="GtkEntryCompletion--inline-completion">inline
   --  completion</link>.
   --
   --  Name: Text_Column_Property
   --  Type: Gint
   --  Flags: read-write
   --  The column of the model containing the strings. Note that the strings
   --  must be UTF-8.

   Inline_Completion_Property : constant Glib.Properties.Property_Boolean;
   Inline_Selection_Property : constant Glib.Properties.Property_Boolean;
   Minimum_Key_Length_Property : constant Glib.Properties.Property_Int;
   Model_Property : constant Glib.Properties.Property_Object;
   Popup_Completion_Property : constant Glib.Properties.Property_Boolean;
   Popup_Set_Width_Property : constant Glib.Properties.Property_Boolean;
   Popup_Single_Match_Property : constant Glib.Properties.Property_Boolean;
   Text_Column_Property : constant Glib.Properties.Property_Int;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "action-activated"
   --     procedure Handler
   --       (Self  : access Gtk_Entry_Completion_Record'Class;
   --        Index : Gint);
   --    --  "index": the index of the activated action
   --  Gets emitted when an action is activated.
   --
   --  "cursor-on-match"
   --     function Handler
   --       (Self  : access Gtk_Entry_Completion_Record'Class;
   --        Model : Gtk.Tree_Model.Gtk_Tree_Model;
   --        Iter  : TreeIter) return Boolean;
   --    --  "model": the Gtk.Tree_Model.Gtk_Tree_Model containing the matches
   --    --  "iter": a GtkTreeIter positioned at the selected match
   --  Gets emitted when a match from the cursor is on a match of the list.The
   --  default behaviour is to replace the contents of the entry with the
   --  contents of the text column in the row pointed to by Iter.
   --  Returns True if the signal has been handled
   --
   --  "insert-prefix"
   --     function Handler
   --       (Self   : access Gtk_Entry_Completion_Record'Class;
   --        Prefix : UTF8_String) return Boolean;
   --    --  "prefix": the common prefix of all possible completions
   --  Gets emitted when the inline autocompletion is triggered. The default
   --  behaviour is to make the entry display the whole prefix and select the
   --  newly inserted part. Applications may connect to this signal in order to
   --  insert only a smaller part of the Prefix into the entry - e.g. the entry
   --  used in the Gtk.File_Chooser.Gtk_File_Chooser inserts only the part of
   --  the prefix up to the next '/'.
   --  Returns True if the signal has been handled
   --
   --  "match-selected"
   --     function Handler
   --       (Self  : access Gtk_Entry_Completion_Record'Class;
   --        Model : Gtk.Tree_Model.Gtk_Tree_Model;
   --        Iter  : TreeIter) return Boolean;
   --    --  "model": the Gtk.Tree_Model.Gtk_Tree_Model containing the matches
   --    --  "iter": a GtkTreeIter positioned at the selected match
   --  Gets emitted when a match from the list is selected. The default
   --  behaviour is to replace the contents of the entry with the contents of
   --  the text column in the row pointed to by Iter.
   --  Returns True if the signal has been handled

   Signal_Action_Activated : constant Glib.Signal_Name := "action-activated";
   Signal_Cursor_On_Match : constant Glib.Signal_Name := "cursor-on-match";
   Signal_Insert_Prefix : constant Glib.Signal_Name := "insert-prefix";
   Signal_Match_Selected : constant Glib.Signal_Name := "match-selected";

private
   Inline_Completion_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inline-completion");
   Inline_Selection_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inline-selection");
   Minimum_Key_Length_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("minimum-key-length");
   Model_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("model");
   Popup_Completion_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("popup-completion");
   Popup_Set_Width_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("popup-set-width");
   Popup_Single_Match_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("popup-single-match");
   Text_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("text-column");
end Gtk.Entry_Completion;

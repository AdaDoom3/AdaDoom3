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
--  A Gtk_Assistant is a widget used to represent a generally complex
--  operation split into several steps, guiding the user through its pages and
--  controlling the page flow to collect the necessary data.
--
--  </description>
--  <group>Windows</group>
--  <testgtk>create_assistant.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Pixbuf;    use Gdk.Pixbuf;
with Glib;          use Glib;
with Glib.Types;    use Glib.Types;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.Window;    use Gtk.Window;

package Gtk.Assistant is

   type Gtk_Assistant_Record is new Gtk_Window_Record with null record;
   type Gtk_Assistant is access all Gtk_Assistant_Record'Class;

   type Gtk_Assistant_Page_Type is
        (Gtk_Assistant_Page_Content,
         Gtk_Assistant_Page_Intro,
         Gtk_Assistant_Page_Confirm,
         Gtk_Assistant_Page_Summary,
         Gtk_Assistant_Page_Progress);
      --  Definition of various page types.  See Get_Page_Type/Set_Page_Type
      --  for more info.

      type Gtk_Assistant_Page_Func is access function
        (Current_Page : Gint;
         D : System.Address) return Gint;
      pragma Convention (C, Gtk_Assistant_Page_Func);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Assistant : out Gtk_Assistant);
   procedure Initialize (Assistant : access Gtk_Assistant_Record'Class);
   --  Creates a new Gtk.Assistant.Gtk_Assistant.
   --  Since: gtk+ 2.10

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_assistant_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Action_Widget
      (Assistant : access Gtk_Assistant_Record;
       Child     : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds a widget to the action area of a Gtk.Assistant.Gtk_Assistant.
   --  Since: gtk+ 2.10
   --  "child": a Gtk.Widget.Gtk_Widget

   function Append_Page
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class) return Gint;
   --  Appends a page to the Assistant.
   --  Since: gtk+ 2.10
   --  "page": a Gtk.Widget.Gtk_Widget

   procedure Commit (Assistant : access Gtk_Assistant_Record);
   --  Erases the visited page history so the back button is not shown on the
   --  current page, and removes the cancel button from subsequent pages. Use
   --  this when the information provided up to the current page is hereafter
   --  deemed permanent and cannot be modified or undone. For example, showing
   --  a progress page to track a long-running, unreversible operation after
   --  the user has clicked apply on a confirmation page.
   --  Since: gtk+ 2.22

   function Get_Current_Page
      (Assistant : access Gtk_Assistant_Record) return Gint;
   procedure Set_Current_Page
      (Assistant : access Gtk_Assistant_Record;
       Page_Num  : Gint);
   --  Switches the page to Page_Num. Note that this will only be necessary in
   --  custom buttons, as the Assistant flow can be set with
   --  Gtk.Assistant.Set_Forward_Page_Func.
   --  Since: gtk+ 2.10
   --  "page_num": index of the page to switch to, starting from 0. If
   --  negative, the last page will be used. If greater than the number of
   --  pages in the Assistant, nothing will be done.

   function Get_N_Pages
      (Assistant : access Gtk_Assistant_Record) return Gint;
   --  Returns the number of pages in the Assistant
   --  Since: gtk+ 2.10

   function Get_Nth_Page
      (Assistant : access Gtk_Assistant_Record;
       Page_Num  : Gint) return Gtk.Widget.Gtk_Widget;
   --  Returns the child widget contained in page number Page_Num. if Page_Num
   --  is out of bounds.
   --  Since: gtk+ 2.10
   --  "page_num": The index of a page in the Assistant, or -1 to get the last
   --  page;

   function Get_Page_Complete
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean;
   procedure Set_Page_Complete
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class;
       Complete  : Boolean);
   --  Sets whether Page contents are complete. This will make
   --  Since: gtk+ 2.10
   --  "page": a page of Assistant
   --  "complete": the completeness status of the page

   function Get_Page_Header_Image
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gdk.Pixbuf.Gdk_Pixbuf;
   procedure Set_Page_Header_Image
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class;
       Pixbuf    : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Sets a header image for Page. This image is displayed in the header
   --  area of the assistant when Page is the current page.
   --  Since: gtk+ 2.10
   --  "page": a page of Assistant
   --  "pixbuf": the new header image Page

   function Get_Page_Side_Image
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gdk.Pixbuf.Gdk_Pixbuf;
   procedure Set_Page_Side_Image
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class;
       Pixbuf    : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Sets a header image for Page. This image is displayed in the side area
   --  of the assistant when Page is the current page.
   --  Since: gtk+ 2.10
   --  "page": a page of Assistant
   --  "pixbuf": the new header image Page

   function Get_Page_Title
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class)
       return UTF8_String;
   procedure Set_Page_Title
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class;
       Title     : UTF8_String);
   --  Sets a title for Page. The title is displayed in the header area of the
   --  assistant when Page is the current page.
   --  Since: gtk+ 2.10
   --  "page": a page of Assistant
   --  "title": the new title for Page

   function Get_Page_Type
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk_Assistant_Page_Type;
   procedure Set_Page_Type
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class;
       The_Type  : Gtk_Assistant_Page_Type);
   --  Sets the page type for Page. The page type determines the page behavior
   --  in the Assistant.
   --  Since: gtk+ 2.10
   --  "page": a page of Assistant
   --  "type": the new type for Page

   function Insert_Page
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class;
       Position  : Gint) return Gint;
   --  Inserts a page in the Assistant at a given position.
   --  Since: gtk+ 2.10
   --  "page": a Gtk.Widget.Gtk_Widget
   --  "position": the index (starting at 0) at which to insert the page, or
   --  -1 to append the page to the Assistant

   function Prepend_Page
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class) return Gint;
   --  Prepends a page to the Assistant.
   --  Since: gtk+ 2.10
   --  "page": a Gtk.Widget.Gtk_Widget

   procedure Remove_Action_Widget
      (Assistant : access Gtk_Assistant_Record;
       Child     : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes a widget from the action area of a Gtk.Assistant.Gtk_Assistant.
   --  Since: gtk+ 2.10
   --  "child": a Gtk.Widget.Gtk_Widget

   procedure Set_Forward_Page_Func
      (Assistant : access Gtk_Assistant_Record;
       Page_Func : Gtk_Assistant_Page_Func;
       Data      : System.Address;
       Destroy   : Glib.G_Destroy_Notify_Address);
   --  Sets the page forwarding function to be Page_Func, this function will
   --  be used to determine what will be the next page when the user presses
   --  the forward button. Setting Page_Func to null will make the assistant to
   --  use the default forward function, which just goes to the next visible
   --  page.
   --  Since: gtk+ 2.10
   --  "page_func": the Gtk_Assistant_Page_Func, or null to use the default
   --  one
   --  "data": user data for Page_Func
   --  "destroy": destroy notifier for Data

   procedure Update_Buttons_State (Assistant : access Gtk_Assistant_Record);
   --  Forces Assistant to recompute the buttons state. GTK+ automatically
   --  takes care of this in most situations, e.g. when the user goes to a
   --  different page, or when the visibility or completeness of a page
   --  changes. One situation where it can be necessary to call this function
   --  is when changing a value on the current page affects the future page
   --  flow of the assistant.
   --  Since: gtk+ 2.10

   ----------------------
   -- GtkAda additions --
   ----------------------

   generic
   type Data_Type (<>) is private;
   package Generic_Assistant_Functions is
      type Page_Func is access function
        (Current_Page : Gint;
         User_Data    : Data_Type)
      return Gint;
      --  Spec for page forwarding function.

      type Destroy_Notify is access procedure (User_Data : in out Data_Type);
      --  Destroy_Notify is called just prior to the destruction of
      --  User_Data.

      procedure Set_Forward_Page_Func
        (Assistant : Gtk_Assistant;
         Func      : Page_Func;
         User_Data : Data_Type;
         Destroy   : Destroy_Notify := null);
      --  Sets the Assistant's page forwarding function to be Func.  This
      --  function will be used to determine what will be the next page when
      --  the user presses the forward button. Setting Func to null will make
      --  the assistant use the default forward function, which just goes
      --  to the next visible page.
   end Generic_Assistant_Functions;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Assistant_Record, Gtk_Assistant);
   function "+"
     (Widget : access Gtk_Assistant_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Assistant
   renames Implements_Buildable.To_Object;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "apply"
   --     procedure Handler (Self : access Gtk_Assistant_Record'Class);
   --  The ::apply signal is emitted when the apply button is clicked. The
   --  default behavior of the Gtk.Assistant.Gtk_Assistant is to switch to the
   --  page after the current page, unless the current page is the last one. A
   --  handler for the ::apply signal should carry out the actions for which
   --  the wizard has collected data. If the action takes a long time to
   --  complete, you might consider putting a page of type
   --  %GTK_ASSISTANT_PAGE_PROGRESS after the confirmation page and handle this
   --  operation within the Gtk.Assistant.Gtk_Assistant::prepare signal of the
   --  progress page.
   --
   --  "cancel"
   --     procedure Handler (Self : access Gtk_Assistant_Record'Class);
   --  The ::cancel signal is emitted when then the cancel button is clicked.
   --
   --  "close"
   --     procedure Handler (Self : access Gtk_Assistant_Record'Class);
   --  The ::close signal is emitted either when the close button of a summary
   --  page is clicked, or when the apply button in the last page in the flow
   --  (of type %GTK_ASSISTANT_PAGE_CONFIRM) is clicked.
   --
   --  "prepare"
   --     procedure Handler
   --       (Self : access Gtk_Assistant_Record'Class;
   --        Page : Gtk.Widget.Gtk_Widget);
   --    --  "page": the current page
   --  The ::prepare signal is emitted when a new page is set as the
   --  assistant's current page, before making the new page visible. A handler
   --  for this signal can do any preparation which are necessary before
   --  showing Page.

   Signal_Apply : constant Glib.Signal_Name := "apply";
   Signal_Cancel : constant Glib.Signal_Name := "cancel";
   Signal_Close : constant Glib.Signal_Name := "close";
   Signal_Prepare : constant Glib.Signal_Name := "prepare";

end Gtk.Assistant;

-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010-2013, AdaCore               --
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
--  Gtk_Print_Operation is the high-level, portable printing API. It looks a
--  bit different than other GTK+ dialogs such as the Gtk_File_Chooser, since
--  some platforms don't expose enough infrastructure to implement a good print
--  dialog. On such platforms, Gtk_Print_Operation uses the native print
--  dialog. On platforms which do not provide a native print dialog, GTK+ uses
--  its own, see Gtk_Print_Unix_Dialog.
--
--  The typical way to use the high-level printing API is to create a
--  Gtk_Print_Operation object with Gtk_New when the user selects to print.
--  Then you set some properties on it, e.g. the page size, any
--  Gtk_Print_Settings from previous print operations, the number of pages,
--  the current page, etc.
--
--  Then you start the print operation by calling Run. It will then show a
--  dialog, let the user select a printer and options. When the user finished
--  the dialog various signals will be emitted on the Gtk_Print_Operation, the
--  main one being draw-page, which you are supposed to catch and render the
--  page on the provided Gtk_Print_Context using Cairo.
--
--  Note: on UNIX/Linux, Gtk+ is loading at run-time the libraries for printing
--  support. You will need to point the environment variable GTK_EXE_PREFIX
--  to the root directory of your Gtk+ install before calling Run.
--  </description>
--  <c_version>2.16.6</c_version>

with Glib.Error;
with Glib.Object;
with Glib.Properties;
with Gtk.Enums;
with Gtk.Page_Setup;
with Gtk.Print_Settings;
with Gtk.Window;

package Gtk.Print_Operation is

   type Gtk_Print_Operation_Record is
     new Glib.Object.GObject_Record with private;
   type Gtk_Print_Operation is access all Gtk_Print_Operation_Record'Class;

   type Gtk_Print_Status is
     (Status_Initial,
      Status_Preparing,
      Status_Generating_Data,
      Status_Sending_Data,
      Status_Pending,
      Status_Pending_Issue,
      Status_Printing,
      Status_Finished,
      Status_Finished_Aborted);
   pragma Convention (C, Gtk_Print_Status);

   type Gtk_Print_Operation_Result is
     (Result_Error,
      Result_Apply,
      Result_Cancel,
      Result_In_Progress);
   pragma Convention (C, Gtk_Print_Operation_Result);

   type Gtk_Print_Operation_Action is
     (Action_Print_Dialog,
      Action_Print,
      Action_Preview,
      Action_Export);
   pragma Convention (C, Gtk_Print_Operation_Action);

   type Gtk_Print_Error is
     (Error_General,
      Error_Internal_Error,
      Error_Nomem,
      Error_Invalid_File);
   pragma Convention (C, Gtk_Print_Error);

   procedure Gtk_New (Widget : out Gtk_Print_Operation);
   procedure Initialize (Widget : access Gtk_Print_Operation_Record'Class);
   --  Creates a new Gtk_Print_Operation.

   function Get_Type return GType;

   function Error_Quark return GQuark;
   --  Registers an error quark for Gtk_Print_Operation if necessary.

   function Run
     (Op     : access Gtk_Print_Operation_Record;
      Action : Gtk_Print_Operation_Action;
      Parent : access Gtk.Window.Gtk_Window_Record'Class;
      Error  : Glib.Error.GError := null)
      return Gtk_Print_Operation_Result;
   --  Runs the print operation, by first letting the user modify
   --  print settings in the print dialog, and then by printing the
   --  document.
   --
   --  Normally that this function does not return until the rendering of all
   --  pages is complete. You can connect to the status-changed signal on Op
   --  to obtain some information about the progress of the print operation.
   --  Furthermore, it may use a recursive mainloop to show the print dialog.
   --
   --  If you call Set_Allow_Async or set the allow-async property the
   --  operation will run asynchronously if this is supported on the platform.
   --  The done signal will be emitted with the result of the operation when
   --  it is done (i.e. when the dialog is canceled, or when the print succeeds
   --  or fails).
   --
   --     begin
   --        if Settings /= null then
   --           Set_Print_Settings (Print, Settings);
   --        end if;
   --
   --        if Page_Setup /= null then
   --           Set_Default_Page_Setup (Print, Page_Setup);
   --        end if;
   --
   --        Connect (Print, "begin-print", Begin_Print'Access, User_Data);
   --        Connect (Print, "draw-page",   Draw_Page'Access,   User_Data);
   --
   --        Result := Run (Print, Action_Print_Dialog, Parent, Error);
   --
   --        case Result is
   --           when Result_Error =>
   --              Gtk_New
   --                (Error_Dialog,
   --                 Parent,
   --                 Destroy_With_Parent,
   --                 Message_Error,
   --                 Buttons_Close,
   --                 "Error printing file");
   --              Connect
   --                (Error_Dialog, "response", Gtk_Widget_Destroy'Access);
   --              Show (Error_Dialog);
   --              Glib.Error.Error_Free (Error);
   --
   --           when Result_Apply =>
   --              if Settings /= null then
   --                 Unref (Settings);
   --              end if;
   --              Settings := Get_Print_Settings (Print);
   --              Ref (Settings);
   --
   --           when Result_Cancel =>      null;
   --           when Result_In_Progress => null;
   --        end case;
   --
   --  Note that Run can only be called once on a given Gtk_Print_Operation.
   --
   --  Return value: the result of the print operation. A return value of
   --  Result_Apply indicates that the printing was completed successfully.
   --  In this case, it is a good idea to obtain the used print settings with
   --  Get_Print_Settings and store them for reuse with the next print
   --  operation. A value of Result_In_Progress means the operation is running
   --  asynchronously, and will emit the done signal when done.

   procedure Cancel (Op : access Gtk_Print_Operation_Record);
   --  Cancels a running print operation. This function may be called from
   --  a begin-print, paginate or draw-page signal handler to stop the
   --  currently running print operation.

   procedure Draw_Page_Finish (Op : access Gtk_Print_Operation_Record);
   --  Signal that drawing of particular page is complete.
   --
   --  It is called after completion of page drawing (e.g. drawing in another
   --  thread).  If Set_Defer_Drawing was called before, then this function has
   --  to be called by application. In another case it is called by the library
   --  itself.

   procedure Get_Error
     (Op    : access Gtk_Print_Operation_Record;
      Error : Glib.Error.GError);
   --  Call this when the result of a print operation is
   --  Result_Error, either as returned by Run, or in the done signal handler.
   --  The returned GError will contain more details on what went wrong.

   function Get_Print_Settings
     (Op : access Gtk_Print_Operation_Record)
      return Gtk.Print_Settings.Gtk_Print_Settings;
   procedure Set_Print_Settings
     (Op             : access Gtk_Print_Operation_Record;
      Print_Settings : access
                       Gtk.Print_Settings.Gtk_Print_Settings_Record'Class);
   --  Manipulate the print settings for Op. This is typically used to
   --  re-establish print settings from a previous print operation, see Run.
   --
   --  Note that the Get_Print_Setting's return value is null until either
   --  Set_Print_Settings or Run have been called.

   function Get_Status
     (Op : access Gtk_Print_Operation_Record) return Gtk_Print_Status;
   function Get_Status (Op : access Gtk_Print_Operation_Record) return String;
   --  Returns the status of the print operation.
   --
   --  A Gtk_Print_Status is suitable for programmatic use.
   --
   --  The String returned is a translated string representation of the
   --  status of the print operation.  It is suitable for displaying the
   --  print status in, e.g., a Gtk_Statusbar.

   function Is_Finished
     (Op : access Gtk_Print_Operation_Record) return Boolean;
   --  A convenience function to find out whether the print operation
   --  is finished, either successfully (Status_Finished) or unsuccessfully
   --  (Status_Finished_Aborted).
   --
   --  Note: when you enable print status tracking the print operation
   --  can be in a non-finished state even after done has been called, as
   --  the operation status then tracks the print job status on the printer.

   procedure Set_Allow_Async
     (Op          : access Gtk_Print_Operation_Record;
      Allow_Async : Boolean);
   --  Sets whether Run may return before the print operation is completed.
   --  Note that some platforms may not allow asynchronous operation.

   procedure Set_Current_Page
     (Op           : access Gtk_Print_Operation_Record;
      Current_Page : Gint);
   --  Sets the current page.
   --
   --  If this is called before Run, the user will be able to select to print
   --  only the current page.
   --
   --  Note that this only makes sense for pre-paginated documents.

   procedure Set_Custom_Tab_Label
     (Op    : access Gtk_Print_Operation_Record;
      Label : String);
   --  Sets the label for the tab holding custom widgets.

   procedure Set_Defer_Drawing (Op : access Gtk_Print_Operation_Record);
   --  Sets up the Gtk_Print_Operation to wait for calling of Draw_Page_Finish
   --  from application. It can be used for drawing page in another thread.
   --
   --  This function must be called in the callback of "draw-page" signal.

   procedure Set_Export_Filename
     (Op       : access Gtk_Print_Operation_Record;
      Filename : String);
   --  Sets up the Gtk_Print_Operation to generate a file instead
   --  of showing the print dialog. The indended use of this function
   --  is for implementing "Export to PDF" actions. Currently, PDF
   --  is the only supported format.
   --
   --  "Print to PDF" support is independent of this and is done
   --  by letting the user pick the "Print to PDF" item from the list
   --  of printers in the print dialog.

   procedure Set_Job_Name
     (Op       : access Gtk_Print_Operation_Record;
      Job_Name : String);
   --  Sets the name of the print job. The name is used to identify
   --  the job (e.g. in monitoring applications like eggcups).
   --
   --  If you don't set a job name, GTK+ picks a default one by
   --  numbering successive print jobs.

   procedure Set_N_Pages
     (Op      : access Gtk_Print_Operation_Record;
      N_Pages : Gint);
   --  Sets the number of pages in the document.
   --
   --  This MUST be set to a positive number before the rendering starts.
   --  It may be set in a begin-print signal hander.
   --
   --  Note that the page numbers passed to the request-page-setup and
   --  draw-page signals are 0-based, i.e. if the user chooses to print all
   --  pages, the last draw-page signal will be for page N_Pages - 1.

   procedure Set_Show_Progress
     (Op            : access Gtk_Print_Operation_Record;
      Show_Progress : Boolean);
   --  If True, the print operation will show a progress dialog during the
   --  print operation.

   procedure Set_Track_Print_Status
     (Op           : access Gtk_Print_Operation_Record;
      Track_Status : Boolean);
   --  If True, the print operation will try to continue report on the status
   --  of the print job in the printer queues and printer. This can allow your
   --  application to show things like "out of paper" issues, and when the
   --  print job actually reaches the printer.
   --
   --  This function is often implemented using some form of polling, so it
   --  should not be enabled unless needed.

   procedure Set_Unit
     (Op   : access Gtk_Print_Operation_Record;
      Unit : Gtk.Enums.Gtk_Unit);
   --  Sets up the transformation for the cairo context obtained from
   --  Gtk_Print_Context in such a way that distances are measured in
   --  units of Unit.

   procedure Set_Use_Full_Page
     (Op        : access Gtk_Print_Operation_Record;
      Full_Page : Boolean);
   --  If True, the transformation for the cairo context obtained from
   --  Gtk_Print_Context puts the origin at the top left corner of the page
   --  (which may not be the top left corner of the sheet, depending on page
   --  orientation and the number of pages per sheet). Otherwise, the origin
   --  is at the top left corner of the imageable area (i.e. inside the
   --  margins).

   ----------------
   -- Page Setup --
   ----------------

   function Get_Default_Page_Setup
     (Op : access Gtk_Print_Operation_Record)
      return Gtk.Page_Setup.Gtk_Page_Setup;
   procedure Set_Default_Page_Setup
     (Op                 : access Gtk_Print_Operation_Record;
      Default_Page_Setup : access Gtk.Page_Setup.Gtk_Page_Setup_Record'Class);
   --  Makes Default_Page_Setup the default page setup for Op.
   --  Default_Page_Setup may be null.
   --
   --  This page setup will be used by Run, but it can be overridden on a
   --  per-page basis by connecting to the request-page-setup signal.

   function Run_Page_Setup_Dialog
     (Parent     : access Gtk.Window.Gtk_Window_Record'Class;
      Page_Setup : access Gtk.Page_Setup.Gtk_Page_Setup_Record'Class;
      Settings   : access Gtk.Print_Settings.Gtk_Print_Settings_Record'Class)
      return Gtk.Page_Setup.Gtk_Page_Setup;
   --  Runs a page setup dialog, letting the user modify the values from
   --  Page_Setup. If the user cancels the dialog, the returned Gtk_Page_Setup
   --  is identical to the passed in Page_Setup, otherwise it contains the
   --  modifications done in the dialog.
   --
   --  Note that this function may use a recursive mainloop to show the page
   --  setup dialog.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  Name:  Allow_Async_Property
   --  Type:  Boolean
   --  Descr: True if print process may run asynchronous.
   --
   --  Name:  Current_Page_Property
   --  Type:  Int
   --  Descr: The current page in the document
   --
   --  Name:  Custom_Tab_Label_Property
   --  Type:  String
   --  Descr: Label for the tab containing custom widgets.
   --
   --  Name:  Default_Page_Setup_Property
   --  Type:  Object
   --  Descr: The Gtk_Page_Setup used by default
   --
   --  Name:  Export_Filename_Property
   --  Type:  String
   --  Descr: Export filename
   --
   --  Name:  Job_Name_Property
   --  Type:  String
   --  Descr: A string used for identifying the print job.
   --
   --  Name:  N_Pages_Property
   --  Type:  Int
   --  Descr: The number of pages in the document.
   --
   --  Name:  Print_Settings_Property
   --  Type:  Object
   --  Descr: The Gtk_Print_Settings used for initializing the dialog
   --
   --  Name:  Show_Progress_Property
   --  Type:  Boolean
   --  Descr: True if a progress dialog is shown while printing.
   --
   --  Name:  Status_Property
   --  Type:  Enum
   --  Descr: The status of the print operation
   --
   --  Name:  Status_String_Property
   --  Type:  String
   --  Descr: A human-readable description of the status
   --
   --  Name:  Track_Print_Status_Property
   --  Type:  Boolean
   --  Descr: True if the print operation will continue to report on the
   --         print job status after the print data has been sent to the
   --         printer or print server.
   --
   --  Name:  Unit_Property
   --  Type:  Enum
   --  Descr: The unit in which distances can be measured in the context
   --
   --  Name:  Use_Full_Page_Property
   --  Type:  Boolean
   --  Descr: True if the origin of the context should be at the corner of
   --         the page and not the corner of the imageable area
   --
   --  </properties>

   Allow_Async_Property        : constant Glib.Properties.Property_Boolean;
   Current_Page_Property       : constant Glib.Properties.Property_Int;
   Custom_Tab_Label_Property   : constant Glib.Properties.Property_String;
   Default_Page_Setup_Property : constant Glib.Properties.Property_Object;
   Export_Filename_Property    : constant Glib.Properties.Property_String;
   Job_Name_Property           : constant Glib.Properties.Property_String;
   N_Pages_Property            : constant Glib.Properties.Property_Int;
   Print_Settings_Property     : constant Glib.Properties.Property_Object;
   Show_Progress_Property      : constant Glib.Properties.Property_Boolean;
   Status_Property             : constant Glib.Properties.Property_Enum;
   Status_String_Property      : constant Glib.Properties.Property_String;
   Track_Print_Status_Property : constant Glib.Properties.Property_Boolean;
   Unit_Property               : constant Glib.Properties.Property_Enum;
   Use_Full_Page_Property      : constant Glib.Properties.Property_Boolean;

private

   type Gtk_Print_Operation_Record is
     new Glib.Object.GObject_Record with null record;

   Allow_Async_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("allow-async");
   Current_Page_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("current-page");
   Custom_Tab_Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("custom-tab-label");
   Default_Page_Setup_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("default-page-setup");
   Export_Filename_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("export-filename");
   Job_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("job-name");
   N_Pages_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("n-pages");
   Print_Settings_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("print-settings");
   Show_Progress_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-progress");
   Status_Property : constant Glib.Properties.Property_Enum :=
     Glib.Properties.Build ("status");
   Status_String_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("status-string");
   Track_Print_Status_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("track-print-status");
   Unit_Property : constant Glib.Properties.Property_Enum :=
     Glib.Properties.Build ("unit");
   Use_Full_Page_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-full-page");

   pragma Import (C, Get_Type, "gtk_print_operation_get_type");
   pragma Import (C, Error_Quark, "gtk_print_error_quark");

end Gtk.Print_Operation;

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
--  This package provides a ready-to-use high level printing object.
--  Use functionality from Gtk.Print_Operation to manipulate the
--  printing object, and the functionality in this package to provide
--  the handlers for the printing operation.
--
--  Typically, to use this high-level printing API:
--    - derive from the Gtkada_Print_Operation_Record object
--    - override the Draw_Page operation
--    - (optional) override any other operation useful to you
--    - start the print operation by
--          - first setting the number of pages through Set_N_Pages
--          - then calling Connect_And_Run.
--  A dialog will be displayed, letting the user select a printer and options.
--  When the user finishes the dialog, various signals will be emitted on the
--  Gtkada_Print_Operation, which will call the operations on your object.
--
--  Note: on UNIX/Linux, Gtk+ is loading at run-time the libraries for printing
--  support. You will need to point the environment variable GTK_EXE_PREFIX
--  to the root directory of your Gtk+ install before calling Connect_And_Run.
--  </description>
--  <example>
--
--     --  Create a derived type
--
--     type Print_Op_Record is new Gtkada_Print_Operation_Record
--     with record
--        My_Data : My_Type;
--     end record;
--     type Print_Op_Access is access all Print_Op_Record'Class;
--
--     --  Override Draw_Page
--
--     procedure Draw_Page
--       (Print_Operation : access Print_Op_Record;
--        Context         : Gtk_Print_Context;
--        Page_Num        : Gint)
--     is
--        Cr : Cairo_Context;
--     begin
--        Cr := Get_Cairo_Context (Context);
--
--        --  Do drawing here
--
--     end Draw_Page;
--
--
--     --  Do the printing
--     declare
--        Print_Op : Print_Op_Access;
--        Result   : Gtk_Print_Operation_Result;
--     begin
--        --  Initialize
--        Print_Op := new Print_Op_Record;
--        Print_Op.My_Data := Some_Data;
--
--        --  Set the number of pages to print
--        Set_N_Pages (Print_Op, 2);
--
--        --  Launch the printing
--        Result := Connect_And_Run
--          (Print_Op, Action_Print_Dialog, Gtk_Window (Get_Toplevel (Frame)));
--     end;
--
--  </example>
--  <group>Miscellaneous</group>

with Glib; use Glib;
with Glib.Error;

with Gtk.Page_Setup;              use Gtk.Page_Setup;
with Gtk.Print_Context;           use Gtk.Print_Context;
with Gtk.Print_Operation;         use Gtk.Print_Operation;
with Gtk.Print_Operation_Preview; use Gtk.Print_Operation_Preview;
with Gtk.Window;                  use Gtk.Window;

package Gtkada.Printing is

   type Gtkada_Print_Operation_Record is new
     Gtk_Print_Operation_Record with private;
   type Gtkada_Print_Operation is access all Gtkada_Print_Operation_Record;

   procedure Gtk_New (Op : out Gtkada_Print_Operation);
   procedure Initialize (Widget : access Gtkada_Print_Operation_Record'Class);
   --  Initialize the print operation

   function Connect_And_Run
     (Op        : access Gtkada_Print_Operation_Record'Class;
      Action    : Gtk_Print_Operation_Action;
      Parent    : access Gtk_Window_Record'Class;
      Error     : Glib.Error.GError := null)
      return Gtk_Print_Operation_Result;
   --  Runs the print operation, using the handlers installed in Op.
   --  See Gtk.Print_Operations.Run.

   -------------------------
   -- Printing operations --
   -------------------------

   --  The following primitive operations are called by the printing procedure.
   --  By default, these operations do nothing.
   --  It is mandatory to override Draw_Page, the other callbacks are optional.

   procedure Draw_Page
     (Op          : access Gtkada_Print_Operation_Record;
      Context     : Gtk_Print_Context;
      Page_Number : Gint);
   --  Called for every page that is printed. This handler must render the
   --  page Page_Number onto the cairo context obtained from Context using
   --  Gtk.Print_Context.Get_Cairo_Context.
   --
   --  Use Gtk.Print_Operation.Set_Use_Full_Page and
   --  Gtk.Print_Operation.Set_Unit before starting the print operation to set
   --  up the transformation of the cairo context according to your needs.
   --
   --  This is the main printing handler. This has to be overriden for the
   --  printing operation to work.

   procedure Begin_Print
     (Op      : access Gtkada_Print_Operation_Record;
      Context : Gtk_Print_Context);
   --  Called after the user has finished changing print settings in the
   --  dialog, before the actual rendering starts.
   --
   --  A typical use is to use the parameters from the
   --  Gtk_Print_Context and paginate the document accordingly, and then
   --  set the number of pages with Gtk.Print_Operation.Set_N_Pages.

   procedure Done
     (Op     : access Gtkada_Print_Operation_Record;
      Result : Gtk_Print_Operation_Result);
   --  Called when the print operation run has finished doing everything
   --  required for printing.
   --
   --  Result gives you information about what happened during the run.
   --  If Result is Result_Error then you can call Get_Error for more
   --  information.
   --
   --  If you enabled print status tracking then
   --  Gtk.Print_Operation.Is_Finished may still return False after
   --  done was emitted.

   procedure End_Print
     (Op      : access Gtkada_Print_Operation_Record;
      Context : Gtk_Print_Context);
   --  Called after all pages have been rendered.
   --
   --  This handler can clean up any resources that have been allocated
   --  in Begin_Print.

   function Paginate
     (Op      : access Gtkada_Print_Operation_Record;
      Context : Gtk_Print_Context) return Boolean;
   --  Called after the "begin-print" signal, but before the actual rendering
   --  starts. It keeps getting called until it returns True.
   --
   --  The "paginate" signal is intended to be used for paginating a document
   --  in small chunks, to avoid blocking the user interface for a long
   --  time. This function should update the number of pages using
   --  Gtk.Print_Operation.Set_N_Pages, and return True if the document
   --  has been completely paginated.
   --
   --  If you don't need to do pagination in chunks, you can simply do
   --  it all in the "begin-print" handler, and set the number of pages
   --  from there.

   function Preview
     (Op          : access Gtkada_Print_Operation_Record;
      Preview     : Gtk_Print_Operation_Preview;
      Context     : Gtk_Print_Context;
      Parent      : Gtk_Window)
     return Boolean;
   --  Called when a preview is requested from the native dialog.
   --  This should return True if the in order to take over control of the
   --  preview.
   --
   --  The default handler for this signal uses an external viewer
   --  application to preview.
   --
   --  To implement a custom print preview, the overridden implementation
   --  should return True
   --  In order to use the provided Context for the preview implementation, it
   --  must be given a suitable cairo context with Set_Cairo_Context.
   --
   --  The custom preview implementation can use
   --  Gtk.Print_Operation_Preview.Is_Selected and
   --  Gtk.Print_Operation_Preview.Render_Page to find pages which
   --  are selected for print and render them. The preview must be
   --  finished by calling Gtk.Print_Operation_Preview.End_Preview
   --  (typically in response to the user clicking a close button).

   procedure Request_Page_Setup
     (Op          : access Gtkada_Print_Operation_Record;
      Context     : Gtk_Print_Context;
      Page_Number : Gint;
      Setup       : Gtk_Page_Setup);
   --  Called once for every page that is printed, to give the application
   --  a chance to modify the page setup. Any changes done to setup will be
   --  in force only for printing this page.

   procedure Status_Changed (Op : access Gtkada_Print_Operation_Record);
   --  Called between the various phases of the print operation.
   --  See Gtk_Print_Status for the phases that are being discriminated.
   --  Use Gtk.Print_Operation.Get_Status to find out the current
   --  status.

private

   type Gtkada_Print_Operation_Record is new Gtk_Print_Operation_Record with
     null record;

end Gtkada.Printing;

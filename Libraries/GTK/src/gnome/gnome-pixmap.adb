-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2006                          --
--                           AdaCore                                 --
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

with System;

with Gtk;          use Gtk;

package body Gnome.Pixmap is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget   : out Gnome_Pixmap;
      Xpm_Data : Chars_Ptr_Array;
      Width    : Integer;
      Height   : Integer) is
   begin
      Widget := new Gnome_Pixmap_Record;
      Initialize (Widget, Xpm_Data, Width, Height);
   end Gnome_New;

   procedure Gnome_New
     (Widget  : out Gnome_Pixmap;
      Gpixmap : access Gnome_Pixmap_Record) is
   begin
      Widget := new Gnome_Pixmap_Record;
      Initialize (Widget, Gpixmap);
   end Gnome_New;

   procedure Gnome_New
     (Widget   : out Gnome_Pixmap;
      Xpm_Data : Chars_Ptr_Array) is
   begin
      Widget := new Gnome_Pixmap_Record;
      Initialize (Widget, Xpm_Data);
   end Gnome_New;

   procedure Gnome_New
     (Widget   : out Gnome_Pixmap;
      Filename : String) is
   begin
      Widget := new Gnome_Pixmap_Record;
      Initialize (Widget, Filename);
   end Gnome_New;

   procedure Gnome_New
     (Widget     : out Gnome_Pixmap;
      Filename : String;
      Width    : Integer;
      Height   : Integer) is
   begin
      Widget := new Gnome_Pixmap_Record;
      Initialize (Widget, Filename, Width, Height);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget   : access Gnome_Pixmap_Record'Class;
      Xpm_Data : Chars_Ptr_Array;
      Width    : Integer;
      Height   : Integer)
   is
      function Internal
        (Xpm_Data : Chars_Ptr_Array;
         Width    : Integer;
         Height   : Integer)
         return System.Address;
      pragma Import (C, Internal, "gnome_pixmap_new_from_xpm_d_at_size");
   begin
      Set_Object (Widget, Internal (Xpm_Data + Null_Ptr, Width, Height));
   end Initialize;

   procedure Initialize
     (Widget  : access Gnome_Pixmap_Record'Class;
      Gpixmap : access Gnome_Pixmap_Record)
   is
      function Internal (Gpixmap : System.Address) return System.Address;
      pragma Import (C, Internal, "gnome_pixmap_new_from_gnome_pixmap");
   begin
      Set_Object (Widget, Internal (Get_Object (Gpixmap)));
   end Initialize;

   procedure Initialize
     (Widget   : access Gnome_Pixmap_Record'Class;
      Xpm_Data : Chars_Ptr_Array)
   is
      function Internal (Xpm_Data : Chars_Ptr_Array) return System.Address;
      pragma Import (C, Internal, "gnome_pixmap_new_from_xpm_d");
   begin
      Set_Object (Widget, Internal (Xpm_Data + Null_Ptr));
   end Initialize;

   procedure Initialize
     (Widget   : access Gnome_Pixmap_Record'Class;
      Filename : String)
   is
      function Internal (Filename : String) return System.Address;
      pragma Import (C, Internal, "gnome_pixmap_new_from_file");
   begin
      Set_Object (Widget, Internal (Filename & ASCII.NUL));
   end Initialize;

   procedure Initialize
     (Widget   : access Gnome_Pixmap_Record'Class;
      Filename : String;
      Width    : Integer;
      Height   : Integer)
   is
      function Internal
        (Filename : String;
         Width    : Integer;
         Height   : Integer)
         return System.Address;
      pragma Import (C, Internal, "gnome_pixmap_new_from_file_at_size");

   begin
      Set_Object (Widget, Internal (Filename & ASCII.NUL,
                                    Width,
                                    Height));
   end Initialize;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Gpixmap  : access Gnome_Pixmap_Record;
      Filename : String)
   is
      procedure Internal
        (Gpixmap  : System.Address;
         Filename : String);
      pragma Import (C, Internal, "gnome_pixmap_load_file");
   begin
      Internal (Get_Object (Gpixmap),
                Filename & ASCII.NUL);
   end Load_File;

   -----------------------
   -- Load_File_At_Size --
   -----------------------

   procedure Load_File_At_Size
     (Gpixmap  : access Gnome_Pixmap_Record;
      Filename : String;
      Width    : Integer;
      Height   : Integer)
   is
      procedure Internal
        (Gpixmap  : System.Address;
         Filename : String;
         Width    : Integer;
         Height   : Integer);
      pragma Import (C, Internal, "gnome_pixmap_load_file_at_size");
   begin
      Internal (Get_Object (Gpixmap),
                Filename & ASCII.NUL,
                Width,
                Height);
   end Load_File_At_Size;

   ----------------
   -- Load_Xpm_D --
   ----------------

   procedure Load_Xpm_D
     (Gpixmap  : access Gnome_Pixmap_Record;
      Xpm_Data : Chars_Ptr_Array)
   is
      procedure Internal
        (Gpixmap  : System.Address;
         Xpm_Data : Chars_Ptr_Array);
      pragma Import (C, Internal, "gnome_pixmap_load_xpm_d");
   begin
      Internal (Get_Object (Gpixmap), Xpm_Data);
   end Load_Xpm_D;

   ------------------------
   -- Load_Xpm_D_At_Size --
   ------------------------

   procedure Load_Xpm_D_At_Size
     (Gpixmap  : access Gnome_Pixmap_Record;
      Xpm_Data : Chars_Ptr_Array;
      Width    : Integer;
      Height   : Integer)
   is
      procedure Internal
        (Gpixmap  : System.Address;
         Xpm_Data : Chars_Ptr_Array;
         Width    : Integer;
         Height   : Integer);
      pragma Import (C, Internal, "gnome_pixmap_load_xpm_d_at_size");
   begin
      Internal (Get_Object (Gpixmap), Xpm_Data, Width, Height);
   end Load_Xpm_D_At_Size;

end Gnome.Pixmap;

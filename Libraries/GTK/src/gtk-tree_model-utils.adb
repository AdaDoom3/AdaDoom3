-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2008-2013, AdaCore              --
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

package body Gtk.Tree_Model.Utils is

   ----------------------
   -- Create_Tree_Iter --
   ----------------------

   function Init_Tree_Iter
     (Stamp       : Glib.Gint;
      User_Data_1 : System.Address := System.Null_Address;
      User_Data_2 : System.Address := System.Null_Address;
      User_Data_3 : System.Address := System.Null_Address)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
   begin
      return (Stamp, User_Data_1, User_Data_2, User_Data_3);
   end Init_Tree_Iter;

   ---------------
   -- Get_Stamp --
   ---------------

   function Get_Stamp (Self : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gint is
   begin
      return Self.Stamp;
   end Get_Stamp;

   ---------------------
   -- Get_User_Data_1 --
   ---------------------

   function Get_User_Data_1
     (Self : Gtk.Tree_Model.Gtk_Tree_Iter) return System.Address
   is
   begin
      return Self.User_Data;
   end Get_User_Data_1;

   ---------------------
   -- Get_User_Data_2 --
   ---------------------

   function Get_User_Data_2
     (Self : Gtk.Tree_Model.Gtk_Tree_Iter) return System.Address
   is
   begin
      return Self.User_Data2;
   end Get_User_Data_2;

   ---------------------
   -- Get_User_Data_3 --
   ---------------------

   function Get_User_Data_3
     (Self : Gtk.Tree_Model.Gtk_Tree_Iter) return System.Address
   is
   begin
      return Self.User_Data3;
   end Get_User_Data_3;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean is
   begin
      return Self.Stamp = 0;
   end Is_Null;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid
     (Self  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Stamp : Glib.Gint) return Boolean
   is
   begin
      return Self.Stamp = 0 or else Self.Stamp = Stamp;
   end Is_Valid;

end Gtk.Tree_Model.Utils;

-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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

package body Glib.Glist is

   use type System.Address;

   package body Generic_List is
      -----------
      -- Alloc --
      -----------

      procedure Alloc (List : out Glist) is
         function Internal return System.Address;
         pragma Import (C, Internal, "g_list_alloc");
      begin
         Set_Object (List, Internal);
      end Alloc;

      ------------
      -- Append --
      ------------

      procedure Append
        (List : in out Glist;
         Data : in Gpointer)
      is
         function Internal (List : System.Address;
                            Data : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_list_append");
      begin
         Set_Object (List, Internal (Get_Object (List),
                                     Convert (Data)));
      end Append;

      ------------
      -- Concat --
      ------------

      function Concat
        (List1 : in Glist;
         List2 : in Glist)
         return Glist
      is
         function Internal (List1 : System.Address;
                            List2 : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_list_append");
         Tmp : Glist;
      begin
         Set_Object (Tmp, Internal (Get_Object (List1),
                                    Get_Object (List2)));
         return Tmp;
      end Concat;

      ------------
      -- Insert --
      ------------

      procedure Insert
        (List     : in out Glist;
         Data     : in     Gpointer;
         Position : in     Gint)
      is
         function Internal (List : System.Address;
                            Data : System.Address;
                            Pos  : Gint)
                            return System.Address;
         pragma Import (C, Internal, "g_list_insert");
      begin
         Set_Object (List, Internal (Get_Object (List),
                                     Convert (Data), Position));
      end Insert;

      ----------
      -- Find --
      ----------

      function Find
        (List : in Glist;
         Data : in Gpointer) return Glist
      is
         function Internal
           (List : System.Address;
            Data : System.Address) return System.Address;
         pragma Import (C, Internal, "g_list_find");
         Tmp : Glist;

      begin
         Set_Object (Tmp, Internal (Get_Object (List), Convert (Data)));
         return Tmp;
      end Find;

      -----------
      -- First --
      -----------

      function First (List : in Glist) return Glist is
         function Internal (List : System.Address) return System.Address;
         pragma Import (C, Internal, "g_list_first");
         Tmp : Glist;

      begin
         Set_Object (Tmp, Internal (Get_Object (List)));
         return Tmp;
      end First;

      ----------
      -- Free --
      ----------

      procedure Free (List : in out Glist) is
         procedure Internal (List : System.Address);
         pragma Import (C, Internal, "g_list_free");
      begin
         Internal (Get_Object (List));
         List := Null_List;
      end Free;

      --------------
      -- Get_Data --
      --------------

      function Get_Data (List : in Glist) return Gpointer is
         function Internal (List : in System.Address)
                            return System.Address;
         pragma Import (C, Internal, "ada_list_get_data");
      begin
         return Convert (Internal (Get_Object (List)));
      end Get_Data;

      --------------
      -- Get_Data --
      --------------

      function Get_Data_Address (List : in Glist) return System.Address is
         function Internal (List : in System.Address)
                            return System.Address;
         pragma Import (C, Internal, "ada_list_get_data");
      begin
         return Internal (Get_Object (List));
      end Get_Data_Address;

      --------------------
      --  Get_Gpointer  --
      --------------------

      function Get_Gpointer (List : in Glist) return Gpointer is
      begin
         return Convert (Get_Object (List));
      end Get_Gpointer;

      ----------------
      -- Get_Object --
      ----------------

      function Get_Object (Obj : in Glist) return System.Address is
      begin
         return Obj.Ptr;
      end Get_Object;

      -----------
      -- Index --
      -----------

      function Index (List : in Glist; Data : in Gpointer) return Gint is
         function Internal
           (List : System.Address;
            Data : System.Address) return Gint;
         pragma Import (C, Internal, "g_list_index");

      begin
         return Internal (Get_Object (List), Convert (Data));
      end Index;

      ------------------
      --  Is_Created  --
      ------------------

      function Is_Created (List : in Glist) return Boolean is
      begin
         return Get_Object (List) /= System.Null_Address;
      end Is_Created;

      ----------
      -- Last --
      ----------

      function Last (List : in Glist) return Glist is
         function Internal (List : System.Address) return System.Address;
         pragma Import (C, Internal, "g_list_last");
         Tmp : Glist;
      begin
         Set_Object (Tmp, Internal (Get_Object (List)));
         return Tmp;
      end Last;

      ------------
      -- Length --
      ------------

      function Length (List : in Glist) return Guint is
         function Internal (List : System.Address) return Guint;
         pragma Import (C, Internal, "g_list_length");
      begin
         return Internal (Get_Object (List));
      end Length;

      ------------------
      -- List_Reverse --
      ------------------

      procedure List_Reverse (List : in out Glist) is
         function Internal (List : System.Address) return System.Address;
         pragma Import (C, Internal, "g_list_reverse");
      begin
         Set_Object (List, Internal (Get_Object (List)));
      end List_Reverse;

      ----------
      -- Next --
      ----------

      function Next (List : in Glist) return Glist is
         function Internal (List : System.Address) return System.Address;
         pragma Import (C, Internal, "ada_list_next");
         Tmp : Glist;
      begin
         Set_Object (Tmp, Internal (Get_Object (List)));
         return Tmp;
      end Next;

      ---------
      -- Nth --
      ---------

      function Nth (List : in Glist; N    : in Guint) return Glist is
         function Internal
           (List : System.Address; N : Guint) return System.Address;
         pragma Import (C, Internal, "g_list_nth");
         Tmp : Glist;

      begin
         Set_Object (Tmp, Internal (Get_Object (List), N));
         return Tmp;
      end Nth;

      --------------
      -- Nth_Data --
      --------------

      function Nth_Data
        (List : in Glist;
         N    : in Guint) return Gpointer
      is
         function Internal
           (List : System.Address; N : Guint) return System.Address;
         pragma Import (C, Internal, "g_list_nth_data");

      begin
         return Convert (Internal (Get_Object (List), N));
      end Nth_Data;

      --------------
      -- Position --
      --------------

      function Position
        (List : in Glist;
         Link : in Glist) return Gint
      is
         function Internal (List : System.Address;
                            Link : System.Address) return Gint;
         pragma Import (C, Internal, "g_list_position");
      begin
         return Internal (Get_Object (List), Get_Object (Link));
      end Position;

      -------------
      -- Prepend --
      -------------

      procedure Prepend
        (List : in out Glist;
         Data : in Gpointer)
      is
         function Internal (List : System.Address;
                            Data : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_list_prepend");
      begin
         Set_Object (List, Internal (Get_Object (List),
                                     Convert (Data)));
      end Prepend;

      ----------
      -- Prev --
      ----------

      function Prev (List : in Glist) return Glist is
         function Internal (List : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "ada_list_prev");
         Tmp : Glist;
      begin
         Set_Object (Tmp, Internal (Get_Object (List)));
         return Tmp;
      end Prev;

      ------------
      -- Remove --
      ------------

      procedure Remove
        (List : in out Glist;
         Data : in Gpointer)
      is
         function Internal (List : System.Address;
                            Data : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_list_remove");
      begin
         Set_Object (List, Internal (Get_Object (List),
                                     Convert (Data)));
      end Remove;

      -----------------
      -- Remove_Link --
      -----------------

      procedure Remove_Link
        (List : in out Glist;
         Link : in Glist)
      is
         function Internal (List : System.Address;
                            Link : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_list_remove_link");
      begin
         Set_Object (List, Internal (Get_Object (List),
                                     Get_Object (Link)));
      end Remove_Link;

      ----------------
      -- Set_Object --
      ----------------

      procedure Set_Object (Obj   : in out Glist;
                            Value : in     System.Address) is
      begin
         Obj.Ptr := Value;
      end Set_Object;

   end Generic_List;
end Glib.Glist;

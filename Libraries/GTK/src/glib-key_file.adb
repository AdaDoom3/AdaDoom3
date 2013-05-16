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

with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Gtkada.Bindings;      use Gtkada.Bindings;

package body Glib.Key_File is

   function Convert_Then_Free (Item : chars_ptr) return String;
   --  Converts the given C string to an Ada string, and then frees the
   --  C string before returning.

   function Convert_Then_Free
     (Item : chars_ptr_array_access) return GNAT.Strings.String_List;
   --  Converts the given array to a string list, and then frees the
   --  array before returning.

   -----------------------
   -- Convert_Then_Free --
   -----------------------

   function Convert_Then_Free (Item : chars_ptr) return String is
      I : chars_ptr := Item;
   begin
      if Item = Null_Ptr then
         return "";
      else
         declare
            S : constant String := Value (Item);
         begin
            Free (I);
            return S;
         end;
      end if;
   end Convert_Then_Free;

   -----------------------
   -- Convert_Then_Free --
   -----------------------

   function Convert_Then_Free
     (Item : chars_ptr_array_access) return GNAT.Strings.String_List
   is
      I : chars_ptr_array_access := Item;
      No_List : constant GNAT.Strings.String_List (1 .. 0) := (others => null);
   begin
      if Item = null then
         return No_List;
      else
         declare
            G : constant GNAT.Strings.String_List := To_String_List (Item.all);
         begin
            g_strfreev (I);
            return G;
         end;
      end if;
   end Convert_Then_Free;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return Boolean
   is
      function Internal
        (Key_File   : G_Key_File;
         Group_Name : String;
         Key        : String;
         Error      : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "g_key_file_get_boolean");
   begin
      return Boolean'Val (Internal
        (Key_File, Group_Name & ASCII.NUL, Key & ASCII.NUL, Error));
   end Get_Boolean;

   ----------------------
   -- Get_Boolean_List --
   ----------------------

   function Get_Boolean_List
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return Boolean_List
   is
      type Gboolean_Array is array (size_t) of Gboolean;
      pragma Convention (C, Gboolean_Array);
      type Gboolean_Array_Access is access all Gboolean_Array;

      procedure g_free (S : Gboolean_Array_Access);
      pragma Import (C, g_free, "g_free");

      function Internal
        (Key_File   : G_Key_File;
         Group_Name : String;
         Key        : String;
         Length     : access Gsize;
         Error      : Glib.Error.GError)
         return Gboolean_Array_Access;
      pragma Import (C, Internal, "g_key_file_get_boolean_list");

      Empty_Array : constant Boolean_List (1 .. 0) := (others => False);
      Len : aliased Gsize;
      L : Gboolean_Array_Access;
   begin
      L := Internal
        (Key_File,
         Group_Name & ASCII.NUL,
         Key & ASCII.NUL,
         Len'Access,
         Error);

      --  If we get back a null, simply return an array with no items.
      if L = null then
         return Empty_Array;
      end if;

      --  Otherwise, declare, populate, and return a constained array
      --  after freeing the returned array of Gboolean's.
      declare
         List : Boolean_List (0 .. Integer (Len - 1));
      begin
         for I in 0 .. Integer (Len - 1) loop
            List (I) := Boolean'Val (L (size_t (I)));
         end loop;
         g_free (L);
         return List;
      end;
   end Get_Boolean_List;

   -----------------
   -- Get_Comment --
   -----------------

   function Get_Comment
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return String
   is
      function Internal
        (Key_File   : G_Key_File;
         Group_Name : String;
         Key        : String;
         Error      : Glib.Error.GError)
         return chars_ptr;
      pragma Import (C, Internal, "g_key_file_get_comment");
   begin
      return Convert_Then_Free (Internal
        (Key_File, Group_Name & ASCII.NUL, Key & ASCII.NUL, Error));
   end Get_Comment;

   ----------------
   -- Get_Double --
   ----------------

   function Get_Double
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return Gdouble
   is
      function Internal
        (Key_File   : G_Key_File;
         Group_Name : String;
         Key        : String;
         Error      : Glib.Error.GError)
         return Gdouble;
      pragma Import (C, Internal, "g_key_file_get_double");
   begin
      return Internal
        (Key_File, Group_Name & ASCII.NUL, Key & ASCII.NUL, Error);
   end Get_Double;

   ---------------------
   -- Get_Double_List --
   ---------------------

   function Get_Double_List
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return Double_List
   is
      type Gdouble_Array is array (size_t) of Gdouble;
      pragma Convention (C, Gdouble_Array);
      type Gdouble_Array_Access is access all Gdouble_Array;

      function Internal
        (Key_File   : G_Key_File;
         Group_Name : String;
         Key        : String;
         Length     : access Gsize;
         Error      : Glib.Error.GError)
         return Gdouble_Array_Access;
      pragma Import (C, Internal, "g_key_file_get_double_list");

      procedure g_free (S : Gdouble_Array_Access);
      pragma Import (C, g_free, "g_free");

      Empty_Array : constant Double_List (1 .. 0) := (others => 0.0);
      Len : aliased Gsize;
      L : Gdouble_Array_Access;
   begin
      L := Internal
        (Key_File,
         Group_Name & ASCII.NUL,
         Key & ASCII.NUL,
         Len'Access,
         Error);

      --  If we get back a null, simply return an array with no items.
      if L = null then
         return Empty_Array;
      end if;

      --  Otherwise, declare, populate, and return a constained array
      --  after freeing the returned array.
      declare
         List : constant Double_List (0 .. Integer (Len - 1)) :=
           Double_List (L (0 .. size_t (Len - 1)));
      begin
         g_free (L);
         return List;
      end;
   end Get_Double_List;

   ----------------
   -- Get_Groups --
   ----------------

   function Get_Groups
     (Key_File : G_Key_File) return GNAT.Strings.String_List
   is
      function Internal
        (Key_File : G_Key_File;
         Length   : System.Address)
         return chars_ptr_array_access;
      pragma Import (C, Internal, "g_key_file_get_groups");

   begin
      return Convert_Then_Free (Internal (Key_File, System.Null_Address));
   end Get_Groups;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return Gint
   is
      function Internal
        (Key_File   : G_Key_File;
         Group_Name : String;
         Key        : String;
         Error      : Glib.Error.GError)
         return Gint;
      pragma Import (C, Internal, "g_key_file_get_integer");
   begin
      return Internal
        (Key_File, Group_Name & ASCII.NUL, Key & ASCII.NUL, Error);
   end Get_Integer;

   --------------
   -- Get_Keys --
   --------------

   function Get_Keys
     (Key_File   : G_Key_File;
      Group_Name : String;
      Error      : Glib.Error.GError := null)
      return GNAT.Strings.String_List
   is
      function Internal
        (Key_File   : G_Key_File;
         Group_Name : String;
         Length     : System.Address;
         Error      : Glib.Error.GError)
         return chars_ptr_array_access;
      pragma Import (C, Internal, "g_key_file_get_keys");
   begin
      return Convert_Then_Free (Internal
        (Key_File, Group_Name & ASCII.NUL, System.Null_Address, Error));
   end Get_Keys;

   ----------------------
   -- Get_Integer_List --
   ----------------------

   function Get_Integer_List
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return Integer_List
   is
      type Gint_Array is array (size_t) of Gint;
      pragma Convention (C, Gint_Array);
      type Gint_Array_Access is access all Gint_Array;

      function Internal
        (Key_File   : G_Key_File;
         Group_Name : String;
         Key        : String;
         Length     : access Gsize;
         Error      : Glib.Error.GError)
         return Gint_Array_Access;
      pragma Import (C, Internal, "g_key_file_get_integer_list");

      procedure g_free (S : Gint_Array_Access);
      pragma Import (C, g_free, "g_free");

      Empty_Array : constant Integer_List (1 .. 0) := (others => 0);
      Len : aliased Gsize;
      L : Gint_Array_Access;
   begin
      L := Internal
        (Key_File,
         Group_Name & ASCII.NUL,
         Key & ASCII.NUL,
         Len'Access,
         Error);

      --  If we get back a null, simply return an array with no items.
      if L = null then
         return Empty_Array;
      end if;

      --  Otherwise, declare, populate, and return a constained array
      --  after freeing the returned array.
      declare
         List : constant Integer_List (0 .. Integer (Len - 1)) :=
           Integer_List (L (0 .. size_t (Len - 1)));
      begin
         g_free (L);
         return List;
      end;
   end Get_Integer_List;

   -----------------------
   -- Get_Locale_String --
   -----------------------

   function Get_Locale_String
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Locale     : String)
      return String
   is
      function Internal
        (Key_File   : G_Key_File;
         Group_Name : String;
         Key        : String;
         Locale     : String)
         return chars_ptr;
      pragma Import (C, Internal, "g_key_file_get_locale_string");
   begin
      return Convert_Then_Free (Internal
        (Key_File,
         Group_Name & ASCII.NUL,
         Key & ASCII.NUL,
         Locale & ASCII.NUL));
   end Get_Locale_String;

   ----------------------------
   -- Get_Locale_String_List --
   ----------------------------

   function Get_Locale_String_List
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Locale     : String;
      Error      : Glib.Error.GError := null)
      return GNAT.Strings.String_List
   is
      function Internal
        (Key_File   : G_Key_File;
         Group_Name : String;
         Key        : String;
         Locale     : String;
         Length     : System.Address;
         Error      : Glib.Error.GError)
         return chars_ptr_array_access;
      pragma Import (C, Internal, "g_key_file_get_locale_string_list");
   begin
      return Convert_Then_Free (Internal
        (Key_File,
         Group_Name & ASCII.NUL,
         Key & ASCII.NUL,
         Locale & ASCII.NUL,
         System.Null_Address,
         Error));
   end Get_Locale_String_List;

   ---------------------
   -- Get_Start_Group --
   ---------------------

   function Get_Start_Group (Key_File : G_Key_File) return String is
      function Internal (Key_File : G_Key_File) return chars_ptr;
      pragma Import (C, Internal, "g_key_file_get_start_group");
   begin
      return Convert_Then_Free (Internal (Key_File));
   end Get_Start_Group;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return String
   is
      function Internal
        (Key_File   : G_Key_File;
         Group_Name : String;
         Key        : String;
         Error      : Glib.Error.GError)
         return chars_ptr;
      pragma Import (C, Internal, "g_key_file_get_string");
   begin
      return Convert_Then_Free (Internal
        (Key_File, Group_Name & ASCII.NUL, Key & ASCII.NUL, Error));
   end Get_String;

   ---------------------
   -- Get_String_List --
   ---------------------

   function Get_String_List
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return GNAT.Strings.String_List
   is
      function Internal
        (Key_File   : G_Key_File;
         Group_Name : String;
         Key        : String;
         Length     : System.Address;
         Error      : Glib.Error.GError)
         return chars_ptr_array_access;
      pragma Import (C, Internal, "g_key_file_get_string_list");
   begin
      return Convert_Then_Free (Internal
        (Key_File,
         Group_Name & ASCII.NUL,
         Key & ASCII.NUL,
         System.Null_Address,
         Error));
   end Get_String_List;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return String
   is
      function Internal
        (Key_File   : G_Key_File;
         Group_Name : String;
         Key        : String;
         Error      : Glib.Error.GError)
         return chars_ptr;
      pragma Import (C, Internal, "g_key_file_get_value");
   begin
      return Convert_Then_Free (Internal
        (Key_File, Group_Name & ASCII.NUL, Key & ASCII.NUL, Error));
   end Get_Value;

   ---------------
   -- Has_Group --
   ---------------

   function Has_Group
     (Key_File   : G_Key_File;
      Group_Name : String)
      return Boolean
   is
      function Internal
        (Key_File   : G_Key_File;
         Group_Name : String)
         return Gboolean;
      pragma Import (C, Internal, "g_key_file_has_group");
   begin
      return Boolean'Val (Internal (Key_File, Group_Name & ASCII.NUL));
   end Has_Group;

   -------------
   -- Has_Key --
   -------------

   function Has_Key
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return Boolean
   is
      function Internal
        (Key_File   : G_Key_File;
         Group_Name : String;
         Key        : String;
         Error      : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "g_key_file_has_key");
   begin
      return Boolean'Val (Internal
        (Key_File, Group_Name & ASCII.NUL, Key & ASCII.NUL, Error));
   end Has_Key;

   --------------------
   -- Load_From_Data --
   --------------------

   function Load_From_Data
     (Key_File : G_Key_File;
      Data     : String;
      Flags    : G_Key_File_Flags;
      Error    : Glib.Error.GError := null)
      return Boolean
   is
      function Internal
        (Key_File : G_Key_File;
         Data     : String;
         Length   : Gsize;
         Flags    : G_Key_File_Flags;
         Error    : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "g_key_file_load_from_data");
   begin
      return Boolean'Val (Internal
        (Key_File, Data & ASCII.NUL, Gsize (Data'Length), Flags, Error));
   end Load_From_Data;

   -------------------------
   -- Load_From_Data_Dirs --
   -------------------------

   function Load_From_Data_Dirs
     (Key_File : G_Key_File;
      File     : String;
      Flags    : G_Key_File_Flags;
      Error    : Glib.Error.GError := null)
      return Boolean
   is
      function Internal
        (Key_File  : G_Key_File;
         File      : String;
         Full_Path : System.Address;
         Flags     : G_Key_File_Flags;
         Error     : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "g_key_file_load_from_data_dirs");
   begin
      return Boolean'Val (Internal
        (Key_File,
         File & ASCII.NUL,
         System.Null_Address,
         Flags,
         Error));
   end Load_From_Data_Dirs;

   --------------------
   -- Load_From_Dirs --
   --------------------

   function Load_From_Dirs
     (Key_File    : G_Key_File;
      File        : String;
      Search_Dirs : GNAT.Strings.String_List;
      Flags       : G_Key_File_Flags;
      Error       : Glib.Error.GError := null)
      return Boolean
   is
      function Internal
        (Key_File    : G_Key_File;
         File        : String;
         Search_Dirs : System.Address;
         Full_Path   : System.Address;
         Flags       : G_Key_File_Flags;
         Error       : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "g_key_file_load_from_dirs");

      L      : aliased chars_ptr_array := From_String_List (Search_Dirs);
      Result : Gboolean;
   begin
      if Search_Dirs'Length = 0 then
         Result := Internal
           (Key_File,
            File & ASCII.NUL,
            System.Null_Address,
            System.Null_Address,
            Flags,
            Error);
      else
         Result := Internal
           (Key_File,
            File & ASCII.NUL,
            L (L'First)'Address,
            System.Null_Address,
            Flags,
            Error);
      end if;

      for I in L'Range loop
         Free (L (I));
      end loop;

      return Boolean'Val (Result);
   end Load_From_Dirs;

   --------------------
   -- Load_From_File --
   --------------------

   function Load_From_File
     (Key_File : G_Key_File;
      File     : String;
      Flags    : G_Key_File_Flags;
      Error    : Glib.Error.GError := null)
      return Boolean
   is
      function Internal
        (Key_File : G_Key_File;
         File     : String;
         Flags    : G_Key_File_Flags;
         Error    : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "g_key_file_load_from_file");
   begin
      return Boolean'Val (Internal (Key_File, File & ASCII.NUL, Flags, Error));
   end Load_From_File;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Key_File : out G_Key_File) is
      function Internal return G_Key_File;
      pragma Import (C, Internal, "g_key_file_new");
   begin
      Key_File := Internal;
   end Gtk_New;

   --------------------
   -- Remove_Comment --
   --------------------

   function Remove_Comment
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String := "";
      Error      : Glib.Error.GError := null)
      return Boolean
   is
      function Internal
        (Key_File   : G_Key_File;
         Group_Name : chars_ptr;
         Key        : chars_ptr;
         Error      : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "g_key_file_remove_comment");

      G : chars_ptr := String_Or_Null (Group_Name);
      K : chars_ptr := String_Or_Null (Key);
      Result : Gboolean;
   begin
      Result := Internal (Key_File, G, K, Error);
      if G /= Null_Ptr then
         Free (G);
      end if;
      if K /= Null_Ptr then
         Free (K);
      end if;
      return Boolean'Val (Result);
   end Remove_Comment;

   ------------------
   -- Remove_Group --
   ------------------

   function Remove_Group
     (Key_File   : G_Key_File;
      Group_Name : String;
      Error      : Glib.Error.GError := null)
      return Boolean
   is
      function Internal
        (Key_File   : G_Key_File;
         Group_Name : String;
         Error      : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "g_key_file_remove_group");
   begin
      return Boolean'Val (Internal (Key_File, Group_Name & ASCII.NUL, Error));
   end Remove_Group;

   ----------------
   -- Remove_Key --
   ----------------

   function Remove_Key
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return Boolean
   is
      function Internal
        (Key_File   : G_Key_File;
         Group_Name : String;
         Key        : String;
         Error      : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "g_key_file_remove_key");
   begin
      return Boolean'Val (Internal
        (Key_File, Group_Name & ASCII.NUL, Key & ASCII.NUL, Error));
   end Remove_Key;

   -----------------
   -- Set_Boolean --
   -----------------

   procedure Set_Boolean
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      Value      : Boolean)
   is
      procedure Internal
        (Key_File   : G_Key_File;
         Group_Name : chars_ptr;
         Key        : String;
         Value      : Gboolean);
      pragma Import (C, Internal, "g_key_file_set_boolean");
      G : chars_ptr := String_Or_Null (Group_Name);
   begin
      Internal (Key_File, G, Key & ASCII.NUL, Boolean'Pos (Value));
      Free (G);
   end Set_Boolean;

   ----------------------
   -- Set_Boolean_List --
   ----------------------

   procedure Set_Boolean_List
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      List       : Boolean_List)
   is
      procedure Internal
        (Key_File   : G_Key_File;
         Group_Name : chars_ptr;
         Key        : String;
         List       : System.Address;
         Length     : Gsize);
      pragma Import (C, Internal, "g_key_file_set_boolean_list");

      G : chars_ptr := String_Or_Null (Group_Name);
      L : array (List'Range) of Gboolean;
      pragma Convention (C, L);
   begin
      for I in List'Range loop
         L (I) := Boolean'Pos (List (I));
      end loop;
      Internal (Key_File, G, Key & ASCII.NUL, L (L'First)'Address, L'Length);
      Free (G);
   end Set_Boolean_List;

   -----------------
   -- Set_Comment --
   -----------------

   function Set_Comment
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String := "";
      Comment    : String;
      Error      : Glib.Error.GError := null)
      return Boolean
   is
      function Internal
        (Key_File   : G_Key_File;
         Group_Name : chars_ptr;
         Key        : chars_ptr;
         Comment    : String;
         Error      : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "g_key_file_set_comment");

      G : chars_ptr := String_Or_Null (Group_Name);
      K : chars_ptr := String_Or_Null (Key);
      Result : Gboolean;
   begin
      Result := Internal (Key_File, G, K, Comment & ASCII.NUL, Error);
      if G /= Null_Ptr then
         Free (G);
      end if;
      if K /= Null_Ptr then
         Free (K);
      end if;
      return Boolean'Val (Result);
   end Set_Comment;

   ----------------
   -- Set_Double --
   ----------------

   procedure Set_Double
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      Value      : Gdouble)
   is
      procedure Internal
        (Key_File   : G_Key_File;
         Group_Name : chars_ptr;
         Key        : String;
         Value      : Gdouble);
      pragma Import (C, Internal, "g_key_file_set_double");
      G : chars_ptr := String_Or_Null (Group_Name);
   begin
      Internal (Key_File, G, Key & ASCII.NUL, Value);
      Free (G);
   end Set_Double;

   ---------------------
   -- Set_Double_List --
   ---------------------

   procedure Set_Double_List
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      List       : Double_List)
   is
      procedure Internal
        (Key_File   : G_Key_File;
         Group_Name : chars_ptr;
         Key        : String;
         List       : System.Address;
         Length     : Gsize);
      pragma Import (C, Internal, "g_key_file_set_double_list");

      G : chars_ptr := String_Or_Null (Group_Name);
   begin
      Internal
        (Key_File, G, Key & ASCII.NUL, List (List'First)'Address, List'Length);
      Free (G);
   end Set_Double_List;

   -----------------
   -- Set_Integer --
   -----------------

   procedure Set_Integer
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      Value      : Gint)
   is
      procedure Internal
        (Key_File   : G_Key_File;
         Group_Name : chars_ptr;
         Key        : String;
         Value      : Gint);
      pragma Import (C, Internal, "g_key_file_set_integer");

      G : chars_ptr := String_Or_Null (Group_Name);
   begin
      Internal (Key_File, G, Key & ASCII.NUL, Value);
      Free (G);
   end Set_Integer;

   ----------------------
   -- Set_Integer_List --
   ----------------------

   procedure Set_Integer_List
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      List       : Integer_List)
   is
      procedure Internal
        (Key_File   : G_Key_File;
         Group_Name : chars_ptr;
         Key        : String;
         List       : System.Address;
         Length     : Gsize);
      pragma Import (C, Internal, "g_key_file_set_integer_list");

      G : chars_ptr := String_Or_Null (Group_Name);
   begin
      Internal
        (Key_File, G, Key & ASCII.NUL, List (List'First)'Address, List'Length);
      Free (G);
   end Set_Integer_List;

   -----------------------
   -- Set_Locale_String --
   -----------------------

   procedure Set_Locale_String
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      Locale     : String;
      The_String : String)
   is
      procedure Internal
        (Key_File   : G_Key_File;
         Group_Name : chars_ptr;
         Key        : String;
         Locale     : String;
         The_String : String);
      pragma Import (C, Internal, "g_key_file_set_locale_string");

      G : chars_ptr := String_Or_Null (Group_Name);
   begin
      Internal
        (Key_File,
         G,
         Key & ASCII.NUL,
         Locale & ASCII.NUL,
         The_String & ASCII.NUL);
      Free (G);
   end Set_Locale_String;

   ----------------------------
   -- Set_Locale_String_List --
   ----------------------------

   procedure Set_Locale_String_List
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      Locale     : String;
      List       : GNAT.Strings.String_List)
   is
      procedure Internal
        (Key_File   : G_Key_File;
         Group_Name : chars_ptr;
         Key        : String;
         Locale     : String;
         List       : System.Address;
         Length     : Gsize);
      pragma Import (C, Internal, "g_key_file_set_locale_string_list");

      G : chars_ptr := String_Or_Null (Group_Name);
      L : aliased chars_ptr_array := From_String_List (List);
   begin
      if List'Length = 0 then
         Internal
           (Key_File,
            G,
            Key & ASCII.NUL,
            Locale & ASCII.NUL,
            System.Null_Address,
            0);
      else
         Internal
           (Key_File,
            G,
            Key & ASCII.NUL,
            Locale & ASCII.NUL,
            L (L'First)'Address,
            L'Length);
      end if;

      Free (G);
      for I in L'Range loop
         Free (L (I));
      end loop;
   end Set_Locale_String_List;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      The_String : String)
   is
      procedure Internal
        (Key_File   : G_Key_File;
         Group_Name : chars_ptr;
         Key        : String;
         The_String : String);
      pragma Import (C, Internal, "g_key_file_set_string");

      G : chars_ptr := String_Or_Null (Group_Name);
   begin
      Internal (Key_File, G, Key & ASCII.NUL, The_String & ASCII.NUL);
      Free (G);
   end Set_String;

   ---------------------
   -- Set_String_List --
   ---------------------

   procedure Set_String_List
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      List       : GNAT.Strings.String_List)
   is
      procedure Internal
        (Key_File   : G_Key_File;
         Group_Name : chars_ptr;
         Key        : String;
         List       : System.Address;
         Length     : Gsize);
      pragma Import (C, Internal, "g_key_file_set_string_list");

      G : chars_ptr := String_Or_Null (Group_Name);
      L : aliased chars_ptr_array := From_String_List (List);
   begin
      Internal (Key_File, G, Key & ASCII.NUL, L (L'First)'Address, L'Length);
      Free (G);
      for I in L'Range loop
         Free (L (I));
      end loop;
   end Set_String_List;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      Value      : String)
   is
      procedure Internal
        (Key_File   : G_Key_File;
         Group_Name : chars_ptr;
         Key        : String;
         Value      : String);
      pragma Import (C, Internal, "g_key_file_set_value");

      G : chars_ptr := String_Or_Null (Group_Name);
   begin
      Internal (Key_File, G, Key & ASCII.NUL, Value & ASCII.NUL);
      Free (G);
   end Set_Value;

   -------------
   -- To_Data --
   -------------

   function To_Data (Key_File : G_Key_File) return String is
      function Internal
        (Key_File   : G_Key_File;
         Length     : access Gsize;
         Error      : Glib.Error.GError)
         return chars_ptr;
      pragma Import (C, Internal, "g_key_file_to_data");
      Len : aliased Gsize;
      C : chars_ptr;
   begin
      C := Internal (Key_File, Len'Access, null);
      declare
         S : constant String := Value (C, size_t (Len));
      begin
         Free (C);
         return S;
      end;
   end To_Data;

end Glib.Key_File;

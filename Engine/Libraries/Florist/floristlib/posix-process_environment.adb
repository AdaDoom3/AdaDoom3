------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--             P O S I X . P R O C E S S _ E N V I R O N M E N T            --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--             Copyright (C) 1996-1997 Florida State University             --
--                     Copyright (C) 1998-2014, AdaCore                     --
--                                                                          --
--  This file is a component of FLORIST, an  implementation of an  Ada API  --
--  for the POSIX OS services, for use with  the  GNAT  Ada  compiler  and  --
--  the FSU Gnu Ada Runtime Library (GNARL).   The  interface  is intended  --
--  to be close to that specified in  IEEE STD  1003.5 : 1990  and IEEE STD  --
--  1003.5b : 1996.                                                          --
--                                                                          --
--  FLORIST is free software;  you can  redistribute  it and/or  modify it  --
--  under terms of the  GNU  General  Public  License as  published by the  --
--  Free Software Foundation;  either version  2, or (at  your option) any  --
--  later version.  FLORIST is distributed  in  the hope  that  it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without  even the implied  warranty  --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE.  See  the GNU  --
--  General Public License for more details.  You  should have  received a  --
--  copy of the GNU General Public License  distributed  with  GNARL;  see  --
--  file  COPYING.  If not,  write to  the  Free  Software  Foundation, 59  --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.                   --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line,
     POSIX.C,
     POSIX.Implementation,
     Unchecked_Deallocation;

package body POSIX.Process_Environment is

   use POSIX.C;
   use POSIX.Implementation;

   type Environment_List is new POSIX.Implementation.String_List;

   type var_char_ptr_ptr is access all char_ptr;
   pragma Warnings (Off);
   function To_Variable is
      new Unchecked_Conversion (char_ptr_ptr, var_char_ptr_ptr);
   pragma Warnings (On);

   procedure Free is
     new Unchecked_Deallocation (POSIX_String, POSIX_String_Ptr);

   ---------------------------------------
   --  Interfaced C String Subprograms  --
   ---------------------------------------

   function strlen (str : char_ptr) return size_t;
   pragma Import (C, strlen, "strlen");

   function strcpy (dest : char_ptr; src : char_ptr) return char_ptr;
   pragma Import (C, strcpy, "strcpy");

   function strcat (dest : char_ptr; src : char_ptr) return char_ptr;
   pragma Import (C, strcat, "strcat");

   function strncat
     (dest : char_ptr; src : char_ptr; n : size_t) return char_ptr;
   pragma Import (C, strncat, "strncat");

   ----------------------------------------------------------
   --  Interfaced C Environment Subprograms and Variables  --
   ----------------------------------------------------------

   environ : char_ptr_ptr;
   pragma Import (C, environ, "environ");

   function c_setenv
     (name : char_ptr;
      value : char_ptr;
      overwrite : int) return int;
   pragma Import (C, c_setenv, setenv_LINKNAME);

   function c_getenv
     (name : char_ptr) return char_ptr;
   pragma Import (C, c_getenv, getenv_LINKNAME);

   function c_putenv
     (pair : char_ptr) return int;
   --  This creates a potentially permanent reference; the
   --  storage pointed to by pair must not be recovered!
   pragma Import (C, c_putenv, putenv_LINKNAME);

   function c_unsetenv
     (name : char_ptr) return int;
   pragma Import (C, c_unsetenv, unsetenv_LINKNAME);

   -------------------------
   --  Local_Subprograms  --
   -------------------------

   procedure Validate (Name : POSIX_String);
   --  Verify that a name is legal, raising posix_error otherwise.

   function Split_Point (Str : POSIX_String) return Natural;
   --  Return location of first "=" in string,
   --  or zero if no "=" is found.
   --  Assume the string is NUL terminated.

   function Match
     (Pair : POSIX_String_Ptr;
      Name : POSIX_String) return Natural;
   --  Match returns zero unless Pair has the form
   --  Name & '=' & ..., in which case it returns the index
   --  immediately following the '=' in Pair.

   --  The following C-style version of Match is used
   --  only if the environment does not provide one or more
   --  of the standard functions putenv, setenv, getenv, unsetenv.

   function C_Match
     (Pair : char_ptr;
      Name : char_ptr) return char_ptr;

   --  If the C environment has the standard functions to modify
   --  the environment, we use those.  Otherwise, we hack our own.

   function Setenv
     (Name : char_ptr;
      Value : char_ptr;
      Overwrite : int) return int;
   function Unsetenv (Name : char_ptr) return int;
   function Getenv (Name : char_ptr) return char_ptr;
   function Create_Pair (Name, Value : char_ptr) return char_ptr;

   -------------------
   --  Create_Pair  --
   -------------------

   function Create_Pair (Name, Value : char_ptr) return char_ptr is
      Tmp : char_ptr;
      Eqls : aliased constant POSIX_String := "=";
   begin
      Tmp := malloc (strlen (Name) + strlen (Value) + 2);
      Tmp := strcpy (Tmp, Name);
      Tmp := strncat (Tmp, Eqls (1)'Unchecked_Access, 1);
      Tmp := strcat (Tmp, Value);
      return Tmp;
   end Create_Pair;

   --------------
   --  Setenv  --
   --------------

   function Setenv
     (Name : char_ptr;
      Value : char_ptr;
      Overwrite : int) return int is
   begin
      if HAVE_putenv then
         if Overwrite = 0 and then c_getenv (Name) /= null then
            return 0;
         end if;

         return c_putenv (Create_Pair (Name, Value));

      elsif HAVE_setenv then
         return c_setenv (Name, Value, Overwrite);
      else
         declare
            P  : char_ptr_ptr := environ;
            PP : char_ptr_ptr;
            T  : char_ptr_ptr;
            K  : size_t := 0;
         begin
            while P.all /= null loop
               if C_Match (P.all, Name) /= null then
                  if Overwrite = 0 then
                     return 0;
                  end if;
                  --  don't risk freeing P.all!
                  To_Variable (P).all := Create_Pair (Name, Value);
               end if;
               K := K + 1;
               Advance (P);
            end loop;
            PP := malloc ((K + 2) * (char_ptr'Size / char'Size));
            T := PP; P := environ;
            for I in 1 .. K loop
               To_Variable (T).all := P.all; Advance (T); Advance (P);
            end loop;
            To_Variable (T).all := Create_Pair (Name, Value);
            Advance (T); To_Variable (T).all := null;
            environ := PP;
            --  .... this risks storage leakage (see note above)
         end;
         return 0;
      end if;
   end Setenv;

   ----------------
   --  Unsetenv  --
   ----------------

   function Unsetenv (Name : char_ptr) return int is
   begin
      if HAVE_unsetenv then
         return c_unsetenv (Name);
      else
         declare
            P  : char_ptr_ptr := environ;
            PP : char_ptr_ptr;
            Q  : char_ptr;
         begin
            while P.all /= null loop
               Q := C_Match (P.all, Name);
               if Q /= null then
                  loop
                     PP := P;
                     Advance (P);
                     To_Variable (PP).all := P.all;
                     if P.all = null then
                        return 0;
                     end if;
                  end loop;
               end if;
               Advance (P);
            end loop;
         end;
         return 0;
      end if;
   end Unsetenv;

   --------------
   --  Getenv  --
   --------------

   function Getenv (Name : char_ptr) return char_ptr is
   begin
      if HAVE_getenv then
         return c_getenv (Name);
      else
         declare
            P : char_ptr_ptr := environ;
            Q : char_ptr;
         begin
            while P.all /= null loop
               Q := C_Match (P.all, Name);
               if Q /= null then
                  return Q;
               end if;
               Advance (P);
            end loop;
         end;
      end if;
      return null;
   end Getenv;

   ----------------
   --  Validate  --
   ----------------

   procedure Validate (Name : POSIX_String) is
   begin
      if Name = "" then
         Raise_POSIX_Error (Invalid_Argument);
      end if;

      for P in Name'Range loop
         if Name (P) = '=' or Name (P) = NUL then
            Raise_POSIX_Error (Invalid_Argument);
         end if;
      end loop;
   end Validate;

   -------------------
   --  Split_Point  --
   -------------------

   function Split_Point (Str : POSIX_String) return Natural is
   begin
      for J in Str'Range loop
         if Str (J) = '=' then
            return J;
         end if;

         if Str (J) = NUL then
            return 0;
         end if;
      end loop;

      return 0;
   end Split_Point;

   -------------
   --  Match  --
   -------------

   function Match
     (Pair : POSIX_String_Ptr;
      Name : POSIX_String) return Natural
   is
      J, JL, K, KL : Integer;
   begin
      J := Pair'First; K := Name'First;
      JL := Pair'Last; KL := Name'Last;

      while (J <= JL and K <= KL) and then Pair (J) = Name (K) loop
         J := J + 1; K := K + 1;
      end loop;

      --  J > JL or K > KL or Pair (J) /= Name (K)

      if (K > KL and J <= JL) and then Pair (J) = '=' then
         return J + 1;
      end if;

      return 0;
   end Match;

   function C_Match
     (Pair : char_ptr;
      Name : char_ptr) return char_ptr
   is
      J, K : char_ptr;
   begin
      J := Pair; K := Name;

      while (J.all /= NUL and K.all /= NUL) and then J.all = K.all loop
         Advance (J); Advance (K);
      end loop;

      --  J.all = NUL or K.all = NUL or J.all /= K.all

      if K.all = NUL and J.all = '=' then
         Advance (J);
         return J;
      end if;

      return null;
   end C_Match;

   --------------------------
   --  Current Environment --
   --------------------------

   --  .... Change P1003.5?

   --  It is not clear from P1003.5 whether the current environment
   --  should be shared between Ada and C code.  We assume that
   --  it should be shared, and therefore we use the C-language
   --  operations to access the current environment.

   --  P1003.5 says we need to recover the storage of the old value
   --  of the current environment when we modify it.  We must trust
   --  the C interface to do it, if we want compatibility.

   --  We choose not to try to make these operations tasking-safe,
   --  as it is not required by the standard and there is no way
   --  we can make the C interfaces tasking safe.

   --  .... consider trying to reduce storage leakage here
   --  Suppose every string that we allocate to become part of the current
   --  environment is actually part of a record, with a pointer field
   --  that is used to keep a linked list of all such records.
   --  When we change an environment value we could run down the list
   --  and if the string is found, we could safely recover the storage.
   --  Of course, for this to be safe for concurrent usage, we would need
   --  to make the operations that modify the list into critical sections.

   --  Similarly, we could reduce storage leakage for the object corresponding
   --  to the current environment, when we need to grow it, in Setenv.
   --  For example, we might declare :

   --  Our_Environ : char_ptr_ptr := null;
   --  Points to the last storage we malloced and used for C's environ.
   --  Our_Environ_Length : Integer := -1;
   --  The length in pointers of Our_Environ, if that is not null.

   --  When we shrink the environment, we could remember that there is
   --  extra space, using Our_Environ_Length, so that when we need to
   --  grow it again we would not need to allocate a new block.
   --  When we need to allocate a larger block, we could recover the
   --  old one, if environ = Our_Environ.

   ---------------------
   --  Argument_List  --
   ---------------------

   function Argument_List return POSIX.POSIX_String_List is
      use Ada.Command_Line;
      Argv : POSIX_String_List;
   begin
      Append (Argv, To_POSIX_String (Command_Name));
      for I in 1 .. Argument_Count loop
         Append (Argv, To_POSIX_String (Argument (I)));
      end loop;
      return Argv;
   end Argument_List;

   --  .... Consider rewriting the above to use the direct C interface.
   --  That is, get rid of the extra string copying and type conversion.

   -------------------------------------
   --  Copy_From_Current_Environment  --
   -------------------------------------

   procedure Copy_From_Current_Environment (Env : in out Environment) is
      P : char_ptr_ptr := environ;
      Tmp : POSIX_String_List := To_POSIX_String_List (Env);
   begin
      if P /= null then
         while P.all /= null loop
            --  .... concise but inefficient
            --  We first remove the NUL and then reappend it.
            Append (Tmp, Form_POSIX_String (P.all));
            Advance (P);
         end loop;
      end if;
      Env := To_Environment (Tmp);
   end Copy_From_Current_Environment;

   -----------------------------------
   --  Copy_To_Current_Environment  --
   -----------------------------------

   procedure Copy_To_Current_Environment (Env : Environment) is
      procedure Copy_One
        (Name : POSIX_String;
         Value : POSIX_String;
         Quit : in out Boolean);
      procedure Copy_One
        (Name : POSIX_String;
         Value : POSIX_String;
         Quit : in out Boolean) is
         pragma Warnings (Off, Quit);
      begin Set_Environment_Variable (Name, Value);
      end Copy_One;
      procedure Copy_All is
         new For_Every_Environment_Variable (Copy_One);
      --  .... concise but inefficient
      --  We split up pairs, and recombine them,
      --  adding and removing NUL along the way.
      --  If we could count on having putenv(),
      --  the splitting and recombining could be avoided.
   begin
      Clear_Environment;
      Copy_All (Env);
   end Copy_To_Current_Environment;

   ------------------------
   --  Copy_Environment  --
   ------------------------

   procedure Copy_Environment
     (Source : Environment;
      Target : in out Environment) is
      T_Source : constant POSIX_String_List := To_POSIX_String_List (Source);
      T_Target : POSIX_String_List;
      procedure Copy_One (Str : POSIX_String; Done : in out Boolean);
      procedure Copy_One (Str : POSIX_String; Done : in out Boolean) is
         pragma Warnings (Off, Done);
      begin Append (T_Target, Str);
      end Copy_One;
      procedure Copy_All is new For_Every_Item (Copy_One);
   begin
      Clear_Environment (Target);
      Copy_All (T_Source);
      Target := To_Environment (T_Target);
   end Copy_Environment;

   ----------------------------
   --  Environment_Value_Of  --
   ----------------------------

   function Environment_Value_Of
      (Name      : POSIX.POSIX_String;
       Env       : Environment;
       Undefined : POSIX.POSIX_String := "")
      return POSIX.POSIX_String is
      J : Integer;
   begin
      Validate (Name);
      if Env /= null then
         for I in 1 .. Env.Length loop
            exit when Env.List (I) = null;
            J := Match (Env.List (I), Name);
            if J /= 0 then
               return Form_POSIX_String (Env.List (I)(J)'Unchecked_Access);
            end if;
         end loop;
      end if;
      return Undefined;
   end Environment_Value_Of;

   ----------------------------
   --  Environment_Value_Of  --
   ----------------------------

   function Environment_Value_Of
      (Name      : POSIX.POSIX_String;
       Undefined : POSIX.POSIX_String := "")
      return POSIX.POSIX_String is
      c_name : POSIX_String := Name & NUL;
      Result : constant char_ptr :=
                 Getenv (c_name (c_name'First)'Unchecked_Access);
   begin
      Validate (Name);
      if Result = null then
         return Undefined;
      end if;
      return Form_POSIX_String (Result);
   end Environment_Value_Of;

   -------------------------------
   --  Is_Environment_Variable  --
   -------------------------------

   function Is_Environment_Variable
      (Name : POSIX.POSIX_String;
       Env : Environment) return Boolean is
      Result : Boolean := False;
      procedure Check
        (Name : POSIX_String;
         Value : POSIX_String;
         Done : in out Boolean);
      procedure Check
        (Name : POSIX_String;
         Value : POSIX_String;
         Done : in out Boolean) is
         pragma Warnings (Off, Value);
      begin
         if Name = Is_Environment_Variable.Name then
            Result := True;
            Done := True;
         end if;
      end Check;
      procedure Check_All is new For_Every_Environment_Variable (Check);
   begin
      Validate (Name);
      Check_All (Env);
      return Result;
   end Is_Environment_Variable;

   -------------------------------
   --  Is_Environment_Variable  --
   -------------------------------

   function Is_Environment_Variable
      (Name      : POSIX.POSIX_String) return Boolean is
      c_name : POSIX_String := Name & NUL;
   begin
      Validate (Name);
      return Getenv (c_name (c_name'First)'Unchecked_Access) /= null;
   end Is_Environment_Variable;

   -------------------------
   --  Clear_Environment  --
   -------------------------

   procedure Clear_Environment (Env : in out Environment) is
      Tmp : POSIX_String_List := To_POSIX_String_List (Env);
   begin
      Make_Empty (Tmp);
      Env := To_Environment (Tmp);
   end Clear_Environment;

   -------------------------
   --  Clear_Environment  --
   -------------------------

   procedure Clear_Environment is
      P : char_ptr_ptr := environ;
      Strings : POSIX_String_List;
      procedure Clear_One (Str : POSIX_String; Done : in out Boolean);
      procedure Clear_One (Str : POSIX_String; Done : in out Boolean) is
         pragma Warnings (Off, Done);
      begin Check (Unsetenv (Str (Str'First)'Unchecked_Access));
      end Clear_One;
      procedure Clear_All is new For_Every_Item (Clear_One);
   begin
      if P /= null then
         while P.all /= null loop
            --  .... concise but inefficient
            declare
               S : constant POSIX_String := Form_POSIX_String (P.all);
               J : constant Integer := Split_Point (S);
            begin
               Append (Strings, S (1 .. J - 1));
            end;
            Advance (P);
         end loop;
         Clear_All (Strings);
         Make_Empty (Strings);
         P := environ;
         while P.all /= null loop
            Advance (P);
         end loop;
      end if;
   end Clear_Environment;

   --------------------------------
   --  Set_Environment_Variable  --
   --------------------------------

   procedure Set_Environment_Variable
     (Name  :        POSIX.POSIX_String;
      Value :        POSIX.POSIX_String;
      Env   : in out Environment) is
      J, L : Natural;
      Tmp : POSIX_String_List;
   begin
      Validate (Name);
      if Env /= null then
         L := 0;  -- last empty location
         for I in 1 .. Env.Length loop
            if Env.List (I) = null then
               if L = 0 then
                  L := I;
               end if;
               exit;
            end if;
            J := Match (Env.List (I), Name);
            if J /= 0 then
               Free (Env.List (I));
               Env.List (I) :=
                  new POSIX_String'(Name & "=" & Value & NUL);
               Env.Char (I) := Env.List (I)(1)'Unchecked_Access;
               return;
            end if;
         end loop;
         pragma Assert (L /= 0);
         if L < Env.Length then
            Env.List (L) := new POSIX_String'(Name & "=" & Value & NUL);
            Env.Char (L) := Env.List (L)(1)'Unchecked_Access;
            return;
         end if;
      end if;
      Tmp := To_POSIX_String_List (Env);
      Append (Tmp, Name & "=" & Value);
      Env := To_Environment (Tmp);
   end Set_Environment_Variable;

   --------------------------------
   --  Set_Environment_Variable  --
   --------------------------------

   procedure Set_Environment_Variable
      (Name  :     POSIX.POSIX_String;
       Value :     POSIX.POSIX_String) is
      c_name : POSIX_String := Name & NUL;
      c_value : POSIX_String := Value & NUL;
   begin
      Validate (Name);
      Check (Setenv (c_name (c_name'First)'Unchecked_Access,
        c_value (c_value'First)'Unchecked_Access, 1));
   end Set_Environment_Variable;

   -----------------------------------
   --  Delete_Environment_Variable  --
   -----------------------------------

   procedure Delete_Environment_Variable
     (Name  :        POSIX.POSIX_String;
      Env   : in out Environment) is
      K : Natural;
      --  the location where Env.List (I) should be;
      --  eventually lags behind I if we have deleted something
   begin
      Validate (Name);
      if Env /= null then
         K := 1;
         for I in 1 .. Env.Length loop
            --  copy Ith pair down, if necessary
            --  to fill in for deleted pair
            if K /= I then
               Env.List (K) := Env.List (I);
               Env.Char (K) := Env.Char (I);
               Env.List (I) := null;
               Env.Char (I) := null;
            end if;
            exit when Env.List (K) = null;
            if Match (Env.List (K), Name) /= 0 then
               Free (Env.List (K));
               Env.Char (K) := null;
            else
               K := K + 1;
            end if;
         end loop;
      end if;
   end Delete_Environment_Variable;

   -----------------------------------
   --  Delete_Environment_Variable  --
   -----------------------------------

   procedure Delete_Environment_Variable
      (Name  :     POSIX.POSIX_String) is
      c_name : POSIX_String := Name & NUL;
   begin
      Validate (Name);
      Check (Unsetenv (c_name (c_name'First)'Unchecked_Access));
   end Delete_Environment_Variable;

   --------------
   --  Length  --
   --------------

   function Length (Env : Environment) return Natural is
   begin
      return Length (To_POSIX_String_List (Env));
   end Length;

   --------------
   --  Length  --
   --------------

   function Length return Natural is
      P : char_ptr_ptr := environ;
      L : Natural := 0;
   begin
      if P /= null then
         while P.all /= null loop
            L := L + 1; Advance (P);
         end loop;
      end if;
      return L;
   end Length;

   --------------------------------------
   --  For_Every_Environment_Variable  --
   --------------------------------------

   --  .... Should we try to protect against side-effects of Action?
   --  We can do this by making a temporary local copy of the
   --  environment, to use in the traversal.  The cost is the overhead
   --  of making this copy.  We currently choose not to do this,
   --  though it means cannot use For_Every_Environment_Variable to implement
   --  Clear_Environment.

   procedure For_Every_Environment_Variable (Env : Environment) is
      Quit : Boolean := False;
   begin
      if Env = null then
         return;
      end if;
      for I in 1 .. Env.Length loop
         exit when Env.List (I) = null;
         declare
            L : constant Integer := Env.List (I)'Length;
            J : constant Integer := Split_Point (Env.List (I).all);
         begin
            if J /= 0 then
               if J < L then
                  declare
                     Value : constant POSIX_String (1 .. L - (J + 1)) :=
                       Env.List (I)(J + 1 .. L - 1);
                     --  contortion needed so index range starts with 1
                  begin
                     Action (Env.List (I)(1 .. J - 1), Value, Quit);
                  end;
               else
                  Action (Env.List (I)(1 .. J - 1), "", Quit);
               end if;
            end if;
         end;
         exit when Quit;
      end loop;
   end For_Every_Environment_Variable;

   ----------------------------------------------
   --  For_Every_Current_Environment_Variable  --
   ----------------------------------------------

   procedure For_Every_Current_Environment_Variable is
      Quit : Boolean := False;
      P : char_ptr_ptr := environ;
   begin
      if P = null then
         return;
      end if;
      while P.all /= null loop
         declare
            Str : POSIX_String := Form_POSIX_String (P.all);
            I : constant Natural := Split_Point (Str);
         begin
            if I /= 0 then
               Str (I) := NUL;
               Action (Str (1 .. I - 1), Str (I + 1 .. Str'Last), Quit);
            end if;
         end;
         exit when Quit;
         Advance (P);
      end loop;
   end For_Every_Current_Environment_Variable;

   --------------------------------
   --  Change_Working_Directory  --
   --------------------------------

   procedure Change_Working_Directory
     (Directory_Name : POSIX.Pathname) is
      function chdir (path : char_ptr) return int;
      pragma Import (C, chdir, chdir_LINKNAME);
      c_name : POSIX_String := Directory_Name & NUL;
   begin
      Check (chdir (c_name (c_name'First)'Unchecked_Access));
   end Change_Working_Directory;

   -----------------------------
   --  Get_Working_Directory  --
   -----------------------------

   function Get_Working_Directory return POSIX.Pathname is
      function getcwd (buf : char_ptr; size : size_t) return char_ptr;
      pragma Import (C, getcwd, getcwd_LINKNAME);
      Guessed_Length : Positive := 256;
      Result : char_ptr;
   begin
      loop
         declare
            Buf : POSIX_String (1 .. Guessed_Length);
         begin
            Result := getcwd
              (Buf (1)'Unchecked_Access, size_t (Guessed_Length));
            if Result /= null then
               return Form_POSIX_String (Result);
            end if;
         end;
         exit when Fetch_Errno /= ERANGE;
         Guessed_Length := Guessed_Length * 2;
      end loop;
      Raise_POSIX_Error;
      return ""; --  to suppress compiler warning
   end Get_Working_Directory;

end POSIX.Process_Environment;

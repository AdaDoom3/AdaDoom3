--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
with
  Neo.Windows;
use
  Neo.Windows;
separate(Neo.System.Filesystem)
package Implementation_For_Operating_System
  is
  ----------------------
  -- Get_Library_Path --
  ----------------------
    function Get_Library_Path(
      Name : in String_2)
      return String_2
      is
      begin
        return "";
      end Get_Library_Path;
  -------------------
  -- Get_Base_Path --
  -------------------
    function Get_Base_Path
      return String_2
      is
      begin
        return "";
      end Get_Base_Path;
  -------------------
  -- Get_Save_Path --
  -------------------
    function Get_Save_Path
      return String_2
      is
      begin
        return "";
      end Get_Save_Path;
  -------------------------
  -- Get_Executable_Path --
  -------------------------
    function Get_Executable_Path
      return String_2
      is
      begin
        return "";
      end Get_Executable_Path;
  --------------------
  -- Get_Disk_Paths --
  --------------------
    function Get_Disk_Paths
      return Array_Access_String_2
      is
      begin
        return (null, null);
      end Get_Disk_Paths;
  --------------------------
  -- Get_Application_Path --
  --------------------------
    function Get_Application_Path(
      Application_Name : in String_2)
      return String_2
      is
      begin
        return "";
      end Get_Application_Path;
  ----------------------
  -- Create_Directory --
  ----------------------
    procedure Create_Directory(
      Path : in String_2)
      is
      begin
        null;
      end Create_Directory;
  --------------------
  -- Make_Directory --
  --------------------
    procedure Make_Directory(
      Path : in String_2)
      is
      begin
        null;
      end Make_Directory;
  ---------------
  -- Make_File --
  ---------------
    procedure Make_File(
      Path : in String_2;
      Data : in Array_Integer_1_Unsigned_C)
      is
      begin
        if Path > MAXIMUM_PATH_FOR_CREATE_FILE then
          raise System_Call_Failure;
        end if;
        File := 
          Create_File(
            Name                 => ,
            Desired_Access       => ,
            Share_Mode           => ,
            Security_Attributes  => ,
            Creation_Desposition => ,
            Flags_And_Attributes => ,
            Template_File        => );
        if File = NULL_ADDRESS then
          raise System_Call_Failure;
        end if;
        if
        Write_File(
          ) = FAILED
        then
          raise System_Call_Failure;
        end if;
        if
        Close_File(
          ) = FAILED
        then
          raise System_Call_Failure;
        end if;
      end Make_File;
    procedure Make_File(
      Path : in String_2;
      Text : in String_2)
      is
      New_Text : Access_String_2_C  := To_Acces_String_2_C(Text);
      Count    : Integer_4_Signed_C := 0;
      File     : Address            := NULL_ADDRESS;
      begin
        if Path > MAXIMUM_PATH_FOR_CREATE_FILE then
          raise System_Call_Failure;
        end if;
        if
        Convert_String_2_C_To_UTF_8(
          Code_Page                   => CODE_PAGE_UTF8,
          Flags                       => 0,
          Original_String             => New_Text,
          Character_Count_Of_Original => Integer_4_Signed_C(String_2'Length),
          Resulting_String            => null,
          Byte_Count_Of_Result        => Count,
          Default_Character           => null,
          Used_Default_Character      => null) = FAILED
        then
          raise System_Call_Failure;
        end if;
        File := 
          Create_File(
            Name                 => ,
            Desired_Access       => ,
            Share_Mode           => ,
            Security_Attributes  => ,
            Creation_Desposition => ,
            Flags_And_Attributes => ,
            Template_File        => );
        if File = NULL_ADDRESS then
          raise System_Call_Failure;
        end if;
        --------------------
        Create_UTF_8_String:
        --------------------
          declare
          Resulting_Text : aliased Array_Integer_1_Unsigned_C(1..Count) := (others => 0);
          begin
            if
            Convert_String_2_C_To_UTF_8(
              Code_Page                   => CODE_PAGE_UTF8,
              Flags                       => 0,
              Original_String             => New_Text,
              Character_Count_Of_Original => Integer_4_Signed_C(String_2'Length),
              Resulting_String            => Resulting_Text'Access,
              Byte_Count_Of_Result        => Count,
              Default_Character           => null,
              Used_Default_Character      => null) = FAILED
            then
              raise System_Call_Failure;
            end if;
            if
            Write_File(
              ) = FAILED
            then
              raise System_Call_Failure;
            end if;
          end Create_UTF_8_String;
        if
        Close_File(
          ) = FAILED
        then
          raise System_Call_Failure;
        end if;
      end Make_File;
    procedure Make_File(
      Path : in String_2;
      Text : in String_1)
      is
      begin
        if Path > MAXIMUM_PATH_FOR_CREATE_FILE then
          raise System_Call_Failure;
        end if;
        File := 
          Create_File(
            Name                 => ,
            Desired_Access       => ,
            Share_Mode           => ,
            Security_Attributes  => ,
            Creation_Desposition => ,
            Flags_And_Attributes => ,
            Template_File        => );
        if File = NULL_ADDRESS then
          raise System_Call_Failure;
        end if;
        if
        Write_File(
          ) = FAILED
        then
          raise System_Call_Failure;
        end if;
        if
        Close_File(
          ) = FAILED
        then
          raise System_Call_Failure;
        end if;
      end Make_File;
  -------------------
  -- Get_File_List --
  -------------------
    function Get_File_List(
      Path      : in String_2;
      Extension : in String_2 := NULL_STRING_2)
      return Array_Access_String_2
      is
      idStr   search;
      struct _finddata_t findinfo;
      int     findhandle;
      int     flag;
      Index : Integer_4_Signed := 0;
      begin
        if Extension(0) = '/' and Extension'Length = 1 then -- Passing a slash as extension will find directories
          Extension := "";
          Flag := 0;
        else
          Flag := ATTRIBUTE_SUB_DIRECTORY;
        end if;
        Text_IO.Put.All(TAB & "Get_File_List: Searching " & Directory)
        if Extension = NULL_STRING_2 then
          Text_IO.New_Line.All;
        else
          Text_IO.Put_Line(" for files of extension " & Extension & ".");
        end if;
        -- A bit ineffiecient
        Find_Handle := Find_First(Search, Find_Information'Address);
        -- if Find_Handle = -1 then
        --   return -1;
        -- end if;
        loop
          if Flag xor (Find_Information.Attribute and ATTRIBUTE_SUB_DIRECTORY) /= 0 then
            Index := Index + 1;
          end if;
          exit when Find_Next(Find_Handle, Find_Information'Address) /= -1;
        end loop;
        ---------------
        Compose_Result:
        ---------------
          declare
          Result : Array_String_2(1..Index + 1) := (others => null);
          begin
            if Index /= 0 then
              Find_Handle := Find_First(Search, Find_Information'Address);
              -- if Find_Handle = -1 then
              --   return -1;
              -- end if;
              Index := 1;
              loop
                if Flag xor (Find_Information.Attribute and ATTRIBUTE_SUB_DIRECTORY) /= 0 then
                  Index := Index + 1;
                  Result(Index + 1
                end if;
                exit when Find_Next(Find_Handle, Find_Information'Address) /= -1;
              end loop;
            end if;
            Find_Close(Find_Handle);
            return Result;
          end Compose_Result;
        return (null, null);
      end Get_File_List;
  ---------------------------
  -- Search_Subdirectories --
  ---------------------------
    function Search_Subdirectories(
      Path : in String_2)
      return Array_Access_String_2
      is
      begin
        return (null, null);
      end Search_Subdirectories;
  ---------------------
  -- Is_In_Directory --
  ---------------------
    function Is_In_Directory(
      Path : in String_2)
      return Boolean
      is
      begin
        return False;
      end Is_In_Directory;
  end Implementation_For_Operating_System;

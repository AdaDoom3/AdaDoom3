package body Neo.File is
    package body Handler is
      procedure Save    (Path : in String_2; Item : in Type_To_Handle);
      procedure Save    (Path : in String_2; Item : in Array_Type_To_Handle);
      procedure Convert (Path : in String_2;             Format : in Discrete_Format);
      function Convert  (Path : in String_2;             Format : in Discrete_Format) return Type_To_Handle;
      function Convert  (Path : in String_2;             Format : in Discrete_Format) return Array_Type_To_Handle;
      function Convert  (Item : in Type_To_Handle;       Format : in Discrete_Format) return Array_Stream_Element;
      function Convert  (Item : in Type_To_Handle;       Format : in Discrete_Format) return Type_To_Handle;
      function Convert  (Item : in Array_Type_To_Handle; Format : in Discrete_Format) return Array_Type_To_Handle;
      function Load     (Item : in Stream_Element_Array)                              return Type_To_Handle;
      function Load     (Item : in Stream_Element_Array)                              return Array_Type_To_Handle;
      function Load     (Path : in String_2)                                          return Type_To_Handle;
      function Load     (Path : in String_2)                                          return Array_Type_To_Handle;
    end Handler;






    
    procedure Get_Possible_Formats(Path    : in String_2;  Formats : in Array_Record_Format) return Array_Integer_4_Signed is
      type Vector_Enumerated_Format is new Ada.Containers.Vector;
      Matching_Formats : Vector_Enumerated_Format;
      Extension        : String_2       := Get_Extension(Path);
      Header           : String_1(1..8) := (others => NULL_CHARACTER_1);
      begin
        for I in FORMATS'range loop
          if FORMATS(I).Extensions /= null then
            for J in FORMATS(I).Extensions'range loop
              exit when To_String_2_Unbounded(FORMATS(I).Extensions(J)) = To_String_2_Unbounded(Extension);
            end loop;
          end if;
          if I = FORMATS'last then
            Put_Debug_Line(Localize(WARN_ITEM_EXTENSION_NOT_KNOWN));
          end if;
        end loop;
        if
        String_1'read(image.stream, Signature);
        for I in FORMATS'range loop
          if FORMATS(I).Signatures = null then
            Matching_Formats.Add_Back(I);
          else
            for J in FORMATS(I).Signatures'range loop
              if To_String_2_Unbounded(FORMATS(I).Signatures(J)) = To_String_2_Unbounded(Signature) then
                Matching_Formats.Add_Front(I);
                exit;
              end if;
            end loop;
          end if;
        end loop;
        if Size(Matching_Formats) = 0 then
          raise Unknown_Format;
        end if;
      end Get_Possible_Formats;
    ---------------
    procedure Save(
    ---------------
      Path     : in String_2;
      Graphics : in Array_Record_Graphic)
      is
      Format : Enumerated_Format := Get_Specifics(Graphics(1)).Specifics.Format;
      begin
        for I in Graphics'range loop
          if Get_Specifics(Graphics(I)).Specifics.Format /= Format then
            raise Attempted_To_Save_Under_Multiple_Formats;
          end if;
        end loop;
        Save(Find(Format)(1), Path, Data);
      end Save;
    --------------
    function Load(
    --------------
      Path        : in String_2;
      Start_Frame : in Integer_4_Positive := 1)
      return Array_Record_Graphic
      is
      function Get_Possible_Image_Formats
        is new Get_Possible_Formats(Enumerated_Format, FORMATS);
      Formats : Array_Format := Get_Possible_Image_Formats(Path);
      begin
        for I in Formats'range loop
          ------------
          Search_Tags:
          ------------
            declare
            Tags : Array_Tag := Find(Formats(I));
            begin
              for J in Tags'range loop
                if I = Formats'last and J = Tags'last then
                  return Load(Tags(J), Path);
                end if;
                --------
                Attempt:
                --------
                  begin
                    return Load(Tags(J), Path);
                  exception
                    when others =>
                      null;
                  end Attempt;
              end loop;
            end Search_Tags;
        end loop;
      end Load;
    function Find(
      Format : in Enumerated_Format)
      return Array_Tag
      is
      type Vector_Enumerated_Format
        is new Ada.Containers.Vector();
      Format  : Enumerated_Format := Get(Path);
      Results : Vector_Enumerated_Format;
      Formats : array(1..7) of Tag :=);
      begin
        for I in Formats'range loop
          if Format = Get(Formats(I)) then
            Results.Add(Get(Formats(I)));
          elsif Size(Results) < 1 and then I = Formats'last then
            raise Unsupported_Format;
          end if;
        end loop;
        return To_Array(Results);
      end Find;
  end Neo.File;
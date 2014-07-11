package Neo.File is
    Unknown     : Exception;
    Currupt     : Exception;
    Unsupported : Exception;
    type Record_Format is record
        Extensions : Access_Array_String_2;
        Signatures : Access_Array_String_2;
      end record;
    type Array_Record_Format is array(Positive range <>) of Record_Format;
    generic
      type Type_To_Handle;
      type Discrete_Format is (<>);
      Formats : Array_Record_Format;
      Loaders : array(Discrete_Format'range) of access function(Data : in Array_Stream_Element) return Type_To_Handle;
      Savers  : array(Discrete_Format'range) of access function(Item : in Type_To_Handle)       return Array_Stream_Element;
    package Handler is
      procedure Save    (Path : in String_2; Item : in Type_To_Handle);
      procedure Save    (Path : in String_2; Item : in Array_Type_To_Handle);
      procedure Convert (Path : in String_2;             Format : in Discrete_Format);
      function Convert  (Path : in String_2;             Format : in Discrete_Format) return Type_To_Handle;
      function Convert  (Path : in String_2;             Format : in Discrete_Format) return Array_Type_To_Handle;
      function Convert  (Item : in Array_Type_To_Handle; Format : in Discrete_Format) return Array_Stream_Element;
      function Convert  (Item : in Type_To_Handle;       Format : in Discrete_Format) return Array_Stream_Element;
      function Load     (Item : in Stream_Element_Array)                              return Type_To_Handle;
      function Load     (Item : in Stream_Element_Array)                              return Array_Type_To_Handle;
      function Load     (Path : in String_2)                                          return Type_To_Handle;
      function Load     (Path : in String_2)                                          return Array_Type_To_Handle;
    end Handler;
    procedure Save      (Path : in String_2; Item : in Array_Stream_Element);
    function Load       (Path : in String_2)      return Array_Stream_Element;
    function Find_Paths (Extension : in String_2) return Array_String_2;
    -- Hash funcitons?
private
    function Get_Possible_Formats(Path : in String_2) return Array_Format;
    function Get_Possible_Formats(Data : in Stream_Element_Array) return Array_Format;
  end Neo.File;
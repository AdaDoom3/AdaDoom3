package Neo.World.Menu is
  type Enumerated_Element  is (Text_Element, Bind_Element, List_Element, Tile_Element, Edit_Element, Choice_Element, Slider_Element);
  type Enumerated_Position is (Top_Left_Position,    Top_Center_Position,    Top_Right_Position,
                               Center_Left_Position, Center_Position,        Center_Right_Position,
                               Bottom_Left_Position, Bottom_Center_Position, Bottom_Right_Position);
  type Record_Element(Kind : in Enumerated_Element) is record
      X                 : Integer_4_Signed    := 0;
      Y                 : Integer_4_Signed    := 0;
      Width             : Integer_4_Positive  := 1;
      Height            : Integer_4_Positive  := 1;
      Color             : Record_Color        := COLOR_BLACK;
      Opacity           : Float_4_Percent     := 100.0;
      Rotation          : 
      Position          : Enumerated_Position := Top_Left_Position;
      Background        : String_2_Unbounded  := NULL_STRING_2_UNBOUNDED;
      Do_Show           : Boolean             := True;
      Do_Fit_Background : Boolean             := False;
      case Kind is
        when List_Element =>
          Scrollbar_X        : Integer_4_Signed    := 0;
          Scrollbar_Y        : Integer_4_Signed    := 0;
          Scrollbar_Top      : String_2_Unbounded  := NULL_STRING_2_UNBOUNDED;
          Scrollbar_Middle   : String_2_Unbounded  := NULL_STRING_2_UNBOUNDED;
          Scrollbar_Bottom   : String_2_Unbounded  := NULL_STRING_2_UNBOUNDED;
          Scrollbar_Slider   : String_2_Unbounded  := NULL_STRING_2_UNBOUNDED;
          Scrollbar_Position : Enumerated_Position := Top_Left_Position;
        when Choice_Element | Text_Element | Bind_Element | Edit_Element =>
          Font          : String_2_Unbounded  := NULL_STRING_2_UNBOUNDED;
          Text_Size     : Integer_4_Positive  := 12;
          Text_Color    : Record_Color        := COLOR_WHITE;
          Text_Position : Enumerated_Position := Center_Position;
          case Kind is
            when Bind_Element =>
              Reference_Bind : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
            when Choice_Element | Edit_Element | Enumerated_Position =>
              Reference_Variable : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
              case Kind is
                when Choice_Element =>
                  Arrow_Next : 
                  Arrow_Previous :
              when others => null; end case;
          when others => null; end case;
      when others => null; end case;
    end record;
  procedure Add(Parent : ; Element : in Record_Element);
  procedure Remove(Element : in );
private
  
end Neo.World.Menu;

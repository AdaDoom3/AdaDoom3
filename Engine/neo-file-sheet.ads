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
package Neo.File.Sheet
  is
  ---------------
  -- Constants --
  ---------------
    ILLEGAL_SYMBOLS               : constant String_2       := "/,.<>;:[]{}\|_)(*&^%$#@!~`-+=";
    DELINEATING_SYMBOLS           : constant String_2       := """'`";
    SINGLE_LINE_COMMENT_PREFIXES  : constant Array_String_2 :=
      new Array_String_2(
        new String_2("//"),
        new String_2("#"),
        new String_2(";"));
  ------------------
  -- Enumerations --
  ------------------
    type Enumerated_Format
      is(
      Comma_Separated_Values_Format,
      --
      -- 1967 International Business Machines
      --      http://web.archive.org/web/20120818015507/http://tools.ietf.org/html/rfc4180
      --
      Initialization_Format,
      --
      -- 1981 Microsoft
      --      http://web.archive.org/web/20130522030131/http://docs.services.mozilla.com/server-devguide/confspec.html
      --
      Configuration_Format);
      --
      -- 2003 Id Software
      --      Mapping player input is accomplished through the use of the bind console command which accepts console
      --      commands, variables, and impluses as valid input.
      --
      --      The syntax is defined below in BNF (Backus Naur Form)
      --      http://web.archive.org/web/20130514163308/http://cui.unige.ch/db-research/Enseignement/analyseinfo/AboutBNF.html
      --
      --      <number>   - number character
      --      <anything> - printable character
      --      <legal>    - character that is not in ILLEGAL_SYMBOLS
      --      <ascii>    - ascii alphabetic character
      --      <space>    - space or tab character
      --      <endline>  - optional carrage return character followed by a line feed character
      --
      --      <impulse> ::=
      --        <ascii>             | <number>           | kp<number>        |
      --        _attack[{<number>}] | _impulse{<number>} | _button{<number>} |
      --        _speed              | _strafe            | _zoom             | togglemenu |
      --        _back               | _forward           | _left             | _right     |
      --        _movedown           | _moveup            | _moveleft         | _moveright |
      --        _lookdown           | _lookup            | _mlook            |
      --
      --      <end>                               ::= ;
      --      <comment>                           ::= //
      --      <seporation>                        ::= {<space>}
      --      <spacing>                           ::= [<seporation>]
      --      <setvariable>                       ::= {<legal>} <seporation> {<number>}
      --      <setvariableorcreate>               ::= set  <seporation> <setvariable>
      --      <setvariableorcreateandthenarchive> ::= seta <seporation> <setvariable>
      --      <command>                           ::= {<setvariable> | <setvariableorcreate> | <setvariableorcreateandthenarchive>}
      --      <commandlist>                       ::= <command> | {<command> <end>}
      --      <bind>                              ::= bind <seporation> <impulse> <seporation> "<commandlist>" [<end>]
      --      <configurationformat>               ::= [{<spacing> [<bind> | <commandlist>] <spacing> [<comment> <anything>] <endline>}]
      --
  -------------
  -- Records --
  -------------
    type Record_Data(
      Format : Enumerated_Format := Comma_Separated_Values_Format)
      is record
        Entries : Vector_Vector_String_2;
        case Format is
          when Initialization_Format =>
            Heading : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
          when others =>
            null;
        end case;
      end record;
  ------------
  -- Arrays --
  ------------
    type Array_Record_Data
      is array(Positive range <>)
      of Record_Data;
  -----------------
  -- Subprograms --
  -----------------
    function Load(
      Path : in String_2)
      return Array_Record_Data;
    procedure Save(
      Path   : in String_2;
      Format : in Array_Record_Data);
-------
private
-------
  ---------------
  -- Constants --
  ---------------
    FORMATS : Array_Record_Format(Enumerated_Format'range) :=(
      Comma_Separated_Values_Format =>(
        Signatures => null,
        Extensions =>
          new Array_String_2(
            new String_2("CSV")),
      Initialization_Format =>(
        Signatures => null,
        Extensions =>
          new Array_String_2(
            new String_2("INI"),
            new String_2("CONF"),
            new String_2("CFG")),
      Configuration_Format =>(
        Signatures => null,
        Extensions =>
          new Array_String_2(
            new String_2("RC"),
            new String_2("CONF"),
            new String_2("CFG")));
  --------------
  -- Packages --
  --------------
    package CSV
      is
        function Load(
          Path : in String_2)
          return Array_Record_Graphic;
        procedure Save(
          Path    : in String_2;
          Graphic : in Array_Record_Graphic);
      end CSV;
    package INI
      is
        function Load(
          Path : in String_2)
          return Array_Record_Graphic;
        procedure Save(
          Path    : in String_2;
          Graphic : in Array_Record_Graphic);
      end INI;
    package CFG
      is
        function Load(
          Path : in String_2)
          return Array_Record_Graphic;
        procedure Save(
          Path    : in String_2;
          Graphic : in Array_Record_Graphic);
      end CFG;
  --------------------
  -- Implementation --
  --------------------
    package Instantiation
      is new Implementation(
        Type_To_Handle => Neo.File.Sheet.Record_Data,
        Format         => Neo.File.Sheet.Enumerated_Format,
        Formats        => Neo.File.Sheet.FORMATS
        Operations     =>(
          Flexible_Image_Transfer_System_Format        => (CSV.Load'access, CSV.Save'access),
          Portable_Network_Graphics_Format             => (INI.Load'access, INI.Save'access),
          Joint_Photographic_Experts_Group_2000_Format => (CFG.Load'access, CFG.Save'access));
  end Neo.File.Sheet;

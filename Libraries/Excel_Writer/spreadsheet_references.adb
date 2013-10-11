package body Spreadsheet_references is

  ---------------
  -- Reference --
  ---------------

  function Reference(
    row, column: Positive;
    style      : Reference_style:= A1
  )
  return String
  is
    rs: constant String:= Positive'Image(row);
    cs: constant String:= Positive'Image(column);
    -- Skip the @#*$! leading space...
    r: constant String:= rs(rs'First+1..rs'Last);
    c: constant String:= cs(cs'First+1..cs'Last);
    -- A bit tricky: 'A'..'Z' are not like digits,
    -- or rather like digits without a zero (1..9)!
    --
    -- You have exactly (26**n - 1) * 26 / 25 combinations
    -- for a code with 1 to n letters!

    function Base_26(n: Natural) return String is
    begin
      if n <= 25 then
        return (1 => Character'Val(Character'Pos('A') + n));
      else
        return Base_26(n / 26 - 1) & Base_26(n mod 26);
      end if;
    end Base_26;
  begin
    case style is
      when A1 =>
        return Base_26(column - 1) & r;
      when R1C1 =>
        return 'R' & r & 'C' & c;
    end case;
  end Reference;

  ---------
  -- Row --
  ---------

  function Row (reference: String) return Positive is
    r, c: Positive;
  begin
    Split(reference, r, c);
    return r;
  end Row;

  ------------
  -- Column --
  ------------

  function Column (reference: String) return Positive is
    r, c: Positive;
  begin
    Split(reference, r, c);
    return c;
  end Column;

  -----------
  -- Split --
  -----------

  procedure Split (reference: String; row, column: out Positive) is
    phase: Positive range 1..4:= 1;
    r, c, d, cc: Natural:= 0;
    s: Character;
  begin
    if reference = "" then
      raise Invalid_spreadsheet_reference;
    end if;
    for i in reference'Range loop
      s:= reference(i);
      if s in 'a'..'z' then
        s:= Character'Val(Character'Pos(s) - Character'Pos('a') + Character'Pos('A'));
      end if;
      case s is
        when '0'..'9' =>
          case phase is
            when 1 =>
              if i = reference'First then
                raise Invalid_spreadsheet_reference; -- cannot start with a digit
              end if;
              phase:= 2;
            when 2 | 4 =>
              null; -- already in a digit
            when 3 => -- We begin the column in R1C1 style
              if c /= 18 or cc /= 3 then
                -- 1st letter code must be exactly "R" and 2nd must be "C"
                raise Invalid_spreadsheet_reference;
              end if;
              c:= 0;
              phase:= 4;
          end case;
          d:= Character'Pos(s) - Character'Pos('0');
          case phase is
            when 1 | 3 =>
              null; -- we never get here.
            when 2 =>
              r:= r * 10 + d;
            when 4 =>
              c:= c * 10 + d;
          end case;
        when 'A'..'Z' =>
          case phase is
            when 1 | 3 =>
              null; -- already in a letter code
            when 2 =>
              phase:= 3;
            when 4 => -- not a 3rd letter code!
              raise Invalid_spreadsheet_reference;
          end case;
          d:= Character'Pos(s) - Character'Pos('A') + 1;
          case phase is
            when 2 | 4 =>
              null; -- we never get here.
            when 1 =>
              c:= c * 26 + d;
            when 3 =>
              cc:= cc * 26 + d;
          end case;
        when others =>
          raise Invalid_spreadsheet_reference;
      end case;
    end loop;
    row:= r;
    column:= c;
  end Split;

end Spreadsheet_references;

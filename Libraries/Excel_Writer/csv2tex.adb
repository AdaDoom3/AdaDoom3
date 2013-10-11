------------------------------------------------------------------------------
--  File:            CSV2TeX.adb
--  Description:     Converts CSV (text with Comma Separated Values) input
--                   into LaTeX array output. NB: the special characters
--                   like '%', '\', '&', '{',... should be translated before!
--                   CSV is "the" ASCII format for Lotus 1-2-3 and MS Excel
--  Date / Version:  21-Jan-2010 ; 2-Mar-2004 ; 22-Apr-2003
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Ada.Text_IO, Ada.Strings.Fixed;
with CSV; -- replaces CSV_Parser

procedure CSV2TeX is
  use Ada.Text_IO, Ada.Strings;
  l: Integer;
  s: String(1..1000);
  first: Boolean:= True;
begin
  while not End_of_File(standard_input) loop
    Get_Line(s,l);
    declare
      line: String renames s(1..l);
      bds: constant CSV.Fields_Bounds:= CSV.Get_Bounds( line, ';' );
    begin
      if first then
        Put_Line("% Array translated by CSV2TeX");
        Put_Line("% Check http://excel-writer.sourceforge.net/ ,");
        Put_Line("% in the ./extras directory");
        Put("\begin{array}{");
        for i in bds'Range loop
          Put('c');
          if i < bds'Last then
            Put('|');
          end if;
        end loop;
        Put_Line("} % array description");
        first:= False;
      end if;
      for i in bds'Range loop
        Put(
          Ada.Strings.Fixed.Trim(
            CSV.Unquote(CSV.Extract(line, bds, i)), Both
          )
        );
        if i < bds'Last then
          Put("&");
        end if;
      end loop;
    end;
    Put_Line("\\");
  end loop;
  Put_Line("\end{array}");
end CSV2TeX;
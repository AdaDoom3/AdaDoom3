with Text_IO;                           use Text_IO;
with Spreadsheet_references;            use Spreadsheet_references;

procedure Test_Spreadsheet_references is
  ii, jj, n0: Positive;
begin
  Split("xfd1234", ii, jj);
  Put_Line(Integer'Image(ii) & Integer'Image(jj));
  for i in 1..256 loop
    Put_Line(Reference(i,i));
  end loop;
  -- Excel 2007 and later has 16384 = 2**16 instead of 256 = 2**8 columns
  for i in 700..710 loop -- ZZ=702, AAA=703
    Put_Line(Reference(i,i));
  end loop;
  for i in 16382..16386 loop -- XFD=16384
    Put_Line(Reference(i,i) & " = " & Reference(i,i,R1C1));
  end loop;
  for n in 1..5 loop
    n0:= (26**n - 1) * 26 / 25;
    for i in n0-2..n0+2 loop
      Put_Line(Reference(i,i) & " = " & Reference(i,i,R1C1));
    end loop;
  end loop;
  for i in 1..123 loop
    for j in 1..16389 loop
      for style in Reference_style loop
        Split(Reference(i,j,style),ii,jj);
        if i /= ii or j /= jj then
          raise Program_Error;
        end if;
      end loop;
    end loop;
  end loop;
end Test_Spreadsheet_references;

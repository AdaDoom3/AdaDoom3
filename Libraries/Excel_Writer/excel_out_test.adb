-- This test procedure for Excel_Outis in the Ada 95 syntax,
-- for compatibility with a larger number of development systems.
-- With Ada 2005 and later, you can also write "xl.Write(...)" etc. everywhere.
--

with Excel_Out;                         use Excel_Out;

with Ada.Calendar, Ada.Streams.Stream_IO;

procedure Excel_Out_Test is

  procedure Small_demo is
    xl: Excel_Out_File;
  begin
    Create(xl, "Small.xls");
    Put_Line(xl, "This is a small demo for Excel_Out");
    for row in 3 .. 8 loop
      for column in 1 .. 8 loop
        Write(xl, row, column, row * 1000 + column);
      end loop;
    end loop;
    Close(xl);
  end Small_demo;

  procedure Big_demo is
    xl: Excel_Out_File;
    font_1, font_2, font_3, font_4, font_5, font_6: Font_type;
    fmt_1, fmt_2, fmt_3, fmt_4, fmt_5, fmt_6, fmt_7, fmt_8: Format_type;
    custom_num: Number_format_type;
  begin
    Create(xl, "Big.xls");
    -- Some page layout...
    Header(xl, "Big demo");
    Footer(xl, "&D");
    Margins(xl, 1.2, 1.1, 0.9, 0.8);
    Print_Row_Column_Headers(xl);
    Print_Gridlines(xl);
    --
    Write_default_column_width(xl, 7);
    Write_column_width(xl, 1, 15); -- set to width of 15 times '0'
    Write_column_width(xl, 5, 11);
    Write_column_width(xl, 14, 0); -- hide this column
    --
    Write_default_row_height(xl, 20);
    Write_row_height(xl, 13, 0);   -- hide this row
    Write_row_height(xl, 100, 30);
    --
    Define_font(xl, "Arial", 10, font_1, regular, blue);
    Define_font(xl, "Courier New", 12, font_2, bold & italic, red);
    Define_font(xl, "Times New Roman", 14, font_3, bold);
    Define_font(xl, "Arial Narrow", 16, font_4, bold);
    Define_font(xl, "Calibri", 16, font_5, bold, red);
    Define_font(xl, "Calibri", 9, font_6);
    --
    Define_number_format(xl, custom_num, "0.000000"); -- 6 decimals
    --
    Define_format(xl, font_1, percent_0, fmt_1, centred, right);
    Define_format(xl, font_2, decimal_2, fmt_2);
    Define_format(xl, font_3, decimal_0, fmt_3, centred);
    Define_format(xl, font_4, general,   fmt_4, border => top & bottom);
    Define_format(xl, font_1, percent_2_plus, fmt_5, centred, right);
    Define_format(xl, font_5, general,   fmt_6, border => box);
    Define_format(xl, font_1, custom_num,  fmt_7, centred);
    Define_format(xl, font_6, general, fmt_8);
    --
    Use_format(xl, fmt_4);
    Put(xl, "This is a big demo for Excel Writer / Excel_Out");
    Merge(xl, 6);
    Next(xl);
    Put(xl, "Version: " & version);
    Merge(xl, 1);
    Jump_to(xl, 1, 13);
    Put(xl, "Ref.: " & reference);
    Merge(xl, 3);

    Use_format(xl, fmt_2);
    for column in 1 .. 9 loop
      Write(xl, 2, column, Long_Float(column) + 0.5);
    end loop;
    Use_format(xl, fmt_8);
    Put(xl, "  <- = column + 0.5");

    Use_format(xl, fmt_3);
    for row in 4 .. 7 loop
      for column in 1 .. 9 loop
        Write(xl, row, column, row * 1000 + column);
      end loop;
    end loop;
    Use_format(xl, fmt_8);
    Put(xl, "  <- = row * 1000 + column");

    Use_format(xl, fmt_4);
    for column in 1 .. 20 loop
      Write(xl, 9, column, Character'Val(64 + column) & "");
    end loop;

    Use_format(xl, fmt_6);
    Write(xl, 11, 1, "Calibri font");
    Use_format(xl, fmt_8);
    Write(xl, 11, 4, "First number:");
    Write(xl, 11, 6, Long_Float'First);
    Write(xl, 11, 8, "Last number:");
    Write(xl, 11, 10, Long_Float'Last);
    Write(xl, 11, 12, "Smallest number:");
    Write(xl, 11, 15, (1.0+Long_Float'Model_Epsilon) * Long_Float'Model_Small);

    for row in 13 .. 300 loop
      Use_format(xl, fmt_1);
      Write(xl, row, 3, Long_Float(row) * 0.01);
      Use_format(xl, fmt_5);
      Put(xl, Long_Float(row-100) * 0.001);
      Use_format(xl, fmt_7);
      Put(xl, Long_Float(row-13) + 0.123456);
    end loop;
    Close(xl);
  end Big_demo;

  function My_nice_sheet(size: Positive) return String is
    xl: Excel_Out_String;
  begin
    Create(xl);
    Put_Line(xl, "This Excel file is fully created in memory.");
    Put_Line(xl, "It can be stuffed directly into a zip stream,");
    Put_Line(xl, "or sent from a server!");
    Put_Line(xl, "- see ZipTest @ unzip-ada or zip-ada");
    for row in 1 .. size loop
      for column in 1 .. size loop
        Write(xl, row + 5, column, 0.01 + Long_Float(row * column));
      end loop;
    end loop;
    Close(xl);
    return Contents(xl);
  end My_nice_sheet;

  procedure String_demo is
    use Ada.Streams.Stream_IO;
    f: File_Type;
  begin
    Create(f, Out_File, "From_string.xls");
    String'Write(Stream(f), My_nice_sheet(200));
    Close(f);
  end String_demo;

  procedure Speed_test is
    xl: Excel_Out_File;
    use Ada.Calendar;
    t0, t1: Time;
    iter: constant:= 1000;
    size: constant:= 150;
    secs: Long_Float;
  begin
    Create(xl, "Speed_test.xls");
    t0:= Clock;
    for i in 1..iter loop
      declare
        dummy: constant String:= My_nice_sheet(size);
      begin
        if dummy = "" then
          null;
        end if;
      end;
    end loop;
    t1:= Clock;
    secs:= Long_Float(t1-t0);
    Put_Line(xl,
      "Time (seconds) for creating" &
      Integer'Image(iter) & " sheets with" &
      Integer'Image(size) & " x" &
      Integer'Image(size) & " =" &
      Integer'Image(size**2) & " cells"
    );
    Put_Line(xl, secs);
    Put_Line(xl, "Sheets per second");
    Put_Line(xl, Long_Float(iter) / secs);
    Close(xl);
  end Speed_test;

begin
  Small_demo;
  Big_demo;
  String_demo;
  Speed_test;
end Excel_Out_Test;

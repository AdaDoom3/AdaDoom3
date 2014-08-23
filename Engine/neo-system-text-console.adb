package body Neo.System.Text.Console is
  package body Import is separate;
  function Is_Running return Boolean is begin return Main_Task.Is_Running;                                                                           end Is_Running;
  procedure Send_Log                 is begin Open_Webpage(ERROR_REPORTING_URL); exception when others => Put_Debug_Line(Localize(FAILED_SEND_LOG)); end Send_Log;
  procedure Save_Log is
    File        : File_Type;
    File_Stream : Stream_Access;
    Buffer      : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
    Offset      : Time_Offset        := UTC_Time_Offset(Clock);
    Path        : String_1           := To_String_1(To_String_2(SPECIFICS.Username)) & "_" &
      Trim(Month_Number'image(Month(Clock, Offset)), Both) & "-" &Trim( Day_Number'image(Day(Clock, Offset)), Both)  & "-" & Trim(Year_Number'image(Year(Clock, Offset)), Both)  & "_" &
      Trim(Hour_Number'image(Hour(Clock, Offset)), Both)   & "-" & Trim(Minute_Number'image(Minute(Clock, 0)), Both) & "-" & Trim(Second_Number'image(Second(Clock)), Both) & ".txt";
    begin
      Create(File, Out_File, To_String_1(To_String_2(Neo.System.SPECIFICS.Path) & SPECIFICS.Separator & PATH_LOGS & SPECIFICS.Separator) & Path);
      File_Stream := Stream(File);
      Buffer := To_String_2_Unbounded(Get_Log);
      for I in 1..Length(Buffer) loop Character_2'write(File_Stream, Element(Buffer, I)); end loop;
      Close(File);
      Open_Text(To_String_2(Neo.System.SPECIFICS.Path) & SPECIFICS.Separator & PATH_LOGS & SPECIFICS.Separator & To_String_2(Path));
    exception when Call_Failure => Put_Debug_Line(Localize(FAILED_SAVE_LOG));
    end Save_Log;
  procedure Initialize is begin Main_Task.Initialize; end Initialize;
  procedure Finalize is begin Main_Task.Finalize; end Finalize;
  procedure Run is
    begin
      Import.Run;
    exception when Call_Failure => Put_Debug_Line(Localize(FAILED_INITIALIZE));
    end Run;
end Neo.System.Text.Console;

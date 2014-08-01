package body Neo.System.Text.Console is
    procedure Test is
      begin
        Put_Title(Localize("TEXT CONSOLE TEST"));
        Initialize;
        --Finalize;
        --delay 1000.0;
        --Finalize;
      end Test;
    package body Import is separate;
    package Instantiation is new Import(Save_Log, Send_Log, Save_Log);
    function Is_Running return Boolean is begin return Main_Task.Is_Running; end Is_Running;
    procedure Send_Log is begin Open_Webpage(ERROR_REPORTING_URL); exception when others => Put_Debug_Line(Localize(FAILED_SEND_LOG)); end Send_Log;
    procedure Copy_Log is begin Set_Clipboard(Get_Log);            exception when others => Put_Debug_Line(Localize(FAILED_COPY_LOG)); end Copy_Log;
    procedure Save_Log is
      File   : File_Type;
      Offset : Time_Offset := UTC_Time_Offset(Clock);
      Path   : String_1    := To_String_1(PATH_LOGS & To_String_2(SPECIFICS.Username)) & "_" &
        Trim(Month_Number'image(Month(Clock, Offset)), Both) & "-" &Trim( Day_Number'image(Day(Clock, Offset)), Both)  & "-" & Trim(Year_Number'image(Year(Clock, Offset)), Both)  & "_" &
        Trim(Hour_Number'image(Hour(Clock, Offset)), Both)   & "-" & Trim(Minute_Number'image(Minute(Clock, 0)), Both) & "-" & Trim(Second_Number'image(Second(Clock)), Both) & ".txt";
      begin
        Create(File, Out_File, Path);
        Put_Line(File, Get_Log);
        Close(File);
        Open_Text(To_String_2(Path));
      exception when Call_Failure => Put_Debug_Line(Localize(FAILED_SAVE_LOG));
      end Save_Log;
    procedure Initialize is
      begin
        if not Is_Running then Main_Task.Initialize; else Put_Debug_Line(Localize(FAILED_ALREADY_OPEN)); end if;
      end Initialize;
    procedure Finalize is
      begin
        if Is_Running then Main_Task.Finalize; else Put_Debug_Line(Localize(FAILED_NOT_OPEN)); end if;
      end Finalize;
    procedure Run is
      begin
        begin
          Instantiation.Run;
        --exception when Call_Failure => Put_Debug_Line(Localize(FAILED_INITIALIZE));
        end;
        Main_Task.Finalize;
      end Run;
  end Neo.System.Text.Console;

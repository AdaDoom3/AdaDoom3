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
with
  Neo.Foundation.Output,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
use
  Neo.Foundation.Output,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
package Neo.Library.Timers
  is
  ----------------
  -- Exceptions --
  ----------------
    Timer_Started_Before_Being_Stopped  : Exception;
    Timer_Stopped_Without_Being_Started : Exception;
  -------------
  -- Records --
  -------------
    type Record_Timer
      is private;
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    function Get_Clock_Ticks(
      Timer : in Record_Timer)
      return Integer_8_Natural;
    function Get_Milliseconds(
      Timer : in Record_Timer)
      return Integer_8_Natural;
    procedure Start(
      Timer : in Record_Timer);
    procedure Stop(
      Timer : in Record_Timer);
    procedure Sleep_For_Seconds(
      Number_Of_Seconds : in Integer_8_Positive);
    procedure Sleep_For_Milliseconds(
      Number_Of_Milliseconds : in Integer_8_Positive);
    function "+"(
      Left  : in Record_Timer;
      Right : in Record_Timer)
      return Record_Timer;
    function "-"(
      Left  : in Record_Timer;
      Right : in Record_Timer)
      return Record_Timer;
-------
private
-------
  -------------
  -- Records --
  -------------
    type Record_Timer
      is record
        Base       : Float_8_Natural := 0.0;
        Start_Time : Float_8_Natural := 0.0;
        Is_Stopped : Boolean         := False;
      end record;
  end Neo.Library.Timers;

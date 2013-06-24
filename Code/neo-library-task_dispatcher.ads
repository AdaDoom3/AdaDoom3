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
  System,
  System.Storage_Elements,
  Neo.System.Processor,
  Neo.Foundation.Output,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
use
  System,
  System.Storage_Elements,
  Neo.System.Processor,
  Neo.Foundation.Output,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
package Neo.Library.Task_Dispatcher
  ---------------
  -- Constants --
  ---------------
    RECOMMENDED_JOB_EXECUTION_TICK_MIMIMUM    : constant := 1_000;
    RECOMMENDED_JOB_EXECUTION_TICK_MAXIMUM    : constant := 100_000;
    DO_PUT_WARNING_IF_JOB_VIOLATES_TICK_RANGE : constant Boolean := True;
    DEFAULT_JOB_PRIORITY                      :
    STORAGE_SIZE                              : constant Storage_Count    := ;
    PRIORITY_MAXIMUM                          : constant Priority         := Priority'last;
    WORKER_STORAGE_SIZE_DEFAULT               : constant Integer_4_Signed := 16#200_000#;
    WORKER_STORAGE_SIZE_MINIMUM               : constant Integer_4_Signed := 16#4_000#;
  -------------
  -- Records --
  -------------
    type Record_Status
      is record
        Number_Of_Workers_Total      : Positive_Worker_Count := ;
        Number_Of_Workers_Avaliable  : Worker_Count_Type     := ;
        Number_Of_Executed_Jobs      : := ;
        Time_Most_Recent_Job_Started : Time := ;
        Duration_Average_Job_Wasted  : Duration := 0.0;
        Duration_Average_Job_Total   : Duration := 0.0;
        Worker_Stack_Size            : Storage_Count := ;
      end record;
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    procedure Submit(
      Job      : in Access_Procedure;
      Priority : in Any_Priority := DEFAULT_JOB_PRIORITY;
      Name     : in String_2     := NULL_STRING_2);
    function Get_Status
      return Record_Status;
-------
private
-------
  ---------------
  -- Protected --
  ---------------
    protected type Protected_Data
      is
        function Get_Status
          return Record_Status;
        procedure Set_Number_Of_Workers(
          Workers : in );
        procedure Set_Number_Of_Avaliable_Workers (
          Avaliable_Workers : in );
        procedure Set_Number_Of_Executed_Jobs(
          Executed_Jobs : in );
        procedure Set_Last_Submission_Time_Started(
          Time_Started : in );
        procedure Set_Worker_Stack_Size(
          Stack_Size : in );
        procedure Add_To_Average_Job_Duration(
          Average : in );
        procedure Add_To_Average_Job_Duration_Wasted(
          Average_Wasted : in );
      private
        Status : Record_Status;
      end Protected_Data;
  --...
  end Neo.Library.Task_Dispatcher;

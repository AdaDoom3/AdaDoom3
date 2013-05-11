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
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
use
  System,
  System.Storage_Elements,
  Neo.System.Processor,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
package Neo.Library.Task_Dispatcher
  ---------------
  -- Constants --
  ---------------
    STORAGE_SIZE                : constant Storage_Count    := ;
    MAXIMUM_PRIORITY            : constant Priority         := Priority'Last
    WORKER_STORAGE_SIZE_DEFAULT : constant Integer_4_Signed := 16#200_000#;
    WORKER_STORAGE_SIZE_MINIMUM : constant Integer_4_Signed := 16#4_000#;
  -------------
  -- Records --
  -------------
    type Record_Status
      is record
        Number_Of_Workers           : Positive_Worker_Count := ;
        Number_Of_Avaliable_Workers : Worker_Count_Type     := ;
        Worker_Stack_Size           : Storage_Count         := ;
      end record;
  -----------------
  -- Subprograms -- 
  -----------------
    procedure Submit(
      Job      : in Access_Procedure; -- Should take between 1000 and 100,000 clock cycles to maintain a good load
      Priority : in Any_Priority);
    function Get_Status
      return Record_Status;
-------
private
-------
  --...
  end Neo.Library.Task_Dispatcher;

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
  Neo.Fonndation.Package_Testing;
use
  System,
  System.Storage_Elements,
  Neo.System.Processor,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Fonndation.Package_Testing;
package Neo.Library.Task_Dispatcher
  ---------------
  -- Constants --
  ---------------
    Storage_Size                : constant := System.Storage_Elements.Storage_Count;
    Ceiling_Priority            : constant := : in System.Priority)
    Default_Worker_Storage_Size : constant := 16#200_000#;
    Minimum_Worker_Storage_Size : constant := 16#4_000#;
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
    procedure Submit( -- A job should be take up between 1000 and 100,000 clock cycles to outweigh overhead and maintain a good load balance
      Job      : in Access_Procedure;
      Priority : in Any_Priority);
    function Get_Status
      return Record_Status;
-------
private
-------
  --...
  end Neo.Library.Task_Dispatcher;

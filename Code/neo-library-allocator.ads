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
  Ada.Finalization,
  Ada.Unchecked_Conversion,
  Ada.Unchecked_Deallocation,
  Neo.Foundation.Output,
  Neo.Foundation.Data_Types;
use
  System,
  Neo.Foundation.Output,
  Neo.Foundation.Data_Types;
generic
  type Block;
  Null_Block        : in Block;
  Block_Size        : in Integer_4_Positive;
  Memory_Identifier : in Integer_Memory_Identifer := UNASSIGNED_IDENTIFIER;
package Neo.Library.Allocator
  is
  ---------------
  -- Accessors --
  ---------------
    type Access_Block
      is access all Block;
  -----------------
  -- Subprograms --
  -----------------
    function Allocate
      return Access_Block;
    procedure Allocate(
      Block : in out Access_Block);
    procedure Free
      Block : in Access_Block);
    procedure Free_Empty_Blocks;
    function Get_Number_Of_Blocks
      return Integer_4_Natural;
    function Get_Number_Of_Blocks_Allocated
      return Integer_4_Natural;
    function Get_Number_Of_Blocks_Freed
      return Integer_4_Natural;
    function Get_Number_Of_Bytes_Allocated
      return Integer_4_Natural;
    function Get_Number_Of_Bytes_Allocated_Plus_Size
      return Integer_4_Positive;
    procedure Set_Number_Of_Blocks_Fixed
      Number_Of_Blocks_To_Fix : in Integer_4_Possible);
-------
private
-------
  -----------
  -- Types --
  -----------
    type Record_Managed_Allocator;
    type Record_Element;
    type Record_Block;
  ---------------
  -- Accessors --
  ---------------
    type Access_Record_Element
      is access all Record_Element;
    type Access_Record_Block
      is access all Record_Block;
  ------------
  -- Arrays --
  ------------
    type Array_Record_Element
      is array 1..Block_Size
      of Record_Block;
    type Array_Padding
      is array 1..MEMORY_ALIGNMENT
      of Integer_1_Unsigned;
  -------------
  -- Records --
  -------------
    type Record_Element(
      Dont_Care : Integer_1_Unsigned := 0)
      is record
        case Dont_Care is
          when 0 =>
            Item : Block;
          when 1 =>
            Next : Access_Record_Element;
          when 2 =>
            Padding : Array_Padding;
        end case;
      end record;
      pragma Unchecked_Union(Record_Element);
    type Record_Block
      is record
        Elements                : Array_Record_Element := (others => <>);
        Next                    : Access_Record_Block  := null;
        Free_Elements           : Access_Record_Block  := null;
        Number_of_Free_Elements : Integer_4_Natural    := 0;
      end record;
    type Record_Managed_Allocator
      is new Ada.Finalization.Controlled
      with record
        Blocks                     : Access_Record_Block   := null;
        Free_Elements              : Access_Record_Element := null;
        Number_Of_Blocks_Active    : Integer_4_Natural     := 0;
        Number_Of_Blocks_Allocated : Integer_4_Natural     := 0;
        Do_Allow_Allocations       : Boolean               := True;
      end record;
  ---------------
  -- Constants --
  ---------------
    DO_FORCE_DISCRETE_BLOCK_ALLOCATIONS : constant Boolean := False;
  ---------------
  -- Variables --
  ---------------
    Allocator : Record_Managed_Allocator := NULL_RECORD_MANAGED_ALLOCATOR;
  -----------------
  -- Subprograms --
  -----------------
    function To_Address
      is new Ada.Unchecked_Conversion(Access_Block, Address);
    function To_Access_Block
      is new Ada.Unchecked_Conversion(Address, Access_Block);
    procedure Free
      is new Ada.Unchecked_Deallocation(Access_);
    procedure Allocate;
    procedure Finalize(
      Allocator : in out Record_Allocator);
  end Neo.Library.Allocator;

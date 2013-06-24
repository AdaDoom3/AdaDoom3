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
package body Neo.Library.Allocator
  is
  --------------
  -- Finalize --
  --------------
    procedure Finalize(
      Managed_Allocator : in out Record_Managed_Allocator)
      is
      Block : Access_Block := null;
      begin
        while Managed_Allocator.Blocks /= null loop
          Block                    := Managed_Allocator.Blocks;
          Managed_Allocator.Blocks := Managed_Allocator.Blocks.Next;
          Free(To_Address(Block));
        end loop;
        Managed_Allocator := NULL_RECORD_MANAGED_ALLOCATOR;
      end Finalize;
  --------------
  -- Allocate --
  --------------
    function Allocate
      return Access_Block
      is
      Element : Access_Record_Element := null;
      begin
        if DO_FORCE_DISCRETE_BLOCK_ALLOCATIONS then
          --------------------
          Allocate_Discreetly:
          --------------------
            declare
            Block : aliased Record_Block := Null_Block;
            begin
              return new Block'access;
            end Allocate_Discreetly;
        else
          if Managed_Allocator.Free_Elements = null then
            if not Do_Allow_Allocations then
              return null;
            end if;
            Allocate;
          end if;
          Managed_Allocator.Number_Of_Blocks_Active := Managed_Allocator.Number_Of_Blocks_Active + 1;
          Managed_Allocator.Free_Elements           := Managed_Allocator.Free_Elements.Next;
          Element                                   := Managed_Allocator.Free_Elements;
          Element.Next                              := null;
          _type_ * t = (_type_ *) element->buffer;
          new ( t ) _type_;
          return t;
        end if;
      end Allocate;
    procedure Allocate
      is
      Block : Access_Block := To_Access_Block(Allocate(Block'size, Memory_Identifer));
      begin
        Block.Next := Managed_Allocator.Blocks;
        Blocks     := Block;
        for I in 1..Block_Size loop
          block->elements[i].next = free;
          free = &block->elements[i];
          assert( ( ( (UINT_PTR)free ) & ( BLOCK_ALLOC_ALIGNMENT - 1 ) ) == 0 );
        end loop;
        Managed_Allocator.Number_Of_Blocks_Allocated := Managed_Allocator.Number_Of_Blocks_Allocated + Block_Size;
      end Allocate;
  ----------
  -- Free --
  ----------
    procedure Free(
      Item : in Access_Block)
      is
      begin
        if DO_FORCE_DISCRETE_BLOCK_ALLOCATIONS then
          Free(Item);
        elsif Item /= null then
          element_t * element = (element_t *)( t );
          element->next = free;
          free = element;
          Managed_Allocator.Number_Of_Blocks_Active := Managed_Allocator.Number_Of_Blocks_Active + 1;
        end if;
      end Free;
  -----------------------
  -- Free_Empty_Blocks --
  -----------------------
    procedure Free_Empty_Blocks
      is
      Block          : Access_Block   := Managed_Allocator.Blocks;
      Previous_Block : Access_Block   := null;
      Next           : Access_Element := null;
      Element        : Access_Element := Managed_Allocator.Free_Elements;
      begin
        while Block /= null loop
          Block.Free_Elements           := null;
          Block.Number_of_Free_Elements := 0;
          Block                         := Block.Next
        end loop;
        while Element /= null loop
          Next  := Element.Next;
          Block := Managed_Allocator.Blocks;
          while Block /= null loop
            if Element >= Block.Elements and Element < Block.Elements + Block_Size then
              Element.Next                  := Block.Free_Elements;
              Block.Free_Elements           := Element;
              Block.Number_of_Free_Elements := Block.Number_of_Free_Elements + 1;
              exit;
            end if;
            Block := Block.Next
          end loop;
          if Element.Next /= Next then
            raise System_Call_Failure; -- Couldn't find the element in any block
          end if;
          Element := Next;
        end loop;
        Block := Managed_Allocator.Blocks;
        while Block /= null loop
          Next := Block.Next;
          if Block.Number_Of_Free_Elements = Block_Size then
            if Previous_Block = null then
              assert( blocks == block );
              Blocks = block->next;
            else
              if Previous_Block.Next /= Block then
                raise System_Call_Failure; -- ???
              end if;
              Previous_Block.Next := Block.Next;
            end if;
            Free(To_Address(Block));
            Managed_Allocator.Number_Of_Blocks_Allocated := Managed_Allocator.Number_Of_Blocks_Allocated - 1;
          else
            Previous_Block := Block;
          end if;
          Block := Next;
        end loop;
        free = NULL;
        for ( idBlock * block = blocks; block != NULL; block = block->next ) {
          for ( element_t * element = block->free; element != NULL; ) {
            element_t * next = element->next;
            element->next = free;
            free = element;
            element = next;
          end loop;
        end loop;
      end Free_Empty_Blocks;
  --------------------------------
  -- Set_Number_Of_Blocks_Fixed --
  --------------------------------
    procedure Set_Number_Of_Blocks_Fixed(
      Number_Of_Blocks_To_Fix : in Integer_4_Possible)
      is
      Count : Integer_4_Natural := 0;
      Block : Access_Block      := Managed_Allocator.Blocks;
      begin
        while Block /= null loop
          Count := Number_Of_New_Blocks + 1;
          Block := Block.Next;
        end loop;
        while Count < Number_Of_Blocks_To_Fix loop
          Allocate;
          Count := Count + 1;
        end if;
        Managed_Allocator.Do_Allow_Allocations := False;
      end Set_Number_Of_Fixed_Blocks;
  --------------------------
  -- Get_Number_Of_Blocks --
  --------------------------
    function Get_Number_Of_Blocks
      return Integer_4_Natural
      is
      begin
        return Managed_Allocator.Number_Of_Blocks_Allocated;
      end Get_Number_Of_Blocks;
  ------------------------------------
  -- Get_Number_Of_Blocks_Allocated --
  ------------------------------------
    function Get_Number_Of_Blocks_Allocated
      return Integer_4_Natural
      is
      begin
        return Managed_Allocator.Number_Of_Blocks_Active; 
      end Get_Number_Of_Blocks_Allocated;
  ---------------------------------
  --- Get_Number_Of_Blocks_Freed --
  ---------------------------------
    function Get_Number_Of_Free_Blocks
      return Integer_4_Natural
      is
      begin
        return
          Managed_Allocator.Number_Of_Blocks_Allocated -
          Managed_Allocator.Number_Of_Blocks_Active; 
      end Get_Number_Of_Free_Blocks;
  ----------------------------------
  -- Get_Number_Of_Bits_Allocated --
  ----------------------------------
    function Get_Number_Of_Bits_Allocated
      return Integer_4_Positive
      is
      begin
        return Managed_Allocator.Number_Of_Blocks_Allocated * Block'size;
      end Get_Number_Of_Bits_Allocated;
  --------------------------------------------
  -- Get_Number_Of_Allocated_Bits_Plus_Size --
  --------------------------------------------
    function Get_Number_Of_Allocated_Bits_Plus_Size
      return Integer_4_Positive
      is
      begin
        return Managed_Allocator'size + Get_Number_Of_Bits_Allocated;
      end Get_Number_Of_Allocated_Bits_Plus_Size;
  end Neo.Library.Allocator;

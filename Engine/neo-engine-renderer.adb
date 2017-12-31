
--                                                                                                                                      --
--                                                         N E O  E N G I N E                                                           --
--                                                                                                                                      --
--                                                 Copyright (C) 2016 Justin Squirek                                                    --
--                                                                                                                                      --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the --
-- Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                    --
--                                                                                                                                      --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of                --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                            --
--                                                                                                                                      --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                       --
--                                                                                                                                      --

-- Renderer for the global engine state
separate (Neo.Engine) package body Renderer is

  --------------
  -- Settings --
  --------------

  -- Acceptable image formats for our render targets
  SUPPORTED_SWAPCHAIN_FORMAT : constant VkSurfaceFormatKHR   := (VK_FORMAT_B8G8R8A8_UNORM,     VK_COLOR_SPACE_SRGB_NONLINEAR_KHR);
  SUPPORTED_DEPTH_FORMATS    : constant Array_Int_Unsigned_C := (VK_FORMAT_D32_SFLOAT_S8_UINT, VK_FORMAT_D24_UNORM_S8_UINT);
  
  -- Feature afd
  INSTANCE_EXTENSIONS : constant Array_Ptr_Char_8_C := (C (VK_KHR_SURFACE_EXTENSION_NAME), Get_Vulkan_Extension);
  DEVICE_EXTENSIONS   : constant Array_Ptr_Char_8_C := (C (VK_KHR_MAINTENANCE1_NAME),
                                                        C (VK_KHR_MAINTENANCE2_NAME),
                                                        C (VK_KHR_BIND_MEMORY2_NAME),
                                                        C (VK_KHR_IMAGE_FORMAT_LIST_NAME),
                                                        C (VK_KHR_SWAPCHAIN_EXTENSION_NAME),
                                                        C (VK_KHR_GET_MEMORY_REQUIREMENTS2_NAME));
  DEBUGING_EXTENSIONS : constant Array_Ptr_Char_8_C := (C (VK_LAYER_LUNARG_API_DUMP_EXTENSION_NAME),
                                                        C (VK_LAYER_LUNARG_CORE_VALIDATION_NAME),
                                                        C (VK_LAYER_LUNARG_OBJECT_TRACKER_NAME),
                                                        C (VK_LAYER_LUNARG_PARAMETER_VALIDATION_NAME));

  -------------
  -- Handles --
  -------------
  
  Queue,             
  Surface,                           
  Instance,                          
  Swapchain,                        
  Render_Pass,                       
  Command_Pool,                      
  Render_Status,                     
  Acquire_Status,     
  Device,                
  Physical_Device,                    
  Current_Pipeline,       
  Pipeline_Cache,
  Pipeline_Layout,   
  Descriptor_Set,
  Descriptor_Pool,
  Descriptor_Set_Layout : aliased Ptr := NULL_PTR;  
  
  -----------------
  -- Information --
  -----------------

  Queue_Family      : aliased Int_Unsigned_C                   := 0;
  Surface_Extent    : aliased VkExtent2D                       := (others => <>);
  Swapchain_Format  : aliased VkSurfaceFormatKHR               := (others => <>);
  Depth_Format      : aliased VkSurfaceFormatKHR               := (others => <>);
  Surface_Details   : aliased VkSurfaceCapabilitiesKHR         := (others => <>);
  Format_Properties : aliased VkFormatProperties               := (others => <>);
  Image_Properties  : aliased VkImageFormatProperties          := (others => <>);
  Device_Limits     : aliased VkPhysicalDeviceLimits           := (others => <>);
  Device_Properties : aliased VkPhysicalDeviceProperties       := (others => <>);
  Memory_Properties : aliased VkPhysicalDeviceMemoryProperties := (others => <>);
  
  -----------------
  -- Framebuffer --
  -----------------
  --
  -- ???
  --

  type Framebuffer_State is record
      Image            : aliased Ptr := NULL_PTR;
      Image_View       : aliased Ptr := NULL_PTR;
      Commands         : aliased Ptr := NULL_PTR;
      Descriptor_Pool  : aliased Ptr := NULL_PTR;
      Swapchain_Buffer : aliased Ptr := NULL_PTR;
    end record;
  package Vector_Framebuffer is new Vectors (Framebuffer_State);
  Framebuffer_Status : Safe_Status;
  Framebuffer        : Vector_Framebuffer.Ptr_Unsafe_Array := null;
  Current_Frame      : Positive                            := 1;
  
  --------------
  -- Pipeline --
  --------------
  --
  -- ???
  --

  type Shader_Kind is (Fragment_Shader, Vertex_Shader, Tesselation_Shader);
  type Shader_Array is array (Shader_Kind) of aliased Ptr;
  type Shader_State is record
      Pipeline_Layout         : aliased Ptr          := NULL_PTR;
      Descriptor_Set_Layout   : aliased Ptr          := NULL_PTR;
      Data                    :         Shader_Array := (others => NULL_PTR);
      Vertex_Input            :         Ptr_Array_VkPipelineVertexInputStateCreateInfo := null;
      Vertex_Input_Attributes :         Ptr_Array_VkVertexInputAttributeDescription    := null;
    end record;
  package Map_Shader is new Neo.Core.Hashed (Shader_State);

  -- ???
  type Pipeline_State is record
      Shader : Shader_State := (others => <>);  
      
      -- Rasterization
      Cull_Mode          : Int_Unsigned_C := VK_CULL_MODE_FRONT_BIT;          -- VkCullModeFlagBits 
      Front_Face         : Int_Unsigned_C := VK_FRONT_FACE_COUNTER_CLOCKWISE; -- VkFrontFace
      Has_Mirror_View    : Bool           := False;    
      Has_Polygon_Offset : Bool           := False;
      
      -- Color blending
      Color_Blend              : Int_Unsigned_C := VK_BLEND_OP_ADD;      -- VkBlendOp   
      Alpha_Blend              : Int_Unsigned_C := VK_BLEND_OP_ADD;      -- VkBlendOp 
      Source_Blend_Factor      : Int_Unsigned_C := VK_BLEND_FACTOR_ZERO; -- VkBlendFactor    
      Destination_Blend_Factor : Int_Unsigned_C := VK_BLEND_FACTOR_ZERO; -- VkBlendFactor 
      Color_Mask               : Int_Unsigned_C := 0;                    -- VkColorComponentFlags      
      
      -- Depth stencil                                
      Front_Fail         : Int_Unsigned_C := VK_STENCIL_OP_KEEP;  -- VkStencilOp
      Front_Pass         : Int_Unsigned_C := VK_STENCIL_OP_KEEP;  -- VkStencilOp
      Front_Depth_Fail   : Int_Unsigned_C := VK_STENCIL_OP_KEEP;  -- VkStencilOp
      Back_Fail          : Int_Unsigned_C := VK_STENCIL_OP_KEEP;  -- VkStencilOp
      Back_Pass          : Int_Unsigned_C := VK_STENCIL_OP_KEEP;  -- VkStencilOp
      Back_Depth_Fail    : Int_Unsigned_C := VK_STENCIL_OP_KEEP;  -- VkStencilOp    
      Stencil_Compare    : Int_Unsigned_C := VK_COMPARE_OP_NEVER; -- VkCompareOp  
      Depth_Compare      : Int_Unsigned_C := VK_COMPARE_OP_NEVER; -- VkCompareOp  
      Compare_Mask       : Int_Unsigned_C := 0;          
      Stencil_Reference  : Int_Unsigned_C := 0;  
      Depth_Write_Enable : Bool           := False;
      Depths_Bounds_Test : Bool           := False;  
      Has_Back_Stencil   : Bool           := False;   
    end record;
  type Buffered_Pipeline_State is record
      Val  : Pipeline_State := (others => <>);
      Data : Ptr            := NULL_PTR;
    end record;
  package Vector_Buffered_Pipeline is new Neo.Core.Vectors (Buffered_Pipeline_State);

  -- Global pipeline state and loaded Vulkan pointers
  Pipeline           : Pipeline_State          := (others => <>);
  Previous_Pipeline  : Buffered_Pipeline_State := (others => <>);
  Shaders            : Map_Shader.Unsafe.Map;
  Buffered_Pipelines : Vector_Buffered_Pipeline.Unsafe.Vector;

  -- Junk objects for getting the position attribute on components
  Static_Vertex_Obj   : Static_Vertex_State   := (others => <>);
  Animated_Vertex_Obj : Animated_Vertex_State := (others => <>);

  ---------------------------------
  -- Intermediate Representation --
  ---------------------------------
  --
  -- Definitions and globals for communicating and processing 3D data between the frontend and backend
  --

  type Light_Kind       is (Point_Light,        Blend_Light,        Fog_Light);
  type Interaction_Kind is (Shadow_Interaction, Direct_Interaction, Translucent_Interaction);
  type Visibility_Kind  is (Subview_Visibility, Opaque_Visibility,  Perforated_Visibility, No_Visibility);

  type Space_State is record
      MVP             : Matrix_4D := ZERO_MATRIX_4D;
      Local_To_Global : Matrix_4D := ZERO_MATRIX_4D;
      Local_To_Eye    : Matrix_4D := ZERO_MATRIX_4D;
    end record;

  type Surface_State is record
      Material          : Str_Unbound := NULL_STR_UNBOUND; 
      Space             : Space_State := (others => <>);
      Scissor           : Matrix_4D   := ZERO_MATRIX_4D; 
      Model_Depth_Hack  : Bool        := False;
      Weapon_Depth_Hack : Bool        := False;
    end record;
  package Vector_Surface is new Neo.Core.Vectors (Surface_State);
  type Interaction_Array is array (Interaction_Kind) of Vector_Surface.Unsafe.Vector;

  type Light_State is record
      Material           : Str_Unbound := NULL_STR_UNBOUND;
      Inverse_Projection : Matrix_4D   := ZERO_MATRIX_4D;
      Projection         : Matrix_4D   := ZERO_MATRIX_4D;
      Scissor            : Matrix_4D   := ZERO_MATRIX_4D;
      Origin             : Matrix_4D   := ZERO_MATRIX_4D;
      Fog_Plane          : Matrix_4D   := ZERO_MATRIX_4D;
      Interactions       : Interaction_Array;
    end record;
  package Vector_Light is new Neo.Core.Vectors (Light_State);

  type View_State is record
      Is_Mirror      : Bool        := False;
      Origin         : Matrix_4D   := ZERO_MATRIX_4D;
      Port           : Matrix_4D   := ZERO_MATRIX_4D;
      Scissor        : Matrix_4D   := ZERO_MATRIX_4D;
      World_Space    : Space_State := (others => <>);
      --Surface_Groups : 
      Lights         : Vector_Light.Unsafe.Vector;
    end record;
  package Vector_View is new Neo.Core.Vectors (View_State);

  -- Global data shared between the frontend and backend
  Views : Vector_View.Safe_Vector;

  ------------
  -- Memory --
  ------------
  --
  -- ???
  --

  type Filter_Kind     is (Nearest_Filter,  Linear_Filter);
  type Usage_Kind      is (GPU_Usage,       CPU_Usage,         CPU_To_GPU_Usage,        GPU_To_CPU_Usage);
  type Allocation_Kind is (Free_Allocation, Buffer_Allocation, Linear_Image_Allocation, Optimal_Image_Allocation);

  -- Memory element within a block
  type Memory_Chunk_State;
  type Ptr_Memory_Chunk_State is access all Memory_Chunk_State;
  type Memory_Chunk_State is record
      ID             : Int_Unsigned_C         := 0;
      Offset         : Int_64_Unsigned_C      := 0;
      Size           : Int_64_Unsigned_C      := 0;
      Allocation     : Allocation_Kind        := Free_Allocation;
      Next, Previous : Ptr_Memory_Chunk_State := null;
    end record;
  procedure Free is new Unchecked_Deallocation (Memory_Chunk_State, Ptr_Memory_Chunk_State);

  -- Dynamically allocatable list of memory chucks which share the same usage 
  type Memory_Block_State (Usage : Usage_Kind; Size : Int_64_Unsigned_C; Index : Int_Unsigned_C) is record
      Next_ID       :         Int_Unsigned_C         := 0;      
      Offset        :         Int_64_Unsigned_C      := 0;      
      Allocated     :         Int_64_Unsigned_C      := 0;
      First_Chunk   :         Ptr_Memory_Chunk_State := null;
      Device_Memory : aliased Ptr                    := NULL_PTR;
      Data          : aliased Ptr                    := NULL_PTR;
    end record;
  package Vector_Memory_Block is new Neo.Core.Vectors_Unconstrained (Memory_Block_State); use Vector_Memory_Block.Unsafe;

  -- Reference to a chunk of dynamically allocated memory
  type Memory_State is record
      Block         :         Vector_Memory_Block.Cursor;
      ID            :         Int_Unsigned_C    := 0;
      Offset        :         Int_64_Unsigned_C := 0;
      Size          :         Int_64_Unsigned_C := 0;
      Device_Memory : aliased Ptr               := NULL_PTR;
      Data          : aliased Ptr               := NULL_PTR; -- vkBuffer
    end record;
  package Vector_Memory is new Neo.Core.Vectors (Memory_State);

  -- Internal data structure to deal with buffer type differences 
  type Buffer_State (Usage : Usage_Kind; Size : Int_64_Unsigned_C; Is_Image : Bool) is record 
      Offset :         Int_64_Unsigned_C := 0;
      Memory :         Memory_State      := (others => <>);
      Data   : aliased Ptr               := NULL_PTR;
      case Is_Image is
        when True => 
          References :         Int_Unsigned_C := 0;
          Filter     :         Filter_Kind    := Nearest_Filter;
          View       : aliased Ptr            := NULL_PTR;
          Sampler    : aliased Ptr            := NULL_PTR;
        when False => null;
      end case;
    end record;
  package Vector_Buffer is new Neo.Core.Vectors_Unconstrained (Buffer_State);
  package Hashed_Buffer is new Neo.Core.Hashed (Buffer_State);
  
  -- ???
  package Hashed_Bool is new Neo.Core.Hashed (Bool);

  -- ???
  Buffered_Materials : Hashed_Bool.Unsafe.Map;
  Buffered_Images    : Hashed_Buffer.Unsafe.Map;
  Memory_Blocks      : Vector_Memory_Block.Unsafe.Vector;
  Memory_Garbage     : Vector_Memory.Unsafe.Vector;
  Uniform_Buffers    : Vector_Buffer.Unsafe.Vector;
  Depth_Image        : Buffer_State := (GPU_Usage, Size => 0, Is_Image => True, others => <>);
  
  -- Globals used for staging data for transfer to the GPU
  Staging_Command_Pool,
  Staging_Commands,
  Staging_Memory,
  Staging_Buffer,
  Staging_Fence,
  Staging_Data   : aliased Ptr     := NULL_PTR;
  Staging_Offset : aliased Int_Ptr := 0;
  
  -- ???
  function Find_Memory_Type_Index (Memory_Type_Bits : Int_Unsigned_C; Usage : Usage_Kind) return Int_Unsigned_C is
    Properties, Preferred, Required : Int_Unsigned_C := 0;
    begin
      case Usage is
        when GPU_Usage        => Preferred := VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
        when CPU_To_GPU_Usage => Preferred := VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
                                 Required  := VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT;  
        when CPU_Usage        => Required  := VK_MEMORY_PROPERTY_HOST_COHERENT_BIT or VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT;
        when GPU_To_CPU_Usage => Preferred := VK_MEMORY_PROPERTY_HOST_COHERENT_BIT or VK_MEMORY_PROPERTY_HOST_CACHED_BIT;
                                 Required  := VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT;                                 
      end case;
        
      -- Try to find a memory type that has both required and preferred properties
      for I in 1..Int (Memory_Properties.memoryTypeCount) loop
        Properties := Memory_Properties.memoryTypes (I).propertyFlags;
        if (Memory_Type_Bits and 2**(I - 1)) /= 0 and (Properties and Preferred) > 0 and (Properties and Required) > 0 then
          return Int_Unsigned_C (I - 1);
        end if;
      end loop;    
        
      -- Couldn't find a suitable memory type, so match on the required parts at least
      for I in 1..Int (Memory_Properties.memoryTypeCount) loop
        Properties := Memory_Properties.memoryTypes (I).propertyFlags;
        if (Memory_Type_Bits and 2**(I - 1)) /= 0 and (Properties and Required) > 0 then return Int_Unsigned_C (I - 1); end if;
      end loop;
      
      -- Go with preferred then
      for I in 1..Int (Memory_Properties.memoryTypeCount) loop
        Properties := Memory_Properties.memoryTypes (I).propertyFlags;
        if (Memory_Type_Bits and 2**(I - 1)) /= 0 and (Properties and Preferred) > 0 then return Int_Unsigned_C (I - 1); end if;
      end loop;
      
      -- No suitable memory types
      raise Program_Error;
    end;

  -- ???
  procedure Set (Buffer : in out Buffer_State; Data : Ptr; Data_Size : Int_Ptr) is
    begin
      case Buffer.Usage is
        when GPU_Usage =>
          Assert (Int (Data_Size) mod Byte'Size = 0); 
          Assert (Data_Size <= Max_Upload_Buffer.Get);
  
          -- Flush the garbage if we have reached the max
          if Staging_Offset + Data_Size >= Max_Upload_Buffer.Get then
            declare
            Command_Buffer_Info : aliased VkCommandBufferBeginInfo := (others => <>);
            Memory_Range : aliased VkMappedMemoryRange := (memory => Int_64_Unsigned_C (To_Int_Ptr (Data)), size => VK_WHOLE_SIZE, others => <>);
            Submit_Info : aliased VkSubmitInfo := (commandBufferCount => 1, pCommandBuffers => Staging_Commands'Unchecked_Access, others => <>);
            begin
              vkAssert (vkFlushMappedMemoryRanges (Device, 1, Memory_Range'Unchecked_Access));
              vkAssert (vkQueueSubmit (Queue, 1, Submit_Info'Unchecked_Access, Staging_Fence));
  
              -- Reset the command buffer only after the staging buffer is avaliable
              vkAssert (vkWaitForFences      (Device, 1, Staging_Fence'Unchecked_Access, VK_TRUE, Int_64_Unsigned_C'Last));
              vkAssert (vkResetFences        (Device, 1, Staging_Fence'Unchecked_Access));
              vkAssert (vkBeginCommandBuffer (Staging_Commands, Command_Buffer_Info'Unchecked_Access));
              Staging_Offset := 0;
            end;
          end if;
          
          -- Copy it in
          declare
          Source, Destination : Array_Byte (1..Int (Data_Size) / Byte'Size);
          for Source'Address use Data; for Destination'Address use To_Ptr (To_Int_Ptr (Staging_Data) + Staging_Offset);
          Buffer_Copy : aliased VkBufferCopy := (srcOffset => Int_64_Unsigned_C (Staging_Offset),
                                                 dstOffset => Int_64_Unsigned_C (To_Int_Ptr (Staging_Data) + Staging_Offset),
                                                 size      => Int_64_Unsigned_C (Data_Size), others => <>);
          begin
            Destination := Source;
            vkCmdCopyBuffer (Framebuffer (Current_Frame).Commands, Staging_Buffer, Buffer.Data, 1, Buffer_Copy'Unchecked_Access);
          end;
          
          -- Move over the staging offset to reflect the copied buffer data
          Staging_Offset := Staging_Offset + Int_Ptr (Data_Size);
        when others => null; --Buffer.Memory.Data := Staging_Offset + Offset + data (size);
      end case;
    end;

  -- Grab some memory from a block
  procedure Allocate (Buffer : in out Buffer_State) is
  
    -- ???
    function Align (Offset, Alignment : Int_64_Unsigned_C) return Int_64_Unsigned_C is ((Offset * 2 - 1) and not (Alignment - 1));

    -- Check if two offsets and sizes are suitable for the same page - see the Vulkan spec "Buffer-Image Granularity"
    function Can_Be_On_Same_Page (Offset_A, Size_A, Offset_B : Int_64_Unsigned_C) return Bool is
      (((Offset_A + Size_A - 1) and (not (Device_Properties.limits.bufferImageGranularity - 1))) =
        (Offset_B               and (not (Device_Properties.limits.bufferImageGranularity - 1))));


    -- Check that the memory kinds do not have a granularity conflict
    function Has_Granularity_Conflict (Allocation_A, Allocation_B : Allocation_Kind) return Bool is
      Test_A : Allocation_Kind := (if Allocation_A > Allocation_B then Allocation_B else Allocation_A);
      Test_B : Allocation_Kind := (if Test_A       = Allocation_A then Allocation_B else Allocation_A);
      begin
        return (case Test_A is
                  when Free_Allocation   | Optimal_Image_Allocation => False,
                  when Buffer_Allocation | Linear_Image_Allocation  => Test_B = Optimal_Image_Allocation);
      end;
      
    -- Local Variables
    Allocation          :         Allocation_Kind        := (if Buffer.Is_Image then Optimal_Image_Allocation else Buffer_Allocation);
    Memory_Type_Index   :         Int_Unsigned_C         := 0;
    Offset              :         Int_64_Unsigned_C      := 0;
    Aligned_Size        :         Int_64_Unsigned_C      := 0;
    Memory_Requirements : aliased VkMemoryRequirements   := (others => <>);
    Current             : aliased Ptr_Memory_Chunk_State := null;
    begin 
      
      -- Get the memory requirements and do a linear search to find the usage.... its a small array
      if Buffer.Is_Image then vkGetImageMemoryRequirements (Device, Buffer.Data, Memory_Requirements'Unchecked_Access);
      else vkGetBufferMemoryRequirements (Device, Buffer.Data, Memory_Requirements'Unchecked_Access); end if;
      Memory_Type_Index := Find_Memory_Type_Index (Memory_Requirements.memoryTypeBits, Buffer.Usage);
      
      -- Try to allocate from a suitable block
      for I in Iterate (Memory_Blocks) loop declare Block : Memory_Block_State := Element (I); begin
      
        -- Test if the current block is too small
        if Block.Index = Memory_Type_Index and then Block.Size - Block.Allocated >= Memory_Requirements.size then

          -- Find the best fit chunk
          Current := Block.First_Chunk;
          Inner: while Current /= null loop

            -- Only example chunks that are free 
            if Current.Allocation = Free_Allocation then

              -- Set the offset in case the current chunk's granularity conflicts with the previous chunk
              Offset := Align (Current.Offset, Memory_Requirements.alignment);
              if Current.Previous /= null and then Device_Properties.limits.bufferImageGranularity > 1
                and then Can_Be_On_Same_Page (Current.Previous.Offset, Current.Previous.Size, Offset)
                and then Has_Granularity_Conflict (Current.Previous.Allocation, Allocation)
              then
                Offset := Align (Offset, Device_Properties.limits.bufferImageGranularity);
              end if;

              -- Check the next chunk's allocation for suitability
              Aligned_Size := Offset - Current.Offset + Memory_Requirements.size;
              if Aligned_Size <= Current.Size then 

                -- Bail when the block is too small
                exit when Aligned_Size + Block.Allocated >= Block.Size;

                -- Check for a granularity conflicts with the next chunk before allocating
                if Current.Next = null or else Device_Properties.limits.bufferImageGranularity <= 1
                  or else not Can_Be_On_Same_Page (Offset, Memory_Requirements.size, Current.Next.Offset)
                  or else not Has_Granularity_Conflict (Allocation, Current.Next.Allocation)
                then

                  -- Split the best fitting chunk when the size is not an exact fit
                  if Current.Size > Memory_Requirements.size then
                    Current.Next := new Memory_Chunk_State'(ID         => Block.Next_ID,
                                                            Offset     => Offset + Memory_Requirements.size,
                                                            Size       => Current.Size - Aligned_Size,
                                                            Allocation => Free_Allocation,
                                                            Next       => Current.Next,
                                                            Previous   => Current);
                    Current.Size  := Memory_Requirements.size;
                    Block.Next_ID := Block.Next_ID + 1;
                  end if;

                  -- Assign the chunk and mark it as allocated and build the result
                  Block.Allocated := Block.Allocated + Aligned_Size;
                  Replace_Element (Memory_Blocks, I, Block);
                  Current.Allocation := Allocation;
                  Buffer.Memory := (ID            => Current.ID,
                                    Offset        => Offset,
                                    Size          => Current.Size,
                                    Device_Memory => Block.Device_Memory,
                                    Block         => I,
                                    Data          => (if Buffer.Usage /= GPU_Usage then
                                                        To_Ptr (To_Int_Ptr (Block.Data) + Int_Ptr (Offset)) else NULL_PTR));
                  return;
                end if;
              end if;
            end if;

            -- Advance to the chunk within the block
            Current := Current.Next;
          end loop Inner;
        end if;
      end; end loop;

      -- Allocate a new block since no suitable existing ones could be found
      declare
      Allocate_Info : aliased VkMemoryAllocateInfo := (allocationSize  => Memory_Requirements.size,
                                                       memoryTypeIndex => Memory_Type_Index, others => <>);
      Block : Memory_Block_State := (Index => Memory_Type_Index,
                                     Usage => Buffer.Usage,
                                     Size  => Int_64_Unsigned_C (if Buffer.Usage = GPU_Usage then Max_GPU_Memory.Get
                                                                 else Max_Visible_Memory.Get) * 1024, others => <>);
      begin
        Block.First_Chunk := new Memory_Chunk_State'(Size => Block.Size, others => <>);
        vkAssert (vkAllocateMemory (Device, Allocate_Info'Unchecked_Access, null, Block.Device_Memory'Unchecked_Access));
        if Block.Usage /= GPU_Usage then
          vkAssert (vkMapMemory (Device, Block.Device_Memory, 0, Memory_Requirements.size, 0, Block.Data'Unchecked_Access));
        end if;
        Memory_Blocks.Append (Block);

        -- Create a chunk from the new and return the desired memory from it
        Buffer.Memory := (ID            => Block.First_Chunk.ID,
                          Size          => Block.Size,
                          Device_Memory => Block.Device_Memory,
                          Offset        => Offset,
                          Block         => Memory_Blocks.Last,
                          Data          => To_Ptr ((if Buffer.Usage = GPU_Usage then Max_GPU_Memory.Get
                                                    else Max_Visible_Memory.Get) * 1024**2));
      end;
    end;

  -- Free the global GPU memory garbage
  procedure Free_Memory_Garbage is
    Current, Previous, Next : Ptr_Memory_Chunk_State := null;
    begin
      for Piece of Memory_Garbage loop
        declare Block : Memory_Block_State := Element (Piece.Block); begin   

          -- Find the chunk corresponding to the Memory
          Current := Block.First_Chunk;
          while Current /= null loop
            if Current.ID = Piece.ID then

              -- Join the chunk with the previous one if it is free
              if Current.Previous /= null and then Current.Previous.Allocation = Free_Allocation then
                Previous := Current.Previous;
                Previous.Next := Current.Next;
                if Current.Next /= null then Current.Next.Previous := Previous; end if;
                Previous.Size := Previous.Size + Current.Size;
                Free (Current);
                Current := Previous;
              end if;

              -- Same with the next chunk
              if Current.Next /= null and then Current.Next.Allocation = Free_Allocation then
                Next := Current.Next;
                if Next.Next /= null then Next.Next.Previous := Current; end if;
                Current.Next := Next.Next;
                Current.Size := Current.Size + Next.Size;
                Free (Next);
              end if;
              
              -- Modify block to handle new size
              Block.Allocated := Block.Allocated - Piece.Size;
              Memory_Blocks.Replace_Element (Piece.Block, Block);
              return;
            end if;
            Current := Current.Next;
          end loop;

          -- Kill the block if there is nothing left
          if Block.Allocated = 0 then
            if Block.Usage /= GPU_Usage then vkUnmapMemory (Device, Block.Device_Memory); end if;
            vkFreeMemory (Device, Block.Device_Memory, null);
            Block.Device_Memory := NULL_PTR;
            while Block.First_Chunk /= null loop
              Current := Block.First_Chunk;
              Block.First_Chunk := Current.Next;
              Free (Current);
            end loop;
            Memory_Blocks.Delete (Piece.Block);
          end if;
        end;
      end loop;
      Memory_Garbage.Clear;
    end;

  ---------------
  -- Materials --
  ---------------
  --
  -- ???
  --

  -- Create a lookup string for Buffered_Images which includes material settings in addition the image path
  function Get_Material_Info_Hash (Item : Material_State) return Str_Unbound is
    (U (Model.Filter_Kind'Pos (Item.Filter)'Wide_Image & Clamp_Kind'Pos (Item.Clamp)'Wide_Image));

  -- Manage counters for Buffered_Image references
  procedure Add_Image_References (Path : Str_Unbound) is Buffered_Image : Buffer_State := Buffered_Images (Path);
    begin Buffered_Image.References := Buffered_Image.References + 1; Buffered_Images.Replace (Path, Buffered_Image); end;
  procedure Remove_Image_References (Path : Str_Unbound) is Buffered_Image : Buffer_State := Buffered_Images (Path);
    begin Buffered_Image.References := Buffered_Image.References - 1; Buffered_Images.Replace (Path, Buffered_Image); end;

  -- Load a material's textures and mark it 
  procedure Add_Material (Path : Str) is
    begin

      -- Sanity check
      if Buffered_Materials.Contains (U (Path)) then return; end if;

      -- Buffer the material
      declare
      Material : Material_State := Materials.Get (Path);
      Hash     : Str_Unbound    := Get_Material_Info_Hash (Material);
      begin

        -- Load all images associated with the material to the GPU
        declare Paths : Array_Str_Unbound :=
          (Material.Irradiance, Material.Specular, Material.Normal, Material.Displacement, Material.Metallic, Material.Roughness);
        begin for Path of Paths loop
        
          -- Increment the count if it is loaded already
          if Buffered_Images.Contains (Path & Hash) then Add_Image_References (Path & Hash); return; end if;

          -- Buffer the image 
          declare
          Image               :         Image_State           := Load (S (Path));
          Buffered_Image      :         Buffer_State          := (GPU_Usage, Image.Data'Size, Is_Image => True, others => <>);
          Memory_Requirements : aliased VkMemoryRequirements  := (others => <>);
          Image_View_Info     : aliased VkImageViewCreateInfo := (viewType         => VK_IMAGE_VIEW_TYPE_2D,
                                                                  format           => Image.Internal_Format,
                                                                  components       => (VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                       VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                       VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                       VK_COMPONENT_SWIZZLE_IDENTITY),
                                                                  subresourceRange => (aspectMask => VK_IMAGE_ASPECT_COLOR_BIT,
                                                                                       levelCount => 1,
                                                                                       layerCount => 1, others => <>), others => <>);
          Sampler_Info : aliased VkSamplerCreateInfo := (maxAnisotropy    => 1.0,
                                                         anisotropyEnable => VK_FALSE,
                                                         compareEnable    => VK_FALSE,
                                                         compareOp        => VK_COMPARE_OP_NEVER, others => <>);
          Image_Info : aliased VkImageCreateInfo := (initialLayout => VK_IMAGE_LAYOUT_UNDEFINED,
                                                     sharingMode   => VK_SHARING_MODE_EXCLUSIVE,
                                                     tiling        => VK_IMAGE_TILING_OPTIMAL,
                                                     imageType     => VK_IMAGE_TYPE_2D,
                                                     format        => Image.Internal_Format,
                                                     mipLevels     => Int_Unsigned_C (Image.Mipmaps),
                                                     samples       => VK_SAMPLE_COUNT_1_BIT, -- Int_Unsigned_C (Samples.Get'Val)
                                                     extent        => (Int_Unsigned_C (Image.Width), Int_Unsigned_C (Image.Height), 1),
                                                     arrayLayers   => (if Image.Is_Cube_Map then 6 else 1),
                                                     flags         => (if Image.Is_Cube_Map then VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT else 0),
                                                     usage         => VK_IMAGE_USAGE_TRANSFER_DST_BIT, others => <>);
          begin

            -- Sampler
            case Material.Filter is
              when Linear_Filter =>
                Sampler_Info.minFilter  := VK_FILTER_LINEAR;
                Sampler_Info.magFilter  := VK_FILTER_LINEAR;
                Sampler_Info.mipmapMode := VK_SAMPLER_MIPMAP_MODE_LINEAR;
              when Nearest_Filter =>
                Sampler_Info.minFilter  := VK_FILTER_NEAREST;
                Sampler_Info.magFilter  := VK_FILTER_NEAREST;
                Sampler_Info.mipmapMode := VK_SAMPLER_MIPMAP_MODE_NEAREST;
            end case;
            case Material.Clamp is
              when No_Clamp =>
                Sampler_Info.addressModeU := VK_SAMPLER_ADDRESS_MODE_REPEAT;
                Sampler_Info.addressModeV := VK_SAMPLER_ADDRESS_MODE_REPEAT;
                Sampler_Info.addressModeW := VK_SAMPLER_ADDRESS_MODE_REPEAT;
              when Normal_Clamp =>
                Sampler_Info.addressModeU := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
                Sampler_Info.addressModeV := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
                Sampler_Info.addressModeW := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
              when Zero_Alpha_Clamp =>
                Sampler_Info.borderColor  := VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK;
                Sampler_Info.addressModeU := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
                Sampler_Info.addressModeV := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
                Sampler_Info.addressModeW := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
              when Zero_Clamp =>
                Sampler_Info.borderColor  := VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK;
                Sampler_Info.addressModeU := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
                Sampler_Info.addressModeV := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
                Sampler_Info.addressModeW := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
            end case;
            vkAssert (vkCreateSampler (Device, Sampler_Info'Unchecked_Access, null, Buffered_Image.Sampler'Unchecked_Access)); 
            
            -- Image
            vkAssert (vkCreateImage (Device, Image_Info'Unchecked_Access, null, Buffered_Image.Data'Unchecked_Access));
            vkGetImageMemoryRequirements (Device, Buffered_Image.Data, Memory_Requirements'Unchecked_Access);
            Allocate (Buffered_Image);
            vkAssert (vkBindImageMemory (Device, Buffered_Image.Data, Buffered_Image.Memory.Device_Memory, Buffered_Image.Memory.Offset));
            Set (Buffered_Image, Image.Data'Address, Image.Data'Size);
            
            -- View
            Image_View_Info.image        := Buffered_Image.Data;
            vkAssert (vkCreateImageView  (Device, Image_View_Info'Unchecked_Access, null, Buffered_Image.View'Unchecked_Access));

            -- Register the buffered image
            Buffered_Images.Insert (Path & Hash, Buffered_Image);
          end;
        end loop; end;

        -- Register the buffered material
        Buffered_Materials.Insert (U (Path), True); -- Set it to junk since we only care if it exists
      end;
    end;    
  procedure Remove_Material (Path : Str) is
    Material : Material_State := Materials.Get (Path);
    Hash     : Str_Unbound    := Get_Material_Info_Hash (Material);
    begin
      if Material.Base_Color /= NULL_STR_UNBOUND then Remove_Image_References (Material.Base_Color & Hash); end if;
      if Material.Irradiance /= NULL_STR_UNBOUND then Remove_Image_References (Material.Irradiance & Hash); end if;
      if Material.Specular   /= NULL_STR_UNBOUND then Remove_Image_References (Material.Specular   & Hash); end if;
      if Material.Normal     /= NULL_STR_UNBOUND then Remove_Image_References (Material.Normal     & Hash); end if;
      if Material.Metallic   /= NULL_STR_UNBOUND then Remove_Image_References (Material.Metallic   & Hash); end if;
      if Material.Roughness  /= NULL_STR_UNBOUND then Remove_Image_References (Material.Roughness  & Hash); end if;
      Buffered_Materials.Delete (U (Path));
    end;
  procedure Free_Image_Garbage is
    Pos : Hashed_Buffer.Cursor;
    begin
      for I in Buffered_Images.Iterate loop Pos := I;
        declare Buffered_Image : Buffer_State := Hashed_Buffer.Unsafe.Element (I); begin
          if Buffered_Image.References = 0 then
            vkDestroyImage     (Device, Buffered_Image.Data,    null);
            vkDestroyImageView (Device, Buffered_Image.View,    null);
            vkDestroySampler   (Device, Buffered_Image.Sampler, null);
            Memory_Garbage.Append (Buffered_Image.Memory);
            Buffered_Images.Delete (Pos);
          end if;
        end;
      end loop;
    end;    
  
  --------------
  -- Uniforms --
  --------------
  --                                                   
  -- Scene and rendering configuration
  --
                                                                            
  generic
    type Uniform_T is private;
    Binding, Layout_Set : Int_Unsigned_C;
    Usage : Usage_Kind := GPU_Usage;
  package Uniform is
      procedure Set (Val : Uniform_T);
      function Get return Uniform_T;
    end;
  package body Uniform is   
      Uniform_Index : Positive := 1;
      Current_Val   : Uniform_T;
      function Get return Uniform_T is (Current_Val);
      procedure Set (Val : Uniform_T) is
        Buffer : Buffer_State := Uniform_Buffers.Element (Uniform_Index);
        Buffer_Info : aliased VkDescriptorBufferInfo := (buffer => Buffer.Data,
                                                         offset => Buffer.Offset,
                                                         rang   => Buffer.Size);
        Write : aliased VkWriteDescriptorSet := (dstSet          => To_Ptr (Int_Ptr (Layout_Set)),
                                                 dstBinding      => Binding,
                                                 descriptorCount => 1,
                                                 descriptorType  => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                                 pBufferInfo     => Buffer_Info'Unchecked_Access, others => <>);
        begin
          Current_Val := Val;
          Set (Buffer, Val'Address, Val'Size);
          vkUpdateDescriptorSets (Device, 1, Write'Unchecked_Access, 0, null);
        end;
    
    -- Register a new buffer and save the index for writes/sets
    begin
      Uniform_Buffers.Append ((Usage, Uniform_T'Object_Size, Is_Image => False, others => <>), 1);
      Uniform_Index := Int (Uniform_Buffers.Length);
    end;
                          
  -- ???
  package Model              is new Uniform (Layout_Set => 2, Binding =>  9, Uniform_T => Matrix_4D);
  package Projection         is new Uniform (Layout_Set => 2, Binding => 10, Uniform_T => Matrix_4D);
  package MVP                is new Uniform (Layout_Set => 2, Binding => 11, Uniform_T => Matrix_4D);
  package View               is new Uniform (Layout_Set => 2, Binding => 12, Uniform_T => Matrix_4D);
  package Transform          is new Uniform (Layout_Set => 2, Binding => 13, Uniform_T => Matrix_4D); -- S, T, Q, Enabled
  package Light_Projection   is new Uniform (Layout_Set => 2, Binding => 14, Uniform_T => Matrix_4D); -- S, T, Q
  package Local_Light_Origin is new Uniform (Layout_Set => 2, Binding => 15, Uniform_T => Vector_4D);
  package Local_View_Origin  is new Uniform (Layout_Set => 2, Binding => 16, Uniform_T => Vector_4D);
  package Color_Value        is new Uniform (Layout_Set => 2, Binding => 24, Uniform_T => Vector_4D);
  package Local_To_Global    is new Uniform (Layout_Set => 2, Binding => 25, Uniform_T => Vector_4D);
  package Local_To_Eye       is new Uniform (Layout_Set => 2, Binding => 26, Uniform_T => Vector_4D);
  --package Tesselation_Max    is new Uniform (Layout_Set => 2, Binding =>  6, Uniform_T => Real_64_C);
  --package Tesselation_Amount is new Uniform (Layout_Set => 2, Binding =>  7, Uniform_T => Real_64_C);
  --package Delta_MS           is new Uniform (Layout_Set => 2, Binding =>  8, Uniform_T => Int_64_Unsigned_C);
  --package Test_Alpha         is new Uniform (Layout_Set => 2, Binding => 17, Uniform_T => Vector_4D);
  --package Light_Falloff      is new Uniform (Layout_Set => 2, Binding => 18, Uniform_T => Vector_4D);
  --package Light_Scale        is new Uniform (Layout_Set => 2, Binding => 19, Uniform_T => Vector_4D);
  --package Screen_Factor      is new Uniform (Layout_Set => 2, Binding => 20, Uniform_T => Vector_4D);
  --package UI_Coord           is new Uniform (Layout_Set => 2, Binding => 21, Uniform_T => Vector_4D);
  --package Diffuse_Modifier   is new Uniform (Layout_Set => 2, Binding => 22, Uniform_T => Vector_4D);
  --package Specular_Modifier  is new Uniform (Layout_Set => 2, Binding => 23, Uniform_T => Vector_4D);
  --package Vertex_Color_Mod   is new Uniform (Layout_Set => 2, Binding => 27, Uniform_T => Vector_4D);
  --package Vertex_Color_Add   is new Uniform (Layout_Set => 2, Binding => 28, Uniform_T => Vector_4D);
  --package Overbright         is new Uniform (Layout_Set => 2, Binding => 29, Uniform_T => Vector_4D);
  
  --------------
  -- Samplers --
  --------------
                                                                            
  -- Texture map samplers  
  generic
    Binding, Layout_Set : Int_Unsigned_C;
    Usage : Usage_Kind := GPU_Usage;
  package Sampler is
      procedure Set (Val : Str);
      function Get return Str;
    end;
  package body Sampler is
      Current_Val : Str_Unbound := NULL_STR_UNBOUND;
      function Get return Str is (S (Current_Val));
      procedure Set (Val : Str) is
        Buffered_Image : Buffer_State := Buffered_Images.Element (U (Val));
        Image_Info : aliased VkDescriptorImageInfo := (imageLayout => VK_IMAGE_LAYOUT_GENERAL,
                                                       imageView   => Buffered_Image.View, 
                                                       sampler     => Buffered_Image.Sampler, others => <>);
        Write : aliased VkWriteDescriptorSet := (--dstSet          => Layout_Set,
                                                 dstBinding      => Binding,
                                                 descriptorCount => 1,
                                                 descriptorType  => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                 pImageInfo      => Image_Info'Unchecked_Access, others => <>);
        begin
          Current_Val := U (Val);
          vkUpdateDescriptorSets (Device, 1, Write'Unchecked_Access, 0, null);
        end;
    end;
  package Base_Color_Sampler is new Sampler (Layout_Set => 1, Binding => 0); 
  --package Irradiance_Sampler is new Sampler (Layout_Set => 1, Binding => 1);
  --package Filter_Sampler     is new Sampler (Layout_Set => 1, Binding => 2);
  --package Specular_Sampler   is new Sampler (Layout_Set => 1, Binding => 3);
  --package Normal_Sampler     is new Sampler (Layout_Set => 1, Binding => 4);
  --package Displace_Sampler   is new Sampler (Layout_Set => 1, Binding => 5);
  --package Metallic_Sampler   is new Sampler (Layout_Set => 1, Binding => 6);
  --package Roughness_Sampler  is new Sampler (Layout_Set => 1, Binding => 7);
  --package Projection_Sampler is new Sampler (Layout_Set => 1, Binding => 8);
  --package Falloff_Sampler    is new Sampler (Layout_Set => 1, Binding => 9);

  -- Texture map flags
  package Enable_Base_Color  is new Uniform (Layout_Set => 2, Binding => 0, Uniform_T => Bool);
  --package Enable_Irradiance  is new Uniform (Layout_Set => 2, Binding => 1, Uniform_T => Bool);
  --package Enable_Filter      is new Uniform (Layout_Set => 2, Binding => 2, Uniform_T => Bool); 
  --package Enable_Specular    is new Uniform (Layout_Set => 2, Binding => 3, Uniform_T => Bool); 
  --package Enable_Normal      is new Uniform (Layout_Set => 2, Binding => 4, Uniform_T => Bool); 
  --package Enable_Displace    is new Uniform (Layout_Set => 2, Binding => 5, Uniform_T => Bool); 
  --package Enable_Metallic    is new Uniform (Layout_Set => 2, Binding => 0, Uniform_T => Bool);
  --package Enable_Roughness   is new Uniform (Layout_Set => 2, Binding => 1, Uniform_T => Bool);
  --package Enable_Projection  is new Uniform (Layout_Set => 2, Binding => 2, Uniform_T => Bool); 
  --package Enable_Falloff     is new Uniform (Layout_Set => 2, Binding => 3, Uniform_T => Bool); 
             
  -------------
  -- Shaders --
  -------------
  --
  -- "Samplers" are texture parameters for GPU shaders
  --
    
  Shader_Entry_Name : aliased Str_8_C := "main";
  -- Define a shader for each pass within the backend  
  generic
    Path : Str;                                                              
    Vertex_Bindings : Array_VkDescriptorSetLayoutBinding := (1 => (binding         => 0, -- UBO
                                                                   descriptorCount => 1,
                                                                   descriptorType  => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                                                   stageFlags      => VK_SHADER_STAGE_VERTEX_BIT, others => <>));
    Fragment_Bindings : Array_VkDescriptorSetLayoutBinding := (1 => (binding         => 1, -- Sampler
                                                                     descriptorCount => 1,
                                                                     descriptorType  => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                                     stageFlags      => VK_SHADER_STAGE_FRAGMENT_BIT, others => <>));
    Animated_Vertex_Input : Array_VkVertexInputAttributeDescription := ((binding  => 0, -- Position
                                                                         location => 0,
                                                                         format   => VK_FORMAT_R32G32B32A32_SFLOAT,
                                                                         offset   => Static_Vertex_Obj.Point'Position / 8),
                                                                        (binding  => 0, -- Texture Coordinate
                                                                         location => 1,
                                                                         format   => VK_FORMAT_R16G16_SFLOAT,
                                                                         offset   => Static_Vertex_Obj.Texture'Position / 8),
                                                                        (binding  => 0, -- Normal
                                                                         location => 2,
                                                                         format   => VK_FORMAT_R8G8B8A8_UNORM,
                                                                         offset   => Static_Vertex_Obj.Normal'Position / 8),
                                                                        (binding  => 0, -- Velocity
                                                                         location => 3,
                                                                         format   => VK_FORMAT_R8G8B8A8_UNORM,
                                                                         offset   => Static_Vertex_Obj.Velocity'Position / 8));
    Static_Vertex_Input : Array_VkVertexInputAttributeDescription := ((binding  => 0, -- Texture Coordinate
                                                                       location => 1,
                                                                       format   => VK_FORMAT_R16G16_SFLOAT,
                                                                       offset   => Animated_Vertex_Obj.Texture'Position / 8),
                                                                      (binding  => 0, -- Start weight
                                                                       location => 2,
                                                                       format   => VK_FORMAT_R8G8B8A8_UNORM,
                                                                       offset   => Animated_Vertex_Obj.Start_Weight'Position / 8),
                                                                      (binding  => 0, -- Weight count
                                                                       location => 3,
                                                                       format   => VK_FORMAT_R8G8B8A8_UNORM,
                                                                       offset   => Animated_Vertex_Obj.Weight_Count'Position / 8),
                                                                      (binding  => 0, -- Velocity
                                                                       location => 4,
                                                                       format   => VK_FORMAT_R8G8B8A8_UNORM,
                                                                       offset   => Animated_Vertex_Obj.Velocity'Position / 8));
  package Shader is procedure Commit; end; package body Shader is
    procedure Commit is begin Pipeline.Shader := Shaders.Element (U (Path)); end;
    begin Shaders.Insert (U (Path), (others => <>)); end;
    
  package Depth_Pass  is new Shader ("depth"); 
  package Light_Pass  is new Shader ("light");
  --package Fog_Pass    is new Shader ("fog"); 
  --package Sky_Pass    is new Shader ("sky");
  --package UI_Pass     is new Shader ("ui"); 
  --package Post_Pass   is new Shader ("post");
  --package Shadow_Pass is new Shader (Path                  => "shadow",
  --                                   Static_Vertex_Input   => ((binding  => 0, -- Position
  --                                                              location => 0,
  --                                                              format   => VK_FORMAT_R32G32B32A32_SFLOAT,
  --                                                              offset   => Static_Vertex_Obj.Point'Position / Byte'Size),
  --                                                             (binding  => 0, -- Velocity
  --                                                              location => 4,
  --                                                              format   => VK_FORMAT_R8G8B8A8_UNORM,
  --                                                              offset   => Static_Vertex_Obj.Texture'Position / Byte'Size)),                                     
  --                                   Animated_Vertex_Input => ((binding  => 0, -- Start weight
  --                                                              location => 0,
  --                                                              format   => VK_FORMAT_R32G32B32A32_SFLOAT,
  --                                                              offset   => Animated_Vertex_Obj.Start_Weight'Position / Byte'Size),
  --                                                             (binding  => 0, -- Weight count
  --                                                              location => 4,
  --                                                              format   => VK_FORMAT_R8G8B8A8_UNORM,
  --                                                              offset   => Animated_Vertex_Obj.Weight_Count'Position / Byte'Size),
  --                                                             (binding  => 0, -- Velocity
  --                                                              location => 5,
  --                                                              format   => VK_FORMAT_R8G8B8A8_UNORM,
  --                                                              offset   => Animated_Vertex_Obj.Velocity'Position / Byte'Size)));

  -- Shader feature flags
  package Enable_Skinning    is new Uniform (Layout_Set => 2, Binding => 0, Uniform_T => Bool);
  --package Enable_Tesselation is new Uniform (Layout_Set => 2, Binding => 1, Uniform_T => Bool);
  --package Enable_Bloom       is new Uniform (Layout_Set => 2, Binding => 2, Uniform_T => Bool); 
  --package Enable_HDR         is new Uniform (Layout_Set => 2, Binding => 3, Uniform_T => Bool); 
  --package Enable_Blur        is new Uniform (Layout_Set => 2, Binding => 4, Uniform_T => Bool); 
  --package Enable_Haze        is new Uniform (Layout_Set => 2, Binding => 5, Uniform_T => Bool);
                                                              
  -------------
  -- Staging --
  ------------- 
  --
  -- Load a material into the samplers 3D Data buffers for skeletal mesh skinning, arbitrary limits are used for buffer sizes
  --

  WEIGHT_BUFFER_SET     : constant Natural  := 0; 
  WEIGHT_BUFFER_BINDING : constant Natural  := 0;
  WEIGHT_BUFFER_SIZE    : constant Positive := 666666 * 512;
  JOINT_BUFFER_SET      : constant Natural  := 0; 
  JOINT_BUFFER_BINDING  : constant Natural  := 0;
  JOINT_BUFFER_SIZE     : constant Positive := 666666 * 512;
  Vertex_Buffer         : aliased  Ptr      := NULL_PTR;
  Vertex_Offset         : aliased  Ptr      := NULL_PTR;  
  procedure Stage_Material (Path : Str) is
    Material : Material_State := Materials.Get (Path);
    Hash     : Str_Unbound    := Get_Material_Info_Hash (Material);

    -- Flag a meterial texture and setup a sampler
    procedure Stage_Image (Path        : Str_Unbound;
                           Set_Sampler : access procedure (Path : Str);
                           Set_Flag    : access procedure (Val : Bool)) is
      begin
        if Path = NULL_STR_UNBOUND then Set_Flag (False); return; end if;
        Set_Flag (True);
        Set_Sampler (S (Path & Hash));
      end;
    begin
      if Buffered_Materials.Contains (U (Path)) then raise Program_Error with "Unbuffered material encountered"; end if;
      Stage_Image (Material.Base_Color, Base_Color_Sampler.Set'Access, Enable_Base_Color.Set'Access);
      --Stage_Image (Material.Irradiance, Irradiance_Sampler.Set'Access, Enable_Irradiance.Set'Access);
      --Stage_Image (Material.Specular,   Specular_Sampler.Set'Access,   Enable_Specular.Set'Access);
      --Stage_Image (Material.Normal,     Normal_Sampler.Set'Access,     Enable_Normal.Set'Access);
      --Stage_Image (Material.Metallic,   Metallic_Sampler.Set'Access,   Enable_Metallic.Set'Access);
      --Stage_Image (Material.Roughness,  Roughness_Sampler.Set'Access,  Enable_Roughness.Set'Access);
    end;
  procedure Stage_Animated_Vertices (Mesh : Mesh_State) is
    begin
      Enable_Skinning.Set (True);
      --Adjust_Buffer (Vertex_Buffer, Animated_Verticies (1)'Address, );
      vkCmdBindVertexBuffers (Framebuffer (Current_Frame).Commands, 0, 1, Vertex_Buffer'Address, Vertex_Offset'Address);
    end;
  procedure Stage_Static_Vertices (Mesh : Mesh_State) is
    begin
      Enable_Skinning.Set (False);
      vkCmdBindVertexBuffers (Framebuffer (Current_Frame).Commands, 0, 1, Vertex_Buffer'Address, Vertex_Offset'Address);
    end;
  procedure Stage_Indicies (Triangles : Vector_Triangle.Unsafe.Vector) is
    Data : Vector_Triangle.Unsafe_Array := Vector_Triangle.To_Unsafe_Array (Triangles);
    begin
      null;--vkCmdBindIndexBuffer (Framebuffer (Current_Frame).Commands, Data (1)'Address, Indicies_Offset, VK_INDEX1_TYPE_UINT32);
    end;
  procedure Stage_Joints (Skeleton : Treed_Joint.Unsafe.Tree; Weights : Vector_Indexed_Weight.Unsafe.Vector) is
    Skeleton_Data : Vector_Joint.Unsafe_Array (1..Int (Skeleton.Node_Count)) := (others => (others => <>));
    Weight_Data   : Vector_Weight.Unsafe_Array (1..Int (Weights.Length)) := (others => (others => <>));
    begin
      Enable_Skinning.Set (True);
      --for Tree_Joint of Val loop
        --if Joints (
      --  Joints (Tree_Joint.Parent) := Tree_Joint;
      --end loop;
      --Adjust_Buffer (Joint_Buffer, Skeleton_Data (1)'Address, JOINT_BUFFER_SIZE);
      --vertexCache.GetJointBuffer( vkcontext.jointCacheHandle, &jointBuffer then
      --assert ((jointBuffer.GetOffset() & ( vkcontext.gpu->props.limits.minUniformBufferOffsetAlignment - 1 ) ) == 0 );
      --ubos[ uboIndex++ ] = &jointBuffer;
    end;

  --------------
  -- Subunits --
  --------------

  --procedure Frontend is separate;
  --procedure Backend  is separate;

  ----------------------------
  -- Initialize_Framebuffer --
  ----------------------------

  procedure Initialize_Framebuffer is

    -- Swap Chain
    Indexes : aliased Array_Int_Unsigned_C := (1 => Queue_Family);
    Swapchain_Info : aliased VkSwapchainCreateInfoKHR := (surface          => Surface,
                                                          imageArrayLayers => 1,
                                                          imageFormat      => Swapchain_Format.format,
                                                          imageColorSpace  => Swapchain_Format.colorSpace,
                                                          imageSharingMode => VK_SHARING_MODE_EXCLUSIVE,
                                                          imageUsage       => VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
                                                          compositeAlpha   => VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
                                                          presentMode      => VK_PRESENT_MODE_MAILBOX_KHR,
                                                          clipped          => VK_TRUE,
                                                          oldSwapchain     => Swapchain,
                                                          preTransform     => Surface_Details.currentTransform,
                                                          imageExtent      => Surface_Details.currentExtent,

      -- Pick the queue length or number of images in the swap-chain - push for one more than the minimum for triple buffering
      minImageCount => (if Surface_Details.maxImageCount > 0 and Surface_Details.minImageCount + 1 > Surface_Details.maxImageCount
                        then Surface_Details.maxImageCount else Surface_Details.minImageCount + 1), others => <>);
    Images : aliased Array_Ptr (1..Int (Swapchain_Info.minImageCount)) := (others => NULL_PTR);
                    
    -- Viewport 
    Viewport : aliased VkViewport := (x        => 0.0,
                                      y        => 0.0,
                                      width    => Real_C (Swapchain_Info.imageExtent.width),
                                      height   => Real_C (Swapchain_Info.imageExtent.height),
                                      minDepth => 0.0,
                                      maxDepth => 10.0, others => <>);
    Scissor : aliased VkRect2D := (offset => (0, 0), extent => Swapchain_Info.imageExtent, others => <>);
    Viewport_State_Info : aliased VkPipelineViewportStateCreateInfo := (viewportCount => 1,
                                                                        pViewports    => Viewport'Unchecked_Access,
                                                                        scissorCount  => 1,
                                                                        pScissors     => Scissor'Unchecked_Access, others => <>);

    -- Subpass
    Color_Reference : aliased VkAttachmentReference := (attachment => 0, layout => VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, others => <>);
    Depth_Reference : aliased VkAttachmentReference := (attachment => 1, layout => VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, others => <>);
    Subpasses_Description : aliased Array_VkSubpassDescription := (1 => (pipelineBindPoint       => VK_PIPELINE_BIND_POINT_GRAPHICS,
                                                                         colorAttachmentCount    => 1,
                                                                         pColorAttachments       => Color_Reference'Unchecked_Access,
                                                                         pDepthStencilAttachment => Depth_Reference'Unchecked_Access, others  => <>));
    Subpass_Dependencies : aliased Array_VkSubpassDependency := (1 => (srcSubpass    => VK_SUBPASS_EXTERNAL,
                                                                       dstSubpass    => 0,
                                                                       srcStageMask  => VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                                                                       srcAccessMask => 0,
                                                                       dstStageMask  => VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                                                                       dstAccessMask => VK_ACCESS_COLOR_ATTACHMENT_READ_BIT or
                                                                                        VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT, others => <>));
 
    -- Render Pass
    Clear_Values : aliased Array_VkClearValue := ((color => ((0.0, 1.0, 0.0, 1.0)), others => <>), (depthStencil => (1.0, 0), others => <>));
    Attachments : aliased Array_VkAttachmentDescription := ((format         => Swapchain_Format.format,
                                                             samples        => VK_SAMPLE_COUNT_1_BIT,
                                                             loadOp         => VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                             storeOp        => VK_ATTACHMENT_STORE_OP_STORE,
                                                             stencilLoadOp  => VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                             stencilStoreOp => VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                             initialLayout  => VK_IMAGE_LAYOUT_UNDEFINED,
                                                             finalLayout    => VK_IMAGE_LAYOUT_PRESENT_SRC_KHR, others => <>),
                                                            (format         => Depth_Format.format,
                                                             samples        => VK_SAMPLE_COUNT_1_BIT,
                                                             loadOp         => VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                             storeOp        => VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                             stencilLoadOp  => VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                             stencilStoreOp => VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                             initialLayout  => VK_IMAGE_LAYOUT_UNDEFINED,
                                                             finalLayout    => VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, others => <>));  
    Render_Pass_Info : aliased VkRenderPassCreateInfo := (attachmentCount => Attachments'Length,
                                                          pAttachments    => Attachments (1)'Unchecked_Access,
                                                          subpassCount    => Subpasses_Description'Length,
                                                          pSubpasses      => Subpasses_Description (1)'Unchecked_Access,
                                                          dependencyCount => Subpass_Dependencies'Length,
                                                          pDependencies   => Subpass_Dependencies (1)'Unchecked_Access, others => <>);
    Render_Pass_Begin_Info : aliased VkRenderPassBeginInfo := (clearValueCount => Clear_Values'Length,
                                                               pClearValues    => Clear_Values (1)'Unchecked_Access,
                                                               renderArea      => (Offset => (0, 0),
                                                                                   Extent => Swapchain_Info.imageExtent), others => <>);
    begin
      Assert (not Framebuffer_Status.Occupied);

      -- Wait for the device then create the swapchain and its associated framebuffer images and image views
      Framebuffer := new Vector_Framebuffer.Unsafe_Array (Images'Range);
      vkAssert (vkDeviceWaitIdle (Device));
      vkAssert (vkCreateSwapchainKHR (Device, Swapchain_Info'Unchecked_Access, null, Swapchain'Unchecked_Access));
      vkAssert (vkGetSwapchainImagesKHR (device               => Device,
                                         swapchain            => Swapchain,
                                         pSwapchainImageCount => Swapchain_Info.minImageCount'Unchecked_Access,
                                         pSwapchainImages     => Images (1)'Unchecked_Access));
      for I in Framebuffer'Range loop
        Framebuffer (I).Image := Images (I);
        declare
        Image_View_Info : aliased VkImageViewCreateInfo := (image            => Framebuffer (I).Image,
                                                            viewType         => VK_IMAGE_VIEW_TYPE_2D,
                                                            format           => Swapchain_Format.format,
                                                            components       => (VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                 VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                 VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                 VK_COMPONENT_SWIZZLE_IDENTITY),
                                                            subresourceRange => (aspectMask => VK_IMAGE_ASPECT_COLOR_BIT,
                                                                                 levelCount => 1,
                                                                                 layerCount => 1, others => <>), others => <>);
        begin
          vkAssert (vkCreateImageView (Device, Image_View_Info'Unchecked_Access, null, Framebuffer (I).Image_View'Unchecked_Access));
        end;
      end loop;
 
      -- Render pass
      vkAssert (vkCreateRenderPass (Device, Render_Pass_Info'Unchecked_Access, null, Render_Pass'Unchecked_Access));
                 
      -- Create depth resources and render targets
      declare
      Attachment_Data       : aliased Array_Ptr (Attachments'Range) := (others => NULL_PTR);
      Frame_Buffer_Info     : aliased VkFramebufferCreateInfo       := (others => <>);      
      Depth_Image_View_Info : aliased VkImageViewCreateInfo         := (format           => Depth_Format.format,
                                                                        viewType         => VK_IMAGE_VIEW_TYPE_2D,
                                                                        components       => (VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                             VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                             VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                             VK_COMPONENT_SWIZZLE_IDENTITY),
                                                                        subresourceRange => (aspectMask => VK_IMAGE_ASPECT_DEPTH_BIT
                                                                                                           or VK_IMAGE_ASPECT_STENCIL_BIT,
                                                                                             levelCount => 1,
                                                                                             layerCount => 1, others => <>), others => <>);
      Depth_Image_Info : aliased VkImageCreateInfo := (initialLayout => VK_IMAGE_LAYOUT_UNDEFINED,
                                                       sharingMode   => VK_SHARING_MODE_EXCLUSIVE,
                                                       tiling        => VK_IMAGE_TILING_OPTIMAL,
                                                       imageType     => VK_IMAGE_TYPE_2D,
                                                       samples       => VK_SAMPLE_COUNT_1_BIT,
                                                       format        => Depth_Format.format,
                                                       extent        => (Surface_Extent.width, Surface_Extent.height, 1),
                                                       mipLevels     => 1,
                                                       arrayLayers   => 1,
                                                       usage         => VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, others => <>);
      begin


        -- Depth image
        vkAssert (vkCreateImage (Device, Depth_Image_Info'Unchecked_Access, null, Depth_Image.Data'Unchecked_Access));
        Allocate (Depth_Image);
        vkAssert (vkBindImageMemory (Device, Depth_Image.Data, Depth_Image.Memory.Device_Memory, Depth_Image.Memory.Offset));
        
        -- Depth image view
        Depth_Image_View_Info.image := Depth_Image.Data;
        vkAssert (vkCreateImageView  (Device, Depth_Image_View_Info'Unchecked_Access, null, Depth_Image.View'Unchecked_Access));
        Attachment_Data (2) := Depth_Image.View;

        -- Framebuffer attachments
        for I in Framebuffer'Range loop
          Attachment_Data (1) := Framebuffer (I).Image_View;
          Frame_Buffer_Info := (renderPass      => Render_Pass,
                                attachmentCount => Attachment_Data'Length,
                                pAttachments    => Attachment_Data (1)'Unchecked_Access,
                                width           => Swapchain_Info.imageExtent.width,
                                height          => Swapchain_Info.imageExtent.height,
                                layers          => 1, others => <>);
          vkAssert (vkCreateFramebuffer (Device, Frame_Buffer_Info'Unchecked_Access, null, Framebuffer (I).Swapchain_Buffer'Unchecked_Access));
        end loop;
      end;
      
      -- Signal that the framebuffer is in a valid state and is not in need of reset
      Framebuffer_Status.Occupied (True);
    end;

  --------------------------
  -- Finalize_Framebuffer --
  --------------------------

  procedure Finalize_Framebuffer is
    begin
      Assert (not Framebuffer_Status.Occupied);
    
      -- Depth image
      vkDestroySampler   (Device, Depth_Image.Sampler, null);
      vkDestroyImage     (Device, Depth_Image.Data,    null);
      vkDestroyImageView (Device, Depth_Image.View,    null);
      Memory_Garbage.Append (Depth_Image.Memory);
    
      -- Render targets
      for Frame of Framebuffer.all loop
        vkDestroyDescriptorPool (Device, Frame.Descriptor_Pool,  null);
        vkDestroyImage          (Device, Frame.Image,            null);
        vkDestroyImageView      (Device, Frame.Image_View,       null);
        vkDestroyFramebuffer    (Device, Frame.Swapchain_Buffer, null);
        vkFreeCommandBuffers    (Device, Command_Pool, 1, Frame.Commands'Unchecked_Access);
      end loop;
      Vector_Framebuffer.Free (Framebuffer);      
      
      -- Pipeline
      vkDestroyRenderPass   (Device, Render_Pass, null);
      vkDestroySwapchainKHR (Device, Swapchain,   null);
      
      -- Garbage
      Free_Image_Garbage;
      Free_Memory_Garbage;
    end; 

  ----------------
  -- Initialize --
  ----------------
  
  procedure Initialize is

    -- Load the function pointers from our driver library
    procedure Initialize_Vulkan_Subprograms is new API.Vulkan.Initialize (Get_Vulkan_Subprogram);
    
    -- Guess the VRAM amount from memory properties
    function VRAM (Memory : VkPhysicalDeviceMemoryProperties) return Int_64_Unsigned_C is
      Result : Int_64_Unsigned_C := 0;
      begin
        for Heap of Memory.memoryHeaps loop
          if (Heap.flags and VK_MEMORY_HEAP_DEVICE_LOCAL_BIT) > 0 and Heap.size > Result then Result := Heap.size; end if;
        end loop;
        return Result;
      end;

    -- Temporary variables for fetching and testing physical devices  
    Count,
    Current_Graphics_Family,
    Current_Present_Family,
    Current_Surface_Support   : aliased Int_Unsigned_C                   := 0;
    Current_Depth_Format      : aliased VkSurfaceFormatKHR               := (others => <>);
    Current_Swapchain_Format  : aliased VkSurfaceFormatKHR               := (others => <>);
    Current_Format_Properties : aliased VkFormatProperties               := (others => <>);
    Current_Surface_Details   : aliased VkSurfaceCapabilitiesKHR         := (others => <>);
    Current_Device_Properties : aliased VkPhysicalDeviceProperties       := (others => <>);
    Current_Memory_Properties : aliased VkPhysicalDeviceMemoryProperties := (others => <>);
    
    -- Atomic GPU queue semaphores 
    Semaphore_Info : aliased VkSemaphoreCreateInfo := (others => <>);
                                                                      
    -- Command pools are needed for the creation of command buffers
    Command_Pool_Info : aliased VkCommandPoolCreateInfo := (flags => VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT, others => <>);

    -- Application details
    Engine_Name      : aliased Str_8_C           := To_Str_8_C (NAME_ID);
    Application_Name : aliased Str_8_C           := To_Str_8_C (S (OS_Info.App_Name));
    Application_Info : aliased VkApplicationInfo := (pApplicationName   => C (Application_Name),
                                                     applicationVersion => VK_MAKE_VERSION (VERSION),
                                                     pEngineName        => C (Engine_Name),
                                                     engineVersion      => VK_MAKE_VERSION (VERSION),
                                                     apiVersion         => VK_API_VERSION_1_0, others => <>);
    Instance_Info : aliased VkInstanceCreateInfo := (pApplicationInfo        => Application_Info'Unchecked_Access,
                                                     enabledExtensionCount   => INSTANCE_EXTENSIONS'Length,
                                                     ppenabledExtensionNames => INSTANCE_EXTENSIONS (1)'Address,
                                                     enabledLayerCount       => (if Is_Debugging then DEBUGING_EXTENSIONS'Length else 0),
                                                     ppEnabledLayerNames     => (if Is_Debugging then DEBUGING_EXTENSIONS (1)'Address else NULL_PTR), others => <>);

    -- Enabled extensions and layers for instance and device creation
    Queue_Priority  : aliased Real_C                  := 1.0;
    Queue_Info      : aliased VkDeviceQueueCreateInfo := (queueCount => 1, pQueuePriorities => Queue_Priority'Unchecked_Access, others => <>);
    Device_Features : aliased VkPhysicalDeviceFeatures := (shaderClipDistance                     => VK_TRUE,
                                                           shaderCullDistance                     => VK_TRUE,
                                                           shaderTessellationAndGeometryPointSize => VK_TRUE,
                                                           geometryShader                         => VK_TRUE, others => <>);
    Device_Info : aliased VkDeviceCreateInfo  := (queueCreateInfoCount    => 1,
                                                  pQueueCreateInfos       => Queue_Info'Unchecked_Access,
                                                  pEnabledFeatures        => Device_Features'Unchecked_Access,
                                                  enabledExtensionCount   => (if Is_Debugging then DEVICE_EXTENSIONS'Length else 0),
                                                  ppenabledExtensionNames => (if Is_Debugging then DEVICE_EXTENSIONS (1)'Address else NULL_PTR), others  => <>);
 
    -- Shaders
    Shader                :         Shader_State                      := (others => <>);
    Shader_Module_Info    : aliased VkShaderModuleCreateInfo          := (others => <>);
    Descriptor_Pool_Sizes : aliased Array_VkDescriptorPoolSize (1..2) := (others => (others => <>));
    Descriptor_Pool_Info  : aliased VkDescriptorPoolCreateInfo        := (poolSizeCount => Descriptor_Pool_Sizes'Length,
                                                                          pPoolSizes    => Descriptor_Pool_Sizes (1)'Unchecked_Access,
                                                                          maxSets       => 1, others => <>);

    -- Staging
    Alignment_Mod                :         Int_64_Unsigned_C           := 0;
    Alignment_Size               :         Int_64_Unsigned_C           := 0;
    Memory_Requirements          : aliased VkMemoryRequirements        := (others => <>);
    Fence_Info                   : aliased VkFenceCreateInfo           := (others => <>);
    Memory_Allocate_Info         : aliased VkMemoryAllocateInfo        := (others => <>);
    Command_Buffer_Allocate_Info : aliased VkCommandBufferAllocateInfo := (others => <>);
    Command_Buffer_Begin_Info    : aliased VkCommandBufferBeginInfo    := (others => <>);
    Buffer_Info                  : aliased VkBufferCreateInfo          := (size  => Int_64_Unsigned (Max_Upload_Buffer.Get * 1024**2),
                                                                           usage => VK_BUFFER_USAGE_TRANSFER_SRC_BIT, others => <>);
    begin
      
      -- Load driver
      Initialize_Vulkan_Library;
      Initialize_Vulkan_Subprograms;

      -- Create instance and rendering surface
      vkAssert (vkCreateInstance (Instance_Info'Unchecked_Access, null, Instance'Unchecked_Access));

      -- Create instance and rendering surface
      Surface := Create_Vulkan_Surface (Instance);
      
      -- Pick a physical devices
      vkAssert (vkEnumeratePhysicalDevices (Instance, Count'Unchecked_Access, null));
      declare Physical_Devices : aliased Array_Ptr := (1..Int (Count) => NULL_PTR); begin
        vkAssert (vkEnumeratePhysicalDevices (Instance, Count'Unchecked_Access, Physical_Devices (1)'Unchecked_Access));
      
        -- Find the best physical device and wrap it in a block to catch device verification exceptions
        for Current_Physical_Device of Physical_Devices loop begin
        
          -- Get queues
          vkGetPhysicalDeviceQueueFamilyProperties (Current_Physical_Device, Count'Unchecked_Access, null);
          declare Queue_Family_Properties : aliased Array_VkQueueFamilyProperties := (1..Int (Count) => (others => <>)); begin
            vkGetPhysicalDeviceQueueFamilyProperties (physicalDevice            => Current_Physical_Device,
                                                      pQueueFamilyPropertyCount => Count'Unchecked_Access,
                                                      pQueueFamilyProperties    => Queue_Family_Properties (1)'Unchecked_Access);
  
            -- Verify a queue exists with graphical surface support; then aquire it
            Current_Graphics_Family := -1;
            Current_Present_Family  := -1;
            for I in Queue_Family_Properties'Range loop 
              vkAssert (vkGetPhysicalDeviceSurfaceSupportKHR (physicalDevice   => Current_Physical_Device,
                                                              queueFamilyIndex => Int_Unsigned_C (I - 1),
                                                              surface          => Surface,
                                                              pSupported       => Current_Surface_Support'Unchecked_Access));
               
               -- Check if the device supports surfaces and 3D graphics  
               Current_Present_Family := Int_Unsigned_C (I) - 1;
               if (Queue_Family_Properties (I).queueFlags and VK_QUEUE_GRAPHICS_BIT) > 0 then
                 Current_Graphics_Family := Int_Unsigned_C (I) - 1;
               end if;
               exit when Current_Surface_Support = VK_TRUE and Current_Graphics_Family /= -1;
               Assert (I /= Queue_Family_Properties'Last);
            end loop;
            Assert (Current_Present_Family = Current_Graphics_Family);
          end;
  
          -- Get supported surface image formats for later query
          vkAssert (vkGetPhysicalDeviceSurfaceFormatsKHR (Current_Physical_Device, Surface, Count'Unchecked_Access, null));
          declare Surface_Image_Formats : Vector_VkSurfaceFormatKHR.Unsafe_Array := (1..Int (Count) => (others => <>)); begin
            vkAssert (vkGetPhysicalDeviceSurfaceFormatsKHR (physicalDevice      => Current_Physical_Device,
                                                            surface             => Surface,
                                                            pSurfaceFormatCount => Count'Unchecked_Access,
                                                            pSurfaceFormats     => Surface_Image_Formats (1)'Unchecked_Access));
            for I in Surface_Image_Formats'Range loop
              if SUPPORTED_SWAPCHAIN_FORMAT = Surface_Image_Formats (I) then
                Current_Swapchain_Format := Surface_Image_Formats (I);
                exit;
              end if;
              Assert (I /= Surface_Image_Formats'Last);
            end loop;
          end;
          
          -- Find a suitable depth format
          for I in SUPPORTED_DEPTH_FORMATS'Range loop
            vkGetPhysicalDeviceFormatProperties (physicalDevice    => Current_Physical_Device,
                                                 format            => SUPPORTED_DEPTH_FORMATS (I),
                                                 pFormatProperties => Current_Format_Properties'Unchecked_Access);
                                                     
            -- We require optimal tiling
            if (Current_Format_Properties.optimalTilingFeatures and VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT) > 0 then
              Current_Depth_Format.format := SUPPORTED_DEPTH_FORMATS (I);
              exit;
            end if;
            Assert (I /= SUPPORTED_DEPTH_FORMATS'Last);
          end loop;
            
          -- Check extensions are available
          vkAssert (vkEnumerateDeviceExtensionProperties (Current_Physical_Device, null, Count'Unchecked_Access, null));
          declare Extensions : aliased Array_VkExtensionProperties := (1..Int (Count) => (others => <>)); begin
            vkAssert (vkEnumerateDeviceExtensionProperties (physicalDevice => Current_Physical_Device,
                                                            pLayerName     => null,
                                                            pPropertyCount => Count'Unchecked_Access,
                                                            pProperties    => Extensions (1)'Unchecked_Access));
            for Required_Extension of DEVICE_EXTENSIONS loop
              for I in Extensions'Range loop
                exit when To_Str_8 (Extensions (I).extensionName) = To_Str_8 (Required_Extension);
                Assert (I /= Extensions'Last);
              end loop;
            end loop;
          end;
            
          -- Get device surface presentation mode information
          vkAssert (vkGetPhysicalDeviceSurfacePresentModesKHR (Current_Physical_Device, Surface, Count'Unchecked_Access, null));
          declare Present_Modes : aliased Array_Int_Unsigned_C := (1..Int (Count) => 0); begin
            vkAssert (vkGetPhysicalDeviceSurfacePresentModesKHR (physicalDevice    => Current_Physical_Device,
                                                                 surface           => Surface,
                                                                 pPresentModeCount => Count'Unchecked_Access,
                                                                 pPresentModes     => Present_Modes'Unchecked_Access));
 
            -- We require mailbox mode so we can implement something past double buffering
            for I in Present_Modes'Range loop
              exit when Present_Modes (I) = VK_PRESENT_MODE_MAILBOX_KHR;
              Assert (I /= Present_Modes'Last);
            end loop;
          end;
          
          -- We have found a suitable device, but is it the best one our machine has to offer?
          vkGetPhysicalDeviceProperties       (Current_Physical_Device, Current_Device_Properties'Unchecked_Access);
          vkGetPhysicalDeviceMemoryProperties (Current_Physical_Device, Current_Memory_Properties'Unchecked_Access);
          
          -- Case 1: No chosen device - accept the current one unconditionally
          if Physical_Device = NULL_PTR 
          
            -- Case 2: The chosen device is not discrete and the current one is - prefer the current one
            or else (Device_Properties.deviceType        /= VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU and
                     Current_Device_Properties.deviceType = VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU) 
                     
            -- Case 3: The chosen device and the current one are both either not discrete or discrete - prefer the one with more VRAM
            or else (((Device_Properties.deviceType         = VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU and
                       Current_Device_Properties.deviceType = VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU)
                        or (Device_Properties.deviceType         /= VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU and
                            Current_Device_Properties.deviceType /= VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU))
                     and then VRAM (Current_Memory_Properties) > VRAM (Memory_Properties))
          then
            Device_Properties := Current_Device_Properties;
            Physical_Device   := Current_Physical_Device;
            Swapchain_Format  := Current_Swapchain_Format;
            Depth_Format      := Current_Depth_Format;
            Memory_Properties := Current_Memory_Properties;
            Queue_Family      := Current_Graphics_Family;
          end if;
 
        -- Ignore unsuitable devices and continue looking at the rest of them
        exception when others => null; end; end loop;
 
        -- We may have failed to find a device, so crash if thats the case
        Assert (Physical_Device);
        
        -- Get additional device properties
        vkAssert (vkGetPhysicalDeviceSurfaceCapabilitiesKHR (Physical_Device, Surface, Surface_Details'Unchecked_Access));
        vkAssert (vkGetPhysicalDeviceImageFormatProperties (physicalDevice         => Physical_Device,
                                                            format                 => Swapchain_Format.format, 
    	                                                    typ                    => VK_IMAGE_TYPE_2D,
                                                            tiling                 => VK_IMAGE_TILING_OPTIMAL,
                                                            usage                  => VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
                                                            flags                  => 0,
                                                            pImageFormatProperties => Image_Properties'Unchecked_Access));
        Surface_Extent := Surface_Details.minImageExtent;
        
        -- Log device information
        Line ("GPU: " & To_Str (Device_Properties.deviceName) & " w/ " & VK_VERSION_STR (Device_Properties.driverVersion));
        Line ("Graphics API: Vulkan " & VK_VERSION_STR (Device_Properties.apiVersion));
      exception when others => Line ("Your GPU is incompatible!"); raise Program_Error; end;
      
      -- Logical device
      Queue_Info.queueFamilyIndex := Queue_Family;
      vkAssert (vkCreateDevice (Physical_Device, Device_Info'Unchecked_Access, null, Device'Access));
      vkGetDeviceQueue (Device, Queue_Family, 0, Queue'Unchecked_Access);
        
      -- Command pools
      Command_Pool_Info.queueFamilyIndex := Queue_Family;
      vkAssert (vkCreateCommandPool (Device, Command_Pool_Info'Unchecked_Access, null, Command_Pool'Access));
      vkAssert (vkCreateCommandPool (Device, Command_Pool_Info'Unchecked_Access, null, Staging_Command_Pool'Access));

      -- Staging
      Command_Buffer_Allocate_Info := (commandPool => Staging_Command_Pool, commandBufferCount => 1, others => <>);
      vkAssert (vkAllocateCommandBuffers (Device, Command_Buffer_Allocate_Info'Unchecked_Access, Staging_Commands'Access));
      vkAssert (vkBeginCommandBuffer     (Staging_Commands, Command_Buffer_Begin_Info'Unchecked_Access));
      vkAssert (vkCreateBuffer           (Device, Buffer_Info'Unchecked_Access, null, Staging_Buffer'Access));
      vkGetBufferMemoryRequirements      (Device, Staging_Buffer, Memory_Requirements'Unchecked_Access);
      Alignment_Mod        := Memory_Requirements.size mod Memory_Requirements.alignment;
      Alignment_Size       := Memory_Requirements.size + (if Alignment_Mod = 0 then 0 else Memory_Requirements.alignment - Alignment_Mod);
      Memory_Allocate_Info := (allocationSize  => Alignment_Size,
                               memoryTypeIndex => Find_Memory_Type_Index (Memory_Requirements.memoryTypeBits, CPU_To_GPU_Usage), others => <>);
      vkAssert (vkAllocateMemory   (Device, Memory_Allocate_Info'Unchecked_Access, null, Staging_Memory'Access));
      vkAssert (vkBindBufferMemory (Device, Staging_Buffer, Staging_Memory, 0));
      vkAssert (vkMapMemory        (Device, Staging_Memory, 0, Alignment_Size, 0, Staging_Data'Access));
      vkAssert (vkCreateFence      (Device, Fence_Info'Unchecked_Access, null, Staging_Fence'Access));
      
      -- Semaphores
      vkAssert (vkCreateSemaphore (Device, Semaphore_Info'Unchecked_Access, NULL_PTR, Acquire_Status'Unchecked_Access));
      vkAssert (vkCreateSemaphore (Device, Semaphore_Info'Unchecked_Access, NULL_PTR, Render_Status'Unchecked_Access));
      
      -- Uniform buffers
      for Uniform of Uniform_Buffers loop        
        Buffer_Info := (size  => Uniform.Size,
                        usage => VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT or (if Uniform.Usage = GPU_Usage then VK_BUFFER_USAGE_TRANSFER_DST_BIT else 0), others => <>);
        vkAssert (vkCreateBuffer (Device, Buffer_Info'Unchecked_Access, null, Uniform.Data'Unchecked_Access));
        Allocate (Uniform);
      end loop;

      -- Shaders
      for I in Shaders.Iterate loop
        for Kind in Shader_Kind'Range loop null;
          --declare
          --Data : aliased Array_Byte := Load (S (Map_Shader.Unsafe.Key (I)) & "-" & To_Lower (Kind'Wide_Image)(1..4) & ".spv");
          --begin
          --  Shader_Module_Info := (codeSize => Data'Length, pCode => Data'Address, others => <>);
          --  vkAssert (vkCreateShaderModule (Device, Shader_Module_Info'Unchecked_Access, null, Shader.Data (Kind)'Unchecked_Access));
          --  vkAssert (vkCreateDescriptorSetLayout (device      => Device,
          --                                         pCreateInfo => null,--Map_Shader.Unsafe.Element (I).Descriptor_Set_Layout,
          --                                         pAllocator  => null,
          --                                         pSetLayout  => Descriptor_Set_Layout'Unchecked_Access));
          --exception when others => Shader.Data (Kind) := NULL_PTR; end;
        end loop;
        Shaders.Replace_Element (I, Shader);
      end loop;
      Descriptor_Pool_Sizes := ((typ => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, descriptorCount => 16384, others => <>), -- 16384
                                (typ => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, descriptorCount => 12384, others => <>)); -- Device_Limits.maxDescriptorSetSampledImages ???
      vkAssert (vkCreateDescriptorPool (Device, Descriptor_Pool_Info'Unchecked_Access, null, Descriptor_Pool'Unchecked_Access));
      
      -- Kick off the second stage of initialization
      Framebuffer_Status.Occupied (False);
      Initialize_Framebuffer;
      --Frontend_Task.Initialize;
      --Backend_Task.Initialize;
    exception when others => Line ("Vulkan driver not installed or up to date?"); raise Program_Error; end;

  --------------
  -- Finalize --
  --------------

  -- Kill anything that is not controlled and that is not the main Device or Instance
  procedure Finalize is
    begin

      -- Textures
      for Buffered_Image of Buffered_Images loop
        vkDestroySampler   (Device, Buffered_Image.Sampler, null);
        vkDestroyImage     (Device, Buffered_Image.Data,    null);
        vkDestroyImageView (Device, Buffered_Image.View,    null);
      end loop;

      -- Uniforms
      for Uniform of Uniform_Buffers loop vkDestroyBuffer (Device, Uniform.Data, null); end loop;
      
      -- Vertex buffer
      
      -- Index buffer
      
      -- Joint buffer

      -- Staging 
      vkUnmapMemory        (Device, Staging_Memory);
      vkDestroyFence       (Device, Staging_Fence,  null);
      vkDestroyBuffer      (Device, Staging_Buffer, null);
      vkFreeCommandBuffers (Device, Command_Pool, 1, Staging_Commands'Unchecked_Access);
      
      -- Globals
      vkDestroyDescriptorPool      (Device, Descriptor_Pool,       null);
      vkDestroyDescriptorSetLayout (Device, Descriptor_Set_Layout, null);
      vkDestroySemaphore           (Device, Acquire_Status,        null);
      vkDestroySemaphore           (Device, Render_Status,         null);  
      vkDestroyCommandPool         (Device, Command_Pool,          null);  
    
      -- Framebuffer
      Framebuffer_Status.Occupied (False);
      Finalize_Framebuffer;
      
      -- Device
      vkDestroyDevice     (Device, null);
      vkDestroySurfaceKHR (Instance, Surface, null);
      vkDestroyInstance   (Instance, null);
    end;

  ----------
  -- Draw --
  ----------

  procedure Draw is
    Current_Pipeline    : aliased Ptr                         := NULL_PTR;
    Set_Allocation_Info : aliased VkDescriptorSetAllocateInfo := (descriptorPool     => Descriptor_Pool,
                                                                  descriptorSetCount => 1,
                                                                  pSetLayouts        => Pipeline.Shader.Descriptor_Set_Layout'Unchecked_Access, others => <>);
    begin
    
      -- Optimize out searching through buffered pipelines if our previous one is a match
      if Previous_Pipeline.Val = Pipeline then
        Current_Pipeline := Previous_Pipeline.Data;
        goto Have_Pipeline;
      end if;

      -- Find pipeline
      for Buffered_Pipeline of Buffered_Pipelines loop -- Linear search....
        if Buffered_Pipeline.Val = Pipeline then
          Current_Pipeline := Buffered_Pipeline.Data;
          goto Have_Pipeline;
        end if;
      end loop;

      -- Create a pipeline if one was not found
      declare
      Viewport_Info       : aliased VkPipelineViewportStateCreateInfo      := (viewportCount => 1, scissorCount => 1, others => <>);
      Input_Assembly_Info : aliased VkPipelineInputAssemblyStateCreateInfo := (topology => VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, others => <>);                                                    
      Multisample_Info : aliased VkPipelineMultisampleStateCreateInfo := (sampleShadingEnable  => VK_TRUE,
                                                                          rasterizationSamples => Int_Unsigned_C (Sampling_Kind'Pos (Sampling.Get)),
                                                                          minSampleShading     => 1.0, others => <>);
                                                                          
      -- Shader
      Vertex_Binding : aliased Array_VkVertexInputBindingDescription := (1 => (binding   => 0,
                                                                               stride    => Static_Vertex_State'Object_Size / Byte'Object_Size,
                                                                               inputRate => VK_VERTEX_INPUT_RATE_VERTEX));
      Vertex_Input_Info : aliased VkPipelineVertexInputStateCreateInfo :=
        (vertexBindingDescriptionCount   => Vertex_Binding'Length,
         pVertexBindingDescriptions      => Vertex_Binding (1)'Unchecked_Access,
         vertexAttributeDescriptionCount => Pipeline.Shader.Vertex_Input_Attributes'Length,
         pVertexAttributeDescriptions    => Pipeline.Shader.Vertex_Input_Attributes (1)'Unchecked_Access, others => <>);   
      Shader_Stages_Info : aliased Array_VkPipelineShaderStageCreateInfo := ((stage  => VK_SHADER_STAGE_VERTEX_BIT,
                                                                              module => Pipeline.Shader.Data (Vertex_Shader),
                                                                              pName  => C (Shader_Entry_Name), others => <>),
                                                                             (stage  => VK_SHADER_STAGE_FRAGMENT_BIT,
                                                                              module => Pipeline.Shader.Data (Fragment_Shader),
                                                                              pName  => C (Shader_Entry_Name), others => <>)); 
              
      -- Rasterization
      Rasterizer_Info : aliased VkPipelineRasterizationStateCreateInfo := 
        (polygonMode             => VK_POLYGON_MODE_FILL,
         rasterizerDiscardEnable => VK_FALSE,
         depthClampEnable        => VK_FALSE,
         depthBiasEnable         => To_VkBool32 (Pipeline.Has_Polygon_Offset),
         lineWidth               => 1.0,
         frontFace               => Pipeline.Front_Face,
         cullMode                => (case Pipeline.Cull_Mode is
                                       when VK_CULL_MODE_BACK_BIT  => (if Pipeline.Has_Mirror_View then VK_CULL_MODE_FRONT_BIT
                                                                       else VK_CULL_MODE_BACK_BIT),
                                       when VK_CULL_MODE_FRONT_BIT => (if Pipeline.Has_Mirror_View then VK_CULL_MODE_BACK_BIT
                                                                       else VK_CULL_MODE_FRONT_BIT),
                                       when others                 => Pipeline.Cull_Mode), others => <>);
                                                                          
      -- Depth stencil
      Depth_Stencil_Info : aliased VkPipelineDepthStencilStateCreateInfo :=
        (depthTestEnable       => VK_TRUE,
         depthCompareOp        => Pipeline.Depth_Compare,
         depthWriteEnable      => To_VkBool32 (Pipeline.Depth_Write_Enable),
         depthBoundsTestEnable => To_VkBool32 (Pipeline.Depths_Bounds_Test),
         minDepthBounds        => 0.0,
         maxDepthBounds        => 1.0,
         stencilTestEnable     => To_VkBool32 ((Pipeline.Front_Fail or Pipeline.Front_Pass or Pipeline.Front_Depth_Fail) /= 0 or
                                                 (if not Pipeline.Has_Back_Stencil then False
                                                  else (Pipeline.Back_Fail or Pipeline.Back_Pass or Pipeline.Back_Depth_Fail) /= 0)),
         front                 => (writeMask   => 16#FFFF_FFFF#,
                                   compareOp   => Pipeline.Stencil_Compare,
                                   compareMask => Pipeline.Compare_Mask,
                                   reference   => Pipeline.Stencil_Reference,
                                   failOp      => Pipeline.Front_Fail,
                                   passOp      => Pipeline.Front_Pass,
                                   depthFailOp => Pipeline.Front_Depth_Fail),
         back                  => (writeMask   => 16#FFFF_FFFF#,
                                   compareOp   => Pipeline.Stencil_Compare,
                                   compareMask => Pipeline.Compare_Mask,
                                   reference   => Pipeline.Stencil_Reference,
                                   failOp      => (if Pipeline.Has_Back_Stencil then Pipeline.Back_Fail       else Pipeline.Front_Fail),
                                   passOp      => (if Pipeline.Has_Back_Stencil then Pipeline.Back_Pass       else Pipeline.Front_Pass),
                                   depthFailOp => (if Pipeline.Has_Back_Stencil then Pipeline.Back_Depth_Fail else Pipeline.Front_Depth_Fail)), others => <>); 

      -- Color Blend
      Color_Blend_Attachment : aliased VkPipelineColorBlendAttachmentState :=
        (srcColorBlendFactor => Pipeline.Source_Blend_Factor,
         dstColorBlendFactor => Pipeline.Destination_Blend_Factor,
         srcAlphaBlendFactor => Pipeline.Source_Blend_Factor,
         dstAlphaBlendFactor => Pipeline.Destination_Blend_Factor,
         colorBlendOp        => Pipeline.Color_Blend,
         alphaBlendOp        => Pipeline.Alpha_Blend,
         blendEnable         => (if Pipeline.Destination_Blend_Factor /= VK_BLEND_FACTOR_ZERO
                                   or Pipeline.Source_Blend_Factor /= VK_BLEND_FACTOR_ONE then VK_TRUE else VK_FALSE),
         colorWriteMask      => Pipeline.Color_Mask, others => <>);
      Color_Blend_Info : aliased VkPipelineColorBlendStateCreateInfo := (logicOpEnable   => 0,
                                                                         logicOp         => VK_LOGIC_OP_COPY,
                                                                         attachmentCount => 1,
                                                                         pAttachments    => Color_Blend_Attachment'Unchecked_Access,
                                                                         blendConstants  => (others => 0.0), others => <>);   

      -- Dynamic States
      Dynamic_States : aliased Array_Int_Unsigned_C := (VK_DYNAMIC_STATE_SCISSOR,
                                                        VK_DYNAMIC_STATE_VIEWPORT,
                                                        (if Pipeline.Has_Polygon_Offset then VK_DYNAMIC_STATE_DEPTH_BIAS
                                                         else VK_DYNAMIC_STATE_DEPTH_BOUNDS), -- Jump through some hoops
                                                        VK_DYNAMIC_STATE_DEPTH_BIAS);
      Dynamic_State_Info : aliased VkPipelineDynamicStateCreateInfo := (pDynamicStates    => Dynamic_States (1)'Unchecked_Access,
                                                                        dynamicStateCount => (if Pipeline.Has_Polygon_Offset then 1 else 0) +
                                                                                             (if Pipeline.Has_Polygon_Offset then 1 else 0) + 2, others => <>);

      -- Pipeline
      Pipeline_Info : aliased VkGraphicsPipelineCreateInfo := (stageCount          => 2,
                                                               pStages             => Shader_Stages_Info (1)'Unchecked_Access,
                                                               pVertexInputState   => Vertex_Input_Info'Unchecked_Access,
                                                               pInputAssemblyState => Input_Assembly_Info'Unchecked_Access,
                                                               pRasterizationState => Rasterizer_Info'Unchecked_Access,
                                                               pMultisampleState   => Multisample_Info'Unchecked_Access,
                                                               pDepthStencilState  => Depth_Stencil_Info'Unchecked_Access,
                                                               pColorBlendState    => Color_Blend_Info'Unchecked_Access,
                                                               renderPass          => Render_Pass,
                                                               layout              => Pipeline.Shader.Pipeline_Layout,
                                                               pDynamicState       => Dynamic_State_Info'Unchecked_Access,
                                                               pViewportState      => Viewport_Info'Unchecked_Access, others => <>);
      Pipeline_Layout_Info : aliased VkPipelineLayoutCreateInfo := (setLayoutCount => 1,
                                                                    pSetLayouts    => Descriptor_Set_Layout'Unchecked_Access, others => <>);   

      -- Create a new pipeline and add it to the cache
      begin      
        vkAssert (vkCreateGraphicsPipelines (device          => Device,
                                             pipelineCache   => Pipeline_Cache,
                                             createInfoCount => 1,
                                             pCreateInfos    => Pipeline_Info'Unchecked_Access,
                                             pAllocator      => null,
                                             pPipelines      => Current_Pipeline'Unchecked_Access));
        Buffered_Pipelines.Append ((Pipeline, Current_Pipeline));
      end;
<<Have_Pipeline>>
      Previous_Pipeline := (Pipeline, Current_Pipeline);

      -- Create writes linked with their appropriate additional structure (e.g. VkDescriptorBufferInfo or VkDescriptorImageInfo)
      vkAssert (vkAllocateDescriptorSets (Device, Set_Allocation_Info'Unchecked_Access, Descriptor_Set'Unchecked_Access));
      --if Writes.Length > 0 then 
      --  declare
      --  Result_Writes              : Vector_VkWriteDescriptorSet.Unsafe_Array   := To_Unsafe_Array (Writes);
      --  Result_Buffer_Descriptions : Vector_VkDescriptorBufferInfo.Unsafe_Array := To_Unsafe_Array (Buffer_Descriptions);
      --  Result_Image_Descriptions  : Vector_VkDescriptorImageInfo.Unsafe_Array  := To_Unsafe_Array (Image_Descriptions);
      --  begin null;

          -- Link the data together then update
      --    for Result_Write of Result_Writes loop
      --      if Result_Write.descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER then   
      --        null;
      --      end if;
      --    end loop;

          -- Clear the write data
      --    Writes.Clear;
      --    Write_Indexes.Clear;
      --    Buffer_Descriptions.Clear;
      --    Image_Descriptions.Clear;
      --  end;
      --end if;

      -- Bind descriptors and pipeline
      vkCmdBindDescriptorSets (commandBuffer      => Framebuffer (Current_Frame).Commands, 
                               pipelineBindPoint  => VK_PIPELINE_BIND_POINT_GRAPHICS, 
                               layout             => Pipeline.Shader.Pipeline_Layout,
                               firstSet           => 0,
                               descriptorSetCount => 1,
                               pDescriptorSets    => Descriptor_Set'Unchecked_Access, 
                               dynamicOffsetCount => 0,
                               pDynamicOffsets    => null);
      vkCmdBindPipeline (Framebuffer (Current_Frame).Commands, VK_PIPELINE_BIND_POINT_GRAPHICS, Current_Pipeline);

      -- Draw it
      vkCmdDrawIndexed (commandBuffer => Framebuffer (Current_Frame).Commands,
                        indexCount    => 0, -- Surface.,
                        instanceCount => 1,
                        firstIndex    => 0, --indexOffset >> 1,
                        vertexOffset  => 0, --vertOffset / ( drawSurf->jointCache ? sizeof( idShadowVertSkinned ) : size idShadowVert)),
                        firstInstance => 0);
    end;

  -------------
  -- Present --
  -------------
                                           
  -- Acquire the rendered image, show it in the window, and handle changes to the resolution
  procedure Present is  
    procedure Restart_Framebuffer is begin Finalize_Framebuffer; Initialize_Framebuffer; end;
    Image_Index : aliased Int_Unsigned_C := Int_Unsigned_C (Current_Frame) - 1; -- Zero index conversion for C
    Wait_Stage  : aliased Int_Unsigned_C := VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT; 
    Submit_Info : aliased VkSubmitInfo := (commandBufferCount   => 1,
                                           pWaitDstStageMask    => Wait_Stage'Unchecked_Access,
                                           waitSemaphoreCount   => 1,
                                           pWaitSemaphores      => Acquire_Status'Unchecked_Access,
                                           signalSemaphoreCount => 1,
                                           pSignalSemaphores    => Render_Status'Unchecked_Access, others => <>);
    Present_Info : aliased VkPresentInfoKHR := (swapchainCount     => 1,
                                                pSwapchains        => Swapchain'Unchecked_Access,
                                                waitSemaphoreCount => 1,
                                                pWaitSemaphores    => Render_Status'Unchecked_Access, others => <>);
    begin
      
      -- Recreate the framebuffer if the resolution of our surface has changed
      vkAssert (vkGetPhysicalDeviceSurfaceCapabilitiesKHR (Physical_Device, Surface, Surface_Details'Unchecked_Access));
      if Surface_Extent /= Surface_Details.minImageExtent then
        Surface_Extent := Surface_Details.minImageExtent;
        Restart_Framebuffer; 
        return;
      end if;
 
      -- Fetch the next image index from the swap chain
      vkAssert (vkQueueWaitIdle (Queue));
      if vkAcquireNextImageKHR (Device      => Device,
                                swapchain   => Swapchain,
                                timeout     => Int_64_Unsigned_C'Last,
                                semaphore   => Acquire_Status,
                                fence       => NULL_PTR,
                                pImageIndex => Image_Index'Unchecked_Access) in VK_ERROR_OUT_OF_DATE_KHR | VK_SUBOPTIMAL_KHR
      then
        Restart_Framebuffer;
      end if;

      -- Set the index
      Submit_Info.pCommandBuffers := Framebuffer (Current_Frame).Commands'Unchecked_Access;
      vkAssert (vkQueueSubmit (Queue, 1, Submit_Info'Unchecked_Access, NULL_PTR));
  
      -- Show the resulting image
      Present_Info.pImageIndices := Image_Index'Unchecked_Access;
      if vkQueuePresentKHR (Queue, Present_Info'Unchecked_Access) in VK_ERROR_OUT_OF_DATE_KHR | VK_SUBOPTIMAL_KHR then
        Restart_Framebuffer;
      end if;
    end;
end;

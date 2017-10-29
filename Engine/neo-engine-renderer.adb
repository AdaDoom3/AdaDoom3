
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

  -- Lists of bad drivers used to exclude or prefer certain physical devices - hopefully this stays empty
  type Driver_State is record Version, Vendor_Id, Device_Id : Int_Unsigned_C := 0; end record;
  type Array_Driver_State is array (Positive range <>) of Driver_State;  
  DRIVER_BLACKLIST : constant Array_Driver_State := (1 => (others => <>));
  DRIVER_GREYLIST  : constant Array_Driver_State := (1 => (others => <>));

  -- Device extensions
  DEBUGGING_EXTENSIONS : aliased Array_Str_Unbound := (VK_LAYER_LUNARG_API_DUMP_EXTENSION_NAME,
                                                       VK_LAYER_LUNARG_OBJECT_TRACKER_NAME,
                                                       VK_LAYER_LUNARG_CORE_VALIDATION_NAME,
                                                       VK_LAYER_LUNARG_SWAPCHAIN_NAME,
                                                       VK_LAYER_LUNARG_DEVICE_LIMITS_NAME);
  REQUIRED_EXTENSIONS : aliased Array_Str_Unbound := (VK_KHR_MAINTENANCE1_NAME,
                                                      VK_KHR_MAINTENANCE2_NAME,
                                                      VK_KHR_MAINTENANCE1_NAME,
                                                      VK_KHR_BIND_MEMORY2_NAME,
                                                      VK_KHR_IMAGE_FORMAT_LIST_NAME,
                                                      VK_KHR_SWAPCHAIN_EXTENSION_NAME,
                                                      VK_KHR_GET_MEMORY_REQUIREMENTS2_NAME,
                                                      VK_KHR_GET_SURFACE_CAPABILITIES2_NAME,
                                                      VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES2_NAME);

  -- Required depth and color image formats
  REQUIRED_DEPTH_FORMAT  : constant Int_Unsigned_C       := VK_FORMAT_D32_SFLOAT;
  REQUIRED_IMAGE_FORMATS : constant Array_Int_Unsigned_C := (VK_FORMAT_B8G8R8A8_UNORM,
                                                             VK_FORMAT_ASTC_4x4_UNORM_BLOCK,
                                                             VK_FORMAT_ASTC_5x4_UNORM_BLOCK,
                                                             VK_FORMAT_ASTC_5x5_UNORM_BLOCK,
                                                             VK_FORMAT_ASTC_6x5_UNORM_BLOCK,
                                                             VK_FORMAT_ASTC_6x6_UNORM_BLOCK,
                                                             VK_FORMAT_ASTC_8x5_UNORM_BLOCK,
                                                             VK_FORMAT_ASTC_8x6_UNORM_BLOCK,
                                                             VK_FORMAT_ASTC_8x8_UNORM_BLOCK,
                                                             VK_FORMAT_ASTC_10x5_UNORM_BLOCK,
                                                             VK_FORMAT_ASTC_10x6_UNORM_BLOCK,
                                                             VK_FORMAT_ASTC_10x8_UNORM_BLOCK,
                                                             VK_FORMAT_ASTC_10x10_UNORM_BLOCK,
                                                             VK_FORMAT_ASTC_12x10_UNORM_BLOCK,
                                                             VK_FORMAT_ASTC_12x12_UNORM_BLOCK);

  -------------
  -- Globals --
  -------------
  
  -- Opaque handles
  Queue,             
  Device,
  Surface,                           
  Instance,                          
  Swapchain,                        
  Render_Pass,                       
  Setup_Buffer,                      
  Command_Pool,                      
  Render_Status,                     
  Acquire_Status,                    
  Pipeline_Layout,                   
  Physical_Device,                   
  Graphics_Pipeline,                 
  Vertex_Shader_Model,               
  Fragment_Shader_Model,             
  Descriptor_Set_Layout : aliased Ptr := NULL_PTR;  

  -- Instance information
  Queue_Family : aliased Int_Unsigned_C := 0;
  
  -- Settings and properties
  Depth_Format      : aliased VkSurfaceFormatKHR               := (others => <>);
  Image_Format      : aliased VkSurfaceFormatKHR               := (others => <>);
  Surface_Details   : aliased VkSurfaceCapabilitiesKHR         := (others => <>);
  Device_Properties : aliased VkPhysicalDeviceProperties       := (others => <>);
  Format_Properties : aliased VkFormatProperties               := (others => <>);
  Memory_Properties : aliased VkPhysicalDeviceMemoryProperties := (others => <>);

  --------------
  -- Pipeline --
  --------------
  --
  -- ???
  --

  type Shader_Kind (Fragment_Shader, Vertex_Shader, Tesselation_Shader);
  type Shader_Array is array (Shader_Kind) of aliased Ptr;
  package Map_Shader is new Neo.Core.Vectors (Shader_State);

  type Pipeline_State is record  
      Red_Mask                 : Int_Unsigned_C := 0;     
      Green_Mask               : Int_Unsigned_C := 0;      
      Blue_Mask                : Int_Unsigned_C := 0;      
      Alpha_Mask               : Int_Unsigned_C := 0;     
      Polygon_Offset           : Int_Unsigned_C := 0;     
      Depths_Bounds_Test       : Int_Unsigned_C := 0;     
      Depth_Compare            : Int_Unsigned_C := 0;     
      Depth_Mask               : Int_Unsigned_C := 0;                                  
      Super_Sample             : Int_Unsigned_C := 0;     
      Cull_Mode                : Int_Unsigned_C := VK_CULL_MODE_FRONT_BIT;     
      Clockwise                : Int_Unsigned_C := 0;    
      Source_Blend_Factor      : Int_Unsigned_C := 0;    
      Destination_Blend_Factor : Int_Unsigned_C := 0;    
      Color_Blend              : Int_Unsigned_C := 0;    
      Alpha_Blend              : Int_Unsigned_C := 0;              
      Has_Mirror_View          : Bool           := False;                 
      Has_Separate_Stencil     : Bool           := False;   
      Test_Depth               : Bool           := False;          
      Test_Alpha               : Bool           := False;
      Shader                   : Shader_Array   := (others => NULL_PTR);  
      Tessellation_Depth       : Percent        := 20.0;             
      Vertex_Input             : Ptr_Array_VkPipelineVertexInputStateCreateInfo := NULL_PTR;
    end record;

  type Loaded_Pipeline_State is record
      Value : Pipeline_State := (others => <>);
      Data  : Ptr            := NULL_PTR
    end record;
  package Vector_Loaded_Pipeline is new Neo.Core.Vectors (Loaded_Pipeline_State);

  -- Global pipeline state and loaded Vulkan pointers
  Buffers          : Buffers_State  := (others => <>);
  Pipeline         : Pipeline_State := (others => <>);
  Shaders          : Map_Shader.Unsafe.Map;
  Last_Pipeline    : Vector_Loaded_Pipeline.Cursor;
  Loaded_Pipelines : Vector_Loaded_Pipeline.Unsafe.Vector;

  -- ???
  generic
    Path          : Str;
    Vertex_Input  : Array_VkPipelineVertexInputStateCreateInfo;
  package Shader is procedure Commit; end; package body Shader is
      procedure Commit is
        Entry_Name : aliased Str_8_C := To_Str_8_C ("main" & NULL_STR_8);
        Shader_Stages_Info : aliased Array_VkPipelineShaderStageCreateInfo ((stage  => VK_SHADER_STAGE_VERTEX_BIT,
                                                                             module => Vertex_Shader_Model,
                                                                             pName  => C (Entry_Name), others => <>),
                                                                            (stage  => VK_SHADER_STAGE_FRAGMENT_BIT,
                                                                             module => Fragment_Shader_Model,
                                                                             pName  => C (Entry_Name), others => <>));
        begin
          Pipeline.Shader       := Shaders.Element (U (Path));
          Pipeline.Vertex_Input := Vertex_Input'Unchecked_Access;
        end;
    begin
      Shaders.Insert (U (Path), (others => <>));
    end;

  ---------------------------------
  -- Intermediate Representation --
  ---------------------------------
  --
  -- ???
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
  package Vector_Surface is new Vectors (Surface_State);
  type Surface_Array is array (Surface_Group_Kind) of Surface_Vector.Unsafe.Vector;
  type Interaction_Array is array (Interaction_Kind) of Surface_Vector.Unsafe.Vector;

  type Light_State is record
      Material           : Str_Unbound := NULL_STR_UNBOUND;
      Inverse_Projection : Matrix_4D   := ZERO_MATRIX_4D;
      Projection         : Matrix_4D   := ZERO_MATRIX_4D;
      Scissor            : Matrix_4D   := ZERO_MATRIX_4D;
      Origin             : Matrix_4D   := ZERO_MATRIX_4D;
      Fog_Plane          : Matrix_4D   := ZERO_MATRIX_4D;
      Interactions       : Interaction_Array;
    end record;

  type View_State is record
      Is_Mirror      : Bool        := False;
      Origin         : Matrix_4D   := ZERO_MATRIX_4D;
      Port           : Matrix_4D   := ZERO_MATRIX_4D;
      Scissor        : Matrix_4D   := ZERO_MATRIX_4D;
      World_Space    : Space_State := (others => <>);
      Surface_Groups : 
      Lights         : Vector_Light.Unsafe.Vector;
    end record;

  -- Global data shared between the frontend and backend
  Views : Vector_View.Safe_Vector;

  ----------------
  -- Allocation --
  ----------------
  --
  -- ???
  --

  type Filter_Kind     is (Linear_Filter,   Nearest_Filter);
  type Clamp_Kind      is (No_Clamp,        Zero_Clamp,        Normal_Clamp,     Zero_Alpha_Clamp);
  type Usage_Kind      is (GPU_Usage,       CPU_Usage,         CPU_To_GPU_Usage, GPU_To_CPU_Usage);
  type Allocation_Kind is (Free_Allocation, Buffer_Allocation, Image_Allocation, Linear_Image_Allocation, Optimal_Image_Allocation);

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

  -- Dynamically allocatable list of memory chucks which share the same usage 
  type Memory_Block_State (Index : Int_Unsigned_C; Size : Int_64_Unsigned_C; Usage : Usage_Kind) is record
      Next_ID       : Int_Unsigned_C         := 0;      
      Offset        : Int_64_Unsigned_C      := 0;      
      Allocated     : Int_64_Unsigned_C      := 0;
      Device_Memory : Ptr                    := NULL_PTR;
      Data          : Ptr                    := NULL_PTR;
      First_Chunk   : Ptr_Memory_Chunk_State := null;
    end record;
  package Vector_Memory_Block is new Neo.Core.Vectors (Memory_Block_State);

  -- Reference to a chunk of dynamically allocated memory
  type Memory_State is record
      ID            : Int_Unsigned_C    := 0;
      Offset        : Int_64_Unsigned_C := 0;
      Size          : Int_64_Unsigned_C := 0;
      Device_Memory : Ptr               := NULL_PTR;
      Data          : Ptr               := NULL_PTR;
      Block         : Vector_Memory_Block.Cursor;
    end record;
  package Vector_Memory is new Neo.Core.Vectors (Memory_State);

  -- Globals for block allocation and garbage - managementDevice_Memory => Device_Local_Memory.Get * 1024**2,
  -- Data in the process of being staged along with relevant buffers
  type Staging_State is record
      Commands : Ptr            := NULL_PTR;
      Buffer   : Ptr            := NULL_PTR;
      Data     : Ptr            := NULL_PTR;
      Offset   : Int_Unsigned_C := 0;
    end record;

  -- Wrapped staging data with submission details
  type Staging_Buffer_State is record
      Staging   : Staging_State := (others => <>);
      Fence     : Ptr           := NULL_PTR;
      Submitted : Bool          := False;
    end record;

  -- Internal data structure to deal with buffer type differences 
  type Buffer_State (Usage : Usage_Kind; Size : ) is record 
      Offset     : Natural      := 0;
      Memory     : Memory_State := (others => <>);
      Data       : Ptr          := NULL_PTR;
      Owns_Data  : Bool         := False;
      Is_Static  : Bool         := False;
      Has_Buffer : Bool         := False;
      Data       : Array_Byte   := NULL_PTR;
    end record;

  -- ???
  type Image_State is record
      Format             : Int_Unsigned_C := VK_FORMAT_UNDEFINED;
      Layout             : Int_Unsigned_C := VK_IMAGE_LAYOUT_GENERAL;
      Inernal_Image      : Ptr            := NULL_PTR;
      View               : Ptr            := NULL_PTR;
      Sampler            : Ptr            := NULL_PTR;
      Generator_Function : Ptr            := NULL_PTR;
      Filter             : Int_Unsigned_C := TF_DEFAULT;
      Repeat             : Int_Unsigned_C := TR_REPEAT;
      Usage              : Int_Unsigned_C := TD_DEFAULT;
      Cube_Files         : Int_Unsigned_C := CF_2D;
      Referenced_Outside : Bool           := False;
      Level_Referenced   : Bool           := False;
      Source_File_Time   : Int_Unsigned_C := FILE_NOT_FOUND_TIMESTAMP;
      Binary_File_Time   : Int_Unsigned_C := FILE_NOT_FOUND_TIMESTAMP;
      Reference_Count    : Int_Unsigned_C := 0;
    end record;

  -- ???
  generic
    type Uniform_T is private;
    Binding, Location : Natural;
    Usage : Usage_Kind := GPU_Usage;
  package Uniform is
      procedure Set (Val : Uniform_T);
      function Get return Uniform_T;
    end;

  -- ???
  generic
    Kind : Sampler_Kind;
    Binding, Location : Natural;
    Usage : Usage_Kind := GPU_Usage;
  package Sampler is
      procedure Set (Val : Str);
      function Get return Str;
    end;

  -- ???
  generic Binding, Location : Natural; package Joint_Buffer  is procedure Set (Val : Treed_Joint.Unsafe.Tree);       end;
  generic Binding, Location : Natural; package Index_Buffer  is procedure Set (Val : Vector_Triangle.Unsafe.Vector); end;
  generic Binding, Location : Natural; package Vertex_Buffer is procedure Set (Val : Vector_Vertex.Unsafe.Vector);   end;\

  -- Set during initialization to map Allocation_Kind to the relevant indexes for the physical device
  Staging_Command_Pool : Ptr     := NULL_PTR;
  Mapped_Staging_Data  : Ptr     := NULL_PTR;
  Staging_Memory       : Ptr     := NULL_PTR;
  Staging_Buffer_Index : Int     := 0;
  Garbage_Index        : Natural := 0;
  Local_Memory         : Natural := 0;
  Host_Memory          : Natural := Host_Visible_Memory.Get * 1024**2;
  Usage_IDs            : array (Usage_Kind) of Int_Unsigned_C := (others => 0); 
  Blocks               : Vector_Block.Unsafe.Vector;

  -----------------
  -- Framebuffer --
  -----------------
  --
  -- ???
  --

  type Framebuffer_State is record
      Image            : Ptr := NULL_PTR;
      Image_View       : Ptr := NULL_PTR;
      Image_Garbage    : Ptr := NULL_PTR;
      Memory_Garbage   : Ptr := NULL_PTR;
      Commands         : Ptr := NULL_PTR;
      Descriptor_Pools : Ptr := NULL_PTR;
      Swapchain_Buffer : Ptr := NULL_PTR;
      Staging_Buffer   : Staging_Buffer_State := (others => <>);
      Memory_Garbage   : Vector_Memory.Unsafe.Vector;
      Image_Garbage    : Vector_Image.Unsafe.Vector;
    end record;
  package Vector_Framebuffer is new Neo.Core.Vectors (Framebuffer_State);
  Framebuffer   : Vector_Framebuffer.Unsafe.Vector;
  Current_Frame : Positive := 1;

  ------------
  -- Memory --
  ------------
  --
  -- ???
  --

  -- ???
  function Map_Buffer (Buffer : Buffer_State) return Ptr is (To_Ptr (To_Int_Ptr (Buffer.Memory.Data) + Buffer.Offset));

  -- ???
  procedure Adjust_Buffer (Buffer : in out Internal_Buffer_State) is
    begin
      Assert (Buffer.Kind = Kind);
      if Buffer.Is_Static then
        Stage (Size, 1, Command_Buffer, Stage_Buffer, Stage_Offset);
        memcpy (stageData, data, size);
        Buffer_Copy := (srcOffset => stageOffset,
                        dstOffset => GetOffset() + offset,
                        size      => size,
                        others    => <>);
        vkCmdCopyBuffer (Command_Buffer, stageBuffer, Buffer.API_Object, 1, &bufferCopy);
      else
        Buffer.Memory.Datadata + GetOffset + offset := (const byte *)data [size];
      end if;
    end;

  -- Grab some memory from a block
  function Allocate (Size, Align : Int_Unsigned_C;
                     Usage       : Usage_Kind;
                     Allocation  : Allocation_Kind)
                     return Memory_State is

    -- Check if two offsets and sizes are suitable for the same page - see the Vulkan spec "Buffer-Image Granularity"
    function Can_Be_On_Same_Page (Offset_A, Size_A, Offset_B : Int_64_Unsigned_C) return Bool is
      return (((Offset_A + Size_A - 1) and (not (Device_Properties.limits.bufferImageGranularity - 1))) =
               (Offset_B               and (not (Device_Properties.limits.bufferImageGranularity - 1))));


    -- Check that the memory kinds do not have a granularity conflict
    function Has_Granularity_Conflict (Allocation_A, Allocation_B : Allocation_Kind) return Bool is
      Test_A : Allocation_Kind := (if Allocation_A > Allocation_B then Allocation_B else Allocation_A);
      Test_B : Allocation_Kind := (if Test_A       = Allocation_A then Allocation_B else Allocation_A);
      begin
        return (case Test_A is
                  when Free_Allocation | Optimal_Image_Allocation => False
                  when Buffer_Allocation       => Test_B = Image_Allocation or Test_B = Optimal_Image_Allocation
                  when Image_Allocation        => Test_B in Image_Allocation..Optimal_Image_Allocation
                  when Linear_Image_Allocation => Test_B = Optimal_Image_Allocation);
      end;

    -- Local Variables
    Aligned_Size : Int_64_Unsigned_C  := 0;
    Offset       : Int_64_Unsigned_C  := 0;
    Current      : Memory_Chunk_State := (others => <>);
    begin 

      -- Try to allocate from a suitable block
      for Block of Blocks loop
        if Block.Index = Usage_Indexes (Usage) and then Block.Size - Block.Allocated >= Size then

          -- Find the best fit chunk
          Current := Block.First_Chunk;
          Inner: while Current /= null loop

            -- Only example chunks that are free 
            if Current.Allocation = Free_Allocation then

              -- Set the offset in case there the granularity conflicts with the previous chunk
              Offset := ALIGN (Current.Offset, Align);
              if Current.Previous /= null and then Device_Properties.limits.bufferImageGranularity > 1
                and then Can_Be_On_Same_Page (Current.Previous.Offset, Current.Previous.Size, Offset)
                and then Has_Granularity_Conflict (Current.Previous.Allocation, Allocation)
              then
                Offset := ALIGN (Offset, Granularity);
              end if;

              -- Check next allocation to see if its suitable to use
              Aligned_Size := Offset - Current.Offset + Size;
              if Aligned_Size <= Current.Size then 

                -- Check the block still has enough space
                exit when Aligned_Size + Block.Allocated >= Block.Size;

                -- Check for granularity conflict with the next chunk before attempting to allocate
                if Current.Next = null or else Device_Properties.limits.bufferImageGranularity <= 1
                  or else not Can_Be_On_Same_Page (Offset, Size, Current.Next.Offset)
                  or else not Has_Granularity_Conflict (Allocation, Current.Next.Allocation)
                then

                  -- Split the best fitting chunk when the size is not an exact fit
                  if Current.Size > Size then
                    Current.Next := new Memory_Chunk_State'(ID         => Block.Next_ID,
                                                            Offset     => Offset + Size,
                                                            Size       => Current.Size - Aligned_Size,
                                                            Allocation => Free_Allocation,
                                                            Next       => Current.Next,
                                                            Previous   => Current);
                    Current.Size  := Size;
                    Block.Next_ID := Block.Next_ID + 1;
                  end if;

                  -- Assign the chunk and mark it as allocated and build the result
                  Block.Allocated    := Block.Allocated + Aligned_Size;
                  Current.Allocation := Allocation;
                  return (ID            => Current.ID,
                          Offset        => Offset,
                          Size          => Current.Size,
                          Device_Memory => Block.Device_Memory,
                          Block         => Block'Access,
                          Data          => (if Usage /= GPU_Usage then To_Ptr (To_Int_Ptr (Block.Data) + Offset) else NULL_PTR));
                end if;
              end if;
            end if;

            -- Advance to the chunk within the block
            Current := Current.Next;
          end loop;
        end if;
      end loop;

      -- Allocate a new block since no suitable existing ones could be found
      declare
      Allocate_Info : aliased VkMemoryAllocateInfo := (allocationSize  => Size,
                                                       memoryTypeIndex => Usage_Indexes (Usage),
                                                       others          => <>);
      Block : Block_State := (Memory_Index => Memory_Index (Memory),
                              Size         => (if Usage = GPU_Usage then Device_Local_Memory.Get * 1024 else Host_Visible_Memory.Get * 1024),
                              Usage        => Usage,
                              First_Chunk  => new Memory_Chunk_State'(Size => Block.Size, others => <>),
                              others       => <>);
      begin
        vkAssert (vkAllocateMemory (Device, Allocate_Info'Unchecked_Access, null, Block.Device_Memory'Unchecked_Access));
        if Block.Usage /= GPU_Usage then
          vkAssert (vkMapMemory (Device, Block.Device_Memory, 0, Size, 0, Block.Data'Unchecked_Access));
        end if;
        Blocks.Append (Block);

        -- Create a chunk from the new and return the desired memory from it
        return (ID            => Block.ID,
                Size          => Block.Size,
                Device_Memory => Block.Device_Memory,
                Offset        => Offset,
                Block         => Block.Last,
                Data          => (if Usage = GPU_Usage then Device_Memory else Host_Memory));
      end;
    end;

  -- Free the global GPU memory garbage
  procedure Free_Memory_Garbage is
    Chunk : Ptr_Memory_Chunk_State := null;
    Current, Previous, Next : Memory_Chunk_State := Block.First_Chunk;
    begin
      Garbage_Index := (Garbage_Index + 1) mod NUM_FRAME_DATA;
      for Piece of Buffer_Garbage.Element (Garbage_Index) loop
        Piece.Block.Free (Piece);    

        -- Find the chunk corresponding to the Memory
        while Current /= null loop
          if Current.ID = Memory.ID then

            -- Join the chunk with the previous one if it is free
            if Current.Previous /= null and then Current.Previous.Kind = Free_Allocation then
              Previous := Current.Previous;
              Previous.Next := Current.Next;
              if Current.Next /= null then Current.Next.Previous := Previous; end if;
              Previous.Size := Previous.Size + Current.Size;
              Free (Current);
              Current := Previous;
            end if;

            -- Same with the next chunk
            if Current.Next /= null and then Current.Next.Kind = Free_Allocation then
              Next := Current.Next;
              if Next.Next /= null then Next.Next.Previous := Current; end if;
              Current.Next := Next.Next;
              Current.Size := Current.Size + Next.Size;
              Free (Next);
            end if;

            Block.Allocated := Block.Allocated - Memory.Size;
            return;
          end if;
          Current := Current.Next;
        end loop;

        -- Kill the block if there is nothing left
        if Piece.Block.Allocated = 0 then
          if Block.Usage /= GPU_Usage then vkAssert (vkUnmapMemory (Device, Block.Device_Memory); end if;
          vkFreeMemory (Device, Block.Device_Memory, null);
          Block.Device_Memory := null;
          while Block.First_Chunk /= null loop
            Chunk := Block.First_Chunk;
            Block.First_Chunk := Chunk.Next;
            Free (Chunk);
          end loop;
          Blocks (Piece.Block.Memory_Index).Delete (Piece.Block); -- !!!
        end if;
      end loop;
    end;

  -------------
  -- Staging --
  -------------
  --
  -- ???
  --

  -- Carve out a chunk of the staging buffer
  function Stage (Size, Alignment : Positive) return Stage_State is
    Result : Stage_State  := Framebuffer (Frame).Staging_Buffer.Staging;
    begin
      Assert (Size <= Max_Upload_Buffer.Get);
      Result.Offset := (if Result.Offset mod Alignment = 0 then Result.Offset
                        else Result.Offset + Alignment - Result.Offset mod Alignment);

      -- Flush the garbage if we have reached the max
      if Result.Offset + Size >= Max_Upload_Buffer.Get and not Framebuffer (Frame).Staging_Buffer.Submitted then
        Flush;
      end if;

      -- Move over the pointer or something
      Result.Data   := To_Ptr (To_Int_Address (Result.Data) + Offset);
      Result.Offset := Offset + Size;
      return Result;
    end;
  
  -- Clear the current frame's staging buffer
  procedure Free_Staging_Garbage is
    Command_Buffer_Info : VkCommandBufferBeginInfo := (others => <>);
    Memory_Range : VkMappedMemoryRange := (memory => Memory;
                                           size   => VK_WHOLE_SIZE,
                                           others => <>);
    Submit_Info : VkSubmitInfo := (commandBufferCount => 1,
                                   pCommandBuffers    => Framebuffer (Frame).Staging_Buffer.Commands'Unchecked_Access,
                                   others             => <>);
    begin

      -- ???
      vkFlushMappedMemoryRanges (Device, 1, Memory_Range'Unchecked_Access);
      vkQueueSubmit (Queue, 1, Submit_Info'Unchecked_Access, Framebuffer (Frame).Staging_Buffer.Fence);

      -- Reset the command buffer only after the staging buffer is avaliable
      vkAssert (vkWaitForFences (Device, 1, Framebuffer (Frame).Staging_Buffer.Fence'Unchecked_Access, VK_TRUE, UINT64_MAX));
      vkAssert (vkResetFences   (Device, 1, Framebuffer (Frame).Staging_Buffer.Fence'Unchecked_Access));
      vkAssert (vkBeginCommandBuffer (Framebuffer (Frame).Staging_Buffer.Commands, Command_Buffer_Info'Unchecked_Access));
      Framebuffer (Frame).Staging_Buffer.Offset    := 0;
      Framebuffer (Frame).Staging_Buffer.Submitted := False;
    end;

  -----------
  -- Image --
  -----------

  procedure Add_Image (Path : Str) is

    -- Image 
    Image_Info : aliased VkImageCreateInfo :=
      (initialLayout => VK_IMAGE_LAYOUT_UNDEFINED,
       sharingMode   => VK_SHARING_MODE_EXCLUSIVE,
       tiling        => VK_IMAGE_TILING_OPTIMAL,
       imageType     => VK_IMAGE_TYPE_2D,
       format        => m_internalFormat,
       mipLevels     => m_opts.numLevels,
       samples       => m_opts.samples,
       extent        => (m_opts.width, m_opts.height, 1),
       arrayLayers   => (if m_opts.textureType = TT_CUBIC then 6 else 1),
       flags         => (if m_opts.textureType = TT_CUBIC then VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT else 0),
       usage         => (if m_opts.format = FMT_DEPTH then VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
                         else VK_IMAGE_USAGE_TRANSFER_DST_BIT) or usageFlags,
       others        => <>);
    Sampler_Info : aliased VkSamplerCreateInfo :=
      (maxAnisotropy    => 1.0,
       anisotropyEnable => VK_FALSE;
       compareEnable    => (m_opts.format = FMT_DEPTH);
       compareOp        => (if m_opts.format = FMT_DEPTH then VK_COMPARE_OP_LESS_OR_EQUAL else VK_COMPARE_OP_NEVER),
       others           => <>);
    begin

      -- Set Vulkan flags
      case Image.Filter is
        when Default_Filter | Linear_Filter =>
          Sampler_Info.minFilter  := VK_FILTER_LINEAR;
          Sampler_Info.magFilter  := VK_FILTER_LINEAR;
          Sampler_Info.mipmapMode := VK_SAMPLER_MIPMAP_MODE_LINEAR;
        when Nearest_Filter =>
          Sampler_Info.minFilter  := VK_FILTER_NEAREST;
          Sampler_Info.magFilter  := VK_FILTER_NEAREST;
          Sampler_Info.mipmapMode := VK_SAMPLER_MIPMAP_MODE_NEAREST;
      end case;
      case Image.Clamp is
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

      -- Load the image
      vkAssert (vkCreateSampler   (Device, Sample_Info'Unchecked_Access,     NULL_PTR, Image.Sampler'Unchecked_Access)); 
      vkAssert (vkCreateImage     (Device, Image_Info'Unchecked_Access,      NULL_PTR, Image.Image'Unchecked_Access));
      vkAssert (vkCreateImageView (Device, Image_View_Info'Unchecked_Access, NULL_PTR, Image.View'Unchecked_Access));

      -- Bind the image to a chunk of memory 
      vkGetImageMemoryRequirements (Device, Image.Image, Memory_Requirements'Unchecked_Access);
      Image.Memory := Allocate (Size   => Memory_Requirments.size,
                                Align  => Memory_Requirments.alignment,
                                Kind   => Memory_Requirments.memoryTypeBits, 
                                Usage  => GPU_Usage, 
                                Memory => VULKAN_ALLOCATION_TYPE_IMAGE_OPTIMAL );
      vkAssert (vkBindImageMemory (Device, m_image, m_allocation.deviceMemory, m_allocation.offset ) );

      -- Register the loaded image
      Images.Insert (Path, Image);
    end;

  procedure Remove_Image (Path : Str) is
    begin
       Image_Garbage (Image.Garbage_I).Append ((Image.Sampler, Image.Memory, Image.View, Image.Image));
       Images.Remove (Path);
    end;

  -- ???
  procedure Free_Sampler_Garbage is
    begin
      for Image of Framebuffer.Element (Current_Frame).Image_Garbage loop
        vkDestroyImage     (Device, Image.Data,    NULL_PTR);
        vkDestroyImageView (Device, Image.View,    NULL_PTR);
        vkDestroySampler   (Device, Image.Sampler, NULL_PTR);
      end loop;
      Image_Garbage (Garbage_Index).Clear;
    end;

  --------------
  -- Uniforms --
  --------------
  --
  -- ???
  --

  package body Sampler is
      procedure Set (Path : Str) is
        Image : Image_State := Images.Element (U (Path));
        Image_Info : aliased VkDescriptorImageInfo := (imageLayout => Image.Layout,
                                                       imageView   => Image.View, 
                                                       sampler     => image.Sampler,
                                                       others      => <>);
        Write : aliased VkWriteDescriptorSet := (dstSet          => Set,
                                                 dstBinding      => Binding,
                                                 descriptorCount => 1,
                                                 descriptorType  => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                 pBufferInfo     => Image_Info'Unchecked_Access,
                                                 others          => <>);
        begin
          vkUpdateDescriptorSets (Device, 1, Write'Unchecked_Access, 0, NULL_PTR);
        end;
    end;

  package body Uniform is   
      Uniform_Index : Positive := 1;     

      procedure Set (Val : Uniform_T) is
        Buffer : Buffer_State := Uniforms.Get (Uniform_Index);
        Buffer_Info : aliased VkDescriptorBufferInfo := (buffer => Buffer.API_Object,
                                                         offset => Buffer.Offset,
                                                         rang   => Buffer.Size);
        Write : aliased VkWriteDescriptorSet := (dstSet          => Set,
                                                 dstBinding      => Binding,
                                                 descriptorCount => 1,
                                                 descriptorType  => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                                 pBufferInfo     => Buffer_Info'Unchecked_Access,
                                                 others          => <>);
        begin
          vkUpdateDescriptorSets (Device, 1, Write'Unchecked_Access, 0, NULL_PTR);
        end;
    
    -- Register a new buffer and save the index for writes/sets
    begin
      Uniforms.Append ((Usage, Uniform_T'Object_Size));
      Uniform_Index := Uniforms.Length;
    end;

  -------------
  -- Buffers --
  -------------
  --
  -- ???
  --

  package body Joint_Buffer is

      -- ???
      procedure Set (Val : Treed_Joint.Unsafe.Tree) is
        idUniformBuffer jointBuffer;
        begin
          if prog.usesJoints and vkcontext.jointCacheHandle > 0 then
            vertexCache.GetJointBuffer( vkcontext.jointCacheHandle, &jointBuffer then
            assert ((jointBuffer.GetOffset() & ( vkcontext.gpu->props.limits.minUniformBufferOffsetAlignment - 1 ) ) == 0 );
            ubos[ uboIndex++ ] = &jointBuffer;
          end if;
        end;

    -- ???
    begin
    end;

  package body Vertex_Buffer is

      -- ???
      procedure Set (Val : Vector_Vertex.Unsafe.Vector) is
        begin vkCmdBindVertexBuffers (Commands, 0, 1, &buffer, &offset); end;

    -- ???
    begin

    end;

  package body Index_Buffer is

      -- Bind verticies and indicies
      procedure Set (Val : Vector_Triangle.Unsafe.Vector) is
        begin vkCmdBindIndexBuffer (Commands, buffer, offset, VK_INDEX_TYPE_UINT16); end;

    -- ???
    begin
    end;

  -------------
  -- Shaders --
  -------------
  -- 
  -- ???
  --

  -- 3D Data buffers for skeletal mesh skinning
  package Joints    is new Joint_Buffer  (Set => 0, Binding => 0); 
  package Indices   is new Index_Buffer  (Set => 0, Binding => 1); 
  package Verticies is new Vertex_Buffer (Set => 0, Binding => 2); 

  -- Texture map samplers
  package Color_Sampler      is new Sampler (Set => 1, Binding => 0), 
  package Irradiance_Sampler is new Sampler (Set => 1, Binding => 1),
  package Filter_Sampler     is new Sampler (Set => 1, Binding => 2),
  package Specular_Sampler   is new Sampler (Set => 1, Binding => 3),
  package Normal_Sampler     is new Sampler (Set => 1, Binding => 4),
  package Displace_Sampler   is new Sampler (Set => 1, Binding => 5),
  package Metallic_Sampler   is new Sampler (Set => 1, Binding => 6),
  package Roughness_Sampler  is new Sampler (Set => 1, Binding => 7);
  package Projection_Sampler is new Sampler (Set => 1, Binding => 8);
  package Falloff_Sampler    is new Sampler (Set => 1, Binding => 9);

  -- Feature flags
  package Enable_Skinning    is new Uniform (Set => 2, Binding => 0, Uniform_T => Bool);
  package Enable_Tesselation is new Uniform (Set => 2, Binding => 1, Uniform_T => Bool);
  package Enable_Bloom       is new Uniform (Set => 2, Binding => 2, Uniform_T => Bool); 
  package Enable_HDR         is new Uniform (Set => 2, Binding => 3, Uniform_T => Bool); 
  package Enable_Blur        is new Uniform (Set => 2, Binding => 4, Uniform_T => Bool); 
  package Enable_Haze        is new Uniform (Set => 2, Binding => 5, Uniform_T => Bool); 

  -- Scene information
  package Tesselation_Max    is new Uniform (Set => 2, Binding =>  6, Uniform_T => Real_64_C);
  package Tesselation_Amount is new Uniform (Set => 2, Binding =>  7, Uniform_T => Real_64_C);
  package Delta_MS           is new Uniform (Set => 2, Binding =>  8, Uniform_T => Int_Unsigned_64_C);
  package Model              is new Uniform (Set => 2, Binding =>  9, Uniform_T => Matrix_4D);
  package Projection         is new Uniform (Set => 2, Binding => 10, Uniform_T => Matrix_4D);
  package MVP                is new Uniform (Set => 2, Binding => 11, Uniform_T => Matrix_4D);
  package View               is new Uniform (Set => 2, Binding => 12, Uniform_T => Matrix_4D);
  package Transform          is new Uniform (Set => 2, Binding => 13, Uniform_T => Matrix_4D); -- S, T, Q, Enabled
  package Light_Projection   is new Uniform (Set => 2, Binding => 14, Uniform_T => Matrix_4D); -- S, T, Q
  package Local_Light_Origin is new Uniform (Set => 2, Binding => 15, Uniform_T => Vector_4D);
  package Local_View_Origin  is new Uniform (Set => 2, Binding => 16, Uniform_T => Vector_4D);
  package Test_Alpha         is new Uniform (Set => 2, Binding => 17, Uniform_T => Vector_4D);
  package Light_Falloff      is new Uniform (Set => 2, Binding => 18, Uniform_T => Vector_4D);
  package Light_Scale        is new Uniform (Set => 2, Binding => 19, Uniform_T => Vector_4D);
  package Screen_Factor      is new Uniform (Set => 2, Binding => 20, Uniform_T => Vector_4D);
  package UI_Coord           is new Uniform (Set => 2, Binding => 21, Uniform_T => Vector_4D);
  package Diffuse_Modifier   is new Uniform (Set => 2, Binding => 22, Uniform_T => Vector_4D);
  package Specular_Modifier  is new Uniform (Set => 2, Binding => 23, Uniform_T => Vector_4D);
  package Color_Value        is new Uniform (Set => 2, Binding => 24, Uniform_T => Vector_4D);
  package Local_To_Global    is new Uniform (Set => 2, Binding => 25, Uniform_T => Vector_4D);
  package Local_To_Eye       is new Uniform (Set => 2, Binding => 26, Uniform_T => Vector_4D);
  package Vertex_Color_Mod   is new Uniform (Set => 2, Binding => 27, Uniform_T => Vector_4D);
  package Vertex_Color_Add   is new Uniform (Set => 2, Binding => 28, Uniform_T => Vector_4D);
  package Overbright         is new Uniform (Set => 2, Binding => 29, Uniform_T => Vector_4D);

  -- 
  Normal_Attributes : aliased Array_VkVertexInputAttributeDescription := ((binding  => 0, -- Position
                                                                           location => 0,
                                                                           format   => VK_FORMAT_R32G32B32A32_SFLOAT,
                                                                           offset   => Vertex.Position'Position / Byte'Size),
                                                                          (binding  => 0, -- Texture Coordinate
                                                                           location => 1,
                                                                           format   => VK_FORMAT_R16G16_SFLOAT,
                                                                           offset   => Obj_Vertex.Color'Position / Byte'Size),
                                                                          (binding  => 0, -- Normal
                                                                           location => 2,
                                                                           format   => VK_FORMAT_R8G8B8A8_UNORM,
                                                                           offset   => Vertex.Texture'Position / Byte'Size),
                                                                          (binding  => 0, -- Tangent
                                                                           location => 3,
                                                                           format   => VK_FORMAT_R8G8B8A8_UNORM,
                                                                           offset   => Vertex.Texture'Position / Byte'Size),
                                                                          (binding  => 0, -- Color_1
                                                                           location => 4,
                                                                           format   => VK_FORMAT_R8G8B8A8_UNORM,
                                                                           offset   => Vertex.Texture'Position / Byte'Size),
                                                                          (binding  => 0, -- Color_2
                                                                           location => 5,
                                                                           format   => VK_FORMAT_R8G8B8A8_UNORM,
                                                                           offset   => Vertex.Texture'Position / Byte'Size));

  -- 
  Shadow_Attributes : aliased Array_VkVertexInputAttributeDescription := ((binding  => 0, -- Position
                                                                           location => 0,
                                                                           format   => VK_FORMAT_R32G32B32A32_SFLOAT,
                                                                           offset   => Vertex.Position'Position / Byte'Size),
                                                                          (binding  => 0, -- Color_1
                                                                           location => 4,
                                                                           format   => VK_FORMAT_R8G8B8A8_UNORM,
                                                                           offset   => Vertex.Texture'Position / Byte'Size),
                                                                          (binding  => 0, -- Color_2
                                                                           location => 5,
                                                                           format   => VK_FORMAT_R8G8B8A8_UNORM,
                                                                           offset   => Vertex.Texture'Position / Byte'Size));

  -- Define a shader for each pass within the backend
  package Depth_Pass  is new Shader ("depth",  Normal_Attributes); 
  package UI_Pass     is new Shader ("ui",     Normal_Attributes); 
  package Shadow_Pass is new Shader ("shadow", Shadow_Attributes); 
  package Fog_Pass    is new Shader ("fog",    Normal_Attributes); 
  package Sky_Pass    is new Shader ("sky",    Normal_Attributes);
  package Post_Pass   is new Shader ("post",   Normal_Attributes);
  package Light_Pass  is new Shader ("light",  Normal_Attributes);

  --------------
  -- Subunits --
  --------------

  procedure Frontend is separate;
  procedure Backend  is separate;

  -----------------
  -- Adjust_Mode --
  -----------------
  
  -- Get the game window ready for rendering and initialize the global variables in the spec
  procedure Adjust_Mode (Create_New_Surface : Bool := True; Do_Multi_Monitor : Bool := False) is 
    begin
    
      -- Create instance and rendering surface
      if Create_New_Surface then Surface := Create_Vulkan_Surface (Instance); end if;  
      
      -- Create swap chain and ready the surface
      Adjust_Resolution;
    end;

  ------------------------
  -- Finalize_Swapchain --
  ------------------------

  procedure Finalize_Swapchain is
    begin

      -- Stop the backend becuase it uses the framebuffer
      Backend_Task.Finalize;
    
      -- Kill the framebuffer images
      for Frame of Framebuffer loop
        vkDestroyImageView   (Device, Frame.Image_View,       null);
        vkDestroyFramebuffer (Device, Frame.Swapchain_Buffer, null); 
      end loop;
      Free (Depth_Image);
      
      -- Destroy the pipeline
      vkDestroyPipelineLayout (device, pipelineLayout,   null);
      vkDestroyRenderPass     (device, renderPass,       null);
      vkDestroySwapchainKHR   (Device, swapChain,        null);
      vkFreeCommandBuffers    (Device, Command_Pool, Commands'Length, Commands (1)'Unchecked_Access);
      vkDestroyPipeline       (device, graphicsPipeline, null);
    end; 

  --------------
  -- Finalize --
  --------------

  -- Kill anything that is not controlled and that is not the main Device or Instance
  procedure Finalize is
    begin
      Finalize_Swapchain;

      -- 
      vkDestroySampler             (Device, Texture_Sampler,       null);
      vkDestroyDescriptorPool      (Device, Descriptor_Pool,       null);
      vkDestroyDescriptorSetLayout (Device, Descriptor_Set_Layout, null);
      vkDestroySemaphore           (Device, Acquire_Status,        null);
      vkDestroySemaphore           (Device, Render_Status,         null);  
      vkDestroyCommandPool         (Device, Command_Pool,          null);

      -- Take out the garbage
      Free_Images;
      Free_Memory;
      Flush_Staging;

      -- Staging 
      vkUnmapMemory (Device, Memory);
      for Stage of Stages loop
        vkDestroyFence       (Device, Stage.Fence,  NULL_PTR);
        vkDestroyBuffer      (Device, Stage.Buffer, NULL_PTR);
        vkFreeCommandBuffers (Device, Command_Pool, 1, Stage.Commands'Unchecked_Access);
      end loop;
      Manager := (others => <>);

      -- Buffers
      for Buffer of Internal_Buffers loop
        if Buffer.Owns_Data then
          Free (Buffer.Memory);
          vkDestroyBuffer (Device, m_apiObject, NULL);
          Buffer.API_Object := NULL_PTR;
          Buffer.Memory := (others => <>);
        end if;
        Buffer := (others => <>);
      end loop;
    end;

  -------------
  -- Present --
  -------------
                                           
  -- Acquire the rendered image, show it in the window, and handle changes to the resolution
  procedure Present is  
    Image_Index : aliased Int_Unsigned_C := Current_Frame - 1;
    Wait_Stages : aliased Array_Int_Unsigned_C := (1 => VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT); 
    Submit_Info : aliased VkSubmitInfo := (Command_BufferCount   => 1,
                                           pWaitDstStageMask    => Wait_Stages (1)'Unchecked_Access,
                                           waitSemaphoreCount   => 1,
                                           pWaitSemaphores      => Acquire_Status'Unchecked_Access,
                                           signalSemaphoreCount => 1,
                                           pSignalSemaphores    => Render_Status'Unchecked_Access,
                                           others               => <>);
    Present_Info : aliased VkPresentInfoKHR := (swapchainCount     => 1,
                                                pSwapchains        => Swapchain'Unchecked_Access,
                                                waitSemaphoreCount => 1,
                                                pWaitSemaphores    => Render_Status'Unchecked_Access,
                                                others             => <>);
    begin
 
      -- Fetch the next image index from the swap chain
      case vkAcquireNextImageKHR (Device      => Device,
                                  swapchain   => Swapchain,
                                  timeout     => Int_64_Unsigned_C'Last,
                                  semaphore   => Acquire_Status,
                                  fence       => NULL_PTR,
                                  pImageIndex => Image_Index'Unchecked_Access) is
        when VK_SUCCESS | VK_SUBOPTIMAL_KHR => null;
        when VK_ERROR_OUT_OF_DATE_KHR => Update;
        when others => null; end case;

      -- Set the index
      Submit_Info.pCommandBuffers := Commands (Int (Image_Index) + 1)'Unchecked_Access;
      vkAssert (vkQueueSubmit (Queue, 1, Submit_Info'Unchecked_Access, NULL_PTR));
  
      -- Show the resulting image
      Present_Info.pImageIndices := Image_Index'Unchecked_Access;
      case vkQueuePresentKHR (Queue, Present_Info'Unchecked_Access) is
        when VK_SUCCESS => null;
        when VK_ERROR_OUT_OF_DATE_KHR | VK_SUBOPTIMAL_KHR => Update;
        when others => null; end case;
      vkAssert (vkQueueWaitIdle (Queue));
    end;


  --------------------------
  -- Initialize_Swapchain --
  --------------------------

  procedure Initialize_Swapchain (Width, Height : Int_64_Positive) is

    -- Render Command
    Command_Begin_Info : aliased VkCommandBufferBeginInfo := (flags => VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT, others => <>);
    Allocate_Info : aliased VkCommandBufferAllocateInfo := (commandPool         => Command_Pool,
                                                            level               => VK_COMMAND_BUFFER_LEVEL_PRIMARY,
                                                            Command_BufferCount => Commands'Length, others => <>);
                   
    -- Swap Chain
    Indexes : aliased Array_Int_Unsigned_C := (Graphics_Family, Present_Family);
    Swapchain_Info : aliased VkSwapchainCreateInfoKHR := (surface          => Surface,
                                                          imageArrayLayers => 1,
                                                          imageFormat      => Image_Format.format,
                                                          imageColorSpace  => Image_Format.colorSpace,
                                                          imageSharingMode => VK_SHARING_MODE_EXCLUSIVE,
                                                          imageUsage       => VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
                                                          compositeAlpha   => VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
                                                          presentMode      => VK_PRESENT_MODE_MAILBOX_KHR,
                                                          clipped          => VK_TRUE,
                                                          oldSwapchain     => Swapchain,
                                                          preTransform     => Surface_Details.currentTransform,
                                                          imageExtent      => (width  => Int_Unsigned_C (Current_Width),
                                                                               height => Int_Unsigned_C (Current_Height)),
                            
      -- Pick the queue length or number of images in the swap-chain - push for one more than the minimum for triple buffering
      minImageCount => (if Surface_Details.maxImageCount > 0 and Surface_Details.minImageCount + 1 > Surface_Details.maxImageCount
                        then Surface_Details.maxImageCount
                        else Surface_Details.minImageCount + 1), others => <>);
                    
    -- Viewport 
    Viewport : aliased VkViewport := (x        => 0.0,
                                      y        => 0.0,
                                      width    => Real_C (Swapchain_Info.imageExtent.width),
                                      height   => Real_C (Swapchain_Info.imageExtent.height),
                                      minDepth => 0.0,
                                      maxDepth => 10.0, others => <>);
    Scissor : aliased VkRect2D := (offset => (0, 0),
                                   extent => Swapchain_Info.imageExtent, others => <>);
    Viewport_State_Info : aliased VkPipelineViewportStateCreateInfo := (viewportCount => 1,
                                                                        pViewports    => Viewport'Unchecked_Access,
                                                                        scissorCount  => 1,
                                                                        pScissors     => Scissor'Unchecked_Access, others => <>);

    -- Subpass
    Color_Reference : aliased VkAttachmentReference := (attachment => 0,
                                                        layout     => VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, others => <>);
    Depth_Reference : aliased VkAttachmentReference := (attachment => 1,
                                                        layout     => VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, others => <>);
    Subpasses_Description : aliased Array_VkSubpassDescription := (1 => (pipelineBindPoint       => VK_PIPELINE_BIND_POINT_GRAPHICS,
                                                                         colorAttachmentCount    => 1,
                                                                         pColorAttachments       => Color_Reference'Unchecked_Access,
                                                                         pDepthStencilAttachment => Depth_Reference'Unchecked_Access,
                                                                         others                  => <>));
    Subpass_Dependencies : aliased Array_VkSubpassDependency := (1 => (srcSubpass    => VK_SUBPASS_EXTERNAL,
                                                                       dstSubpass    => 0,
                                                                       srcStageMask  => VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                                                                       srcAccessMask => 0,
                                                                       dstStageMask  => VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                                                                       dstAccessMask => VK_ACCESS_COLOR_ATTACHMENT_READ_BIT or
                                                                                        VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
                                                                       others        => <>));
 
    -- Render Pass
    Clear_Values : aliased Array_VkClearValue := ((color        => ((0.0, 1.0, 0.0, 1.0)), others => <>),
                                                  (depthStencil => (1.0, 0.0),             others => <>));
    Attachments : aliased Array_VkAttachmentDescription := ((format         => Image_Format.format,
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
                                                          pDependencies   => Subpass_Dependencies (1)'Unchecked_Access,
                                                          others          => <>);
    Render_Pass_Begin_Info : aliased VkRenderPassBeginInfo := (clearValueCount => Clear_Values'Length,
                                                               pClearValues    => Clear_Values (1)'Unchecked_Access,
                                                               renderArea      => (Offset => (0, 0),
                                                                                   Extent => Swapchain_Info.imageExtent));
    begin

      -- Ready the resources
      vkDeviceWaitIdle (Device);

      -- Use multiple queues, if they differ use concurrent mode to avoid ownership
      if Graphics_Family /= Present_Family then 
        Swapchain_Info.imageSharingMode      := VK_SHARING_MODE_CONCURRENT;
        Swapchain_Info.queueFamilyIndexCount := Indexes'Length;
        Swapchain_Info.pQueueFamilyIndices   := Indexes (1)'Unchecked_Access;
      else Swapchain_Info.imageSharingMode := VK_SHARING_MODE_EXCLUSIVE; end if;
      vkAssert (vkCreateSwapchainKHR (Device, Swapchain_Info'Unchecked_Access, null, Swapchain'Unchecked_Access));
  
      -- Create image views
      vkAssert (vkGetSwapchainImagesKHR (device               => Device,
                                         swapchain            => Swapchain,
                                         pSwapchainImageCount => Swapchain_Info.minImageCount'Unchecked_Access,
                                         pSwapchainImages     => Images (1)'Unchecked_Access));
      for I in Images'Range loop
        declare
        Image_View_Info : aliased VkImageViewCreateInfo := (image            => Images (I),
                                                            viewGLSL         => VK_IMAGE_VIEW_TYPE_2D,
                                                            format           => Image_Format.Format,
                                                            components       => (VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                 VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                 VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                 VK_COMPONENT_SWIZZLE_IDENTITY),
                                                            subresourceRange => (aspectMask => VK_IMAGE_ASPECT_COLOR_BIT,
                                                                                 levelCount => 1,
                                                                                 layerCount => 1,
                                                                                 others     => <>), others => <>);
        begin
          vkAssert (vkCreateImageView (Device, Image_View_Info'Unchecked_Access, null, Image_Views (I)'Unchecked_Access));
          Image_Views (I) := Make_View (Images (I), Image_Format.format, VK_IMAGE_ASPECT_COLOR_BIT);
        end;
      end loop;
 
      -- Create renderer
      vkAssert (vkCreateRenderPass     (Device, Render_Pass_Info'Unchecked_Access,     null, Render_Pass'Unchecked_Access));
      vkAssert (vkCreatePipelineLayout (Device, Pipeline_Layout_Info'Unchecked_Access, null, Pipeline_Layout'Unchecked_Access));
      Pipeline_Info.layout     := Pipeline_Layout;
      Pipeline_Info.renderPass := Render_Pass;      
      vkAssert (vkCreateGraphicsPipelines (device          => Device,
                                           pipelineCache   => NULL_PTR,
                                           createInfoCount => 1,
                                           pCreateInfos    => Pipeline_Info'Unchecked_Access,
                                           pAllocator      => null,
                                           pPipelines      => Graphics_Pipeline'Unchecked_Access));
                 
      -- Create depth resource      
      declare
      Depth_Image : Image_State (Width      => Swapchain_Info.imageExtent.width,
                                 Height     => Swapchain_Info.imageExtent.height,
                                 Format     => Depth_Format.format,
                                 Tiling     => VK_IMAGE_TILING_OPTIMAL,
                                 Usage      => VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT,
                                 Properties => VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
      Frame_Buffer_Info : aliased VkFramebufferCreateInfo := (others => <>);
      Depth_Image_View  : aliased Ptr                     := Make_View (Depth_Image.Data, Depth_Format.format, 2); --VK_IMAGE_ASPECT_DEPTH_BIT); -- WTF!!!!!
      Attachments       : aliased Array_Ptr               := (NULL_PTR, Depth_Image_View);
      begin
        Image_Layout (depthImage, depthFormat, VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL);

        -- Create frame buffers
        for I in Image_Views'Range loop
          Attachments (1) := Image_Views (I);
          Frame_Buffer_Info := (renderPass      => Render_Pass,
                                attachmentCount => Attachments'Length,
                                pAttachments    => Attachments (1)'Unchecked_Access,
                                width           => Swapchain_Info.imageExtent.width,
                                height          => Swapchain_Info.imageExtent.height,
                                layers          => 1,
                                others          => <>);
          vkAssert (vkCreateFramebuffer (Device, Frame_Buffer_Info'Unchecked_Access, null, Swapchain_Buffers (I)'Unchecked_Access));
        end loop;
      end;
    end;

  ----------------
  -- Initialize --
  ----------------
  
  procedure Initialize is

    -- Load all of the function pointers from a driver
    procedure Initialize_Vulkan_Subprograms is new API.Vulkan.Initialize (Get_Vulkan_Subprogram);

    -- Temporary variables for fetching and testing physical devices  
    Current_Has_Debugging     : aliased Bool                             := False;
    Current_Device            : aliased Ptr                              := NULL_PTR;
    Current_Depth_Format      : aliased VkSurfaceFormatKHR               := (others => <>);
    Current_Image_Format      : aliased VkSurfaceFormatKHR               := (others => <>);
    Current_Format_Properties : aliased VkFormatProperties               := (others => <>);
    Current_Surface_Details   : aliased VkSurfaceCapabilitiesKHR         := (others => <>);
    Current_Device_Properties : aliased VkPhysicalDeviceProperties       := (others => <>);
    Current_Memory_Properties : aliased VkPhysicalDeviceMemoryProperties := (others => <>);
    Current_Graphics_Family   : aliased Int_Unsigned_C                   := 0;
    Current_Present_Family    : aliased Int_Unsigned_C                   := 0;
    Current_Surface_Support   : aliased Int_Unsigned_C                   := 0;
    Count                     : aliased Int_Unsigned_C                   := 0;
    
    -- Creation details for atomic GPU queue semaphores 
    Semaphore_Info : aliased VkSemaphoreCreateInfo := (others => <>);
                                                                      
    -- Creation details for command pools are needed for the creation of command buffers
    Command_Pool_Info : aliased VkCommandPoolCreateInfo := (flags => VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT, others => <>);

    -- Application details to allow tools and drivers to maintain compatibility
    Engine_Name      : aliased Str_8_C           := To_Str_8_C (S (OS_Info.App_Name));
    Application_Name : aliased Str_8_C           := To_Str_8_C (S (OS_Info.App_Name));
    Application_Info : aliased VkApplicationInfo := (pApplicationName   => C (Application_Name),
                                                     applicationVersion => VK_MAKE_VERSION (1, 0, 0),
                                                     pEngineName        => C (Engine_Name),
                                                     engineVersion      => VK_MAKE_VERSION (1, 0, 0),
                                                     apiVersion         => VK_API_VERSION_1_0,
                                                     others             => <>);
    
    -- Creation details for the Vulkan instance - application info
    Instance_Extensions : aliased Array_Str_8_C        := Create ((VK_KHR_SURFACE_EXTENSION_NAME, Get_Vulkan_Extension));
    Debug_Layers        : aliased Array_Str_8_C        := Create (DEBUGGING_EXTENSIONS);
    Instance_Info       : aliased VkInstanceCreateInfo := (pApplicationInfo        => Application_Info'Unchecked_Access,
                                                           enabledExtensionCount   => Length (Instance_Extensions.Data),
                                                           ppenabledExtensionNames => Instance_Extensions.Get_Ref,
                                                           enabledLayerCount       => (if Is_Debugging then Length (Debug_Layers.Data)
                                                                                       else 0),
                                                           ppEnabledLayerNames     => (if Is_Debugging then Debug_Layers.Get_Ref
                                                                                       else NULL_PTR),
                                                           others                  => <>);
                  
    -- Creation details for physical device queues - the count and priority
    Queue_Priority : aliased Real_C                        := 1.0;
    Queue_Infos    : aliased Array_VkDeviceQueueCreateInfo := (1 => (queueCount       => 1,
                                                                     pQueuePriorities => Queue_Priority'Unchecked_Access,
                                                                     others           => <>));
                                                                     
    -- List of enabled features used for Device_Info and device creation
    Device_Features : aliased VkPhysicalDeviceFeatures := (shaderClipDistance                     => VK_TRUE,
                                                           shaderCullDistance                     => VK_TRUE,
                                                           shaderTessellationAndGeometryPointSize => VK_TRUE,
                                                           geometryShader                         => VK_TRUE,
                                                           others                                 => <>);

    -- Enabled extensions and layers for instance and device creation
    Device_Extensions : aliased Array_Str_8_C_State := Create ((VK_KHR_SWAPCHAIN_EXTENSION_NAME));
    Device_Info       : aliased VkDeviceCreateInfo  := (queueCreateInfoCount    => Queue_Infos'Length,
                                                        pQueueCreateInfos       => Queue_Infos (1)'Unchecked_Access,
                                                        pEnabledFeatures        => Device_Features'Unchecked_Access,
                                                        enabledExtensionCount   => (if Is_Debugging then Length (Device_Extensions.Data)
                                                                                    else 0),
                                                        ppenabledExtensionNames => (if Is_Debugging then Device_Extensions.Get_Ref
                                                                                    else NULL_PTR),
                                                        others                  => <>);

    -- Descriptor Sets
    Image_Info : aliased VkDescriptorImageInfo := (imageLayout => VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                   --imageView   =>  ,
                                                   sampler     => textureSampler,
                                                   others      => <>);
    Buffer_Info : aliased VkDescriptorBufferInfo := (buffer    => uniformBuffer,
                                                     offset    => 0,
                                                     rangeSize => sizeof(BufferBufferObject),
                                                     others    => <>);   
    Write_Set : aliased VkWriteDescriptorSet := ((dstSet          => descriptorSet,
                                                  dstBinding      => 0,
                                                  descriptorGLSL  => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC,
                                                  descriptorCount => 1,
                                                  pBufferInfo     => Buffer_Info'Unchecked_Access, --  mUBO.GetDesc(uboDescriptorInfo)
                                                  others          => <>));

    -- Descriptor set settings for pool creation - descriptors are first bound into sets then these sets are bound to the pipeline
    Layout_Bindings : aliased Array_VkDescriptorSetLayoutBinding := ((binding         => 0, -- UBO
                                                                      descriptorCount => 1,
                                                                      descriptorGLSL  => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                                                      stageFlags      => VK_SHADER_STAGE_VERTEX_BIT,
                                                                      others          => <>),
                                                                     (binding         => 1, -- Sampler
                                                                      descriptorCount => 1,
                                                                      descriptorGLSL  => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                                      stageFlags      => VK_SHADER_STAGE_FRAGMENT_BIT,
                                                                      others          => <>));
    Layout_Info : aliased VkDescriptorSetLayoutCreateInfo := (bindingCount => Layout_Bindings'Length,
                                                              pBindings    => Layout_Bindings (1)'Unchecked_Access,
                                                              others       => <>);

    Pool_Sizes : aliased Array_VkDescriptorPoolSize := ((typ             => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                                         descriptorCount => 1,
                                                         others          => <>),
                                                        (typ             => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                         descriptorCount => 1,
                                                         others          => <>));
    Pool_Info : aliased VkDescriptorPoolCreateInfo := (poolSizeCount => Pool_Sizes'Length,
                                                       pPoolSizes    => Pool_Sizes'Unchecked_Access,
                                                       maxSets       => 1,
                                                       others        => <>);
    Allocate_Info : aliased VkDescriptorSetAllocateInfo := (descriptorPool     => Pool_Info'Unchecked_Access,
                                                            descriptorSetCount => 1,
                                                            pSetLayouts        => Layout_Info,
                                                            others             => <>);

    -- Shaders
    Shader             :         Shader_State             := (others => <>);
    Shader_Module_Info : aliased VkShaderModuleCreateInfo := (others => <>);

    -- Buffers
    Alignment_Mod                :         Int_64_Unsigned_C           := 0;
    Alignment_Size               :         Int_64_Unsigned_C           := 0;
    Fence_Info                   : aliased VkFenceCreateInfo           := (others => <>);
    Command_Buffer_Info          : aliased VkCommandBufferBeginInfo    := (others => <>);
    Command_Buffer_Allocate_Info : aliased VkCommandBufferAllocateInfo := (others => <>);
    Memory_Requirments           : aliased VkMemoryRequirements := (others => <>);
    Buffer_Info : VkBufferCreateInfo := (size   => Upload_Buffer.Get * 1024**2,
                                         usage  => VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
                                         others => <>);
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
        vkAssert (vkEnumeratePhysicalDevices (Instance, Count'Unchecked_Access, Physical_Devices'Unchecked_Access));
 
        -- Find the best physical device and wrap it in a block to catch device verification exceptions
        for Current_Physical_Device of Physical_Devices loop begin
  
          -- Get queues
          vkGetPhysicalDeviceQueueFamilyProperties (Current_Physical_Device, Count'Unchecked_Access, null);
          declare Queue_Family_Properties : aliased Array_VkQueueFamilyProperties := (1..Int (Count) => (others => <>)); begin
            vkGetPhysicalDeviceQueueFamilyProperties (physicalDevice            => Current_Physical_Device,
                                                      pQueueFamilyPropertyCount => Count'Unchecked_Access,
                                                      pQueueFamilyProperties    => Queue_Family_Properties'Unchecked_Access);
  
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
  
          -- Verify image format
          vkAssert (vkGetPhysicalDeviceSurfaceFormatsKHR (Current_Physical_Device, Surface, Count'Unchecked_Access, null));
          declare Surface_Formats : aliased Array_VkSurfaceFormatKHR := (1..Int (Count) => (others => <>)); begin
            vkAssert (vkGetPhysicalDeviceSurfaceFormatsKHR (physicalDevice      => Current_Physical_Device,
                                                            surface             => Surface,
                                                            pSurfaceFormatCount => Count'Unchecked_Access,
                                                            pSurfaceFormats     => Surface_Formats'Unchecked_Access));
            Outter: for I in Surface_Formats'Range loop
              for J in SUPPORTED_IMAGE_FORMATS'Range loop
                if Surface_Formats (I) = SUPPORTED_IMAGE_FORMATS (J) then
                  Current_Image_Format := Surface_Formats (I);
                  exit Outter;
                end if;
              end loop;
              Assert (I /= Surface_Formats'Last);
            end loop Outter;
          end;
          
          -- Verify depth format
          for I in SUPPORTED_DEPTH_FORMATS'Range loop
            vkGetPhysicalDeviceFormatProperties (physicalDevice    => Current_Physical_Device,
                                                 format            => SUPPORTED_DEPTH_FORMATS (I),
                                                 pFormatProperties => Current_Format_Properties'Unchecked_Access);
                                                     
            -- We require optimal tiling
            if (Current_Format_Properties.optimalTilingFeatures and SUPPORTED_DEPTH_FORMATS (I)) > 0 then
              Current_Depth_Format.format := SUPPORTED_DEPTH_FORMATS (I);
              exit;
            end if;
            Assert (I /= SUPPORTED_DEPTH_FORMATS'Last);
          end loop;
            
          -- Check if debug extensions are available
          vkAssert (vkEnumerateDeviceExtensionProperties (Current_Physical_Device, null, Count'Unchecked_Access, null));
          declare Extensions : aliased Array_VkExtensionProperties := (1..Int (Count) => (others => <>)); begin
            vkAssert (vkEnumerateDeviceExtensionProperties (physicalDevice => Current_Physical_Device,
                                                            pLayerName     => null,
                                                            pPropertyCount => Count'Unchecked_Access,
                                                            pProperties    => Extensions (1)'Unchecked_Access));
            for Required_Extension of REQUIRED_EXTENSIONS loop
              for I in Extensions'Range loop
                exit when To_Str (Extensions (I).extensionName) = To_Str (Required_Extension);
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

          -- Get physical device and virtual device properties and compare to the current best
          vkAssert (vkGetPhysicalDeviceSurfaceCapabilitiesKHR (physicalDevice       => Current_Physical_Device,
                                                               surface              => Surface,
                                                               pSurfaceCapabilities => Current_Surface_Details'Unchecked_Access));
          vkGetPhysicalDeviceProperties       (Current_Physical_Device, Current_Device_Properties'Unchecked_Access);
          vkGetPhysicalDeviceMemoryProperties (Current_Physical_Device, Current_Memory_Properties'Unchecked_Access);
 
          -- Check if the current device is better than best found or if it is the first
          if Physical_Device = NULL_PTR
 
            -- Perfer physical devices with more VRAM (not the most exact method of determining performance, but good enough)
            --and (Current_Physical_Device )
          then
            Physical_Device   := Current_Physical_Device;
            Surface_Details   := Current_Surface_Details;
            Device_Properties := Current_Device_Properties;
            Memory_Properties := Current_Memory_Properties;
            Depth_Format      := Current_Depth_Format;
            Image_Format      := Current_Image_Format;
            Queue_Family      := Current_Graphics_Family;
          end if;
 
        -- Ignore unsuitable devices and continue looping
        exception when others => null; end; end loop;
 
        -- We may have failed to find a device, so crash if thats the case
        Assert (Physical_Device);
        
        -- Log device information
        Line ("Device Name: "       & To_Str         (Device_Properties.deviceName));
        Line ("Driver Version:"     & VK_VERSION_STR (Device_Properties.driverVersion));
        Line ("Vulkan API Version:" & VK_VERSION_STR (Device_Properties.apiVersion));
      exception when others => Line ("Your GPU is incompatible!"); raise Program_Error; end;
      
      -- Logical device
      Queue_Infos (1).queueFamilyIndex := Queue_Family;
      vkAssert (vkCreateDevice (Physical_Device, Device_Info'Unchecked_Access, null, Device'Unchecked_Access));
      vkGetDeviceQueue (Device, Queue_Family, 0, Queue'Unchecked_Access);
      vkAssert (vkCreateDescriptorSetLayout (Device, Layout_Info'Unchecked_Access, null, Descriptor_Set_Layout'Unchecked_Access));
        
      -- Main command pool
      Command_Pool_Info.queueFamilyIndex := Queue_Family;
      vkAssert (vkCreateCommandPool (Device, Command_Pool_Info'Unchecked_Access, null, Command_Pool'Unchecked_Access));
      
      -- Semaphores
      vkAssert (vkCreateSemaphore (Device, Semaphore_Info'Unchecked_Access, NULL_PTR, Acquire_Status'Unchecked_Access));
      vkAssert (vkCreateSemaphore (Device, Semaphore_Info'Unchecked_Access, NULL_PTR, Render_Status'Unchecked_Access));
      
      -- Shaders
      for Shader_Cursor of Iterate (Shaders) loop
        for Kind in Shader_Kind'Range loop
          declare
          Data : aliased Array_Byte := Load (Shaders.Key (Shader_Cursor) & "-" & To_Lower_Case (Kind'Wide_Image (1..4)) & ".spv");
          begin
            Shader_Module_Info := (codeSize => Data'Length, pCode => Data'Address, others => <>);
            vkAssert (vkCreateShaderModule (Device, Shader_Module_Info'Unchecked_Access, null, Shader (Kind)'Unchecked_Access));
          exception when others => Shader (Kind) := NULL_PTR; end;
        end loop;
        Shaders.Replace (Shader_Cursor, Shader);
      end loop;

      -- Uniform buffers
      for Uniform of Uniforms loop        
        Buffer_Info := (size   => Buffer.Size,
                        usage  => (case Buffer.Kind is
                                     when Index_Buffer  => VK_BUFFER_USAGE_INDEX_BUFFER_BIT
                                     when Vertex_Buffer => VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
                                     when others        => VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT)
                                   or (if Buffer.Usage = GPU_Usage then VK_BUFFER_USAGE_TRANSFER_DST_BIT else 0),
                        others => <>);
        Assert (vkCreateBuffer (Device, Buffer_Info'Unchecked_Access, NULL_PTR, Buffer.Data'Unchecked_Access));
        vkGetBufferMemoryRequirements (Device, Buffer.Data, Memory_Requirements'Unchecked_Access);
        Buffer.Memory := Allocate (Size   => Memory_Requirements.size,
                                   Align  => Memory_Requirements.alignment,
                                   Usage  => Memory_Requirements.memoryTypeBits,
                                   Memory => Buffer_Allocation);
        if Buffer.Data /= NULL_PTR then Adjust_Buffer (Buffer); end if;
      end loop;
      
      -- Staging buffers    
      for Stage of Stages loop
        vkAssert (vkCreateBuffer (Device, Buffer_Info'Unchecked_Access, null, Stage.Uniform'Unchecked_Access));
      end loop;
      vkGetBufferMemoryRequirements (Device, Staging_Buffers (1).Uniform, Buffer_Requirements'Unchecked_Access);
      Alignment_Mod  := Buffer_Requirements.size mod Buffer_Requirements.alignment;
      Alignment_Size := (if Alignment_Mod = 0 then Buffer_Requirements.size 
                         else Buffer_Requirements.size + Buffer_Requirements.alignment - Alignment_Mod);
      vkAssert (vkAllocateMemory (Device, Memory_Allocate_Info'Unchecked_Access, NULL_PTR, Memory'Unchecked_Access));
      for Stage of Staging_Buffers loop
        vkAssert (vkBindBufferMemory (Device, Stage.Uniform, Memory, I * Alignment_Size));
      end loop;

      -- Descriptor sets  
      vkAssert (vkCreateDescriptorPool   (Device, Pool_Info'Unchecked_Access, null, Descriptor_Pool'Unchecked_Access));
      vkAssert (vkAllocateDescriptorSets (Device, Allocate_Info'Unchecked_Access, Descriptor_Set'Unchecked_Access));
      vkAssert (vkUpdateDescriptorSets   (Device, Write_Set'Length, Write_Set'Unchecked_Access, 0, null));

      -- Staging command pools
      vkAssert (vkMapMemory (Device, Memory, 0, Alignment_Size * NUM_FRAME_DATA, 0, Mapped_Staging_Data)));
      vkAssert (vkCreateCommandPool (Device, Command_Pool_Create_Info'Unchecked_Access, NULL_PTR, Command_Pool'Unchecked_Access));
      Command_Buffer_Allocate_Info.commandPool := Command_Pool;
      for Stage of Stages loop
        vkAssert (vkAllocateCommandBuffers (Device, Command_Buffer_Allocate_Info'Unchecked_Access, Stage.Buffers (i).Command_Buffer'Unchecked_Access));
        vkAssert (vkCreateFence            (Device, Fence_Info'Unchecked_Access, NULL_PTR, Stage.Buffers (i).Fence));
        vkAssert (vkBeginCommandBuffer     (Stage.Buffers (i).Command_Buffer, Command_Buffer.BeginInfo'Unchecked_Access));
        Stage.Buffers (i).data := Stage.Buffers (i).mappedData + (i * Alignment_Size);
      end loop;

      -- Kickoff the tasks
      Backend_Task.Initialize;
      Frontend_Task.Initialize;
    exception when others => Line ("Vulkan driver not installed or up to date?"); raise Program_Error; end;

  ----------
  -- Draw --
  ----------

  procedure Draw is
    Set_Allocation_Info : aliased VkDescriptorSetAllocateInfo := (descriptorPool     => m_descriptorPools[ m_currentData ],
                                                                  descriptorSetCount => 1,
                                                                  pSetLayouts        => &prog.descriptorSetLayout);
    begin

      -- Find pipeline
      for Loaded_Pipeline of Loaded_Pipelines loop
        if Loaded_Pipeline.State = Pipeline then
          Current_Pipeline := Loaded_Pipeline.Data;
          exit;
        end if;
      end loop;

      -- Create a pipeline if one was not found
      if Current_Pipeline = NULL_PTR then
        declare

        -- Vertex Description
        Vertex_Binding : aliased VkVertexInputBindingDescription := (binding   => 0,
                                                                     stride    => Vertex_State'Object_Size / Byte'Object_Size,
                                                                     inputRate => VK_VERTEX_INPUT_RATE_VERTEX);
        Vertex : Vertex_State := (others => <>); -- Temp for 'Position attribute
        Vertex_Input_Info : aliased VkPipelineVertexInputStateCreateInfo :=
          (vertexBindingDescriptionCount   => Vertex_Binding'Length,
           pVertexBindingDescriptions      => Vertex_Binding (1)'Unchecked_Access,
           vertexAttributeDescriptionCount => Vertex_Attribute'Length,
           pVertexAttributeDescriptions    => Vertex_Attribute (1)'Unchecked_Access,
           others                          => <>); 

        -- ???
        Input_Assembly_Info : aliased VkPipelineInputAssemblyStateCreateInfo := (topology => VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
                                                                                 others   => <>);
        
        -- ???
        Rasterizer_Info : aliased VkPipelineRasterizationStateCreateInfo := 
          (polygonMode             => VK_POLYGON_MODE_FILL,
           rasterizerDiscardEnable => VK_FALSE,
           depthClampEnable        => VK_FALSE,
           depthBiasEnable         => Pipeline.Polygon_Offset),
           lineWidth               => 1.0,
           frontFace               => Pipeline.Clockwise,
           cullMode                => (case Pipeline.Cull_Mode is
                                         when VK_CULL_MODE_BACK_BIT  => (if Pipeline.Mirror_View then VK_CULL_MODE_FRONT_BIT
                                                                         else VK_CULL_MODE_BACK_BIT)
                                         when VK_CULL_MODE_FRONT_BIT => (if Pipeline.Mirror_View then VK_CULL_MODE_BACK_BIT
                                                                         else VK_CULL_MODE_FRONT_BIT)
                                         when others                 => Pipeline.Cull_Mode), others => <>);
                      
        -- Multisample                                                       
        Multisample_Info : aliased VkPipelineMultisampleStateCreateInfo := (sampleShadingEnable  => Pipeline.Super_Sample,
                                                                            rasterizationSamples => Sample_Count,
                                                                            minSampleShading     => Pipeline.Super_Sample,
                                                                            others               => <>);   
                                                                            
        -- Depth stencil
        Depth_Reference : Int_Unsigned_C := ((stateBits & GLS_STENCIL_FUNC_REF_BITS) >> GLS_STENCIL_FUNC_REF_SHIFT);
        Depth_Mask      : Int_Unsigned_C := ((stateBits & GLS_STENCIL_FUNC_MASK_BITS) >> GLS_STENCIL_FUNC_MASK_SHIFT);
        Back_Stencil    : Int_Unsigned_C := (if Pipeline.Use_Separate_Stencil then Back_Stencil else Front_Stencil);
        Depth_Stencil_Info : aliased VkPipelineDepthStencilStateCreateInfo :=
          Dest_Blend /= Zero_Blend or Pipeline.Src_Blend /= One_Blend),
          (depthTestEnable       => VK_TRUE,
           depthWriteEnable      => Pipeline.Depth_Mask,
           depthCompareOp        => Pipeline.Depth_Compare,
           depthBoundsTestEnable => Pipeline.Depths_Bounds_Test,
           minDepthBounds        => 0.0,
           maxDepthBounds        => 1.0,
           stencilTestEnable     => GLS_STENCIL_FUNC_BITS | GLS_STENCIL_OP_BITS | GLS_SEPARATE_STENCIL)),
           front                 => (writeMask   => 16#FFFF_FFFF#,
                                     compareOp   => stencilCompareOp,
                                     compareMask => mask,
                                     reference   => ref,
                                     failOp      => Front_Stencil,
                                     passOp      => Front_Stencil,
                                     depthFailOp => Front_Stencil),
           back                  => (writeMask   => 16#FFFF_FFFF#,
                                     compareOp   => stencilCompareOp,
                                     compareMask => mask,
                                     reference   => ref,
                                     failOp      => Back_Stencil,
                                     passOp      => Back_Stencil,
                                     depthFailOp => Back_Stencil), others => <>); 

        -- Color Blend
        Color_Blend_Attachment : aliased VkPipelineColorBlendAttachmentState :=
          (srcColorBlendFactor => Pipeline.Source_Factor),
           dstColorBlendFactor => Pipeline.Destination_Factor),
           srcAlphaBlendFactor => Pipeline.Source_Factor),
           dstAlphaBlendFactor => Pipeline.Destination_Factor),
           colorBlendOp        => Pipeline.Color_Blend),
           alphaBlendOp        => Pipeline.Alpha_Blend),
           blendEnable         => Pipeline.Dest_Blend /= Zero_Blend or Pipeline.Src_Blend /= One_Blend),
           colorWriteMask      => Pipeline.Red_Mask or Pipeline.Green_Mask or Pipeline.Blue_Mask or Pipeline.Alpha_Mask,
           others              => <>);
        Color_Blend_Info : aliased VkPipelineColorBlendStateCreateInfo := (logicOpEnable   => 0,
                                                                           logicOp         => VK_LOGIC_OP_COPY,
                                                                           attachmentCount => 1,
                                                                           pAttachments    => Color_Blend_Attachment'Unchecked_Access,
                                                                           blendConstants  => (others => 0.0),
                                                                           others          => <>);   

        -- Dynamic States
        Dynamic_States : aliased Array_Int_Unsigned_C := (VK_DYNAMIC_STATE_SCISSOR,
                                                          VK_DYNAMIC_STATE_VIEWPORT,
                                                          (if not Pipeline.Has_Polygon_Offset then VK_DYNAMIC_STATE_DEPTH_BOUNDS
                                                           else VK_DYNAMIC_STATE_DEPTH_BIAS), -- Jump through some hoops
                                                          VK_DYNAMIC_STATE_DEPTH_BIAS);
        Dynamic_State_Info : aliased VkPipelineDynamicStateCreateInfo := (pDynamicStates    => Dynamic_States (1)'Unchecke_Access,
                                                                          dynamicStateCount => (if Pipeline.Has_Polygon_Offset then 1
                                                                                                else 0) +
                                                                                               (if Pipeline.Has_Polygon_Offset then 1
                                                                                                else 0) + 2, others => <>);

        -- Pipeline
        Viewport_Info : aliased VkPipelineViewportStateCreateInfo := (viewportCount => 1, scissorCount => 1, others => <>);
        Pipeline_Info : aliased VkGraphicsPipelineCreateInfo := (stageCount          => 2,
                                                                 pStages             => Shader_Stages_Info (1)'Unchecked_Access,
                                                                 pVertexInputState   => Vertex_Input_Info'Unchecked_Access,
                                                                 pInputAssemblyState => Input_Assembly_Info'Unchecked_Access,
                                                                 pViewportState      => Viewport_State_Info'Unchecked_Access,
                                                                 pRasterizationState => Rasterizer_Info'Unchecked_Access,
                                                                 pMultisampleState   => Multisample_Info'Unchecked_Access,
                                                                 pDepthStencilState  => Depth_Stencil_Info'Unchecked_Access,
                                                                 pColorBlendState    => Color_Blend_Info'Unchecked_Access,
                                                                 renderPass          => Render_Pass;
                                                                 pVertexInputState   => &vertexInputState;
                                                                 layout              => pipelineLayout;
                                                                 pDynamicState       => Dynamic_State_Info'Unchecked_Access;
                                                                 pViewportState      => Viewport_Info'Unchecked_Access;
                                                                 others              => <>);
        Pipeline_Layout_Info : aliased VkPipelineLayoutCreateInfo := (setLayoutCount => 1,
                                                                      pSetLayouts    => Descriptor_Set_Layout'Unchecked_Access,
                                                                      others         => <>);   


        -- ???
        begin
          Pipeline_Info.renderPass := Render_Pass;      
          vkAssert (vkCreateGraphicsPipelines (device          => Device,
                                               pipelineCache   => pipelineCache,
                                               createInfoCount => 1,
                                               pCreateInfos    => Pipeline_Info'Unchecked_Access,
                                               pAllocator      => null,
                                               pPipelines      => Graphics_Pipeline'Unchecked_Access));
          -- ??? Add to list of pipelines
        end;
      end if;

      -- Create writes linked with their appropriate additional structure (e.g. VkDescriptorBufferInfo or VkDescriptorImageInfo)
      if Writes.Length > 0 then 
        declare
        Result_Writes              : Vector_VkWriteDescriptorSet.Unsafe_Array   := To_Unsafe_Array (Writes);
        Result_Buffer_Descriptions : Vector_VkDescriptorBufferInfo.Unsafe_Array := To_Unsafe_Array (Buffer_Descriptions);
        Result_Image_Descriptions  : Vector_VkDescriptorImageInfo.Unsafe_Array  := To_Unsafe_Array (Image_Descriptions);
        begin

          -- Link the data together then update
          for Result_Write of Result_Writes loop
            if Result_Write.descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER then    

          -- Clear the write data
          Writes.Clear;
          Write_Indexes.Clear;
          Buffer_Descriptions.Clear;
          Image_Descriptions.Clear;
        end;
      end if;

      -- Bind descriptors and pipeline
      vkCmdBindDescriptorSets (commandBuffer      => Commands, 
                               pipelineBindPoint  => VK_PIPELINE_BIND_POINT_GRAPHICS, 
                               layout             => prog.pipelineLayout,
                               firstSet           => 0,
                               descriptorSetCount => 1,
                               pDescriptorSets    => &descSet, 
                               dynamicOffsetCount => 0,
                               pDynamicOffsets    => NULL_PTR);
      vkCmdBindPipeline (Commands, VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline);

      -- Draw it
      vkCmdDrawIndexed (commandBuffer => Commands,
                        indexCount    => drawSurf->numIndexes,
                        instanceCount => 1,
                        firstIndex    => indexOffset >> 1,
                        vertexOffset  => vertOffset / ( drawSurf->jointCache ? sizeof( idShadowVertSkinned ) : size idShadowVert)),
                        firstInstance => 0);
    end;
  end;
end;

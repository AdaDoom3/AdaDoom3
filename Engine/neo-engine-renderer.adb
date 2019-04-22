
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

with Neo.World.CVars; use Neo.World.CVars;

package body Neo.Engine.Renderer is
  
  -------------
  -- Shaders --
  -------------
 
  -- Define a shader for each pass within the backend  
  package body Shader is
    procedure Commit is begin Pipeline.Shader := Shaders.Element (U (Path)); end;
    
    -- Register a new shader
    begin
      Shaders.Insert (U (Path), (To_Unsafe_Vector (Stages), others => <>));
      
      -- Verify the shader only has one of any given stage
      declare
      Has_Stage : array (Stage_Kind) of Bool := (others => False);
      begin
        for I of Stages loop
          if Has_Stage (I.Kind) then
            Line_Error ("When loading shader " & Path & " multiple " & I.Kind'Wide_Image & "'s were found");
            raise Program_Error;
          end if;
          Has_Stage (I.Kind) := True;
        end loop;
      end;
    end;    
                                                                              
  -- Texture map samplers
  package body Sampler is
      Current_Val : Str_Unbound := NULL_STR_UNBOUND;
      function Get return Str is (S (Current_Val));
      procedure Set (Val : Str) is
        Image : Buffer_State := Buffered_Images.Get (U (Val));
        begin
          Writes.Append (Write_Descriptor_State'(Is_Image   => True,
                                                 Image_Info => (imageLayout => VK_IMAGE_LAYOUT_GENERAL,
                                                                imageView   => Image.View, 
                                                                sampler     => Image.Sampler, others => <>),
                                                 Set        => (dstBinding      => Binding,
                                                                descriptorCount => 1,
                                                                descriptorType  => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, others => <>)));
          Current_Val := U (Val);
        end;
    begin Sampler_Count := Sampler_Count + 1; end;
  
  -- "Uniform" data value a.k.a. global shader variable / parameter
  package body Uniform is   
      Uniform_Index : Positive := 1;
      Current_Val   : Uniform_T;
      function Get return Uniform_T is (Current_Val);
      procedure Set (Val : Uniform_T) is
        Buffer : Buffer_State := Buffered_Uniforms.Element (U (Name_ID));
        begin
          Writes.Append (Write_Descriptor_State'(Is_Image    => False,
                                                 Buffer_Info => (buffer => Buffer.Data,
                                                                 offset => Buffer.Offset,
                                                                 rang   => Buffer.Size),
                                                 Set         => (dstBinding      => Binding,
                                                                 descriptorCount => 1,
                                                                 descriptorType  => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, others => <>)));
          Current_Val := Val;
          Safe_Staging.Set_Buffer (Buffer, Val'Address, Val'Size);
        end;
    
    -- Register a new buffer and save the index for writes/sets
    begin
      Buffered_Uniforms.Insert (U (Name_ID), (Usage, Uniform_Buffer, Binding => Binding, Size => Uniform_T'Object_Size / Byte'Size, others => <>));
      Uniform_Index := Int (Buffered_Uniforms.Length);
    end;

  -- Samplers corresponding to each of the components of the material type
  package Base_Color_Sampler is new Sampler (Layout_Set => 1, Binding => 0);
  package Irradiance_Sampler is new Sampler (Layout_Set => 1, Binding => 1);
  package Filter_Sampler     is new Sampler (Layout_Set => 1, Binding => 2);
  package Specular_Sampler   is new Sampler (Layout_Set => 1, Binding => 3);
  package Normal_Sampler     is new Sampler (Layout_Set => 1, Binding => 4);
  package Displace_Sampler   is new Sampler (Layout_Set => 1, Binding => 5);
  package Metallic_Sampler   is new Sampler (Layout_Set => 1, Binding => 6);
  package Roughness_Sampler  is new Sampler (Layout_Set => 1, Binding => 7);
  package Projection_Sampler is new Sampler (Layout_Set => 1, Binding => 8);
  package Falloff_Sampler    is new Sampler (Layout_Set => 1, Binding => 9);
  package Menu_Sampler       is new Sampler (Layout_Set => 1, Binding => 10);
  
  -- Skinning flag
  package Enable_Skinning    is new Uniform (Layout_Set => 2, Binding => 0, Uniform_T => Bool, Name_ID => "enableSkinning");
  
  -- Texture map flags
  package Enable_Base_Color  is new Uniform (Layout_Set => 2, Binding => 0, Uniform_T => Bool, Name_ID => "enableBaseColor");
  package Enable_Irradiance  is new Uniform (Layout_Set => 2, Binding => 1, Uniform_T => Bool, Name_ID => "enableIrradiance");
  package Enable_Filter      is new Uniform (Layout_Set => 2, Binding => 2, Uniform_T => Bool, Name_ID => "enableFilter");
  package Enable_Specular    is new Uniform (Layout_Set => 2, Binding => 3, Uniform_T => Bool, Name_ID => "enableSpecular");
  package Enable_Normal      is new Uniform (Layout_Set => 2, Binding => 4, Uniform_T => Bool, Name_ID => "enableNormal");
  package Enable_Displace    is new Uniform (Layout_Set => 2, Binding => 5, Uniform_T => Bool, Name_ID => "enableDisplace");
  package Enable_Metallic    is new Uniform (Layout_Set => 2, Binding => 0, Uniform_T => Bool, Name_ID => "enableMetallic");
  package Enable_Roughness   is new Uniform (Layout_Set => 2, Binding => 1, Uniform_T => Bool, Name_ID => "enableRoughness");
  package Enable_Projection  is new Uniform (Layout_Set => 2, Binding => 2, Uniform_T => Bool, Name_ID => "enableProjection");
  package Enable_Falloff     is new Uniform (Layout_Set => 2, Binding => 3, Uniform_T => Bool, Name_ID => "enableFalloff");
  
  -- Tesselation 
  package Enable_Tesselation is new Uniform (Layout_Set => 2, Binding => 1, Uniform_T => Bool,      Name_ID => "enableTesselation");
  package Tesselation_Max    is new Uniform (Layout_Set => 2, Binding => 6, Uniform_T => Real_64_C, Name_ID => "tesselationMax");
  package Tesselation_Amount is new Uniform (Layout_Set => 2, Binding => 7, Uniform_T => Real_64_C, Name_ID => "tesselationAmount");  

  ---------------------
  -- Implementations --
  ---------------------
  
  package body Doom3    is separate;
  package body FGED2    is separate;
  package body Raytrace is separate;
  
  procedure Run_Frontend is
    Last_Time : Time := Clock;
    begin
      loop
        View.Set (case Rendering.Get is when Doom3_Rendering    => Doom3.Build_View,
                                        when FGED2_Rendering    => FGED2.Build_View,
                                        when Raytrace_Rendering => Raytrace.Build_View);
        
        -- Save cycles
        delay WINDOW_POLLING_DURATION - (Clock - Last_Time); Last_Time := Clock;        
      end loop;
    end;
  
  procedure Run_Backend is 
  
    -- ???
    procedure Trigger_Framebuffer_Restart is
      begin
      
        -- Signal the main task to perfor a restart and give it some time
        Framebuffer_Status.Occupied (False);
        delay 10.0;
        
        -- There has been a catastrophic failure...
        raise Program_Error;
      end;
  
    -- Image index for presentation
    I           :         Int            := 1;
    Image_Index : aliased Int_Unsigned_C := 0;
    
    -- Timing
    Last_Frame_Time   : Time := Clock + WINDOW_POLLING_DURATION;
    Last_Collect_Time : Time := Clock;
    
    -- ???
    Clear_Value               : aliased VkClearValue             := (color => (1.0, 0.1, 0.1, 1.0), others => <>);
    Command_Buffer_Begin_Info : aliased VkCommandBufferBeginInfo := (others => <>);  
    Render_Pass_Begin_Info    : aliased VkRenderPassBeginInfo    := (renderPass => Render_Pass,
                                                                     renderArea => (offset => (0, 0),
                                                                                    extent => Surface_Extent),
                                                                                    
      clearValueCount => 1,
      pClearValues    => Clear_Value'Unchecked_Access, others => <>);
                                                                                    
    -- ??? 
    Present_Info : aliased VkPresentInfoKHR := (swapchainCount     => 1,
                                                pSwapchains        => Swapchain'Unchecked_Access,
                                                waitSemaphoreCount => 1,
                                                pImageIndices      => Image_Index'Unchecked_Access, others => <>);
                                                                                    
    -- Transition our swap image to present instead of having the renderpass do the transition to avoid additional image barriers
    Image_Memory_Barrier : aliased VkImageMemoryBarrier := (srcQueueFamilyIndex => VK_QUEUE_FAMILY_IGNORED,
                                                            dstQueueFamilyIndex => VK_QUEUE_FAMILY_IGNORED,
                                                            oldLayout           => VK_IMAGE_LAYOUT_UNDEFINED,
                                                            newLayout           => VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
                                                            srcAccessMask       => VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
                                                            subresourceRange    => (aspectMask     => VK_IMAGE_ASPECT_COLOR_BIT,
                                                                                    baseMipLevel   => 0,
                                                                                    levelCount     => 1,
                                                                                    baseArrayLayer => 0,
                                                                                    layerCount     => 1), others => <>);

    -- ???
    Wait_Stage  : aliased Int_Unsigned_C := VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT; 
    Submit_Info : aliased VkSubmitInfo   := (commandBufferCount   => 1,
                                             pWaitDstStageMask    => Wait_Stage'Unchecked_Access,
                                             waitSemaphoreCount   => 1,
                                             signalSemaphoreCount => 1, others => <>);
                                             
    -- Start of Run_Backend
    begin
      loop

        -- Fetch the next image index from the swap chain if there is one
        case vkAcquireNextImageKHR (Device      => Device,
                                    swapchain   => Swapchain,
                                    timeout     => Int_64_Unsigned_C'Last,
                                    semaphore   => Framebuffer (I).Acquire_Status,
                                    fence       => NULL_PTR,
                                    pImageIndex => Image_Index'Unchecked_Access)
        is
          when VK_ERROR_OUT_OF_DATE_KHR | VK_SUBOPTIMAL_KHR => Trigger_Framebuffer_Restart;
          when VK_TIMEOUT | VK_NOT_READY | VK_SUCCESS       => null;
          when others                                       => raise Program_Error;
        end case;
  
        -- Housekeeping
        if GARBAGE_POLLING_DURATION <= Clock - Last_Collect_Time then
          Safe_Allocation.Finalize_Buffers;
          Last_Collect_Time := Clock;
        end if;

        -- Save cycles
        delay WINDOW_POLLING_DURATION - (Clock - Last_Frame_Time); Last_Frame_Time := Clock; 
         
        -- Wait for the frame's commands to finish execution the reinitialize it
        vkAssert (vkWaitForFences       (Device, 1, Framebuffer (I).Fence'Unchecked_Access, VK_TRUE, Int_64_Unsigned_C'Last));
        vkAssert (vkResetFences         (Device, 1, Framebuffer (I).Fence'Unchecked_Access));
        vkAssert (vkResetDescriptorPool (Device, Descriptor_Pool, 0));  
        vkAssert (vkBeginCommandBuffer  (Framebuffer (I).Commands, Command_Buffer_Begin_Info'Unchecked_Access));

        -- End render pass
        Render_Pass_Begin_Info.framebuffer := Framebuffer (Positive (Image_Index + 1)).Swapchain_Buffer;
        vkCmdBeginRenderPass (Framebuffer (I).Commands, Render_Pass_Begin_Info'Unchecked_Access, VK_SUBPASS_CONTENTS_INLINE);
  
        -- Dispatch to the appropriate rendering path
        case Rendering.Get is
          when Doom3_Rendering    => Doom3.Build_Frame    (Framebuffer (I), View.Get);
          when FGED2_Rendering    => FGED2.Build_Frame    (Framebuffer (I), View.Get);
          when Raytrace_Rendering => Raytrace.Build_Frame (Framebuffer (I), View.Get);
        end case;
        
        -- End render pass
        Image_Memory_Barrier.image := Framebuffer (Positive (Image_Index + 1)).Image;
        vkCmdEndRenderPass (Framebuffer (I).Commands);  
        vkCmdPipelineBarrier (commandBuffer            => Framebuffer (I).Commands,
                              srcStageMask             => VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                              dstStageMask             => VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT,
                              dependencyFlags          => 0,
                              memoryBarrierCount       => 0,
                              pMemoryBarriers          => null,
                              bufferMemoryBarrierCount => 0,
                              pBufferMemoryBarriers    => null,
                              imageMemoryBarrierCount  => 1,
                              pImageMemoryBarriers     => Image_Memory_Barrier'Unchecked_Access);        
        vkAssert (vkEndCommandBuffer (Framebuffer (I).Commands));
        
        -- Submit the current frame's commands  
        Submit_Info.pCommandBuffers   := Framebuffer (I).Commands'Unchecked_Access;
        Submit_Info.pWaitSemaphores   := Framebuffer (I).Acquire_Status'Unchecked_Access;
        Submit_Info.pSignalSemaphores := Framebuffer (I).Render_Status'Unchecked_Access;
        vkAssert (vkQueueSubmit (Queue, 1, Submit_Info'Unchecked_Access, Framebuffer (I).Fence));
  
        -- Show the resulting image
        Present_Info.pWaitSemaphores := Framebuffer (I).Render_Status'Unchecked_Access;
        if vkQueuePresentKHR (Queue, Present_Info'Unchecked_Access) in VK_ERROR_OUT_OF_DATE_KHR | VK_SUBOPTIMAL_KHR then
          Trigger_Framebuffer_Restart;
        end if;
        
        -- Iterate the current frame
        I := I mod Framebuffer'Length + 1;    
      end loop;
    end; 

  package Backend_Tasks  is new Tasks (Run_Backend);
  package Frontend_Tasks is new Tasks (Run_Frontend);

  Backend_Task  : Backend_Tasks.Safe_Task;
  Frontend_Task : Frontend_Tasks.Safe_Task;
  
  ----------------
  -- Allocation --
  ----------------
    
  -- ???
  procedure Initialize_Buffer (Buffer : in out Buffer_State; Usage_Bits : Int_Unsigned_C; Data : Ptr; Data_Size : Int_Ptr; Count : Positive) is
    Buffer_Info : aliased VkBufferCreateInfo := (size        => Int_64_Unsigned_C (Data_Size),
                                                 usage       => Usage_Bits or VK_BUFFER_USAGE_TRANSFER_DST_BIT,
                                                 sharingMode => VK_SHARING_MODE_EXCLUSIVE, others => <>);
    begin
      vkAssert (vkCreateBuffer (Device, Buffer_Info'Unchecked_Access, null, Buffer.Data'Unchecked_Access));
      Safe_Allocation.Allocate_Buffer (Buffer);
      vkAssert (vkBindBufferMemory (Device, Buffer.Data, Buffer.Device_Memory, Buffer.Offset));
      Safe_Staging.Set_Buffer (Buffer, Data, Data_Size);
      Buffer.Count := Int_Unsigned_C (Count);
    end;
    
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
        if (Memory_Type_Bits and 2 ** (I - 1)) /= 0 and (Properties and Preferred) > 0 and (Properties and Required) > 0 then
          return Int_Unsigned_C (I - 1);
        end if;
      end loop;    
        
      -- Couldn't find a suitable memory type, so match on the required parts at least
      for I in 1..Int (Memory_Properties.memoryTypeCount) loop
        Properties := Memory_Properties.memoryTypes (I).propertyFlags;
        if (Memory_Type_Bits and 2 ** (I - 1)) /= 0 and (Properties and Required) > 0 then return Int_Unsigned_C (I - 1); end if;
      end loop;
      
      -- Go with preferred when we can't even match on required
      for I in 1..Int (Memory_Properties.memoryTypeCount) loop
        Properties := Memory_Properties.memoryTypes (I).propertyFlags;
        if (Memory_Type_Bits and 2 ** (I - 1)) /= 0 and (Properties and Preferred) > 0 then return Int_Unsigned_C (I - 1); end if;
      end loop;
      
      -- No suitable memory types
      raise Program_Error;
    end;
    
  protected body Safe_Allocation is 
  
      -- ???
      procedure Throw_Away (Buffer : Buffer_State) is begin Buffered_Garbage.Append (Buffer); end;
    
      -- Grab some memory from a Heap
      procedure Allocate_Buffer (Buffer : in out Buffer_State) is
      
        -- #define ALIGN (x, a) (((x) + ((a) - 1)) & ~((a) - 1))
        function Align (Offset, Alignment : Int_64_Unsigned_C) return Int_64_Unsigned_C is ((Alignment - 1 + Offset) and not (Alignment - 1));
    
        -- Check if two offsets and sizes are suitable for the same page - see the Vulkan spec "Buffer-Image Granularity"
        function Can_Be_On_Same_Page (Offset_A, Size_A, Offset_B : Int_64_Unsigned_C) return Bool is
          (((Offset_A + Size_A - 1) and (not (Device_Properties.limits.bufferImageGranularity - 1))) =
            (Offset_B               and (not (Device_Properties.limits.bufferImageGranularity - 1))));
    
    
        -- Check that the memory kinds do not have a granularity conflict
        function Has_Granularity_Conflict (Allocation_A, Allocation_B : Allocation_Kind) return Bool is
          TEST_A : constant Allocation_Kind := (if Allocation_A > Allocation_B then Allocation_B else Allocation_A);
          TEST_B : constant Allocation_Kind := (if TEST_A       = Allocation_A then Allocation_B else Allocation_A);
          begin
            return (case TEST_A is when Free_Allocation                             => False,
                                   when Buffer_Allocation | Linear_Image_Allocation => TEST_B = Optimal_Image_Allocation,
                                   when Optimal_Image_Allocation                    => False);
          end;
          
        -- Global Variables
        Allocation           :         Allocation_Kind      := (if Buffer.Kind = Image_Buffer then Optimal_Image_Allocation else Buffer_Allocation);
        Memory_Type_Index    :         Int_Unsigned_C       := 0;
        Offset, Aligned_Size :         Int_64_Unsigned_C    := 0;
        Memory_Requirements  : aliased VkMemoryRequirements := (others => <>);
        Current              : aliased Ptr_Allocation_State := null;
          
        -- Allocate the buffer from a given chunk and Heap = the globals above are used heavily
        procedure Allocate_From_Chunk (Chunk : Ptr_Allocation_State; Heap_Cursor : Vector_Heap.Cursor) is
          Heap : Heap_State := Element (Heap_Cursor);
          begin
          
            -- Split the best fitting chunk when the size is not an exact fit
            if Chunk.Size > Memory_Requirements.size then
              Chunk.Next := new Allocation_State'(ID       => Heap.Next_ID,
                                                  Offset   => Offset + Memory_Requirements.size,
                                                  Size     => Chunk.Size - Aligned_Size,
                                                  Kind     => Free_Allocation,
                                                  Next     => Chunk.Next,
                                                  Previous => Chunk);
              Chunk.Size := Memory_Requirements.size;
              Heap.Next_ID := Heap.Next_ID + 1;
            end if;
    
            -- Assign the chunk and mark it as allocated
            Heap.Allocated := Heap.Allocated + Chunk.Size;
            Replace_Element (Heaps, Heap_Cursor, Heap);
            Chunk.Kind := Allocation;
            
            -- Build the result
            Buffer.ID            := Chunk.ID;
            Buffer.Offset        := Offset;
            Buffer.Size          := Chunk.Size;
            Buffer.Device_Memory := Heap.Device_Memory;
            Buffer.Heap          := Heap_Cursor;
            if Buffer.Kind /= Image_Buffer and Buffer.Usage /= GPU_Usage then
              Buffer.Data := To_Ptr (To_Int_Ptr (Heap.Data) + Int_Ptr (Offset));
            end if;
          end;
          
        -- Start of Allocate_Buffer
        begin 
          
          -- Get the memory requirements
          if Buffer.Kind = Image_Buffer then vkGetImageMemoryRequirements (Device, Buffer.Data, Memory_Requirements'Unchecked_Access);
          else vkGetBufferMemoryRequirements (Device, Buffer.Data, Memory_Requirements'Unchecked_Access); end if;
          Memory_Type_Index := Find_Memory_Type_Index (Memory_Requirements.memoryTypeBits, Buffer.Usage);
          
          -- Try to allocate from a suitable Heap
          for I in Iterate (Heaps) loop declare Heap : Heap_State := Element (I); begin
          
            -- Test if the current Heap is too small
            if Heap.Index = Memory_Type_Index and then Heap.Size - Heap.Allocated >= Memory_Requirements.size then
    
              -- Find the best fit chunk
              Current := Heap.First_Chunk;
              Inner: while Current /= null loop
    
                -- Only example chunks that are free 
                if Current.Kind = Free_Allocation then
    
                  -- Set the offset in case the current chunk's granularity conflicts with the previous chunk
                  Offset := Align (Current.Offset, Memory_Requirements.alignment);
                  if Current.Previous /= null and then Device_Properties.limits.bufferImageGranularity > 1
                    and then Can_Be_On_Same_Page (Current.Previous.Offset, Current.Previous.Size, Offset)
                    and then Has_Granularity_Conflict (Current.Previous.Kind, Allocation)
                  then
                    Offset := Align (Offset, Device_Properties.limits.bufferImageGranularity);
                  end if;
    
                  -- Check the next chunk's allocation for suitability
                  Aligned_Size := Offset - Current.Offset + Memory_Requirements.size;
                  if Aligned_Size <= Current.Size then 
    
                    -- Bail when the Heap is too small to allocate a new one
                    exit when Aligned_Size + Heap.Allocated >= Heap.Size;
    
                    -- Check for granularity conflicts with the next chunk before allocating
                    if Current.Next = null or else Device_Properties.limits.bufferImageGranularity <= 1
                      or else not Can_Be_On_Same_Page (Offset, Memory_Requirements.size, Current.Next.Offset)
                      or else not Has_Granularity_Conflict (Allocation, Current.Next.Kind)
                    then
                      Allocate_From_Chunk (Current, I);
                      return;
                    end if;
                  end if;
                end if;
    
                -- Advance to the chunk within the Heap
                Current := Current.Next;
              end loop Inner;
            end if;
          end; end loop;
    
          -- Allocate a new Heap since no suitable existing ones could be found
          declare
          Heap : Heap_State := (Index => Memory_Type_Index,
                                Usage => Buffer.Usage,
                                Size  => Int_64_Unsigned_C ((if Buffer.Usage = GPU_Usage then Max_GPU_Memory.Get
                                                             else Max_Visible_Memory.Get) * MB), others => <>);
          Allocate_Info : aliased VkMemoryAllocateInfo := (allocationSize  => Heap.Size,
                                                           memoryTypeIndex => Memory_Type_Index, others => <>);
          begin
          
            -- Make a new heap
            Heap.First_Chunk := new Allocation_State'(Size => Heap.Size, others => <>);    
            vkAssert (vkAllocateMemory (Device, Allocate_Info'Unchecked_Access, null, Heap.Device_Memory'Unchecked_Access));
            if Heap.Usage /= GPU_Usage then
              vkAssert (vkMapMemory (Device, Heap.Device_Memory, 0, Memory_Requirements.size, 0, Heap.Data'Unchecked_Access));
            end if;
            Heaps.Append (Heap);
            
            -- Allocate from its first chunk
            Offset := 0;
            Allocate_From_Chunk (Heap.First_Chunk, Heaps.Last);
          end;
        end;
        
      -- Clear all unused heaps and unreferenced images
      procedure Finalize_Buffers (Force_Total_Finalization : Bool := False) is
      
        -- Cursors for iterating through and removing elements from the Buffered_Images map and the Heaps vector
        I : Hashed_Buffer.Cursor := Buffered_Images.First;
        
        -- Chunk pointers for dealing with each heap's chunk list
        Current, Previous, Next : Ptr_Allocation_State := null;
        
        -- ???
        Heap : Heap_State;
        
        -- Start of Finalize_Buffers
        begin            
          -- !!! THE BELOW LOOP IS GARBAGE !!!
        
          -- Look for unreferenced images and add them to the garbage
--            while Has_Element (I) loop declare Buffered_Image : Buffer_State := Element (I); begin
--              if Force_Total_Finalization or Buffered_Image.References = 0 then
--                vkDestroyImage     (Device, Buffered_Image.Data,    null);
--                vkDestroyImageView (Device, Buffered_Image.View,    null);
--                vkDestroySampler   (Device, Buffered_Image.Sampler, null);
--                Buffered_Garbage.Append (Buffered_Image);
--                Buffered_Images.Delete (I);
--              end if;
--              Hashed_Buffer.Unsafe.Next (I);
--            end; end loop;
          
          -- Take out the trash...
          for Piece of Buffered_Garbage loop
            Heap := Element (Piece.Heap);
            
            -- Find the chunk corresponding to the Memory
            Current := Heap.First_Chunk;
            while Current /= null loop
              if Current.ID = Piece.ID then
                if Piece.Kind /= Image_Buffer then vkDestroyBuffer (Device, Piece.Data, null); end if;
              
                -- Mark it as available
                Current.Kind := Free_Allocation;
    
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
                
                -- Modify Heap to handle new size
                Heap.Allocated := Heap.Allocated - Piece.Size;
                Heaps.Replace_Element (Piece.Heap, Heap);
                exit;
              end if;
              Current := Current.Next;
            end loop;
          end loop;
          Buffered_Garbage.Clear;
          
          -- Kill heaps with nothing left or if we are killing all the heaps during shutdown
          for J in reverse Heaps.First_Index..Heaps.Last_Index loop
            if Force_Total_Finalization or Heaps.Element (J).Allocated = 0 then
            
              -- Clear the GPU allocation
              if Heaps.Element (J).Usage /= GPU_Usage then vkUnmapMemory (Device, Heaps.Element (J).Device_Memory); end if;
              vkFreeMemory (Device, Heaps.Element (J).Device_Memory, null);        
              
              -- Free its chunks
              Current := Heaps.Element (J).First_Chunk;
              while Current /= null loop
                Next := Current.Next;
                Free (Current);
                Current := Next;
              end loop;
              Heaps.Delete (J);
            end if;
          end loop;
        end;    
      end;
    
  -------------
  -- Staging --
  -------------
  
  -- ???
  protected body Safe_Staging is
        
      -- ???
      procedure Initialize is
        Alignment_Mod,
        Alignment_Size               :         Int_64_Unsigned_C           := 0;
        Memory_Requirements          : aliased VkMemoryRequirements        := (others => <>);
        Memory_Allocate_Info         : aliased VkMemoryAllocateInfo        := (others => <>);
        Buffer_Info                  : aliased VkBufferCreateInfo          := (others => <>);
        Command_Buffer_Allocate_Info : aliased VkCommandBufferAllocateInfo := (others => <>);
        Command_Buffer_Begin_Info    : aliased VkCommandBufferBeginInfo    := (others => <>);
        Fence_Info                   : aliased VkFenceCreateInfo           := (flags => VK_FENCE_CREATE_SIGNALED_BIT, others => <>);
        Command_Pool_Info            : aliased VkCommandPoolCreateInfo     := (flags => VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
                                                                               queueFamilyIndex => Queue_Family, others => <>);
        begin       
          
          -- Fence
          vkAssert (vkCreateFence (Device, Fence_Info'Unchecked_Access, null, Staging_Fence'Access)); 
    
          -- Command pool
          vkAssert (vkCreateCommandPool (Device, Command_Pool_Info'Unchecked_Access, null, Staging_Command_Pool'Unchecked_Access));
          Command_Buffer_Allocate_Info := (commandPool => Staging_Command_Pool, commandBufferCount => 1, others => <>);

          -- Command buffer
          vkAssert (vkAllocateCommandBuffers (Device, Command_Buffer_Allocate_Info'Unchecked_Access, Staging_Commands'Access));
          
          -- Buffer
          Buffer_Info := (size => Int_64_Unsigned (Max_Upload_Buffer.Get * MB), usage => VK_BUFFER_USAGE_TRANSFER_SRC_BIT, others => <>); 
          vkAssert (vkCreateBuffer (Device, Buffer_Info'Unchecked_Access, null, Staging_Buffer'Access));
          
          -- Memory for buffer
          vkGetBufferMemoryRequirements (Device, Staging_Buffer, Memory_Requirements'Unchecked_Access);
          Alignment_Mod        := Memory_Requirements.size mod Memory_Requirements.alignment;
          Alignment_Size       := Memory_Requirements.size + (if Alignment_Mod /= 0 then Memory_Requirements.alignment - Alignment_Mod else 0);
          Memory_Allocate_Info := (allocationSize  => Alignment_Size,
                                   memoryTypeIndex => Find_Memory_Type_Index (Memory_Requirements.memoryTypeBits, CPU_To_GPU_Usage), others => <>);
          vkAssert (vkAllocateMemory   (Device, Memory_Allocate_Info'Unchecked_Access, null, Staging_Device_Memory'Access));
          vkAssert (vkBindBufferMemory (Device, Staging_Buffer, Staging_Device_Memory, 0));
          vkAssert (vkMapMemory        (Device, Staging_Device_Memory, 0, Alignment_Size, 0, Staging_Data'Access));
        end;
          
      -- ???
      procedure Finalize is
        begin
          vkAssert (vkWaitForFences (Device, 1, Staging_Fence'Unchecked_Access, VK_TRUE, Int_64_Unsigned_C'Last));
          vkUnmapMemory             (Device, Staging_Device_Memory);
          vkFreeCommandBuffers      (Device, Staging_Command_Pool, 1, Staging_Commands'Unchecked_Access);
          vkDestroyCommandPool      (Device, Staging_Command_Pool,  null); 
          vkFreeMemory              (Device, Staging_Device_Memory, null);
          vkDestroyFence            (Device, Staging_Fence,         null);
          vkDestroyBuffer           (Device, Staging_Buffer,        null);
        end;
        
      -- ???
      procedure Set_Buffer (Buffer : in out Buffer_State; Data : Ptr; Data_Size : Int_Ptr) is
        begin
          case Buffer.Usage is
            when GPU_Usage =>
              Assert (Data_Size <= Max_Upload_Buffer.Get * MB);
              
              -- Copy it in
              declare
              Command_Buffer_Info : aliased VkCommandBufferBeginInfo := (others => <>);
              Memory_Range        : aliased VkMappedMemoryRange      := (memory => Int_64_Unsigned_C (To_Int_Ptr (Staging_Device_Memory)),
                                                                         size   => VK_WHOLE_SIZE, others => <>);
              Submit_Info         : aliased VkSubmitInfo             := (commandBufferCount => 1,
                                                                         pCommandBuffers    => Staging_Commands'Unchecked_Access, others => <>);
              
              -- Objects for raw data manipulation
              Source, Destination : Array_Byte (1..Int (Data_Size) / Byte'Size);
              for Source'Address      use Data;
              for Destination'Address use To_Ptr (To_Int_Ptr (Staging_Data) + Staging_Offset);
              Buffer_Copy : aliased VkBufferCopy := (srcOffset => Int_64_Unsigned_C (Staging_Offset),
                                                     dstOffset => Int_64_Unsigned_C (To_Int_Ptr (Staging_Data) + Staging_Offset),
                                                     size      => Int_64_Unsigned_C (Data_Size), others => <>);
              begin
              
                -- Reset the command buffer only after the staging buffer is avaliable
                vkAssert (vkWaitForFences      (Device, 1, Staging_Fence'Unchecked_Access, VK_TRUE, Int_64_Unsigned_C'Last));
                vkAssert (vkResetFences        (Device, 1, Staging_Fence'Unchecked_Access));
                vkAssert (vkBeginCommandBuffer (Staging_Commands, Command_Buffer_Info'Unchecked_Access));
                
                Destination := Source;
                vkCmdCopyBuffer (Staging_Commands, Staging_Buffer, Buffer.Data, 1, Buffer_Copy'Unchecked_Access);
                
                -- Move over the staging offset to reflect the copied buffer data
                Staging_Offset := Staging_Offset + Int_Ptr (Data_Size);
                
                -- Submit the queue and flush to the GPU
                vkAssert (vkFlushMappedMemoryRanges (Device, 1, Memory_Range'Unchecked_Access));
                vkAssert (vkEndCommandBuffer (Staging_Commands));
                vkAssert (vkQueueSubmit (Queue, 1, Submit_Info'Unchecked_Access, Staging_Fence));   
                Staging_Offset := 0;
              end;
            when others => null; --Buffer.Memory.Data := Staging_Offset + Offset + data (size);
          end case;
        end;
      end;
      
  ---------------
  -- Buffering --
  ---------------

  -- Create a lookup string for Buffered_Images which includes material settings in addition the image path
  function Get_Material_Info_Hash (Item : Material_State) return Str_Unbound is
    (NULL_STR_UNBOUND); -- (U (Model.Filter_Kind'Pos (Item.Filter)'Wide_Image & Clamp_Kind'Pos (Item.Clamp)'Wide_Image));

  -- Manage counters for Buffered_Material references
  procedure Add_Material_Reference    (Path : Str) is null;
  procedure Remove_Material_Reference (Path : Str) is null;
    
  -- Manage counters for Buffered_Image references
  procedure Add_Image_Reference (Path : Str_Unbound) is Buffered_Image : Buffer_State := Buffered_Images.Get (Path);
    begin Buffered_Image.References := Buffered_Image.References + 1; Buffered_Images.Replace (Path, Buffered_Image); end;
  procedure Remove_Image_Reference (Path : Str_Unbound) is Buffered_Image : Buffer_State := Buffered_Images.Get (Path);
    begin Buffered_Image.References := Buffered_Image.References - 1; Buffered_Images.Replace (Path, Buffered_Image); end;
  
  -- Add a 3D mesh to the GPU memory
  procedure Buffer_Mesh (Path : Str) is
    Mesh            : Ptr_Mesh_State := Meshes.Get (Path);
    Buffered_Mesh   : Vector_Buffer_Surface.Unsafe.Vector;
    Current_Surface : Buffer_Surface_State (Mesh.Skeleton /= null);
    begin   
      for Geometry of Mesh.Geometries loop
      
        -- When we have an animated mesh we have to add the weights
        if Mesh.Skeleton /= null then
        
          -- Weight buffer
          null;
        --  Initialize_Buffer (Buffer     => Current_Surface.Weights,
        --                     Usage_Bits => VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
        --                     Data       => Geometry.Bone_Weights (1)'Address,
        --                     Data_Size  => (Data'Length * Weight_State'Object_Size) / Byte'Size,
        --                     Count      => Geometry.Animated_Surfaces.Element (I).Weights.Length);
        end if;  
        
  
        -- Index buffer    
        Initialize_Buffer (Buffer     => Current_Surface.Indicies,
                           Usage_Bits => VK_BUFFER_USAGE_INDEX_BUFFER_BIT,
                           Data       => Geometry.Indicies (1, 1)'Address,
                           Data_Size  => Geometry.Indicies'Size / Byte'Size,
                           Count      => Geometry.Index_Count);
      
        -- Vertex buffer
        Initialize_Buffer (Buffer     => Current_Surface.Vertices,
                           Usage_Bits => VK_BUFFER_USAGE_VERTEX_BUFFER_BIT,
                           Data       => Geometry.Verticies (1)'Address,
                           Data_Size  => Geometry.Verticies'Size / Byte'Size,
                           Count      => Geometry.Vertex_Count);
        
        -- Add the result to the global buffered meshes
        Buffered_Mesh.Append (Current_Surface);
      end loop;
      Buffered_Meshes.Insert (Path, Buffered_Mesh);
    end;
    
  -- ???
  procedure Unbuffer_Mesh (Path : Str) is
    Buffered_Mesh : Vector_Buffer_Surface.Unsafe.Vector := Buffered_Meshes.Get (Path);
    begin
      for Surface of Buffered_Mesh loop
        Remove_Material_Reference (S (Surface.Material));        
        Safe_Allocation.Throw_Away (Surface.Vertices);
        Safe_Allocation.Throw_Away (Surface.Indicies);
        --if Surface.Is_Animated then Safe_Allocation.Throw_Away (Surface.Weights); end if;
      end loop;    
      Buffered_Meshes.Delete (Path);
    end;

  -- Load a material's textures and mark it 
  procedure Buffer_Material (Path : Str) is
    begin
      null;
--        -- Sanity check
--        if Buffered_Materials.Has (Path) then return; end if;
--  
--        -- Buffer the material and load all images associated with the material to the GPU
--        declare
--        Material :          Material_State    := Materials.Get (Path);
--        Hash     :          Str_Unbound       := Get_Material_Info_Hash (Material);
--        PATHS    : constant Array_Str_Unbound :=
--          (Material.Irradiance, Material.Specular, Material.Normal, Material.Displacement, Material.Metallic, Material.Roughness);
--        begin
--          for Path of PATHS loop
--          
--            -- Increment the count if it is loaded already
--            if Buffered_Images.Has (Path & Hash) then Add_Image_Reference (Path & Hash);
--            
--            -- Otherwise, buffer the image
--            else
--              --Load_Image (S (Path));
--              declare
--              Image               :         Image_State           := (others => <>);--Images;
--              Buffered_Image      :         Buffer_State          := (Size => Image.Data'Size / Byte'Size, others => <>);
--              Image_View_Info     : aliased VkImageViewCreateInfo := (viewType         => VK_IMAGE_VIEW_TYPE_2D,
--                                                                      format           => Image.Internal_Format,
--                                                                      components       => (VK_COMPONENT_SWIZZLE_IDENTITY,
--                                                                                           VK_COMPONENT_SWIZZLE_IDENTITY,
--                                                                                           VK_COMPONENT_SWIZZLE_IDENTITY,
--                                                                                           VK_COMPONENT_SWIZZLE_IDENTITY),
--                                                                      subresourceRange => (aspectMask => VK_IMAGE_ASPECT_COLOR_BIT,
--                                                                                           levelCount => 1,
--                                                                                           layerCount => 1, others => <>), others => <>);
--              Sampler_Info : aliased VkSamplerCreateInfo := (maxAnisotropy    => 1.0,
--                                                             anisotropyEnable => VK_FALSE,
--                                                             compareEnable    => VK_FALSE,
--                                                             compareOp        => VK_COMPARE_OP_NEVER, others => <>);
--              Image_Info : aliased VkImageCreateInfo := (initialLayout => VK_IMAGE_LAYOUT_UNDEFINED,
--                                                         sharingMode   => VK_SHARING_MODE_EXCLUSIVE,
--                                                         tiling        => VK_IMAGE_TILING_OPTIMAL,
--                                                         imageType     => VK_IMAGE_TYPE_2D,
--                                                         format        => Image.Internal_Format,
--                                                         mipLevels     => Int_Unsigned_C (Image.Mipmaps),
--                                                         samples       => VK_SAMPLE_COUNT_1_BIT, -- Int_Unsigned_C (Samples.Get'Val)
--                                                         extent        => (Int_Unsigned_C (Image.Width), Int_Unsigned_C (Image.Height), 1),
--                                                         arrayLayers   => (if Image.Is_Cube_Map then 6 else 1),
--                                                         flags         => (if Image.Is_Cube_Map then VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT else 0),
--                                                         usage         => VK_IMAGE_USAGE_TRANSFER_DST_BIT, others => <>);
--              begin
--    
--                -- Sampler
--                case Material.Filter is
--                  when Linear_Filter =>
--                    Sampler_Info.minFilter  := VK_FILTER_LINEAR;
--                    Sampler_Info.magFilter  := VK_FILTER_LINEAR;
--                    Sampler_Info.mipmapMode := VK_SAMPLER_MIPMAP_MODE_LINEAR;
--                  when Nearest_Filter =>
--                    Sampler_Info.minFilter  := VK_FILTER_NEAREST;
--                    Sampler_Info.magFilter  := VK_FILTER_NEAREST;
--                    Sampler_Info.mipmapMode := VK_SAMPLER_MIPMAP_MODE_NEAREST;
--                end case;
--                case Material.Clamp is
--                  when No_Clamp =>
--                    Sampler_Info.addressModeU := VK_SAMPLER_ADDRESS_MODE_REPEAT;
--                    Sampler_Info.addressModeV := VK_SAMPLER_ADDRESS_MODE_REPEAT;
--                    Sampler_Info.addressModeW := VK_SAMPLER_ADDRESS_MODE_REPEAT;
--                  when Normal_Clamp =>
--                    Sampler_Info.addressModeU := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
--                    Sampler_Info.addressModeV := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
--                    Sampler_Info.addressModeW := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
--                  when Zero_Alpha_Clamp =>
--                    Sampler_Info.borderColor  := VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK;
--                    Sampler_Info.addressModeU := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
--                    Sampler_Info.addressModeV := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
--                    Sampler_Info.addressModeW := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
--                  when Zero_Clamp =>
--                    Sampler_Info.borderColor  := VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK;
--                    Sampler_Info.addressModeU := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
--                    Sampler_Info.addressModeV := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
--                    Sampler_Info.addressModeW := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
--                end case;
--                vkAssert (vkCreateSampler (Device, Sampler_Info'Unchecked_Access, null, Buffered_Image.Sampler'Address)); 
--                
--                -- Buffer the image
--                vkAssert (vkCreateImage (Device, Image_Info'Unchecked_Access, null, Buffered_Image.Data'Unchecked_Access));
--                Safe_Allocation.Allocate_Buffer (Buffered_Image);
--                vkAssert (vkBindImageMemory (Device, Buffered_Image.Data, Buffered_Image.Device_Memory, Buffered_Image.Offset));
--                Safe_Staging.Set_Buffer (Buffered_Image, Image.Data'Address, Image.Data'Size / Byte'Size);
--                Image_View_Info.image := Buffered_Image.Data;
--                vkAssert (vkCreateImageView (Device, Image_View_Info'Unchecked_Access, null, Buffered_Image.View'Address));
--    
--                -- Register the buffered image
--                Buffered_Images.Insert (Path & Hash, Buffered_Image);
--              end;
--            end if;
--          end loop;
--  
--          -- Register the buffered material
--          Buffered_Materials.Insert (U (Path), Material); -- Set it to junk since we only care if it exists
--        end;
    end;  
    
  -- Remove a material from the "buffered" map and decrement all of the references to buffered images so that they may be freed
  procedure Unbuffer_Material (Path : Str) is -- Depends on the material's domain and must be in sync with the Material_State type
    begin
      null;
--      Material : Material_State := Materials.Get (Path);
--      Hash     : Str_Unbound    := Get_Material_Info_Hash (Material);
--      begin
--      
--        -- Make sure we can't unbuffer a material until all buffered surfaces using it get unbuffered first
--        --if Material_Reference.Length > 0 and then Is_Debugging then Line_Warn ("Unbuffering referenced material?"); end if; 
--      
--        case Material.Domain is
--          when others =>
--            if Material.Base_Color /= NULL_STR_UNBOUND then Remove_Image_Reference (Material.Base_Color & Hash); end if;
--            if Material.Irradiance /= NULL_STR_UNBOUND then Remove_Image_Reference (Material.Irradiance & Hash); end if;
--            if Material.Specular   /= NULL_STR_UNBOUND then Remove_Image_Reference (Material.Specular   & Hash); end if;
--            if Material.Normal     /= NULL_STR_UNBOUND then Remove_Image_Reference (Material.Normal     & Hash); end if;
--            if Material.Metallic   /= NULL_STR_UNBOUND then Remove_Image_Reference (Material.Metallic   & Hash); end if;
--            if Material.Roughness  /= NULL_STR_UNBOUND then Remove_Image_Reference (Material.Roughness  & Hash); end if;
--        end case;
--        Buffered_Materials.Delete (U (Path));
    end;      
    
  procedure Free_Unreferenced_Materials is
    begin
      null;
    end;
    
  -----------------
  -- Framebuffer --
  -----------------
  
  -- ???
  procedure Restart_Framebuffer is
    begin
    
      -- Reset the surface size when it is not 0 since it could be a minimize event
      vkAssert (vkGetPhysicalDeviceSurfaceCapabilitiesKHR (Physical_Device, Surface, Surface_Details'Unchecked_Access));
      if Surface_Details.minImageExtent.width /= 0 and Surface_Details.minImageExtent.height /= 0 then
        Surface_Extent := Surface_Details.minImageExtent;
      
        -- Cleanup, everybody
        Finalize_Framebuffer;
        Safe_Allocation.Finalize_Buffers;
      
        -- Start anew
        Initialize_Framebuffer;
      end if;
    end;
    
  -- ???
  procedure Finalize_Framebuffer is
    begin
      Backend_Task.Finalize;
      if Frontend_Task.Running then Frontend_Task.Finalize; end if;
    
      -- Framebuffer
      for Frame of Framebuffer.all loop
        vkAssert (vkWaitForFences (Device, 1, Frame.Fence'Unchecked_Access, VK_TRUE, Int_64_Unsigned_C'Last));
        vkDestroySemaphore   (Device, Frame.Acquire_Status,   null);
        vkDestroySemaphore   (Device, Frame.Render_Status,    null);  
        vkDestroyImageView   (Device, Frame.Image_View,       null);
        vkDestroyFramebuffer (Device, Frame.Swapchain_Buffer, null);
        vkDestroyFence       (Device, Frame.Fence,            null);
        vkFreeCommandBuffers (Device, Command_Pool, 1, Frame.Commands'Unchecked_Access);
      end loop;
      Vector_Framebuffer.Free (Framebuffer);  
      
      -- Depth image
      vkDestroySampler   (Device, Depth_Image.Sampler, null);
      vkDestroyImage     (Device, Depth_Image.Data,    null);
      vkDestroyImageView (Device, Depth_Image.View,    null);
      Safe_Allocation.Throw_Away (Depth_Image);
    
      -- Render pass
      vkDestroyRenderPass (Device, Render_Pass, null);
      
      -- Swapchain
      vkDestroySwapchainKHR (Device, Swapchain, null);
      Swapchain := NULL_PTR;
    end; 
    
  -- ???
  procedure Initialize_Framebuffer is
   
    -- Syncronization
    Semaphore_Info : aliased VkSemaphoreCreateInfo := (others => <>);
    Fence_Info     : aliased VkFenceCreateInfo     := (flags => VK_FENCE_CREATE_SIGNALED_BIT, others => <>);
    
    -- Swap Chain
    Indexes        : aliased Array_Int_Unsigned_C     := (1 => Queue_Family);
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
   
    -- Framebuffer
    Frame_Buffer_Info            : aliased VkFramebufferCreateInfo     := (others => <>);  
    Command_Buffer_Allocate_Info : aliased VkCommandBufferAllocateInfo := (commandPool => Command_Pool, commandBufferCount => 1, others => <>);
                    
    -- Viewport 
    Viewport : aliased VkViewport := (x        => 0.0,
                                      y        => 0.0,
                                      minDepth => 0.0,
                                      maxDepth => 1.0, -- 10.0,
                                      width    => Real_C (Swapchain_Info.imageExtent.width),
                                      height   => Real_C (Swapchain_Info.imageExtent.height), others => <>);
    Scissor : aliased VkRect2D := (offset => (0, 0), extent => Swapchain_Info.imageExtent, others => <>);
    Viewport_State_Info : aliased VkPipelineViewportStateCreateInfo := (viewportCount => 1,
                                                                        pViewports    => Viewport'Unchecked_Access,
                                                                        scissorCount  => 1,
                                                                        pScissors     => Scissor'Unchecked_Access, others => <>);
  
    -- Depth image
    Depth_Image_View_Info : aliased VkImageViewCreateInfo := (format           => Depth_Format.format,
                                                              viewType         => VK_IMAGE_VIEW_TYPE_2D,
                                                              components       => (VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                   VK_COMPONENT_SWIZZLE_IDENTITY),
                                                              subresourceRange => (levelCount => 1,
                                                                                   layerCount => 1, 
                                                                                   aspectMask => VK_IMAGE_ASPECT_DEPTH_BIT or
                                                                                                 VK_IMAGE_ASPECT_STENCIL_BIT, others => <>), others => <>);
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
          
    -- Subpass
    Depth_Reference       : aliased VkAttachmentReference      := (attachment => 1, layout => VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, others => <>);
    Color_Reference       : aliased VkAttachmentReference      := (attachment => 0, layout => VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, others => <>);
    Subpasses_Description : aliased Array_VkSubpassDescription := (1 => (pipelineBindPoint       => VK_PIPELINE_BIND_POINT_GRAPHICS,
                                                                         colorAttachmentCount    => 1,
                                                                         pColorAttachments       => Color_Reference'Unchecked_Access,
                                                                         pDepthStencilAttachment => null, others => <>)); -- Depth_Reference'Unchecked_Access, others  => <>));
                                            
    -- Render Pass
    Attachments : aliased Array_VkAttachmentDescription := (1 => (format         => Swapchain_Format.format,
                                                             samples        => VK_SAMPLE_COUNT_1_BIT,
                                                             loadOp         => VK_ATTACHMENT_LOAD_OP_CLEAR, -- VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                             storeOp        => VK_ATTACHMENT_STORE_OP_STORE,
                                                             stencilLoadOp  => VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                             stencilStoreOp => VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                             initialLayout  => VK_IMAGE_LAYOUT_UNDEFINED,
                                                             finalLayout    => VK_IMAGE_LAYOUT_PRESENT_SRC_KHR, others => <>)); --VK_IMAGE_LAYOUT_GENERAL, others => <>)); --,
                                                            --(format         => Depth_Format.format,
                                                            -- samples        => VK_SAMPLE_COUNT_1_BIT,
                                                            -- loadOp         => VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                            -- storeOp        => VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                            -- stencilLoadOp  => VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                            -- stencilStoreOp => VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                            -- initialLayout  => VK_IMAGE_LAYOUT_UNDEFINED,
                                                            -- finalLayout    => VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, others => <>));
                                                           -- (format         => Swapchain_Format.format,
                                                           --  samples        => VK_SAMPLE_COUNT_1_BIT,
                                                           --  loadOp         => VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                           --  storeOp        => VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                           --  stencilLoadOp  => VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                           --  stencilStoreOp => VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                           --  initialLayout  => VK_IMAGE_LAYOUT_UNDEFINED,
                                                           --  finalLayout    => VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, others => <>));
    Attachment_Data  : aliased Array_Ptr (Attachments'Range) := (others => NULL_PTR);  
    Render_Pass_Info : aliased VkRenderPassCreateInfo := (attachmentCount => Attachments'Length,
                                                          pAttachments    => Attachments (1)'Unchecked_Access,
                                                          subpassCount    => Subpasses_Description'Length,
                                                          pSubpasses      => Subpasses_Description (1)'Unchecked_Access, others => <>);
    begin
    
      -- Make sure we are safe
      vkAssert (vkDeviceWaitIdle (Device));
  
      -- Swapchain
      vkAssert (vkCreateSwapchainKHR (Device, Swapchain_Info'Unchecked_Access, null, Swapchain'Unchecked_Access));
      vkAssert (vkGetSwapchainImagesKHR (device               => Device,
                                         swapchain            => Swapchain,
                                         pSwapchainImageCount => Swapchain_Info.minImageCount'Unchecked_Access,
                                         pSwapchainImages     => Images (1)'Unchecked_Access));
      Framebuffer := new Vector_Framebuffer.Unsafe_Array (Images'Range);
      for I in Images'Range loop
        Framebuffer (I).Image := Images (I);
        declare
        Image_View_Info : aliased VkImageViewCreateInfo := (image            => Framebuffer (I).Image,
                                                            viewType         => VK_IMAGE_VIEW_TYPE_2D,
                                                            format           => Swapchain_Format.format,
                                                            components       => (VK_COMPONENT_SWIZZLE_IDENTITY, VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                 VK_COMPONENT_SWIZZLE_IDENTITY, VK_COMPONENT_SWIZZLE_IDENTITY),
                                                            subresourceRange => (aspectMask => VK_IMAGE_ASPECT_COLOR_BIT,
                                                                                 levelCount => 1,
                                                                                 layerCount => 1, others => <>), others => <>);
        begin
          vkAssert (vkCreateImageView (Device, Image_View_Info'Unchecked_Access, null, Framebuffer (I).Image_View'Address));
        end;
      end loop;
  
      -- Render pass
      vkAssert (vkCreateRenderPass (Device, Render_Pass_Info'Unchecked_Access, null, Render_Pass'Unchecked_Access));
  
      -- Depth image
      vkAssert (vkCreateImage (Device, Depth_Image_Info'Unchecked_Access, null, Depth_Image.Data'Unchecked_Access));
      Safe_Allocation.Allocate_Buffer (Depth_Image);
      vkAssert (vkBindImageMemory (Device, Depth_Image.Data, Depth_Image.Device_Memory, Depth_Image.Offset));
      Depth_Image_View_Info.image := Depth_Image.Data;
      vkAssert (vkCreateImageView  (Device, Depth_Image_View_Info'Unchecked_Access, null, Depth_Image.View'Address));
  
      -- Framebuffer
      --Attachment_Data (2) := Depth_Image.View;
      for Frame of Framebuffer.all loop
        Attachment_Data (1) := Frame.Image_View;
        Frame_Buffer_Info := (renderPass      => Render_Pass,
                              attachmentCount => Attachment_Data'Length,
                              pAttachments    => Attachment_Data (1)'Unchecked_Access,
                              width           => Swapchain_Info.imageExtent.width,
                              height          => Swapchain_Info.imageExtent.height,
                              layers          => 1, others => <>);
        vkAssert (vkCreateFramebuffer      (Device, Frame_Buffer_Info'Unchecked_Access, null, Frame.Swapchain_Buffer'Unchecked_Access));
        vkAssert (vkAllocateCommandBuffers (Device, Command_Buffer_Allocate_Info'Unchecked_Access, Frame.Commands'Unchecked_Access));
        vkAssert (vkCreateSemaphore        (Device, Semaphore_Info'Unchecked_Access, null, Frame.Acquire_Status'Unchecked_Access));
        vkAssert (vkCreateSemaphore        (Device, Semaphore_Info'Unchecked_Access, null, Frame.Render_Status'Unchecked_Access));
        vkAssert (vkCreateFence            (Device, Fence_Info'Unchecked_Access,     null, Frame.Fence'Unchecked_Access));  
      end loop;
      
      -- Reset the indexes reate the inital view
      View.Set (case Rendering.Get is when Doom3_Rendering    => Doom3.Build_View,
                                      when FGED2_Rendering    => FGED2.Build_View,
                                      when Raytrace_Rendering => Raytrace.Build_View);
      Backend_Task.Initialize;
      Frontend_Task.Initialize;
      Framebuffer_Status.Occupied (True);
    end;
    
  ----------------------
  -- Finalize_Drawing --
  ----------------------
  
  -- Kill anything that is not controlled
  procedure Finalize_Drawing is
    begin
    
      -- Shaders
      -- ???
      
      -- Models
      for Key of Buffered_Meshes.Keys loop Unbuffer_Mesh (S (Key)); end loop;
      Safe_Allocation.Throw_Away (Joint_Buffer);

      -- Uniforms
      for Uniform of Buffered_Uniforms loop Safe_Allocation.Throw_Away (Uniform); end loop;

      -- Staging
      Safe_Staging.Finalize;

      -- Framebuffer
      Finalize_Framebuffer;
      
      -- Pools
      vkDestroyDescriptorPool (Device, Descriptor_Pool, null);
      vkDestroyCommandPool    (Device, Command_Pool,    null);
      
      -- Memory heaps and chunks
      Safe_Allocation.Finalize_Buffers (Force_Total_Finalization => True);
      
      -- Device
      vkDestroyDevice     (Device, null);
      vkDestroySurfaceKHR (Instance, Surface, null);
      vkDestroyInstance   (Instance, null);
    end;  
    
  ----------
  -- Draw --
  ----------
   
  -- Main drawing routine
  procedure Draw (Data : Bottom_Level_State; Commands : in out Ptr; Surface_Sort : Surface_Sort_Array) is
    Size_Junk : aliased Int_64_Unsigned_C := 0;
    Mesh                : Vector_Buffer_Surface.Unsafe.Vector := Buffered_Meshes.Get (Data.Mesh);
    Descriptor_Set      : aliased Ptr                         := NULL_PTR;
    Current_Pipeline    : aliased Ptr                         := NULL_PTR; 
    Set_Allocation_Info : aliased VkDescriptorSetAllocateInfo := (descriptorSetCount => 1,
                                                                  descriptorPool     => Descriptor_Pool, 
                                                                  pSetLayouts        => Pipeline.Shader.Descriptor_Set_Layout'Unchecked_Access, others => <>);
    begin 
    
      -- Bind joints
      if Data.Is_Animated then
        null;
--          Debug_Assert (Data.Pose.Node_Count > 0);
--          declare
--          Joints : Vector_Joint.Unsafe_Array (1..Int (Data.Pose.Node_Count)) := (others => <>);
--          begin
--            Writes.Append ((Is_Image    => False,
--                            Buffer_Info => (buffer => Joint_Buffer.Data,
--                                            offset => Joint_Buffer.Offset,
--                                            rang   => Joint_Buffer.Size),
--                            Set         => (dstBinding      => JOINT_BUFFER_BINDING,
--                                            descriptorCount => 1,
--                                            descriptorType  => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, others => <>)));
--            Safe_Staging.Set_Buffer (Joint_Buffer, Joints (1)'Address, Joint_State'Object_Size * Joints'Length / Byte'Size);
--          end;
      end if;
      
      -- Draw surfaces of the mesh that match the desired visibility
      for Surface of Mesh loop
      
        -- Fetch the material and create a hash we can use to stage buffered image components of the materials
        declare
        Material : Material_State;-- := Materials.Get (S (Surface.Material));--(Data.Materials_Map.Element (Surface.Material)));
        Hash     : Str_Unbound;--    := Get_Material_Info_Hash (Material);

        -- Flag a meterial texture and setup a sampler
        procedure Stage_Image (Path        : Str_Unbound;
                               Set_Sampler : access procedure (Path : Str);
                               Set_Flag    : access procedure (Val  : Bool)) is
          begin
            if Path = NULL_STR_UNBOUND then Set_Flag (False); return; end if;
            Set_Flag (True);
            Set_Sampler (S (Path & Hash));
          end;
          
        -- Start of Draw's main loop
        begin
          --Debug_Assert (Data.Is_Animated = Surface.Is_Animated);
          
          -- Optimize
          if True then -- Surface_Sort (Material.Sort) then
          
            -- Optimize out searching through buffered pipelines if our previous one is a match
            if Previous_Pipeline.Val = Pipeline then
              Current_Pipeline := Previous_Pipeline.Data;        
goto Have_Pipeline; -- Gotos to keep the nesting sane
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
            
            -- Static viewport junk
            Viewport : aliased VkViewport := 
              (x        => 0.0,
               y        => 0.0,
               width    => Real_C (Surface_Details.minImageExtent.width),
               height   => Real_C (Surface_Details.minImageExtent.height),
               minDepth => 0.0,
               maxDepth => 1.0);
            Scissor : aliased VkRect2D := ((0, 0), (Surface_Details.minImageExtent.width, Surface_Details.minImageExtent.height));
            Viewport_Info       : aliased VkPipelineViewportStateCreateInfo      := (viewportCount => 1,
                                                                                     scissorCount  => 1,
                                                                                     pViewports    => Viewport'Unchecked_Access,
                                                                                     pScissors     => Scissor'Unchecked_Access,
                                                                                     others => <>);
            Input_Assembly_Info : aliased VkPipelineInputAssemblyStateCreateInfo := (topology => VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, others => <>);                                                 
            Multisample_Info    : aliased VkPipelineMultisampleStateCreateInfo   := (sampleShadingEnable  => VK_FALSE,
                                                                                     rasterizationSamples => VK_SAMPLE_COUNT_1_BIT, --rasterizationSamples => Image_Properties.sampleCounts,
                                                                                     minSampleShading     => 0.0, -- 1.0,
                                                                                     others => <>);
                                                                                       --Int_Unsigned_C (Sampling_Kind'Pos (Sampling.Get)), others => <>);
                                                                                
            -- Shader
            Vertex_Input_Attributes : aliased Vector_VkVertexInputAttributeDescription.Unsafe_Array :=
              Vector_VkVertexInputAttributeDescription.To_Unsafe_Array (Pipeline.Shader.Vertex_Attributes);
            Vertex_Binding : aliased Array_VkVertexInputBindingDescription := (1 => Pipeline.Shader.Vertex_Binding);
            Vertex_Input_Info : aliased VkPipelineVertexInputStateCreateInfo :=
              (vertexBindingDescriptionCount   => Vertex_Binding'Length,
               pVertexBindingDescriptions      => Vertex_Binding (1)'Unchecked_Access,
               vertexAttributeDescriptionCount => Vertex_Input_Attributes'Length,
               pVertexAttributeDescriptions    => Vertex_Input_Attributes (1)'Unchecked_Access, others => <>);   
            Shader_Stages_Info : aliased Array_VkPipelineShaderStageCreateInfo :=
              Vector_VkPipelineShaderStageCreateInfo.To_Unsafe_Array (Pipeline.Shader.Stages_Info);
                    
            -- Rasterization
            Rasterizer_Info : aliased VkPipelineRasterizationStateCreateInfo := 
              (polygonMode             => VK_POLYGON_MODE_FILL,
               rasterizerDiscardEnable => VK_FALSE,
               depthClampEnable        => VK_FALSE,
               depthBiasEnable         => To_VkBool32 (Pipeline.Has_Polygon_Offset),
               lineWidth               => 1.0,
               frontFace               => Pipeline.Front_Face,
               cullMode                => VK_CULL_MODE_NONE, others => <>);
                                           --(case Pipeline.Cull_Mode is
                                           --  when VK_CULL_MODE_BACK_BIT  => (if Pipeline.Has_Mirror_View then VK_CULL_MODE_FRONT_BIT
                                           --                                  else VK_CULL_MODE_BACK_BIT),
                                           --  when VK_CULL_MODE_FRONT_BIT => (if Pipeline.Has_Mirror_View then VK_CULL_MODE_BACK_BIT
                                           --                                  else VK_CULL_MODE_FRONT_BIT),
                                           --  when others                 => Pipeline.Cull_Mode), others => <>);
                                                                                
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
                                                               else VK_DYNAMIC_STATE_DEPTH_BOUNDS), -- Jump through some hoops ???
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
                                                                     pDepthStencilState  => null, -- Depth_Stencil_Info'Unchecked_Access,
                                                                     pColorBlendState    => Color_Blend_Info'Unchecked_Access,
                                                                     renderPass          => Render_Pass,
                                                                     layout              => Pipeline.Shader.Pipeline_Layout,
                                                                     pDynamicState       => null, -- Dynamic_State_Info'Unchecked_Access,
                                                                     pViewportState      => Viewport_Info'Unchecked_Access, others => <>);
            Pipeline_Layout_Info : aliased VkPipelineLayoutCreateInfo := (setLayoutCount => 1,
                                                                          pSetLayouts    => Set_Allocation_Info.pSetLayouts, others => <>);   
        
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
            
-- Bind the current pipeline and remember the previous one to potentially avoid searching on the next draw
<<Have_Pipeline>>       
            Previous_Pipeline := (Pipeline, Current_Pipeline);    
            vkCmdBindPipeline (Commands, VK_PIPELINE_BIND_POINT_GRAPHICS, Current_Pipeline);         
            
            -- Stage material
--              if Buffered_Materials.Has (Surface.Material) then raise Program_Error with "Unbuffered material encountered"; end if;
--              case Material.Domain is -- Must match Neo.Data.Model.Material_State
--                when Surface_Domain =>
--                  Stage_Image (Material.Base_Color, Base_Color_Sampler.Set'Access, Enable_Base_Color.Set'Access);
--                  Stage_Image (Material.Irradiance, Irradiance_Sampler.Set'Access, Enable_Irradiance.Set'Access);
--                  Stage_Image (Material.Specular,   Specular_Sampler.Set'Access,   Enable_Specular.Set'Access);
--                  Stage_Image (Material.Normal,     Normal_Sampler.Set'Access,     Enable_Normal.Set'Access);
--                  Stage_Image (Material.Metallic,   Metallic_Sampler.Set'Access,   Enable_Metallic.Set'Access);
--                  Stage_Image (Material.Roughness,  Roughness_Sampler.Set'Access,  Enable_Roughness.Set'Access);
--                when others => null;
--              end case;
            
            -- Bind writes linked with their appropriate additional structure (e.g. VkDescriptorBufferInfo or VkDescriptorImageInfo)
--              vkAssert (vkAllocateDescriptorSets (Device, Set_Allocation_Info'Unchecked_Access, Descriptor_Set'Unchecked_Access));
--              if Writes.Length > 0 then
--                declare
--                Writes_Internal  : Vector_Write_Descriptor.Unsafe_Array               := Vector_Write_Descriptor.To_Unsafe_Array (Writes);
--                Resulting_Writes : Array_VkWriteDescriptorSet (Writes_Internal'Range) := (others => <>);
--                begin     
--                  for I in Writes_Internal'Range loop
--                    Resulting_Writes (I)        := Writes_Internal (I).Set;
--                    Resulting_Writes (I).dstSet := Descriptor_Set;
--                    if Writes_Internal (I).Is_Image then Resulting_Writes (I).pImageInfo := Writes_Internal (I).Image_Info'Address;
--                    else Resulting_Writes (I).pBufferInfo := Writes_Internal (I).Buffer_Info'Address; end if;
--                  end loop;
--                  vkUpdateDescriptorSets (Device, Resulting_Writes'Length, Resulting_Writes (1)'Unchecked_Access, 0, null);        
--                end;        
--              end if;
--              vkCmdBindDescriptorSets (commandBuffer      => Commands, 
--                                       pipelineBindPoint  => VK_PIPELINE_BIND_POINT_GRAPHICS, 
--                                       layout             => Pipeline.Shader.Pipeline_Layout,
--                                       firstSet           => 0,
--                                       descriptorSetCount => 1,
--                                       pDescriptorSets    => Descriptor_Set'Unchecked_Access, 
--                                       dynamicOffsetCount => 0,
--                                       pDynamicOffsets    => null);
        
            -- Bind 3D data
            vkCmdBindVertexBuffers (commandBuffer => Commands,
                                    firstBinding  => 0,
                                    bindingCount  => 1,
                                    pBuffers      => Surface.Vertices.Data'Address,
                                    pOffsets      => Size_Junk'Address); --Surface.Vertices.Offset'Address);  
            vkCmdBindIndexBuffer   (commandBuffer => Commands,
                                    buffer        => Surface.Indicies.Data,
                                    offset        => 0, -- Surface.Indicies.Offset,
                                    indexType     => VK_INDEX_TYPE_UINT32);
                                     
            -- Draw
            vkCmdDrawIndexed (Commands, Surface.Indicies.Count, 1, 0, 0, 0);
            
            -- Kill the current Descriptor_Set... does one really need to be allocated for every draw? Also, what is the overhead of freeing?
            --vkAssert (vkFreeDescriptorSets (Device, Descriptor_Pool, 1, Descriptor_Set'Unchecked_Access));
            Writes.Clear;
          end if;
        end;
      end loop;
    end;    
  
  ------------------------
  -- Initialize_Drawing --
  ------------------------
  
  procedure Initialize_Drawing is

    -- Load the function pointers from our driver library
    procedure Initialize_Vulkan_Subprograms is new API.Vulkan.Initialize (Get_Vulkan_Subprogram);
    
    -- Guess the amount of video RAM in MB from a device's memory properties
    function Guess_VRAM (Memory : VkPhysicalDeviceMemoryProperties) return Int_64_Unsigned_C is
      Result : Int_64_Unsigned_C := 0;
      begin
        for Heap of Memory.memoryHeaps loop
          if (Heap.flags and VK_MEMORY_HEAP_DEVICE_LOCAL_BIT) > 0 and Heap.size > Result then Result := Heap.size; end if;
        end loop;
        return Result;
      end;

    -- Temporaries for fetching and testing physical device features
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
                                                                      
    -- Command pools    
    Buffer_Info       : aliased VkBufferCreateInfo      := (others => <>);
    Command_Pool_Info : aliased VkCommandPoolCreateInfo := (flags => VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT, others => <>);
    
    -- Application details
    Instance_Extensions : aliased Array_Ptr_Char_8_C := (C (VK_KHR_SURFACE_EXTENSION_NAME), Get_Vulkan_Extension);
    Engine_Name         : aliased Str_8_C            := To_Str_8_C (NAME_ID);
    Application_Name    : aliased Str_8_C            := To_Str_8_C (S (OS_Info.App_Name));
    Application_Info    : aliased VkApplicationInfo  := (pApplicationName   => C (Application_Name),
                                                         applicationVersion => VK_MAKE_VERSION (VERSION),
                                                         pEngineName        => C (Engine_Name),
                                                         engineVersion      => VK_MAKE_VERSION (VERSION),
                                                         apiVersion         => VK_API_VERSION_1_1, others => <>);
    Instance_Info : aliased VkInstanceCreateInfo := (pApplicationInfo        => Application_Info'Unchecked_Access,
                                                     enabledExtensionCount   => Instance_Extensions'Length,
                                                     ppenabledExtensionNames => Instance_Extensions (1)'Address,
                                                     enabledLayerCount       => (if Is_Debugging then DEBUGING_EXTENSIONS'Length else 0),
                                                     ppEnabledLayerNames     => (if Is_Debugging then DEBUGING_EXTENSIONS (1)'Address else NULL_PTR), others => <>);
   
    -- Enabled extensions and layers for instance and device creation
    Queue_Priority  : aliased Real_C                   := 1.0;
    Queue_Info      : aliased VkDeviceQueueCreateInfo  := (queueCount => 1, pQueuePriorities => Queue_Priority'Unchecked_Access, others => <>);
    Device_Features : aliased VkPhysicalDeviceFeatures := (shaderClipDistance                     => VK_TRUE,
                                                           shaderCullDistance                     => VK_TRUE,
                                                           shaderTessellationAndGeometryPointSize => VK_TRUE,
                                                           geometryShader                         => VK_TRUE, others => <>);
    Device_Info : aliased VkDeviceCreateInfo := (queueCreateInfoCount    => 1,
                                                 pQueueCreateInfos       => Queue_Info'Unchecked_Access,
                                                 pEnabledFeatures        => Device_Features'Unchecked_Access,
                                                 enabledExtensionCount   => DEVICE_EXTENSIONS'Length,
                                                 ppenabledExtensionNames => DEVICE_EXTENSIONS (1)'Address, others  => <>);
 
    -- Shaders
    Layout_Bindings       :         Vector_VkDescriptorSetLayoutBinding.Unsafe.Vector;
    Shader_Flags          :         Int_Unsigned_C                    := 0;
    Shader_Path           :         Str_Unbound                       := NULL_STR_UNBOUND;
    Shader                :         Shader_State                      := (others => <>);
    Shader_Stage_Info     : aliased VkPipelineShaderStageCreateInfo   := (others => <>);
    Shader_Module_Info    : aliased VkShaderModuleCreateInfo          := (others => <>);
    Shader_Pipeline_Info  : aliased VkPipelineLayoutCreateInfo        := (setLayoutCount => 1, others => <>);
    Layout_Binding_Info   : aliased VkDescriptorSetLayoutCreateInfo   := (others => <>);
    Descriptor_Pool_Sizes : aliased Array_VkDescriptorPoolSize (1..2) := (others => (others => <>));
    Descriptor_Pool_Info  : aliased VkDescriptorPoolCreateInfo        := (poolSizeCount => Descriptor_Pool_Sizes'Length,
                                                                           pPoolSizes    => Descriptor_Pool_Sizes (1)'Unchecked_Access, others => <>);
    
    -- Start of Initialize_Drawing
    begin
      
      -- Load driver
      Initialize_Vulkan_Library;
      Initialize_Vulkan_Subprograms;
      
      -- Create our Vulkan instance and handle the case of debugging extensions not being supported
      if VkCreateInstance (Instance_Info'Unchecked_Access, null, Instance'Unchecked_Access) = VK_ERROR_LAYER_NOT_PRESENT then
      
        -- If we are not debugging then we have no idea why VK_ERROR_LAYER_NOT_PRESENT would be returned...
        if not Is_Debugging then raise Program_Error; end if;
        
        -- Otherwise print some info to the user, remove the debugging layers, and try instantiating the instance again
        Line_Info ("Using Vulkan's debugging layer requires the LunarG" & Char'Val (16#00AE#)
                   & " Vulkan" & Char'Val (16#2122#) & " SDK!" & EOL
                   & "It can be downloaded from https://vulkan.lunarg.com" & EOL
                   & "Continuing without the debugging layer enabled...");
        Instance_Info.enabledLayerCount   := 0;
        Instance_Info.ppEnabledLayerNames := NULL_PTR;
        VkAssert (VkCreateInstance (Instance_Info'Unchecked_Access, null, Instance'Unchecked_Access));
      end if;
      
      -- Create rendering surface
      Surface := Create_Vulkan_Surface (Instance);
      
      -- Pick a physical devices
      vkAssert (vkEnumeratePhysicalDevices (Instance, Count'Unchecked_Access, null));
      declare Physical_Devices : aliased Array_Ptr := (1..Int (Count) => NULL_PTR); begin
        vkAssert (vkEnumeratePhysicalDevices (Instance, Count'Unchecked_Access, Physical_Devices (1)'Unchecked_Access));
      
        -- Find the best physical device and wrap it in a Heap to catch device verification exceptions
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
          declare Surface_Image_Formats : Array_VkSurfaceFormatKHR := (1..Int (Count) => (others => <>)); begin
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
                     and then Guess_VRAM (Current_Memory_Properties) > Guess_VRAM (Memory_Properties))
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
        Line ("VRAM: " & To_Str (Real_64 (Guess_VRAM (Memory_Properties)) / Real_64 (GB)) & " GB");
        Line ("Graphics API: Vulkan " & VK_VERSION_STR (Device_Properties.apiVersion));
        
        -- Check limits
        Assert (Int_Unsigned_C (Buffered_Uniforms.Length) <= Device_Properties.limits.maxDescriptorSetUniformBuffers);
        Assert (Int_Unsigned_C (Sampler_Count)            <= Device_Properties.limits.maxDescriptorSetSamplers);
      exception when others => Line_Error ("Your GPU is incompatible!"); raise Program_Error; end;
      
      -- Logical device
      Queue_Info.queueFamilyIndex := Queue_Family;
      vkAssert (vkCreateDevice (Physical_Device, Device_Info'Unchecked_Access, null, Device'Access));
      vkGetDeviceQueue (Device, Queue_Family, 0, Queue'Unchecked_Access);
         
      -- Command pool
      Command_Pool_Info.queueFamilyIndex := Queue_Family;
      vkAssert (vkCreateCommandPool (Device, Command_Pool_Info'Unchecked_Access, null, Command_Pool'Access));
      
      -- Staging
      Safe_Staging.Initialize;
      
      -- Joint buffer
      Buffer_Info := (size => Joint_Buffer.Size, usage => VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT or VK_BUFFER_USAGE_TRANSFER_DST_BIT, others => <>);
      vkAssert (vkCreateBuffer (Device, Buffer_Info'Unchecked_Access, null, Joint_Buffer.Data'Unchecked_Access));
      Safe_Allocation.Allocate_Buffer (Joint_Buffer);
      vkAssert (vkBindBufferMemory (Device, Joint_Buffer.Data, Joint_Buffer.Device_Memory, Joint_Buffer.Offset));
      
      -- Uniforms
      for Uniform of Buffered_Uniforms loop        
        Buffer_Info := (size  => Uniform.Size,
                        usage => VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT or
                                   (if Uniform.Usage = GPU_Usage then VK_BUFFER_USAGE_TRANSFER_DST_BIT else 0), others => <>);
        vkAssert (vkCreateBuffer (Device, Buffer_Info'Unchecked_Access, null, Uniform.Data'Unchecked_Access));
        Safe_Allocation.Allocate_Buffer (Uniform);
        vkAssert (vkBindBufferMemory (Device, Uniform.Data, Uniform.Device_Memory, Uniform.Offset));
      end loop;
      
      -- Descriptor pool
      Descriptor_Pool_Sizes := ((typ             => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                 descriptorCount => Device_Properties.limits.maxUniformBufferRange, others => <>),
                                (typ             => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                 descriptorCount => Device_Properties.limits.maxDescriptorSetSampledImages, others => <>));
      Descriptor_Pool_Info.maxSets := Descriptor_Pool_Sizes (1).descriptorCount + Descriptor_Pool_Sizes (2).descriptorCount;
      vkAssert (vkCreateDescriptorPool (Device, Descriptor_Pool_Info'Unchecked_Access, null, Descriptor_Pool'Unchecked_Access));
              
      -- Shaders
      for I in Shaders.Iterate loop
        Shader := Map_Shader.Unsafe.Element (I);
        
        -- Position
        Shader.Vertex_Attributes.Append ((format   => VK_FORMAT_R32G32B32_SFLOAT, 
                                          location => 0,
                                          offset   => 0, others => <>));
                     
        -- Normal
        Shader.Vertex_Attributes.Append ((format   => VK_FORMAT_R32G32B32_SFLOAT, 
                                          location => 1,
                                          offset   => Vector_3D'Object_Size / Byte'Size, others => <>));
                                                
        -- Texture
        Shader.Vertex_Attributes.Append ((format   => VK_FORMAT_R32G32_SFLOAT,    
                                          location => 2,
                                          offset   => Vector_3D'Object_Size / Byte'Size * 2, others => <>)); 
                                          
        -- Binding
        Shader.Vertex_Binding := (binding => 0, stride => Vertex_State'Object_Size / Byte'Size, inputRate => VK_VERTEX_INPUT_RATE_VERTEX);
                                
        -- A complete shader is made of multiple stages - each with their own SPIR-V program
        for Stage of Shader.Stages loop
          Shader_Flags := (case Stage.Kind is when Vertex_Stage      => VK_SHADER_STAGE_VERTEX_BIT,
                                              when Fragment_Stage    => VK_SHADER_STAGE_FRAGMENT_BIT,
                                              when Tesselation_Stage => VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT,
                                              when Geometry_Stage    => VK_SHADER_STAGE_GEOMETRY_BIT);
        
          -- Build the shader's current stage path and verify it exists
          Shader_Path := OS_Info.App_Path & U (PATH_SHADERS) & Map_Shader.Unsafe.Key (I) & "-" & To_Lower (Stage.Kind'Wide_Image) (1..4) & ".spv";          
          Line (S (Shader_Path));
          if not Exists (To_Str_8 (S (Shader_Path))) then -- Str_8 !!!
            Line_Error ("Missing shader stage " & S (Shader_Path));
            raise Program_Error;
          end if;
          
          -- Sampler descriptors
          case Stage.Kind is
            when Fragment_Stage =>
            
              -- This must correspond to the Material_State type   
              case Stage.Domain is         
                when No_Domain   => null;
                when Menu_Domain => Layout_Bindings.Append ((binding => Menu_Sampler.Get_Binding,       descriptorType => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, others => <>));
                when others      => Layout_Bindings.Append ((binding => Base_Color_Sampler.Get_Binding, descriptorType => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, others => <>));
                  
                  -- 
                  case Stage.Domain is
                    when Light_Domain =>
                      null;
                    when others =>         
                      Layout_Bindings.Append ((binding => Irradiance_Sampler.Get_Binding, descriptorType => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, others => <>));
                      Layout_Bindings.Append ((binding => Specular_Sampler.Get_Binding,   descriptorType => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, others => <>));
                      Layout_Bindings.Append ((binding => Normal_Sampler.Get_Binding,     descriptorType => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, others => <>));
                      Layout_Bindings.Append ((binding => Displace_Sampler.Get_Binding,   descriptorType => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, others => <>));
                      Layout_Bindings.Append ((binding => Metallic_Sampler.Get_Binding,   descriptorType => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, others => <>));
                      Layout_Bindings.Append ((binding => Roughness_Sampler.Get_Binding,  descriptorType => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, others => <>));
                  end case;
              end case;
          when others => null; end case;
          
          -- Uniform descriptors
          for Uniform of Stage.Uniforms loop
            if Uniform /= NULL_STR_UNBOUND then
              Layout_Bindings.Append ((binding        => Buffered_Uniforms.Element (Uniform).Binding,
                                       descriptorType => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                       stageFlags     => Shader_Flags, others => <>));
            end if;
          end loop;
            
          -- Load the stage's SPIR-V data
          declare Program : aliased Array_Byte := Load (S (Shader_Path), Padding => 4); begin
            Shader_Module_Info := (codeSize => Program'Length, pCode => Program'Address, others => <>);
            vkAssert (vkCreateShaderModule (Device, Shader_Module_Info'Unchecked_Access, null, Stage.Program'Unchecked_Access));
          exception when others => Line_Error ("Error loading shader " & S (Shader_Path)); raise Program_Error; end;
          
          -- Load the descriptors
          if Layout_Bindings.Length > 0 then
            declare
            Result_Layout_Bindings : aliased Array_VkDescriptorSetLayoutBinding :=
              Vector_VkDescriptorSetLayoutBinding.To_Unsafe_Array (Layout_Bindings);
            begin              
              Layout_Binding_Info.bindingCount := Int_Unsigned_C (Result_Layout_Bindings'Length);
              Layout_Binding_Info.pBindings    := Result_Layout_Bindings (1)'Unchecked_Access;
              vkAssert (vkCreateDescriptorSetLayout (device      => Device,
                                                     pCreateInfo => Layout_Binding_Info'Unchecked_Access,
                                                     pAllocator  => null,
                                                     pSetLayout  => Shader.Descriptor_Set_Layout'Unchecked_Access));  
            end;
          end if;
          Shader.Stages_Info.Append ((stage => Shader_Flags, module => Stage.Program, pName => C (Shader_Entry_Name), others => <>));
            
          -- Stage is set - clear the layouts
          Layout_Bindings.Clear;   
        end loop;
        
        -- Pipeline Layout
        Shader_Pipeline_Info.pSetLayouts := Shader.Descriptor_Set_Layout'Unchecked_Access;        
        vkAssert (vkCreatePipelineLayout (Device, Shader_Pipeline_Info'Unchecked_Access, null, Shader.Pipeline_Layout'Unchecked_Access));
        Shaders.Replace_Element (I, Shader);
      end loop;       
      
      -- Kick off the second stage!
      Initialize_Framebuffer;
    end; -- exception when others => Line_Error ("Vulkan driver not installed or up to date?"); raise Program_Error; end;
end;

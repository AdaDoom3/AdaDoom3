
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

  -- Driver information used to exclude or prefer certain physical devices
  type Driver_State is record
      Version   : Int_32_Unsigned_C := 0;
      Vendor_Id : Int_32_Unsigned_C := 0;
      Device_Id : Int_32_Unsigned_C := 0;
    end record;
  DRIVER_BLACKLIST : constant array Driver_State := ((others => <>));
  DRIVER_GREYLIST  : constant array Driver_State := ((others => <>));

  -- Additional device extensions that must be present
  REQUIRED_EXTENSIONS : constant Array_Str_Unbound := (U (VK_EXT_DEBUG_REPORT_EXTENSION_NAME)); -- Support debugging layers

  -- Surface depth formats that a device is allowed to support
  SUPPORTED_SURFACE_FORMATS : constant Array_Int_32_Unsigned_C:= (VK_FORMAT_D32_SFLOAT,
                                                                  VK_FORMAT_D32_SFLOAT_S8_UINT,
                                                                  VK_FORMAT_D24_UNORM_S8_UINT);

  -- Amount of idle frontend light job tasks that are allowed to sit idle until tasks start being destroyed
  MAX_IDLE_LIGHT_JOBS : constant Positive := 10;
  FOG_SCALE           : constant Float    := 0.001;

  -------------
  -- Globals --
  -------------

  -- Current recogonized width and height of the render surface
  Current_Width, Current_Height : aliased Natural := 0;

  -- Frame buffer
  Images : aliased Array_Ptr (1..3) := null;

  -- Handles
  Instance, Surface, Swap_Chain, Command_Pool, Graphics_Queue, Present_Queue,
  Render_Status, Acquire_Status, Physical_Device, Device : aliased Ptr := NULL_PTR;

  -- Settings, and instance information
  Family_Index, Present_Family, Present_Mode, Image_Index : aliased Int_32_Unsigned_C := 0;
  Surface_Capabilities : aliased VkSurfaceCapabilitiesKHR         := (others => <>);
  Device_Properties    : aliased VkPhysicalDeviceProperties       := (others => <>);
  Swap_Chain_Info      : aliased VkSwapchainCreateInfoKHR         := (others => <>);
  General_Properties   : aliased VkPhysicalDeviceProperties       := (others => <>);
  Memory_Properties    : aliased VkPhysicalDeviceMemoryProperties := (others => <>);
  Surface_Format       : aliased VkSurfaceFormatKHR               := (others => <>);

  --------------------
  -- Command_Buffer --
  --------------------

  -- Controller
  type Command_Buffer_State is new Controlled with record
      Data : Ptr := null;
    end record;
  procedure Initialize (Command_Buffer : in out Command_Buffer_State);
  procedure Finalize   (Command_Buffer : in out Command_Buffer_State);

  -- Controlled primatives
  procedure Initialize (Command_Buffer : in out Command_Buffer_State) is
      Begin_Info    : aliased VkCommandBufferBeginInfo    := (flags              => VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
                                                              others             => <>);
      Allocate_Info : aliased VkCommandBufferAllocateInfo := (level              => VK_COMMAND_BUFFER_LEVEL_PRIMARY,
                                                              commandPool        => Command_Pool
                                                              commandBufferCount => 1,
                                                              others             => <>);
    begin
      vkAllocateCommandBuffers (Device, Allocate_Info'Unchecked_Access, Command_Buffer'Unchecked_Access);
      vkBeginCommandBuffer (Command_Buffer, Begin_Info'Unchecked_Access);
    end;
  procedure Finalize (Command_Buffer : in out Command_Buffer_State) is
    Submit_Info : aliased VkSubmitInfo := (commandBufferCount => 1,
                                           pCommandBuffers    => Command_Buffer'Unchecked_Access,
                                           others             => <>);
    begin
      vkEndCommandBuffer (Command_Buffer);
      vkQueueSubmit (Graphics_Queue, 1, Submit_Info'Unchecked_Access, VK_NULL_HANDLE);
      vkQueueWaitIdle (Graphics_Queue);
      vkFreeCommandBuffers (Device, Command_Pool 1, Command_Buffer'Unchecked_Access);
    end;

  -- -----------
  -- -- Image --
  -- -----------

  -- -- Controller
  -- type Image_State (Width, Height, Tiling, Usage, Properties : Int_32_Unsigned_C) is new Controlled with record
  --     Properties : Int_32_Unsigned_C;
  --     Layout     : Int_32_Unsigned_C;
  --     Data       : Ptr;
  --   end record;
  -- procedure Initialize (Image : in out Image_State);
  -- procedure Finalize   (Image : in out Image_State);

  -- -- Copy used for staging
  -- function Copy (Image : in out Image_State) return Image_State is
  --   Command_Buffer : aliased VkCommandBuffer          := (others         => <>); 
  --   Subresource    : aliased VkImageSubresourceLayers := (aspectMask     => VK_IMAGE_ASPECT_COLOR_BIT,
  --                                                         baseArrayLayer => 0,
  --                                                         mipLevel       => 0,
  --                                                         layerCount     => 1,
  --                                                         others         => <>);
  --   Image_Copy     : aliased VkImageCopy              := (srcSubresource => Subresource,
  --                                                         dstSubresource => Subresource,
  --                                                         srcOffset      => (0, 0, 0),
  --                                                         dstOffset      => (0, 0, 0),
  --                                                         extent         => (Width, Height, depth => 1),
  --                                                         others         => <>);
  --   begin
  --     Begin_Commands (Command_Buffer);
  --     vkCmdCopyImage (commandBuffer  => Command_Buffer,
  --                     srcImage       => Image,
  --                     srcImageLayout => VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
  --                     dstImage       => Result,
  --                     dstImageLayout => VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
  --                     regionCount    => 1,
  --                     pRegions       => Image_Copy'Unchecked_Access);
  --     End_Commands (Command_Buffer);
  --     return Result;
  --   end;
 
  -- -- Grab a view 
  -- function Make_View (Image : in out Image_State) return ___ is
  --   Image_View_Info : aliased VkImageViewCreateInfo := (subresourceRange => (aspectMask     => aspectFlags,
  --                                                                            baseMipLevel   => 0,
  --                                                                            levelCount     => 1,
  --                                                                            baseArrayLayer => 0,
  --                                                                            layerCount     => 1,
  --                                                                            others         => <>),
  --                                                       image            => image,
  --                                                       viewType         => VK_IMAGE_VIEW_TYPE_2D,
  --                                                       format           => format,
  --                                                       others           => <>);
  --   begin
  --     vkAssert (vkCreateImageView(Device, &viewInfo, null, imageView.replace)))
  --     return Result;
  --   end;

  -- -- Controlled primatives
  -- procedure Initialize (Image : in out Image_State) is
  --   Memory_Requirements : aliased VkMemoryRequirements := (others         => <>);
  --   Image_Info          : aliased VkImageCreateInfo    := (imageType      => VK_IMAGE_TYPE_2D,
  --                                                          extent         => (width, height, depth => 1),
  --                                                          mipLevels      => 1,
  --                                                          arrayLayers    => 1,
  --                                                          format         => format,
  --                                                          tiling         => tiling,
  --                                                          initialLayout  => VK_IMAGE_LAYOUT_PREINITIALIZED,
  --                                                          usage          => usage,
  --                                                          samples        => VK_SAMPLE_COUNT_1_BIT,
  --                                                          sharingMode    => VK_SHARING_MODE_EXCLUSIVE,
  --                                                          others         => <>);
  --   Allocate_Info      : aliased VkMemoryAllocateInfo  := (allocationSize => Memory_Requirements.size,
  --                                                          others         => <>);
  --   begin
  --     vkAssert (vkCreateImage (Device, Image_Info'Unchecked_Access, null, image.replace));
  --     vkGetImageMemoryRequirements (Device, image, Memory_Requirements'Unchecked_Access);
  --     vkAllocateMemory (Device, Allocate_Info'Unchecked_Access, null, imageMemory.replace)
  --     vkBindImageMemory (Device, image, imageMemory, 0);
  --   end;
  -- procedure Finalize (Image : in out Image_State) is
  --   begin
  --     null;
  --   end;
  -- procedure Adjust (Image : in out Image_State) is
  --   Command_Buffer : aliased VkCommandBuffer      := (others              => <>); 
  --   Barrier        : aliased VkImageMemoryBarrier := (oldLayout           => oldLayout,
  --                                                     newLayout           => newLayout,
  --                                                     srcQueueFamilyIndex => VK_QUEUE_FAMILY_IGNORED,
  --                                                     dstQueueFamilyIndex => VK_QUEUE_FAMILY_IGNORED,
  --                                                     image               => image);
  --   begin

  --     -- ???
  --     if newLayout := VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL then
  --       Barrier.subresourceRange.aspectMask := VK_IMAGE_ASPECT_DEPTH_BIT;
  --       if format := VK_FORMAT_D32_SFLOAT_S8_UINT or format := VK_FORMAT_D24_UNORM_S8_UINT then
  --         Barrier.subresourceRange.aspectMask := Barrier.subresourceRange.aspectMask or VK_IMAGE_ASPECT_STENCIL_BIT;
  --       end if;
  --     else Barrier.subresourceRange.aspectMask := VK_IMAGE_ASPECT_COLOR_BIT; end if;

  --     -- ???
  --     Barrier.subresourceRange.baseMipLevel   := 0;
  --     Barrier.subresourceRange.levelCount     := 1;
  --     Barrier.subresourceRange.baseArrayLayer := 0;
  --     Barrier.subresourceRange.layerCount     := 1;

  --     -- Set new image layout
  --     if oldLayout := VK_IMAGE_LAYOUT_PREINITIALIZED and newLayout := VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL then
  --       Barrier.srcAccessMask := VK_ACCESS_HOST_WRITE_BIT;
  --       Barrier.dstAccessMask := VK_ACCESS_TRANSFER_READ_BIT;
  --     elsif oldLayout := VK_IMAGE_LAYOUT_PREINITIALIZED and newLayout := VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL then
  --       Barrier.srcAccessMask := VK_ACCESS_HOST_WRITE_BIT;
  --       Barrier.dstAccessMask := VK_ACCESS_TRANSFER_WRITE_BIT;
  --     elsif oldLayout := VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL and newLayout := VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL then
  --       Barrier.srcAccessMask := VK_ACCESS_TRANSFER_WRITE_BIT;
  --       Barrier.dstAccessMask := VK_ACCESS_SHADER_READ_BIT;
  --     elsif oldLayout := VK_IMAGE_LAYOUT_UNDEFINED and newLayout := VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL then
  --       Barrier.srcAccessMask := 0;
  --       Barrier.dstAccessMask := VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT or VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
  --     else raise Program_Error with "Unknown image layout transition?!"; end if;

  --     -- Send transition commands
  --     Begin_Commands (Command_Buffer);
  --     vkCmdPipelineBarrier (commandBuffer            => Command_Buffer,
  --                           srcStageMask             => VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT,
  --                           dstStageMask             => VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT,
  --                           dependencyFlags          => 0,
  --                           memoryBarrierCount       => 0,
  --                           pMemoryBarriers          => null,
  --                           bufferMemoryBarrierCount => 0,
  --                           pBufferMemoryBarriers    => null,
  --                           imageMemoryBarrierCount  => 1,
  --                           pImageMemoryBarriers     => Barrier);
  --     End_Commands (Command_Buffer);
  --   end;

  -------------
  -- Texture --
  -------------

  -- Controller
  type Texture_State is new Controlled with record
    end record;
  procedure Initialize (Texture : in out Texture_State);
  procedure Finalize   (Texture : in out Texture_State);

  -- Controlled primatives
  procedure Initialize (Texture : in out Texture_State) is
    Subresource : aliased VkImageSubresource := (aspectMask := VK_IMAGE_ASPECT_COLOR_BIT;
                                                mipLevel := 0;
                                                arrayLayer := 0;
                                                others => <>);
    VkSubresourceLayout stagingImageLayout;
    vkGetImageSubresourceLayout(Device, stagingImage, &subresource, &stagingImageLayout);
    void* data;
    VDeleter<VkImage> stagingImageDevice, vkDestroyImageend;
    VDeleter<VkDeviceMemory> stagingImageMemoryDevice, vkFreeMemoryend;
    Staging_Image : Image_State (texWidth,
                                texHeight,
                                VK_FORMAT_R8G8B8A8_UNORM,
                                VK_IMAGE_TILING_LINEAR,
                                VK_IMAGE_USAGE_TRANSFER_SRC_BIT,
                                VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
    begin
      vkMapMemory (Device, stagingImageMemory, 0, imageSize, 0, &data);

      -- Pad 
      if stagingImageLayout.rowPitch /= texWidth * 4 then
          uint8_t* dataBytes := reinterpret_cast<uint8_t*>(data);
          for (int y := 0; y < texHeight; y++) 
              memcpy(&dataBytes[y * stagingImageLayout.rowPitch], &pixels[y * texWidth * 4], texWidth * 4);
      else memcpy (data, pixels, imageSize); end if;

      --
      vkUnmapMemory (Device, stagingImageMemory);
      Create_Image (texWidth, texHeight, VK_FORMAT_R8G8B8A8_UNORM, VK_IMAGE_TILING_OPTIMAL, VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, Texture, textureImageMemory);
      Staging.Set_Properties (VK_FORMAT_R8G8B8A8_UNORM, VK_IMAGE_LAYOUT_PREINITIALIZED, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL);
      Texture.Set_Properties  (VK_FORMAT_R8G8B8A8_UNORM, VK_IMAGE_LAYOUT_PREINITIALIZED, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
      Copy_Image (stagingImage, Texture, texWidth, texHeight);
      Texture.Set_Layout (Texture, VK_FORMAT_R8G8B8A8_UNORM, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
      return True;
    end;
  procedure Finalize (Texture : in out Texture_State) is
    begin
      null;
    end;

  ------------
  -- Buffer --
  ------------

  -- Controller
  type Buffer_State (Size : Int_64_Unsigned_C; Usage, Properties : Int_32_Unsigned_C) is new Controlled with record
      Data   : Ptr;
      Memory : Ptr;
    end record;
  procedure Initialize (Buffer : in out Buffer_State);
  procedure Finalize   (Buffer : in out Buffer_State);

  -- Copy for staging
  function Copy (Buffer : in Buffer_State) return Buffer_State is
    Begin_Info    : aliased VkCommandBufferBeginInfo := (flags              => VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
                                                         others             => <>);
    VkBufferCopy  : aliased Copy_Region              := (size               => size,
                                                         others             => <>);
    Allocate_Info : aliased VkMemoryAllocateInfo     := (level              => VK_COMMAND_BUFFER_LEVEL_PRIMARY,
                                                         commandPool        => Command_Pool
                                                         commandBufferCount => 1,
                                                         others             => <>);
    Submit_Info   : aliased VkSubmitInfo             := (commandBufferCount => 1,
                                                         pCommandBuffers    => Command_Buffer'Unchecked_Access,
                                                         others             => <>);

    begin
      vkAllocateCommandBuffers (Device, &allocInfo, Command_Buffer'Unchecked_Access);
      vkBeginCommandBuffer     (Command_Buffer, Begin_Info'Unchecked_Access);
      vkCmdCopyBuffer          (Command_Buffer, srcBuffer, dstBuffer, 1, &copyRegion);
      vkEndCommandBuffer       (Command_Buffer);
      vkQueueSubmit            (Graphics_Queue, 1, &submitInfo, VK_NULL_HANDLE);
      vkQueueWaitIdle          (Graphics_Queue);
      vkFreeCommandBuffers     (Device, Command_Pool 1, Command_Buffer'Unchecked_Access);
    end;

  -- Controlled primitives
  procedure Initialize (Buffer : in out Buffer_State) is
    Memory_Requirements : aliased VkMemoryRequirements := (others         => <>);
    Allocate_Info       : aliased VkMemoryAllocateInfo := (allocationSize => Memory_Requirements.size,
                                                           others         => <>);
    Buffer_Info         : aliased VkBufferCreateInfo   := (size           => Size,
                                                           usage          => Usage,
                                                           sharingMode    => VK_SHARING_MODE_EXCLUSIVE,
                                                           others         => <>);
    begin
      vkAssert (vkCreateBuffer (Device, Buffer_Info'Unchecked_Access, null, buffer.replace));
      for I in 1..Memory_Properties.memoryTypeCount loop
        if (Memory_Properties.memoryTypeBits and 2**I) > 0 and (Memory_Properties.memoryTypes (I).propertyFlags and Properties) := Properties then
          Allocate_Info.memoryTypeIndex := I;
          exit;
        elsif I = Memory_Properties.memoryTypeCount then raise Program_Error with "Failed to find suitable memory type!?"; end if;
      end loop;
      vkAssert (vkAllocateMemory (Device, Allocate_Info'Unchecked_Access, null, bufferMemory.replace));
      vkBindBufferMemory (Device, buffer, bufferMemory, 0);
      return ;
    end;
  procedure Finalize (Buffer : in out Buffer_State) is
    begin
      null;
    end;

  --------------
  -- Geometry --
  --------------

  -- Controller
  type Geometry_State is new Controlled with record
      Index_Buffer  : aliased Ptr := null;
      Vertex_Buffer : aliased Ptr := null;
    end record;  
  procedure Initialize (Geometry : in out Geometry_State);
  procedure Finalize   (Geometry : in out Geometry_State);

  -- Controlled primitives
  procedure Initialize (Geometry : in out Geometry_State) is
    void* data;
    Raw_Indicies   : aliased Array_Real_32_C (1..Indicies'Length * 3) := (others => 0.0); -- For-expression possible ???
    Staging        : aliased VkBuffer                                 := (others => <>);
    Staging_Memory : aliased VkDeviceMemory                           := (others => <>);
    Buffer_Size    : aliased VkDeviceSize                             := Raw_Indicies'Object_Size;
    begin

      -- Create vertex buffer
      Create_Buffer(bufferSize, VK_BUFFER_USAGE_TRANSFER_SRC_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, stagingBuffer, stagingBufferMemory);
      vkMapMemory (Device, stagingBufferMemory, 0, bufferSize, 0, &data);
      memcpy (data, vertices.data, (size_t) bufferSize);
      vkUnmapMemory(Device, stagingBufferMemory);
      Create_Buffer(bufferSize, VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_VERTEX_BUFFER_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, vertexBuffer, vertexBufferMemory);
      copyBuffer(stagingBuffer, vertexBuffer, bufferSize);

      -- Create index buffer
      Create_Buffer(bufferSize, VK_BUFFER_USAGE_TRANSFER_SRC_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, stagingBuffer, stagingBufferMemory);
      vkMapMemory(Device, stagingBufferMemory, 0, bufferSize, 0, &data);
      memcpy(data, indices.data, (size_t) bufferSize);
      vkUnmapMemory(Device, stagingBufferMemory);
      Create_Buffer(bufferSize, VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_INDEX_BUFFER_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, indexBuffer, indexBufferMemory);
      copyBuffer(stagingBuffer, indexBuffer, bufferSize)   
    end;
  procedure Adjust (Geometry : in out Geometry_State) is
    Stage_Flags : aliased Int_32_Unsigned_C := VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
    Submit_Info : aliased VkSubmitInfo      := (waitSemaphoreCount   => 1,
                                                pWaitSemaphores      => imageAvailableSemaphore,
                                                pWaitDstStageMask    => waitStages,
                                                commandBufferCount   => 1,
                                                pCommandBuffers      => &commandBuffers[imageIndex],
                                                signalSemaphoreCount => 1,
                                                pSignalSemaphores    => renderFinishedSemaphore,
                                                others               => <>);
    begin
      vkAssert (vkQueueSubmit(Graphics_Queue, 1, Submit_Info'Unchecked_Access, VK_NULL_HANDLE));
    end;
  procedure Finalize (Geometry : in out Geometry_State) is
    begin
      vkDestroyBuffer
      vkDestroyBuffer
      vkFreeMemory
      vkFreeMemory
    end;

  ----------
  -- Draw --
  ----------
  --
  --
  --

  procedure Draw () is
    idIndexBuffer * indexBuffer := (if vertexCache.CacheIsStatic( surf.indexCache ) &vertexCache.staticData.indexBuffer
                                     else &vertexCache.frameData[vertexCache.drawListNum].indexBuffer);
    idVertexBuffer * vertexBuffer  := (if vertexCache.CacheIsStatic( surf.ambientCache ) &vertexCache.staticData.vertexBuffer
                                        else vertexBuffer = &vertexCache.frameData[vertexCache.drawListNum].vertexBuffer);
    begin

      -- 
      if surf.jointCache then
        idJointBuffer jointBuffer;
        Assert (!vertexCache.GetJointBuffer( surf.jointCache, &jointBuffer )
        assert( ( jointBuffer.GetOffset() & ( glConfig.uniformBufferOffsetAlignment - 1 ) ) == 0);
        const GLuint ubo = reinterpret_cast< GLuint >( jointBuffer.GetAPIObject());
        qglBindBufferRange( GL_UNIFORM_BUFFER, 0, ubo, jointBuffer.GetOffset(), jointBuffer.GetNumJoints() * sizeof( idJointMat ));
      end
      renderProgManager.CommitUniforms;

      -- 
      if backEnd.glState.currentIndexBuffer /= (GLuint)indexBuffer.GetAPIObject() or not r_useStateCaching.Get then
        qglBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, (GLuint)indexBuffer.GetAPIObject());
        backEnd.glState.currentIndexBuffer = (GLuint)indexBuffer.GetAPIObject;
      end if;

      -- 
      if backEnd.glState.vertexLayout != LAYOUT_DRAW_VERT or backEnd.glState.currentVertexBuffer /= (GLuint)vertexBuffer.GetAPIObject or not r_useStateCaching.Get then
        qglBindBufferARB( GL_ARRAY_BUFFER_ARB, (GLuint)vertexBuffer.GetAPIObject());
        backEnd.glState.currentVertexBuffer = (GLuint)vertexBuffer.GetAPIObject;

        -- 
        qglEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_VERTEX);
        qglEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_NORMAL);
        qglEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_COLOR);
        qglEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_COLOR2);
        qglEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_ST);
        qglEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_TANGENT);

        -- 
        qglVertexAttribPointerARB( PC_ATTRIB_INDEX_VERTEX, 3, GL_FLOAT, GL_FALSE, sizeof( idDrawVert ), (void *)( DRAWVERT_XYZ_OFFSET ));
        qglVertexAttribPointerARB( PC_ATTRIB_INDEX_NORMAL, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof( idDrawVert ), (void *)( DRAWVERT_NORMAL_OFFSET ));
        qglVertexAttribPointerARB( PC_ATTRIB_INDEX_COLOR, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof( idDrawVert ), (void *)( DRAWVERT_COLOR_OFFSET ));
        qglVertexAttribPointerARB( PC_ATTRIB_INDEX_COLOR2, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof( idDrawVert ), (void *)( DRAWVERT_COLOR2_OFFSET ));
        qglVertexAttribPointerARB( PC_ATTRIB_INDEX_ST, 2, GL_HALF_FLOAT, GL_TRUE, sizeof( idDrawVert ), (void *)( DRAWVERT_ST_OFFSET ));
        qglVertexAttribPointerARB( PC_ATTRIB_INDEX_TANGENT, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof( idDrawVert ), (void *)( DRAWVERT_TANGENT_OFFSET ));
        backEnd.glState.vertexLayout = LAYOUT_DRAW_VERT;
      end
      
      -- 
      qglDrawElementsBaseVertex( GL_TRIANGLES, 
                    r_singleTriangle.GetBool() ? 3 : surf.numIndexes,
                    GL_INDEX_TYPE,
                    (triIndex_t *)(int)( ibHandle >> VERTCACHE_OFFSET_SHIFT ) & VERTCACHE_OFFSET_MASK,
                    (int)( surf.ambientCache >> VERTCACHE_OFFSET_SHIFT ) & VERTCACHE_OFFSET_MASK / sizeof ( idDrawVert ));
    end;
    
  ----------
  -- Main --
  ----------
  --
  -- Primary routines accessed by the engine to setup, present, and shutdown the renderer
  --

  -- Get the game window ready for rendering and initialize the global variables in the spec
  procedure Initialize is 

    -- Load all of the function pointers from a driver
    procedure Initialize_Vulkan_Subprograms is new Neo.API.Vulkan.Initialize_Subprograms (Get_Vulkan_Subprogram);

    -- Create shader model
    function Load_Shader (Path : Str) return Ptr is
      Shader_Module_Info : aliased VkShaderModuleCreateInfo := (codeSize => code.size, pCode => (uint32_t*) code.data, others => <>);
      begin vkAssert (vkCreateShaderModule (Device, Shader_Module_Info'Unchecked_Access, null, shaderModule.replace)); end;

    -- Temporary variables for fetching and testing physical devices
    Count, Current_Present_Mode, Current_Family_Index,
    Current_Present_Family       : aliased Int_32_Unsigned_C                := 0;
    Current_Surface_Capabilities : aliased VkSurfaceCapabilitiesKHR         := (others => <>);
    Current_Device_Properties    : aliased VkPhysicalDeviceProperties       := (others => <>);
    Current_Memory_Properties    : aliased VkPhysicalDeviceMemoryProperties := (others => <>);
    Current_Surface_Format       : aliased VkSurfaceFormatKHR               := (others => <>);

    -- General application details to allow tools and drivers to make decisions about your application without needing to guess
    Application_Info : aliased VkApplicationInfo := (pApplicationName   => To_Str_8_C (S (Get_Information.Name)),
                                                     applicationVersion => VK_MAKE_VERSION (1, 0, 0),
                                                     pEngineName        => To_Str_8_C (NAME_ID & " " & VERSION),
                                                     apiVersion         => VK_MAKE_VERSION (1, 0, 0),
                                                     others             => <>);

    -- Creation details for the Vulkan instance - enabled extensions and application info
    Instance_Info : aliased VkInstanceCreateInfo := (pApplicationInfo        => Application_Info'Unchecked_Access,
                                                     enabledExtensionCount   => Get_Enabled_Extensions'Length,
                                                     ppenabledExtensionNames => Get_Enabled_Extensions'Unchecked_Access,
                                                     others                  => <>);

    -- 
    Format_Properties : aliased VkFormatProperties := (others => <>);

    -- Creation details for physical device queues - the count and priority
    Queue_Priority : aliased Real_32_C               := 1.0; 
    Queue_Info     : aliased VkDeviceQueueCreateInfo := (queueCount       => 3, -- Triple buffering
                                                         pQueuePriorities => Queue_Priority'Unchecked_Access,
                                                         others           => <>);

    -- List of enabled features used for Device_Info and device creation
    Device_Features : aliased VkPhysicalDeviceFeatures := (shaderClipDistance                     => VK_TRUE,
                                                           shaderCullDistance                     => VK_TRUE,
                                                           shaderTessellationAndGeometryPointSize => VK_TRUE,
                                                           geometryShader                         => VK_TRUE,
                                                           others                                 => <>);

    -- Creation details for a Vulkan non-physical device - features, queues, and debug layers
    Device_Info : aliased VkDeviceCreateInfo := (queueCreateInfoCount    => 1,
                                                 pQueueCreateInfos       => Queue_Info'Unchecked_Access,
                                                 pEnabledFeatures        => Device_Features'Unchecked_Access,
                                                 enabledExtensionCount   => Enabled_Extensions'Length,
                                                 ppEnabledExtensionNames => Enabled_Extensions,
                                                 others                  => <>);
    

    -- Creation details for command pools are needed for the creation of command buffers
    Command_Pool_Info : aliased VkCommandPoolCreateInfo := (flags  => VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
                                                            -- Allows individual command buffers to be reused or reused by restarting them
                                                            others => <>);

    -- Descriptor for the command pool, descriptors are first bound into sets then these sets are bound to the pipeline
    Pool_Sizes : aliased array VkDescriptorPoolSize := ((kind            => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                                         descriptorCount => 1,
                                                         others          => <>),
                                                        (kind            => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
                                                         descriptorCount => 1,
                                                         others          => <>));

    -- Creation details for the command pool descriptor
    Pool_Info : aliased VkDescriptorPoolCreateInfo := (poolSizeCount => Pool_Sizes'Length,
                                                       pPoolSizes    => Pool_Sizes'Unchecked_Access,
                                                       maxSets       => 1,
                                                       others        => <>);

    -- Creation details for atomic GPU queue semaphores 
    Semaphore_Info : aliased VkSemaphoreCreateInfo := (others => <>);

    -- Creation details for the uniform buffer objects for global data used by shaders
    Layout_Bindings : aliased array VkDescriptorSetLayoutBinding := ((binding            => 0,
                                                                      descriptorCount    => 1,
                                                                      descriptorType     => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                                                      pImmutableSamplers => null,
                                                                      stageFlags         => VK_SHADER_STAGE_VERTEX_BIT,
                                                                      others             => <>),
                                                                     (binding            => 1,
                                                                      descriptorCount    => 1,
                                                                      descriptorType     => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                                      pImmutableSamplers => null,
                                                                      stageFlags         => VK_SHADER_STAGE_FRAGMENT_BIT,
                                                                      others             => <>));

    -- Creation details for 
    Layout_Info : aliased VkDescriptorSetLayoutCreateInfo := (bindingCount => Layout_Bindings'Length,
                                                              pBindings    => Layout_Bindings'Unchecked_Access,
                                                              others       => <>);


    -- -- 
    -- Allocate_Info : aliased VkDescriptorSetAllocateInfo := (descriptorPool     => Pool_Info,
    --                                                         descriptorSetCount => 1,
    --                                                         pSetLayouts        => Layout_Info,
    --                                                         others             => <>);

    -- -- 
    -- Buffer_Info : aliased VkDescriptorBufferInfo := (buffer    => uniformBuffer,
    --                                                  offset    => 0,
    --                                                  rangeSize => sizeof(UniformBufferObject),
    --                                                  others    => <>);

    -- -- 
    -- Image_Info : aliased VkDescriptorImageInfo := (imageLayout => VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
    --                                                imageView   =>  ,
    --                                                sampler     => textureSampler,
    --                                                others      => <>);

    -- -- 
    -- Write_Set : aliased array VkWriteDescriptorSet := ((dstSet          => descriptorSet,
    --                                                     dstBinding      => 0,
    --                                                     dstArrayElement => 0,
    --                                                     descriptorType  => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
    --                                                     descriptorCount => 1,
    --                                                     pBufferInfo     => &bufferInfo),
    --                                                     others          => <>),
    --                                                    (dstSet          => descriptorSet,
    --                                                     dstBinding      => 1,
    --                                                     dstArrayElement => 0,
    --                                                     descriptorType  => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
    --                                                     descriptorCount => 1,
    --                                                     pImageInfo      => imageInfo'Unchecked_Access,
    --                                                     others          => <>));
    begin

      -- Load driver
      Initialize_Vulkan_Library;
      Initialize_Vulkan_Subprograms;

      -- Create instance
      vkAssert (vkCreateInstance (Instance_Info'Access, null, Instance'Access));

      -- Create rendering surface
      Surface := Create_Surface (Instance);

      -- Get all of the physical devices
      vkAssert (vkEnumeratePhysicalDevices (Instance, Count'Unchecked_Access, null));
      Assert (Count > 0);
      declare Physical_Devices : aliased Array_Ptr (1..Int (Count)); begin
        vkAssert (vkEnumeratePhysicalDevices (Instance, Count'Unchecked_Access, Physical_Devices'Unchecked_Access));

        -- Find the best physical device and by looping through all of them and verifying requirements
        for Current_Physical_Device of Physical_Devices loop

          -- Make a block to catch device verification exceptions
          begin

            -- Get queues
            vkGetPhysicalDeviceQueueFamilyProperties (Current_Physical_Device, Count'Unchecked_Access, null);
            declare Queue_Family_Properties : aliased array VkQueueFamilyProperties (1..Int (Count)); begin
              vkGetPhysicalDeviceQueueFamilyProperties (Current_Physical_Device, Count'Unchecked_Access, Queue_Family_Properties);

              -- Verify a queue exists with graphical surface support and aquire it
              Queue_Family_Index   := -1;
              Queue_Present_Family := -1;
              Inner: for I in Queue_Family_Properties'Range loop 
                vkAssert (vkGetPhysicalDeviceSurfaceSupportKHR (Current_Physical_Device, Int_32_Unsigned_C (I), Surface, Surface_Support'Unchecked_Access));

                -- Verify the device supports surfaces and 3d graphics  
                if Queue_Family_Properties (I).queueCount > 0 and Surface_Support /= VK_FALSE then Queue_Present_Family := Int_32_Unsigned_C (I); end if;

                -- Theoretically we could have a device that only does compute stuff
                if Queue_Family_Properties (I).queueFlags > 0 and VK_QUEUE_GRAPHICS_BIT > 0 then Queue_Family_Index := Int_32_Unsigned_C (I); end if;

                exit Inner when Current_Queue_Family_Index /= -1 and Current_Queue_Present_Family /= -1;
              end loop Inner;
              Assert (Current_Queue_Family_Index /= -1 and Current_Queue_Present_Family /= -1);
            end;

            -- Get surface formats
            vkAssert (vkGetPhysicalDeviceSurfaceFormatsKHR (Current_Physical_Device, Surface, Count'Unchecked_Access, null));
            declare Surface_Formats : aliased array VkSurfaceFormatKHR (1..Int (Count)); begin
              vkAssert (vkGetPhysicalDeviceSurfaceFormatsKHR (Current_Physical_Device, Surface, Count'Unchecked_Access, Surface_Formats'Unchecked_Access));

              -- Verify the device has at least one of the supported surface depth formats
              Inner: for Supported_Surface_Format of SUPPORTED_SURFACE_FORMATS loop
                vkGetPhysicalDeviceFormatProperties (Current_Physical_Device, format, Physical_Device_Properties'Unchecked_Access);
                if (Device_Properties.optimalTilingFeatures and VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT) > 0 then
                  Current_Surface_Format := Supported_Surface_Format;
                  exit Inner;
                end if;
                Assert (Supported_Surface_Format /= SUPPORTED_SURFACE_FORMATS'Last);
              end loop Inner;
            end;

            -- Create a virtual device for testing
            vkAssert (vkCreateDevice (Current_Physical_Device, Device_Info'Unchecked_Access, null, Current_Device'Unchecked_Access));

            -- Get extensions
            vkEnumerateDeviceExtensionProperties (Current_Device, null, Count'Unchecked_Access, null);
            declare Extensions : aliased array (1..Int (Count)) of VkExtensionProperties; begin
              vkEnumerateDeviceExtensionProperties(Current_Device, null, Count'Unchecked_Access, availableExtensions.data);

              -- Verify required extensions
              for Required_Extension of Join (REQUIRED_EXTENSIONS, Get_Extensions) loop
                Inner: for Extension of Extensions loop
                  exit Inner when Extension := Required_Extension;
                  Assert (Extension /= Extensions'Last);
                end loop;
              end loop Inner;
            end;

            -- Get device surface presentation mode information
            vkGetPhysicalDeviceSurfacePresentModesKHR (Current_Device, Surface, Count'Unchecked_Access, null);
            declare Present_Modes : aliased array  (1..Int (Count)); begin
              vkGetPhysicalDeviceSurfacePresentModesKHR (Current_Device, Surface, Count'Unchecked_Access, Present_Modes'Unchecked_Access);

              -- Set the default present mode to first-in-first-out which is the only mode guarenteed to be available
              Surface_Present_Mode := VK_PRESENT_MODE_FIFO_KHR;

              -- Look for the mailbox mode so we can implement triple buffering
              for Present_Mode of Present_Modes loop
                if Present_Mode := VK_PRESENT_MODE_MAILBOX_KHR then Surface_Present_Mode := Present_Mode; end if;
              end loop;
            end;

            -- Get physical device and virtual device properties and compare to the current best
            vkGetPhysicalDeviceSurfaceCapabilitiesKHR (Current_Device, Surface, Current_Surface_Capabilities'Unchecked_Access);
            vkGetPhysicalDeviceProperties             (Current_Physical_Device, Current_Device_Properties'Unchecked_Access);
            vkGetPhysicalDeviceMemoryProperties       (Current_Physical_Device, Current_Memory_Properties'Unchecked_Access);

            -- Check if the current device is better than best found or if it is the first
            if Current_Physical_Device := NULL_PTR

            -- Prefer mailbox mode, but don't require it
            or ((Surface_Present_Mode := VK_PRESENT_MODE_FIFO_KHR or Current_Surface_Present_Mode := VK_PRESENT_MODE_MAILBOX_KHR)

                -- Perfer physical devices with more VRAM (not the most exact method of determining performance, but good enough)
                and (Current_Physical_Device )
            then
              Surface_Present_Mode := Current_Surface_Present_Mode;
              Surface_Capabilities := Current_Surface_Capabilities;
              Device_Properties    := Current_Physical_Device_Properties;
              Memory_Properties    := Current_Memory_Properties;
              Surface_Format       := Current_Surface_Format;
              Queue_Family_Index   := Current_Queue_Family_Index;
              Queue_Present_Family := Current_Queue_Present_Family;
            end if;

          -- Ignore unsuitable devices
          exception when others => end;

          -- Destroy the current device created for testing purposes
          if Device /= null then
            vkDestroyDevice (Current_Device, null);
            Current_Device := null;
          end if;
        end loop;

        -- We may have failed to find a device, so crash
        Assert (Physical_Device);

        -- Create the logical device based on the selected physical device and fetch the appropriate queues
        vkAssert (vkCreateDevice (Current_Physical_Device, Device_Info'Unchecked_Access, null, Current_Device'Unchecked_Access));
        vkGetDeviceQueue (Current_Device, Queue_Index_Graphics, 0, Queue_Graphics'Unchecked_Access);
        vkGetDeviceQueue (Current_Device, Queue_Index_Present,  0, Queue_Present'Unchecked_Access);

        -- Log device information
        Line ("API Version:"    & Img (Device_Properties.apiVersion));
        Line ("Driver Version:" & Img (Device_Properties.driverVersion));
        Line ("Vender ID:"      & Img (Device_Properties.vendorID));
        Line ("Device ID:"      & Img (Device_Properties.deviceID));
        Line ("Device Type:"    & Img (Device_Properties.deviceType));
        Line ("Device Name: "   & To_Str (Physical_Device_Properties.deviceName));
      end;

      -- Create swap chain and ready the surface
      Update;

      -- Create command pool
      vkAssert (vkCreateCommandPool (Device, Command_Pool_Info'Unchecked_Access, null, Command_Pool'Unchecked_Access));

      -- Create descriptor sets
      vkAssert (vkCreateDescriptorSetLayout (Device, Layout_Info'Unchecked_Access,   null, descriptorSetLayout.replace));
      vkAssert (vkCreateDescriptorPool      (Device, Pool_Info'Unchecked_Access,     null, descriptorPool.replace))
      vkAssert (vkAllocateDescriptorSets    (Device, Allocate_Info'Unchecked_Access, &descriptorSet));
      vkAssert (vkUpdateDescriptorSets      (Device, Write_Set'Length,               Write_Set, 0, null);
 
      -- Create semaphores
      vkAssert (vkCreateSemaphore (Device, Semaphore_Info'Unchecked_Access, null, Acquire_Status'Unchecked_Access));
      vkAssert (vkCreateSemaphore (Device, Semaphore_Info'Unchecked_Access, null, Render_Status'Unchecked_Access));

      -- Start the thing
      Frontend.Initialize;
      Backend.Initialize;
    end;

  -- Kill the global instance state
  procedure Finalize is
    begin
      vkAssert (vkDeviceWaitIdle (Device));
      vkDestroySemaphore         (Device, Acquire_Status, null);
      vkDestroySemaphore         (Device, Render_Status,  null);
      vkDestroyCommandPool       (Device, Command_Pool,   null);
      vkDestroySwapchainKHR      (Device, Swap_Chain,     null);
      vkDestroyDevice            (Device,                 null);
      vkDestroyInstance          (Instance,               null);
      Finalize_Vulkan_Library;
    end;

  -- Recreate the swapchain among other things
  procedure Update is
    -- Count                  : aliased Int_32_Unsigned_C                      := 0;
    -- Pre_Transform          : aliased Int_32_Unsigned_C                      := 0;
    -- Setup_Buffer           : aliased Ptr                                    := null;

    -- Vertex_Shader_Model    : aliased VkShaderModule                         := Load_Shader ("shaders/vert.spv");
    -- Fragment_Shader_Model  : aliased VkShaderModule                         := Load_Shader ("shaders/frag.spv");
    -- Begin_Info             : aliased VkCommandBufferBeginInfo               := (others                  => <>);

    -- 
    Swap_Chain_Info : aliased VkSwapchainCreateInfoKHR := (flags  => VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT,
                                                           others => <>);

    --
    Memory_Barrier : aliased VkImageMemoryBarrier := (srcQueueFamilyIndex => VK_QUEUE_FAMILY_IGNORED,
                                                      dstQueueFamilyIndex => VK_QUEUE_FAMILY_IGNORED,
                                                      oldLayout           => VK_IMAGE_LAYOUT_UNDEFINED,
                                                      newLayout           => VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
                                                      subresourceRange    => (VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1),
                                                      others              => <>);

    --
    Setup_Buffer_Info : aliased VkCommandBufferAllocateInfo := (commandPool        => Command_Pool,
                                                                level              => VK_COMMAND_BUFFER_LEVEL_PRIMARY,
                                                                commandBufferCount => 1,
                                                                others             => <>);

    -- 
    Submit_Info : aliased VkSubmitInfo := (commandBufferCount => 1,
                                           pCommandBuffers    => Setup_Buffer'Unchecked_Access,
                                           others             => <>);

    -- 
    Color_Attachment : aliased VkAttachmentDescription := (format         => swapChainImageFormat,
                                                           samples        => VK_SAMPLE_COUNT_1_BIT,
                                                           loadOp         => VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                           storeOp        => VK_ATTACHMENT_STORE_OP_STORE,
                                                           stencilLoadOp  => VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                           stencilStoreOp => VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                           initialLayout  => VK_IMAGE_LAYOUT_UNDEFINED,
                                                           finalLayout    => VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
                                                           others         => <>);

    -- 
    Depth_Attachment : aliased VkAttachmentDescription := (format         => findDepthFormat,
                                                           samples        => VK_SAMPLE_COUNT_1_BIT,
                                                           loadOp         => VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                           storeOp        => VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                           stencilLoadOp  => VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                           stencilStoreOp => VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                           initialLayout  => VK_IMAGE_LAYOUT_UNDEFINED,
                                                           finalLayout    => VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                                           others         => <>);

    -- 
    Color_Reference : aliased VkAttachmentReference := (attachment => 0,
                                                        layout     => VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                                        others     => <>);

    -- 
    Depth_Reference : aliased VkAttachmentReference := (attachment => 1,
                                                        layout     => VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                                        others     => <>);

    -- 
    Subpass_Description : aliased VkSubpassDescription := (pipelineBindPoint       => VK_PIPELINE_BIND_POINT_GRAPHICS,
                                                           colorAttachmentCount    => 1,
                                                           pColorAttachments       => &colorAttachmentRef,
                                                           pDepthStencilAttachment => &depthAttachmentRef,
                                                           others                  => <>);

    -- 
    Subpass_Dependency : aliased VkSubpassDependency := (srcSubpass    => VK_SUBPASS_EXTERNAL,
                                                         dstSubpass    => 0,
                                                         srcStageMask  => VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                                                         srcAccessMask => 0,
                                                         dstStageMask  => VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                                                         dstAccessMask => VK_ACCESS_COLOR_ATTACHMENT_READ_BIT or VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
                                                         others        => <>);

    -- 
    -- Attachments            : aliased Array_VkAttachmentDescription          := (colorAttachment, depthAttachment);
    -- Render_Pass_Info       : aliased VkRenderPassCreateInfo                 := (attachmentCount         => attachments.size,
    --                                                                             pAttachments            => attachments.data,
    --                                                                             subpassCount            => 1,
    --                                                                             pSubpasses              => &subpass,
    --                                                                             dependencyCount         => 1,
    --                                                                             pDependencies           => &dependency,
    --                                                                             others                  => <>);
    -- Vertex_Stage_Info      : aliased VkPipelineShaderStageCreateInfo        := (stage                   => VK_SHADER_STAGE_VERTEX_BIT,
    --                                                                             module                  => Vertex_Shader_Model,
    --                                                                             pName                   => "main",
    --                                                                             others                  => <>);
    -- Fragment_Stage_Info    : aliased VkPipelineShaderStageCreateInfo        := (stage                   => VK_SHADER_STAGE_FRAGMENT_BIT,
    --                                                                             module                  => fragShaderModule,
    --                                                                             pName                   => "main",
    --                                                                             others                  => <>);
    -- Shader_Stages_Info     : aliased VkPipelineShaderStageCreateInfo        := (vertShaderStageInfo, fragShaderStageInfo);
    -- Vertex_Input_Info      : aliased VkPipelineVertexInputStateCreateInfo   := (vertexBindingCount      => 1,
    --                                                                             vertexAttributeCount    => Vertex::getAttributeDescriptions;.size,
    --                                                                             pVertexBindingDesc      => &Vertex::getBindingDescription;,
    --                                                                             pVertexAttributeDesc    => Vertex::getAttributeDescriptions;.data,
    --                                                                             others                  => <>);
    -- Input_Assembly_Info    : aliased VkPipelineInputAssemblyStateCreateInfo := (topology                => VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
    --                                                                             primitiveRestartEnable  => 0,
    --                                                                             others                  => <>);
    -- Viewport               : aliased VkViewport                             := (x                       => 0.0,
    --                                                                             y                       => 0.0,
    --                                                                             width                   => Real_C (swapChainExtent.width),
    --                                                                             height                  => Real_C (swapChainExtent.height),
    --                                                                             minDepth                => 0.0,
    --                                                                             maxDepth                => 1.0,
    --                                                                             others                  => <>);
    -- Scissor                : aliased VkRect2D                               := (offset                  => (0, 0),
    --                                                                             extent                  => swapChainExtent,
    --                                                                             others                  => <>);
    -- Viewport_State_Info    : aliased VkPipelineViewportStateCreateInfo      := (viewportCount           => 1,
    --                                                                             pViewports              => &viewport,
    --                                                                             scissorCount            => 1,
    --                                                                             pScissors               => &scissor,
    --                                                                             others                  => <>);
    -- Rasterizer_Info        : aliased VkPipelineRasterizationStateCreateInfo := (depthClampEnable        => 0,
    --                                                                             rasterizerDiscardEnable => 0,
    --                                                                             polygonMode             => VK_POLYGON_MODE_FILL,
    --                                                                             lineWidth               => 1.0,
    --                                                                             cullMode                => VK_CULL_MODE_BACK_BIT,
    --                                                                             frontFace               => VK_FRONT_FACE_COUNTER_CLOCKWISE,
    --                                                                             depthBiasEnable         => 0,
    --                                                                             others                  => <>);
    -- Multisample_Info       : aliased VkPipelineMultisampleStateCreateInfo   := (sampleShadingEnable     => 0,
    --                                                                             rasterizationSamples    => VK_SAMPLE_COUNT_1_BIT,
    --                                                                             others                  => <>);
    -- Depth_Stencil_Info     : aliased VkPipelineDepthStencilStateCreateInfo  := (depthTestEnable         => 1,
    --                                                                             depthWriteEnable        => 1,
    --                                                                             depthCompareOp          => VK_COMPARE_OP_LESS,
    --                                                                             depthBoundsTestEnable   => 0,
    --                                                                             stencilTestEnable       => 0,
    --                                                                             others                  => <>);
    -- Color_Blend_Attachment : aliased VkPipelineColorBlendAttachmentState    := (blendEnable             => 0
    --                                                                             colorWriteMask          => VK_COLOR_COMPONENT_R_BIT or VK_COLOR_COMPONENT_G_BIT or
    --                                                                                                        VK_COLOR_COMPONENT_B_BIT or VK_COLOR_COMPONENT_A_BIT,
    --                                                                             others                  => <>);
    -- Color_Blend_Info       : aliased VkPipelineColorBlendStateCreateInfo    := (logicOpEnable           => 0,
    --                                                                             logicOp                 => VK_LOGIC_OP_COPY,
    --                                                                             attachmentCount         => 1,
    --                                                                             pAttachments            => &colorBlendAttachment,
    --                                                                             blendConstants          => (others => 0.0),
    --                                                                             others                  => <>);
    -- Descriptor_Set_Layout  : aliased VkDescriptorSetLayout                  := (descriptorSetLayout); -- ???
    -- Pipeline_Layout_Info   : aliased VkPipelineLayoutCreateInfo             := (setLayoutCount          => 1,
    --                                                                             pSetLayouts             => setLayouts,
    --                                                                             others                  => <>);
    -- Pipeline_Info          : aliased VkGraphicsPipelineCreateInfo           := (stageCount              => 2,
    --                                                                             pStages                 => shaderStages,
    --                                                                             pVertexInputState       => &vertexInputInfo,
    --                                                                             pInputAssemblyState     => &inputAssembly,
    --                                                                             pViewportState          => &viewportState,
    --                                                                             pRasterizationState     => &rasterizer,    2
    --                                                                             pMultisampleState       => &multisampling,
    --                                                                             pDepthStencilState      => &depthStencil,
    --                                                                             pColorBlendState        => &colorBlending,
    --                                                                             layout                  => pipelineLayout,
    --                                                                             renderPass              => renderPass,
    --                                                                             subpass                 => 0,
    --                                                                             basePipelineHandle      => VK_NULL_HANDLE,
    --                                                                             others                  => <>);
    -- Frame_Buffer           : aliased VkFramebufferCreateInfo                := (others                  => <>);
    -- Render_Pass_Begin_Info : aliased VkRenderPassBeginInfo                  := (renderArea              => (offset => (0, 0), extent => swapChainExtent),
    --                                                                             others                  => <>);
    -- Allocate_Info          : aliased VkCommandBufferAllocateInfo            := (commandPool             => commandPool;
    --                                                                             level                   => VK_COMMAND_BUFFER_LEVEL_PRIMARY;
    --                                                                             commandBufferCount      => (uint32_t) commandBuffers.size; 
    --                                                                             others                  => <>);
    -- Clear_Values           : aliased Array_VkClearValue                     := ((color                  => (0.0, 0.0, 0.0, 1.0),
    --                                                                              others                 => <>),
    --                                                                             (depthStencil           => (1.0, 0.0),
    --                                                                              others                 => <>));
    -- Vertex_Buffers         : aliased array VkBuffer                         := (vertexBuffer);
    -- Offsets                : aliased array VkDeviceSize                     := (0);
    begin

      -- Find the resolution of the swap chain images - some window managers force us to pick the resolution that best matches the window
      Swap_Extent := (if Surface_Capabilities.currentExtent /= Int_32_Unsigned_C'Last then Surface_Capabilities.currentExtent
                      else (width  => Int_32_Unsigned_C'Max (Surface_Capabilities.minImageExtent.width,  Int_32_Unsigned_C (Window_Width.Get)),
                            height => Int_32_Unsigned_C'Max (Surface_Capabilities.minImageExtent.height, Int_32_Unsigned_C (Window_Height.Get))));
      -- Pick the queue length or number of images in the swap-chain - push for one more than the minimum for triple buffering
      Desired_Images := (if Surface_Capabilities.maxImageCount > 0 and Surface_Capabilities.minImageCount + 1 > Surface_Capabilities.maxImageCount then Surface_Capabilities.maxImageCount
                         else Surface_Capabilities.minImageCount + 1);

      -- Set the swap chain image details
      Swap_Chain_Info := (surface          => Surface,
                          minImageCount    => Desired_Images,
                          imageFormat      => Surface_Format.format,
                          imageColorSpace  => Surface_Format.colorSpace,
                          imageExtent      => Swap_Extent,
                          imageUsage       => VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
                          oldSwapchain     => Swap_Chain,
                          imageArrayLayers => 1,
                          preTransform     => Surface_Capabilities.currentTransform,
                          compositeAlpha   => VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
                          presentMode      => Present_Mode,
                          clipped          => 1,
                          others           => <>);

      -- Use multiple queues, one for graphics and one for presentation. If they differ use concurrent mode to avoid ownership... for now
      if Graphics_Family /= Present_Family then
        Swap_Chain_Info.imageSharingMode      := VK_SHARING_MODE_CONCURRENT;
        Swap_Chain_Info.queueFamilyIndexCount := 2;
        Swap_Chain_Info.pQueueFamilyIndices   := queueFamilyIndices;
      else Swap_Chain_Info.imageSharingMode := VK_SHARING_MODE_EXCLUSIVE; end if;

      -- Create the swapchain
      vkAssert (vkCreateSwapchainKHR (Device, Swap_Chain_Info'Unchecked_Access, null, Swap_Chain'Unchecked_Access));

      -- Explicitly query the number of desired images
      vkAssert (vkGetSwapchainImagesKHR (Device, Swap_Chain, Count'Unchecked_Access, null));
      Images := new Array_Ptr (1.. Int (Count));
      vkAssert (vkGetSwapchainImagesKHR (Device, Swap_Chain, Count'Unchecked_Access, Images));

      -- Create image views
      for I in 1..Images'Length loop
        Image_Views (I) := Make_Image_View (Images (I), swapChainImageFormat, VK_IMAGE_ASPECT_COLOR_BIT);
      end loop;

      -- Create the renderer
      vkAssert (vkCreateRenderPass        (Device, &Render_Pass_Info, null, renderPass.replace));
      vkAssert (vkCreatePipelineLayout    (Device, &pipelineLayoutInfo, null, pipelineLayout.replace);
      vkAssert (vkCreateGraphicsPipelines (Device, VK_NULL_HANDLE, 1, &pipelineInfo, null, graphicsPipeline.replace);

      -- Create depth resource
      Create_Image (swapChainExtent.width, swapChainExtent.height, depthFormat, VK_IMAGE_TILING_OPTIMAL, VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, depthImage, depthImageMemory);
      Create_Image_View (depthImage, depthFormat, VK_IMAGE_ASPECT_DEPTH_BIT, depthImageView);
      Image_Layout (depthImage, depthFormat, VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL);

      -- Create the image frame buffers
      for I in 1..Image_Views'Range loop
        Frame_Buffer := (renderPass      => renderPass;
                         attachmentCount => attachments.size;
                         pAttachments    => attachments.data;
                         width           => swapChainExtent.width;
                         height          => swapChainExtent.height;
                         layers          => 1;
                         others          => <>);
        vkAssert (vkCreateFramebuffer (Device, &framebufferInfo, null, swapChainFramebuffers[i].replace));
      end loop;

      -- Create command buffers
      if Command_Buffers'Length > 0 then vkFreeCommandBuffers (Device, Command_Pool commandBuffers.size, commandBuffers.data); end if;
      commandBuffers.resize (swapChainFramebuffers.size);
      vkAssert (vkAllocateCommandBuffers(Device, &allocInfo, commandBuffers.data));
      for I in Command_Buffers'Range loop
        vkBeginCommandBuffer (Command_Buffers (I), Begin_Info'Unchecked_Access);
        Render_Pass_Info.renderPass      := renderPass;
        Render_Pass_Info.framebuffer     := swapChainFramebuffers[i];
        Render_Pass_Info.clearValueCount := clearValues.size;
        Render_Pass_Info.pClearValues    := clearValues.data;
        vkCmdBeginRenderPass         (Command_Buffers (I), Render_Pass_Info'Unchecked_Access, VK_SUBPASS_CONTENTS_INLINE);
        vkCmdBindPipeline            (Command_Buffers (I), VK_PIPELINE_BIND_POINT_GRAPHICS, graphicsPipeline);
        vkCmdBindVertexBuffers       (Command_Buffers (I), 0, 1, vertexBuffers, offsets);
        vkCmdBindIndexBuffer         (Command_Buffers (I), indexBuffer, 0, VK_INDEX_TYPE_UINT32);
        vkCmdBindDescriptorSets      (Command_Buffers (I), VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout, 0, 1, descriptorSet'Unchecked_Access, 0, null);
        vkCmdDrawIndexed             (Command_Buffers (I), indices.size, 1, 0, 0, 0);
        vkCmdEndRenderPass           (Command_Buffers (I));
        vkAssert (vkEndCommandBuffer (Command_Buffers (I)));
      end loop;
    end;

  -- Acquire the rendered image, show it in the window, and handle changes to the resolution
  procedure Present is

    -- 
    Present_Info : aliased VkPresentInfoKHR := (swapchainCount     => 1,
                                                pSwapchains        => Swap_Chain'Unchecked_Access,
                                                pImageIndices      => Image_Index'Unchecked_Access,
                                                waitSemaphoreCount => 1,
                                                pWaitSemaphores    => Render_Status'Unchecked_Access,
                                                others             => <>);

    -- 
    Submit_Info : aliased VkSubmitInfo := (commandBufferCount => 1,
                                           pCommandBuffers    => Setup_Buffer'Unchecked_Access,
                                           waitSemaphoreCount => 1,
                                           pWaitSemaphores    => Render_Status'Unchecked_Access,
                                           others             => <>);
    begin null;

      -- Recreate the swapchain and pipeline of the resolution changes
      if Current_Width /= Window_Width.Get or Current_Height /= Window_Height.Get then
        Update;
        Current_Width  := Window_Width.Get;
        Current_Height := Window_Height.Get;
      end if;

      -- Fetch the next image from the swap chain
      case vkAcquireNextImageKHR (Device      => Device,
                                  swapchain   => Swap_Chain,
                                  timeout     => Int_64_Unsigned_C'Last,
                                  semaphore   => Acquire_Status,
                                  fence       => null,
                                  pImageIndex => Image_Index'Unchecked_Access) is
        when VK_SUCCESS | VK_SUBOPTIMAL_KHR => null;
        when VK_ERROR_OUT_OF_DATE_KHR => Recreate_Swap_Chain;
        when others => raise Program_Error with "Failed to acquire swap chain image!";
      end case;

      -- Show the resulting image
      case vkQueuePresentKHR (Queue, Present_Info'Unchecked_Access) is
        when VK_SUCCESS => null;
        when VK_ERROR_OUT_OF_DATE_KHR | VK_SUBOPTIMAL_KHR => Recreate_Swap_Chain;
        when others => raise Program_Error with "Failed to present swap chain image!";
      end case;
    end;
end;
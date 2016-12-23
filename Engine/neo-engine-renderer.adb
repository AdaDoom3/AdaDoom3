
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

  --------------------------
  -- Minimum Requirements --
  --------------------------
  --
  -- ???
  --

  DRIVER_BLACKLIST        : constant := ;
  DRIVER_GREYLIST         : constant := ;
  MINIMUM_GPU_SPEED       : constant := ;
  MINIMUM_VRAM            : constant := ;
  REQUIRED_EXTENSIONS     : constant := ;
  SUPPORTED_DEPTH_FORMATS : constant := VK_FORMAT_D32_SFLOAT, VK_FORMAT_D32_SFLOAT_S8_UINT, VK_FORMAT_D24_UNORM_S8_UINT},
                                       VK_IMAGE_TILING_OPTIMAL, VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT);

  -------------
  -- Globals --
  -------------
  --
  -- ???
  --

  Instance, Command_Pool, Queue, Surface, Swap_Chain,
  Render_Status, Acquire_Status,
  Physical_Device, Device : aliased Ptr := NULL_PTR;

  Current_Width      : aliased Natural                          := 0;
  Current_Height     : aliased Natural                          := 0;
  Image_Index        : aliased Int_32_Unsigned_C                := 0;
  Desired_Images     : aliased Int_32_Unsigned_C                := 0;
  Images             : aliased Ptr_Array_Ptr                    := null;
  Swap_Chain_Info    : aliased VkSwapchainCreateInfoKHR         := (others => <>);
  General_Properties : aliased VkPhysicalDeviceProperties       := (others => <>);
  Memory_Properties  : aliased VkPhysicalDeviceMemoryProperties := (others => <>);
  Surface_Forma      : aliased VkSurfaceFormatKHR               := (others => <>);

  --------------------
  -- Command_Buffer --
  --------------------
  --
  -- ???
  --

  -- Controller
  type Command_Buffer_State is new Controlled with record
      Data : Ptr := NULL_PTR;
    end record;
  procedure Initialize (Command_Buffer : in out Command_Buffer_State);
  procedure Finalize   (Command_Buffer : in out Command_Buffer_State);

  -- Controlled primatives
  procedure Initialize (Command_Buffer : in out Command_Buffer_State) is
      Begin_Info    : aliased VkCommandBufferBeginInfo    := (flags              => VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
                                                              others             => <>);
      Allocate_Info : aliased VkCommandBufferAllocateInfo := (level              => VK_COMMAND_BUFFER_LEVEL_PRIMARY,
                                                              commandPool        => commandPool,
                                                              commandBufferCount => 1,
                                                              others             => <>);
    begin
      vkAllocateCommandBuffers (device, Allocate_Info'Unchecked_Access, Command_Buffer'Unchecked_Access);
      vkBeginCommandBuffer (Command_Buffer, &beginInfo);
    end;
  procedure Finalize (Command_Buffer : in out Command_Buffer_State) is
    Submit_Info : aliased VkSubmitInfo := (commandBufferCount => 1,
                                           pCommandBuffers    => Command_Buffer'Unchecked_Access,
                                           others             => <>);
    begin
      vkEndCommandBuffer (Command_Buffer);
      vkQueueSubmit (graphicsQueue, 1, &submitInfo, VK_NULL_HANDLE);
      vkQueueWaitIdle (graphicsQueue);
      vkFreeCommandBuffers (device, commandPool, 1, Command_Buffer'Unchecked_Access);
    end;

  -----------
  -- Image --
  -----------
  --
  -- ???
  --

  -- Controller
  type Image_State (Width, Height, Tiling, Usage, Properties : Int_32_Unsigned_C) is new Controlled with record
      Properties : Int_32_Unsigned_C;
      Layout     : Int_32_Unsigned_C;
      Data       : Ptr;
    end record;
  procedure Initialize (Image : in out Image_State);
  procedure Finalize   (Image : in out Image_State);

  -- Copy used for staging
  function Copy (Image : in out Image_State) return Image_State is
    Command_Buffer : aliased VkCommandBuffer          := (others         => <>); 
    Subresource    : aliased VkImageSubresourceLayers := (aspectMask     => VK_IMAGE_ASPECT_COLOR_BIT,
                                                          baseArrayLayer => 0,
                                                          mipLevel       => 0,
                                                          layerCount     => 1,
                                                          others         => <>);
    Image_Copy     : aliased VkImageCopy              := (srcSubresource => Subresource,
                                                          dstSubresource => Subresource,
                                                          srcOffset      => (0, 0, 0),
                                                          dstOffset      => (0, 0, 0),
                                                          extent         => (Width, Height, depth => 1),
                                                          others         => <>);
    begin
      Begin_Commands (Command_Buffer);
      vkCmdCopyImage (commandBuffer  => Command_Buffer,
                      srcImage       => srcImage,
                      srcImageLayout => VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                      dstImage       => dstImage,
                      dstImageLayout => VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                      regionCount    => 1,
                      pRegions       => Image_Copy'Unchecked_Access);
      End_Commands (Command_Buffer);
    end;
 
  -- Grab a view 
  function Make_View (Image : in out Image_State) return ___ is
    Image_View_Info : aliased VkImageViewCreateInfo := (image            => image,
                                                        viewType         => VK_IMAGE_VIEW_TYPE_2D,
                                                        format           => format,
                                                        subresourceRange => (aspectMask     => aspectFlags,
                                                                             baseMipLevel   => 0,
                                                                             levelCount     => 1,
                                                                             baseArrayLayer => 0,
                                                                             layerCount     => 1,
                                                                             others         => <>), others => <>);
    begin
      vkAssert (vkCreateImageView(Device, &viewInfo, NULL_PTR, imageView.replace())))
      return Result;
    end;

  -- Controlled primatives
  procedure Initialize (Image : in out Image_State) is
    Memory_Requirements : aliased VkMemoryRequirements := (others         => <>);
    Image_Info          : aliased VkImageCreateInfo    := (imageType      => VK_IMAGE_TYPE_2D,
                                                           extent         => (width, height, depth => 1),
                                                           mipLevels      => 1,
                                                           arrayLayers    => 1,
                                                           format         => format,
                                                           tiling         => tiling,
                                                           initialLayout  => VK_IMAGE_LAYOUT_PREINITIALIZED,
                                                           usage          => usage,
                                                           samples        => VK_SAMPLE_COUNT_1_BIT,
                                                           sharingMode    => VK_SHARING_MODE_EXCLUSIVE,
                                                           others         => <>);
    Allocate_Info      : aliased VkMemoryAllocateInfo  := (allocationSize => Memory_Requirements.size,
                                                           others         => <>);
    begin
      vkAssert (vkCreateImage (Device, Image_Info'Unchecked_Access, NULL_PTR, image.replace()));
      vkGetImageMemoryRequirements (Device, image, Memory_Requirements'Unchecked_Access);
      vkAllocateMemory (Device, Allocate_Info'Unchecked_Access, NULL_PTR, imageMemory.replace())
      vkBindImageMemory (Device, image, imageMemory, 0);
    end;
  procedure Finalize (Image : in out Image_State) is
    begin
      null;
    end;
  procedure Adjust (Image : in out Image_State) is
    Command_Buffer : aliased VkCommandBuffer      := (others              => <>); 
    Barrier        : aliased VkImageMemoryBarrier := (oldLayout           => oldLayout,
                                                      newLayout           => newLayout,
                                                      srcQueueFamilyIndex => VK_QUEUE_FAMILY_IGNORED,
                                                      dstQueueFamilyIndex => VK_QUEUE_FAMILY_IGNORED,
                                                      image               => image);
    begin

      -- ???
      if newLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL then
        Barrier.subresourceRange.aspectMask := VK_IMAGE_ASPECT_DEPTH_BIT;
        if format = VK_FORMAT_D32_SFLOAT_S8_UINT or format = VK_FORMAT_D24_UNORM_S8_UINT then
          Barrier.subresourceRange.aspectMask := Barrier.subresourceRange.aspectMask or VK_IMAGE_ASPECT_STENCIL_BIT;
        end if;
      else Barrier.subresourceRange.aspectMask := VK_IMAGE_ASPECT_COLOR_BIT; end if;

      -- ???
      Barrier.subresourceRange.baseMipLevel   := 0;
      Barrier.subresourceRange.levelCount     := 1;
      Barrier.subresourceRange.baseArrayLayer := 0;
      Barrier.subresourceRange.layerCount     := 1;

      -- Set new image layout
      if oldLayout = VK_IMAGE_LAYOUT_PREINITIALIZED and newLayout = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL then
          Barrier.srcAccessMask := VK_ACCESS_HOST_WRITE_BIT;
          Barrier.dstAccessMask := VK_ACCESS_TRANSFER_READ_BIT;
      elsif oldLayout = VK_IMAGE_LAYOUT_PREINITIALIZED and newLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL then
          Barrier.srcAccessMask := VK_ACCESS_HOST_WRITE_BIT;
          Barrier.dstAccessMask := VK_ACCESS_TRANSFER_WRITE_BIT;
      elsif oldLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL and newLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL then
          Barrier.srcAccessMask := VK_ACCESS_TRANSFER_WRITE_BIT;
          Barrier.dstAccessMask := VK_ACCESS_SHADER_READ_BIT;
      elsif oldLayout = VK_IMAGE_LAYOUT_UNDEFINED and newLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL then
          Barrier.srcAccessMask := 0;
          Barrier.dstAccessMask := VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT or VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
      else raise Program_Error with "Unknown image layout transition?!"; end if;

      -- Send transition commands
      Begin_Commands (Command_Buffer);
      vkCmdPipelineBarrier (commandBuffer            => Command_Buffer,
                            srcStageMask             => VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT,
                            dstStageMask             => VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT,
                            dependencyFlags          => 0,
                            memoryBarrierCount       => 0,
                            pMemoryBarriers          => NULL_PTR,
                            bufferMemoryBarrierCount => 0,
                            pBufferMemoryBarriers    => NULL_PTR,
                            imageMemoryBarrierCount  => 1,
                            pImageMemoryBarriers     => Barrier);
      End_Commands (Command_Buffer);
    end;

  -------------
  -- Texture --
  -------------
  --
  -- ???
  --

  -- Controller
  type Texture_State is new Controlled with record
    end record;
  procedure Initialize (Texture : in out Texture_State);
  procedure Finalize   (Texture : in out Texture_State);

  -- Controlled primatives
  procedure Initialize (Texture : in out Texture_State) is
    VkImageSubresource subresource = {};
    subresource.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
    subresource.mipLevel = 0;
    subresource.arrayLayer = 0;
    VkSubresourceLayout stagingImageLayout;
    vkGetImageSubresourceLayout(Device, stagingImage, &subresource, &stagingImageLayout);
    void* data;
    VDeleter<VkImage> stagingImage{Device, vkDestroyImage};
    VDeleter<VkDeviceMemory> stagingImageMemory{Device, vkFreeMemory};
    createImage(texWidth, texHeight, VK_FORMAT_R8G8B8A8_UNORM, VK_IMAGE_TILING_LINEAR, VK_IMAGE_USAGE_TRANSFER_SRC_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, stagingImage, stagingImageMemory);
    begin
      vkMapMemory (Device, stagingImageMemory, 0, imageSize, 0, &data);

      -- Pad 
      if stagingImageLayout.rowPitch = texWidth * 4 then memcpy (data, pixels, imageSize);
      else
          uint8_t* dataBytes = reinterpret_cast<uint8_t*>(data);
          for (int y = 0; y < texHeight; y++) {
              memcpy(&dataBytes[y * stagingImageLayout.rowPitch], &pixels[y * texWidth * 4], texWidth * 4);
      end if;
      vkUnmapMemory (Device, stagingImageMemory);
      createImage (texWidth, texHeight, VK_FORMAT_R8G8B8A8_UNORM, VK_IMAGE_TILING_OPTIMAL, VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, textureImage, textureImageMemory);
      stagingImagee.Properties := VK_FORMAT_R8G8B8A8_UNORM, VK_IMAGE_LAYOUT_PREINITIALIZED, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL);
      textureImage.Properties := VK_FORMAT_R8G8B8A8_UNORM, VK_IMAGE_LAYOUT_PREINITIALIZED, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
      copyImage(stagingImage, textureImage, texWidth, texHeight);
      transitionImageLayout(textureImage, VK_FORMAT_R8G8B8A8_UNORM, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
      return True;
    end;
  procedure Finalize (Texture : in out Texture_State) is
    begin
      null;
    end;

  ------------
  -- Buffer --
  ------------
  --
  -- ???
  --

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
                                                         commandPool        => commandPool,
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
      vkQueueSubmit            (graphicsQueue, 1, &submitInfo, VK_NULL_HANDLE);
      vkQueueWaitIdle          (graphicsQueue);
      vkFreeCommandBuffers     (Device, commandPool, 1, Command_Buffer'Unchecked_Access);
    end;

  -- Controlled primitives
  procedure Initialize (Buffer : in out Buffer_State) is
    Memory_Requirements : aliased VkMemoryRequirements := (others         => <>);
    Allocate_Info       : aliased VkMemoryAllocateInfo := (allocationSize => Memory_Requirements.size,
    Buffer_Info         : aliased VkBufferCreateInfo   := (size           => Size,
                                                           usage          => Usage,
                                                           sharingMode    => VK_SHARING_MODE_EXCLUSIVE,
                                                           others         => <>);
    begin
      vkAssert (vkCreateBuffer (Device, Buffer_Info'Unchecked_Access, NULL_PTR, buffer.replace()));
      for I in 1..Memory_Properties.memoryTypeCount loop
        if (Memory_Properties.memoryTypeBits and 2**I) > 0 and (Memory_Properties.memoryTypes (I).propertyFlags and Properties) = Properties then
          Allocate_Info.memoryTypeIndex := I;
          exit;
        elsif I = Memory_Properties.memoryTypeCount then raise Program_Error with "Failed to find suitable memory type!?"; end if;
      end loop;
      vkAssert (vkAllocateMemory (Device, Allocate_Info'Unchecked_Access, NULL_PTR, bufferMemory.replace()));
      vkBindBufferMemory (Device, buffer, bufferMemory, 0);
      return ;
    end;
  procedure Finalize (Buffer : in out Buffer_State) is
    begin
      null;
    end;

  -------------
  -- Surface --
  -------------
  --
  -- ???
  --

  -- Controller
  type Surface_State is new Controlled with record
      Index_Buffer  : aliased Ptr := NULL_PTR;
      Vertex_Buffer : aliased Ptr := NULL_PTR;
    end record;  
  procedure Initialize (Surface : in out Surface_State);
  procedure Finalize   (Surface : in out Surface_State);

  -- Controlled primitives
  procedure Initialize (Surface : in out Surface_State) is
    void* data;
    Raw_Indicies   : aliased Array_Real_32_C (1..Indicies'Length * 3) := (others => 0.0); -- For-expression possible ???
    Staging        : aliased VkBuffer                                 := (others => <>);
    Staging_Memory : aliased VkDeviceMemory                           := (others => <>);
    Buffer_Size    : aliased VkDeviceSize                             := Raw_Indicies'Object_Size;
    begin

      -- Create vertex buffer
      createBuffer(bufferSize, VK_BUFFER_USAGE_TRANSFER_SRC_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, stagingBuffer, stagingBufferMemory);
      vkMapMemory(Device, stagingBufferMemory, 0, bufferSize, 0, &data);
      memcpy(data, vertices.data(), (size_t) bufferSize);
      vkUnmapMemory(Device, stagingBufferMemory);
      createBuffer(bufferSize, VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_VERTEX_BUFFER_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, vertexBuffer, vertexBufferMemory);
      copyBuffer(stagingBuffer, vertexBuffer, bufferSize);

      -- Create index buffer
      createBuffer(bufferSize, VK_BUFFER_USAGE_TRANSFER_SRC_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, stagingBuffer, stagingBufferMemory);
      vkMapMemory(Device, stagingBufferMemory, 0, bufferSize, 0, &data);
      memcpy(data, indices.data(), (size_t) bufferSize);
      vkUnmapMemory(Device, stagingBufferMemory);
      createBuffer(bufferSize, VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_INDEX_BUFFER_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, indexBuffer, indexBufferMemory);
      copyBuffer(stagingBuffer, indexBuffer, bufferSize)   
    end;
  procedure Adjust (Surface : in out Surface_State) is
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
      vkAssert (vkQueueSubmit(graphicsQueue, 1, Submit_Info'Unchecked_Access, VK_NULL_HANDLE));
    end;
  procedure Finalize (Surface : in out Surface_State) is
    begin
      vkDestroyBuffer
      vkDestroyBuffer
      vkFreeMemory
      vkFreeMemory
    end;

  -----------------------
  -- Task_Render_Light --
  -----------------------

--   task body Task_Render_Light is
--     begin
--       loop

--         -------------
--         -- Prepare --
--         -------------

--         accept Prepare (Light : ) do
--           View.View_Entities.Append (Render_Lights.Shadow_Only_View_Entities);

--           -- Evaluate light shader registers

--           -- Additve lights with no stage in the light shader is positive, remove the light
--           if Light.Kind = Fog_Light or Light.Kind = Blend_Light then
--             Outter: for Stage of Light.Stages loop
--               if Light.Stage.Registers.Condition_Register then

--                 -- Snap tiny values to zero
--                 for Register of Light.Stage.Registers loop
--                   if Register < 0.001 then Register := 0.0; end if;
--                 end loop;

--                 -- We found a light that add something
--                 for Register of Light.Stage.Registers loop
--                   exit Outter when Register > 0.0;
--                 end loop;              
--               end if;
--               if Stage = Light.Stages.Last then 

--   -- We didn't find any addition to the scene
--   goto Skip_Light;
--               end if;
--             end loop Outter;
--           end if;

--           -- Copy date to backend

--           -- Create a "fog plane" to represent light far clip plane

--           -- Calculate the matrix that projects the zero-to-one cube to exactly cover the light frustum in clip space

--           -- Calculate the project bounds, either not clipped at all, near clipped, or fully clipped
        
--           -- Build the scissor

--           -- Create interactions with all entities the light may touch and add entities that may cast shadows
--           for Reference of Light.References loop

--             -- Some lights have their center of projection outside of the world so ignore areas not connected to the light center
--             if not Light.World.Areas_Are_Connected (Light.Area, Reference.Area, PS_BLOCK_VIEW) then

--               -- Check the models in this area
--               for Model of Reference.Entities loop
--                 if not (vLight->entityInteractionState[ edef->index ] != viewLight_t::INTERACTION_UNCHECKED or else
                        
--                         -- A large fraction of static entity / light pairs will still have no interactions even though they are both present in the same area(s)
--                         eModel != NULL && !eModel->IsDynamicModel() && inter == INTERACTION_EMPTY or else
                        
--                         -- We don't want the lights on weapons to illuminate anything else. There are two assumptions here - that
--                         -- allowLightInViewID is only used for weapon lights, and that all weapons will have weaponDepthHack. A more general
--                         -- solution would be to have an allowLightOnEntityID field.
--                         light->parms.allowLightInViewID && light->parms.pointLight && !eParms.weaponDepthHack or else

--                         --
--                         (eParms.noShadow || ( eModel && !eModel->ModelHasShadowCastingSurfaces() ) ) && !edef->IsDirectlyVisible() or else
--                         eModel && !eModel->ModelHasInteractingSurfaces() && !eModel->ModelHasShadowCastingSurfaces() or else
--                         inter == NULL and (eParms.noDynamicInteractions or R_CullModelBoundsToLight( light, edef->localReferenceBounds, edef->modelRenderMatrix )) or else 
--                         edef->IsDirectlyVisible() 
--                         !lightCastsShadows 

--               end loop;
--             end if;
--           end loop;
--         end
--         end;

--       ------------
--       -- Render --
--       ------------

--       accept Render ( : ) do
--            -- Note the usage of vertexcache.Position that abstrace VBO or not VBO
--           for Surface of View.Light_Surfaces loop

--             -- Prepare GPU data
--             qglColorPointer           (4, GL_UNSIGNED_BYTE, sizeof( idDrawVert ), vertexCache.Position( surf->geo->ambientCache->color );
--             qglVertexAttribPointerARB (11, 3, GL_FLOAT, false, sizeof( idDrawVert ), vertexCache.Position( surf->geo->ambientCache->normal.ToFloatPtr() );
--             qglVertexAttribPointerARB (10, 3, GL_FLOAT, false, sizeof( idDrawVert ), vertexCache.Position( surf->geo->ambientCache->tangents[1].ToFloatPtr() );
--             qglVertexAttribPointerARB (9, 3, GL_FLOAT, false, sizeof( idDrawVert ), vertexCache.Position( surf->geo->ambientCache->tangents[0].ToFloatPtr() );
--             qglVertexAttribPointerARB (8, 2, GL_FLOAT, false, sizeof( idDrawVert ), vertexCache.Position( surf->geo->ambientCache->st.ToFloatPtr() );
--             qglVertexPointer          (3, GL_FLOAT, sizeof( idDrawVert ), vertexCache.Position( surf->geo->ambientCache->xyz.ToFloatPtr() );

--             -- Modify the light projection if needed
--             if Surface.Space /= Backend.Current_Space then
--               Backend.Current_Space := Surface.Space;
--               qgLoadMatrixf (Surface.Space.Model_View_Matrix);
--             end if;

--             -- Modify light scissor if needed
--             if Surface.Scissor /= Backend.Current_Scissor then
--               Backend.Current_Scissor := Surface.Scissor;
              
--             end if;
--           end loop;
--         end;
-- <<Skip_Light>>
-- -- !!!

--       end loop;
--     end;
--   package Task_Render_Light is new Tasks (Task_Render_Light);
--   package Hashed_Render_Light is new Vectors (Task_Render_Light.Safe_Task);

  -------------------
  -- Task_Frontend --
  -------------------
  --
  -- ???
  --
  
  -- The frontend - also performs the step of filling the depth buffer Visible_Surface_Determination
  procedure Run_Frontend is
--     Area : Int;
--     procedure Point_In_Area is
--       begin
--       end;
--     procedure Flood_Portals is
--       begin
--       end;
    begin
--       for View of Views loop

--         -- Setup the view matrix
--         Viewer := (Axis.XX, Axis.YX, Axis.ZX, -Origin.X * Axis.XX - Origin.Y * YX - Origin.Z * Axis.ZX,
--                    Axis.XY, Axis.YY, Axis.ZY, -Origin.X * Axis.XY - Origin.Y * YY - Origin.Z * Axis.ZY,
--                    Axis.XZ, Axis.YZ, Axis.ZZ, -Origin.X * Axis.XZ - Origin.Y * YZ - Origin.Z * Axis.ZZ);

--         -- Setup the projection matrix (use the ol' "infinite far z trick")
--         Jitter_Values := (if Jitter.Get then (Random_Float, Random_Float) else  (others => <>));
--         R_SetupProjectionMatrix

--         -- Setup render matricies for faster culliong
--         View.Render_Projection := Transpose (View.Projection);
--         View.World_Space.MVP := View.Render_Projection * Transpose (View.World_Space.Model_View);
--         View.Frustum := ((4)(3) => Z_Near.Get, others => -Get_Frustum_Planes (View.World_Space.MVP));

--         -- Walk the BSP tree to find the current area
--         View.Level.Iterate (Point_In_Area);
--         View.Area := Area;

--         -- Add lights
--         for Light of View.Lights loop
--           if not Render_Lights.Has_Element (Light.Name) then
--             Render_Lights.Append (Light.Name);
--             Render_LightS.Element (Light.Name).Initialize (Light);
--           end if;
--         end loop;

--         -- Check each light
--         for Render_Light of Render_Lights loop
--           if not View.Lights.Has_Element (Render_Light.Name) or Render_Light.Area = -1 then
--             Render_Lights.Remove (Render_Light.Name);
--           end if;
--           Render_Light.Prepare;
--         end loop;

--         -- Determin all possible connected areas for light-behind-door culling
--         if Area = OUTSIDE_AREA then View.Connected_Areas := (others => True)
--         else View.Level.Portals.Iterate_Subtree (Flood_Portals); end if;

--         -- Add or remove GUI surfaces

--         -- Fill the depth buffer
--         for Mesh of View.Meshes loop
--           case Mesh.Material.Coverage is
--             when Opaque_Coverage =>
--             when Perferated_Coverage =>
--               for Stage of Mesh.Material.Stages loop
--               end loop;
--           when others => null; end case;
--         end loop;

--         -- Trigger light render
--         for Render_Light of Render_Lights loop Render_Light.Render; end loop;
--       end loop;
--     end;
--   package Task_Frontend is new Tasks (Run_Frontend);
--   Frontend : Task_Frontend.Safe_Task;

  -----------------
  -- Run_Backend --
  -----------------
  --
  -- ???
  --

  procedure Run_Backend is
    begin
      null;
      -- Post process
      -- Present
    end;

  ----------
  -- Main --
  ----------
  --
  -- ???
  --

  -- Get the game window ready for rendering and initialize the global variables in the spec
  procedure Initialize is 

    -- Load all of the function pointers from a driver
    procedure Initialize_Vulkan_Subprograms is new Neo.API.Vulkan.Initialize_Subprograms (Get_Vulkan_Subprogram);

    -- Create shader model
    function Load_Shader (Path : Str) return  is
      Shader_Module_Info : aliased VkShaderModuleCreateInfo := (codeSize => code.size();
                                                                pCode    => (uint32_t*) code.data(),
                                                                others   => <>);
      begin vkAssert (vkCreateShaderModule (Device, Shader_Module_Info'Unchecked_Access, NULL_PTR, shaderModule.replace())) end;
    
    -- Structures and variables for configuration
    Count, Surface_Support : aliased Int_32_Unsigned_C                      := 0;
    Queue_Priority         : aliased Real_32_C                              := 1.0; 
    Command_Pool_Info      : aliased VkCommandPoolCreateInfo                := (flags                   => VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
                                                                                others                  => <>);
    Semaphore_Info         : aliased VkSemaphoreCreateInfo                  := (flags                   => 0,
                                                                                others                  => <>);
    Application_Info       : aliased VkApplicationInfo                      := (pApplicationName        => To_Str_8_C (S (Get_Information.Name)),
                                                                                applicationVersion      => VK_MAKE_VERSION (1, 0, 0),
                                                                                pEngineName             => To_Str_8_C (NAME_ID & " " & VERSION),
                                                                                apiVersion              => VK_MAKE_VERSION (1, 0, 0),
                                                                                others                  => <>);
    Instance_Info          : aliased VkInstanceCreateInfo                   := (pApplicationInfo        => Application_Info'Unchecked_Access,
                                                                                enabledExtensionCount   => Enabled_Extensions.All'Length,
                                                                               ppenabledExtensionNames => Get_Extensions,
                                                                                others                  => <>);
    Queue_Info             : aliased VkDeviceQueueCreateInfo                := (queueCount              => 2, -- Double buffering
                                                                                pQueuePriorities        => Queue_Priority'Unchecked_Access,
                                                                                others                  => <>);
    Device_Features        : aliased VkPhysicalDeviceFeatures               := (shaderClipDistance      => 1,
                                                                                shaderCullDistance      => 1,
                                                                                geometryShader          => 1,
                                                                                shaderTessellationAndGeometryPointSize => 1,
                                                                                others                  => <>);
    Device_Info            : aliased VkDeviceCreateInfo                     := (queueCreateInfoCount    => 1,
                                                                                pQueueCreateInfos       => Queue_Info'Unchecked_Access,
                                                                                pEnabledFeatures        => Device_Features'Unchecked_Access,
                                                                                enabledExtensionCount   => sizeof kDevice_Extensions / sizeof *kDevice_Extensions, -- !!!
                                                                                ppEnabledExtensionNames => kDevice_Extensions; -- !!!
                                                                                enabledLayerCount       => sizeof kLayers / sizeof *kLayers,
                                                                                others                  => <>);
    UBO_Layout_Binding     : aliased VkDescriptorSetLayoutBinding           := (binding                 => 0,
                                                                                descriptorCount         => 1,
                                                                                descriptorType          => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                                                                pImmutableSamplers      => NULL_PTR,
                                                                                stageFlags              => VK_SHADER_STAGE_VERTEX_BIT,
                                                                                others                  => <>);
    Sampler_Layout_Binding : aliased VkDescriptorSetLayoutBinding           := (binding                 => 1,
                                                                                descriptorCount         => 1,
                                                                                descriptorType          => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                                                pImmutableSamplers      => NULL_PTR,
                                                                                stageFlags              => VK_SHADER_STAGE_FRAGMENT_BIT,
                                                                                others                  => <>);
    Layout_Bindings        : aliased Array_VkDescriptorSetLayoutBinding     := (UBO_Layout_Binding, Sampler_Layout_Binding);
    Layout_Info            : aliased VkDescriptorSetLayoutCreateInfo        := (bindingCount            => bindings.size(),
                                                                                pBindings               => bindings.data(),
                                                                                others                  => <>);
    Pool_Sizes             : aliased  := ((kind = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
                                           descriptorCount = 1;
                                           others => <>),
                                          (kind = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
                                           descriptorCount = 1;
                                           others => <>));
    Pool_Info              : aliased := (poolSizeCount = poolSizes.size();
                                          pPoolSizes = data();
                                          maxSets = 1;
                                          others => <>);
    VkDescriptorSetLayout layouts[] = {descriptorSetLayout};
    Allocate_Info          : aliased VkDescriptorSetAllocateInfo := (descriptorPool = descriptorPool;
                                                                    descriptorSetCount = 1;
                                                                    pSetLayouts = layouts;
                                                                    others => <>);

    VkDescriptorBufferInfo bufferInfo = {};
    bufferInfo.buffer = uniformBuffer;
    bufferInfo.offset = 0;
    bufferInfo.range = sizeof(UniformBufferObject);
    VkDescriptorImageInfo imageInfo = {};
    imageInfo.imageLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
    imageInfo.imageView = textureImageView;
    imageInfo.sampler = textureSampler;
    std::array<VkWriteDescriptorSet, 2> descriptorWrites = {};
    descriptorWrites[0].sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
    descriptorWrites[0].dstSet = descriptorSet;
    descriptorWrites[0].dstBinding = 0;
    descriptorWrites[0].dstArrayElement = 0;
    descriptorWrites[0].descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
    descriptorWrites[0].descriptorCount = 1;
    descriptorWrites[0].pBufferInfo = &bufferInfo;
    descriptorWrites[1].sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
    descriptorWrites[1].dstSet = descriptorSet;
    descriptorWrites[1].dstBinding = 1;
    descriptorWrites[1].dstArrayElement = 0;
    descriptorWrites[1].descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
    descriptorWrites[1].descriptorCount = 1;
    descriptorWrites[1].pImageInfo = &imageInfo;
    begin

      -- Grap function pointers
      Initialize_Vulkan_Library;
      Initialize_Vulkan_Subprograms;

      -- Create instance
      vkAssert (vkCreateInstance (Instance_Info'Access, NULL_PTR, Instance'Address));

      -- Create rendering surface
      Surface := Create_Surface (Instance);
      VkAssert (Surface_Support);

      -- Aquire a physical device
      vkAssert (vkEnumeratePhysicalDevices (Instance, Count'Unchecked_Access, null));
      if Count = 0 then raise Program_Error with "No Vulkan Device!?"; end if;
      declare Physical_Devices : aliased Ptr_Array_Ptr := new Array_Ptr (1..Int (Count)); begin
        vkAssert (vkEnumeratePhysicalDevices (Instance, Count'Unchecked_Access, Physical_Devices));
        Outter: for I in Physical_Devices'Range loop

          -- Get queue
          vkGetPhysicalDeviceQueueFamilyProperties (Physical_Device, Count'Unchecked_Access, null);
          declare Queue_Family_Properties : aliased Ptr_Array_VkQueueFamilyProperties := new Array_VkQueueFamilyProperties (1..Int (Count)); begin
            vkGetPhysicalDeviceQueueFamilyProperties (Physical_Device, Count'Unchecked_Access, Queue_Family_Properties);
            Inner: for I in Queue_Family_Properties.All'Range loop 
              if (Queue_Family_Properties (I).queueFlags and VK_QUEUE_GRAPHICS_BIT) > 0 then
                Queue_Info.queueFamilyIndex := Int_32_Unsigned_C (I);
              end if;
              vkAssert (vkGetPhysicalDeviceSurfaceSupportKHR (Physical_Device, Queue_Info.queueFamilyIndex, Surface, Surface_Support'Unchecked_Access));
              exit Inner when indices.isComplete;
            end loop Inner;      
          end;

          -- Get extensions
          vkEnumerateDeviceExtensionProperties(device, nullptr, &extensionCount, nullptr);
          std::vector<VkExtensionProperties> availableExtensions(extensionCount);
          vkEnumerateDeviceExtensionProperties(device, nullptr, &extensionCount, availableExtensions.data());
          std::set<std::string> requiredExtensions(deviceExtensions.begin(), deviceExtensions.end());

          -- Get swapchain
          SwapChainSupportDetails details;
          vkAssert (vkGetPhysicalDeviceSurfaceFormatsKHR (Physical_Device, Surface, Count'Unchecked_Access, null));
          declare Surface_Formats : aliased Ptr_Array_VkSurfaceFormatKHR := new Array_VkSurfaceFormatKHR (1..Int (Count)); begin
            vkAssert (vkGetPhysicalDeviceSurfaceFormatsKHR (Physical_Device, Surface, Count'Unchecked_Access, Surface_Formats));
            for I in Surface_Formats.All'Range loop
              if Surface_Formats (I).Format = VK_FORMAT_B8G8R8A8_SRGB then
                -- Do something here
                exit;
              end if;
            end loop;
          end;
          vkGetPhysicalDeviceSurfaceCapabilitiesKHR(device, surface, &details.capabilities);
          uint32_t formatCount;
          vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &formatCount, nullptr);
          if (formatCount != 0 then
              details.formats.resize(formatCount);
              vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &formatCount, details.formats.data());
          end if;
          uint32_t presentModeCount;
          vkGetPhysicalDeviceSurfacePresentModesKHR(device, surface, &presentModeCount, nullptr);

          -- Get supported formats
          for (VkFormat format : candidates) {
            VkFormatProperties props;
            vkGetPhysicalDeviceFormatProperties(physicalDevice, format, &props);
            if (tiling == VK_IMAGE_TILING_LINEAR && (props.linearTilingFeatures & features) == features) {
              return format;
            elsif (tiling == VK_IMAGE_TILING_OPTIMAL && (props.optimalTilingFeatures & features) == features) {
              return format;
            end if;
          end loop;

          -- Get properties and compare to the current best (VRAM, feature amount, driver version, and GPU speed)
          vkGetPhysicalDeviceProperties (Physical_Device, Physical_Device_Properties'Unchecked_Access);
          vkGetPhysicalDeviceMemoryProperties (Physical_Device, Physical_Device_Memory_Properties'Unchecked_Access);

          -- Verify the device is suitable
          if Has_Proper_Support then

            -- See if we already found a faster device           
          elsif Count = Physical_Devices'Last then raise Program_Error with ""; end if;
        end loop;
        if 

        -- Log device information in case of error reports
        Line ("API Version:"    & Int_32_Unsigned_C'Wide_Image (Physical_Device_Properties.apiVersion));
        Line ("Driver Version:" & Int_32_Unsigned_C'Wide_Image (Physical_Device_Properties.driverVersion));
        Line ("Vender ID:"      & Int_32_Unsigned_C'Wide_Image (Physical_Device_Properties.vendorID));
        Line ("Device ID:"      & Int_32_Unsigned_C'Wide_Image (Physical_Device_Properties.deviceID));
        Line ("Device Type:"    & Int_32_Unsigned_C'Wide_Image (Physical_Device_Properties.deviceType));
        Line ("Device Name: "   & To_Str (Physical_Device_Properties.deviceName));
      end;

      -- Create virtual device
      vkAssert (vkCreateDevice (Physical_Device, Device_Info'Unchecked_Access, NULL_PTR, Device'Address));
      vkGetDeviceQueue (Device, Queue_Info.graphicsFamily, 0, graphicsQueue'Address);
      vkGetDeviceQueue (Device, Queue_Info.presentFamily, 0, Queue'presentQueue);

      -- ???
      Update;

      -- Create command pool
      vkAssert (vkCreateCommandPool (Device, Command_Pool_Info'Unchecked_Access, NULL_PTR, Command_Pool'Address));

      -- ???
      vkAssert (vkCreateDescriptorSetLayout (Device, &layoutInfo, NULL_PTR, descriptorSetLayout.replace()));
      vkAssert (vkCreateDescriptorPool      (device, &poolInfo, nullptr, descriptorPool.replace()))
      vkAssert (vkAllocateDescriptorSets    (device, &allocInfo, &descriptorSet));
      vkAssert (vkUpdateDescriptorSets      (device, descriptorWrites.size(), descriptorWrites.data(), 0, nullptr);


      -- Create semaphores
      vkAssert (vkCreateSemaphore (Device, Semaphore_Info'Unchecked_Access, NULL_PTR, Acquire_Status'Address));
      vkAssert (vkCreateSemaphore (Device, Semaphore_Info'Unchecked_Access, NULL_PTR, Render_Status'Address));

    end;

  -- Kill the global instance state
  procedure Finalize is
    begin null; 
      vkAssert (vkDeviceWaitIdle (Device));
      vkDestroySemaphore         (Device, Acquire_Status, NULL_PTR);
      vkDestroySemaphore         (Device, Render_Status,  NULL_PTR);
      vkDestroyCommandPool       (Device, Command_Pool,   NULL_PTR);
      vkDestroySwapchainKHR      (Device, Swap_Chain,     NULL_PTR);
      vkDestroyDevice            (Device,                 NULL_PTR);
      vkDestroyInstance          (Instance,               NULL_PTR);
      Finalize_Vulkan_Library;
    end;

  -- Recreate the swapchain among other 
  procedure Update is -- Optimize for resize?
    Count               : aliased Int_32_Unsigned_C                         := 0;
    Pre_Transform       : aliased Int_32_Unsigned_C                         := 0;
    Setup_Buffer        : aliased Ptr                                       := NULL_PTR;
    Begin_Info          : aliased VkCommandBufferBeginInfo                  := (others                  => <>);
    Swap_Chain_Info     : aliased VkSwapchainCreateInfoKHR                  := (flags                   => VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT,
                                                                                others                  => <>);
    Memory_Barrier      : aliased VkImageMemoryBarrier                      := (srcQueueFamilyIndex     => VK_QUEUE_FAMILY_IGNORED,
                                                                                dstQueueFamilyIndex     => VK_QUEUE_FAMILY_IGNORED,
                                                                                oldLayout               => VK_IMAGE_LAYOUT_UNDEFINED,
                                                                                newLayout               => VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
                                                                                subresourceRange        => (VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1),
                                                                                others                  => <>);
    Setup_Buffer_Info   : aliased VkCommandBufferAllocateInfo               := (commandPool             => Command_Pool,
                                                                                level                   => VK_COMMAND_BUFFER_LEVEL_PRIMARY,
                                                                                commandBufferCount      => 1,
                                                                                others                  => <>);
    Submit_Info         : aliased VkSubmitInfo                              := (commandBufferCount      => 1,
                                                                                pCommandBuffers         => Setup_Buffer'Address,
                                                                                others                  => <>);
    Color_Attachment    : aliased VkAttachmentDescription                   := (format                  => swapChainImageFormat,
                                                                                samples                 => VK_SAMPLE_COUNT_1_BIT,
                                                                                loadOp                  => VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                                storeOp                 => VK_ATTACHMENT_STORE_OP_STORE,
                                                                                stencilLoadOp           => VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                stencilStoreOp          => VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                initialLayout           => VK_IMAGE_LAYOUT_UNDEFINED,
                                                                                finalLayout             => VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
                                                                                others                  => <>);
    Depth_Attachment    : aliased VkAttachmentDescription                   := (format                  => findDepthFormat(),
                                                                                samples                 => VK_SAMPLE_COUNT_1_BIT,
                                                                                loadOp                  => VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                                storeOp                 => VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                stencilLoadOp           => VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                stencilStoreOp          => VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                initialLayout           => VK_IMAGE_LAYOUT_UNDEFINED,
                                                                                finalLayout             => VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                                                                others                  => <>);
    Color_Reference       : aliased VkAttachmentReference                   := (attachment              => 0,
                                                                                layout                  => VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                                                                others                  => <>);
    Depth_Reference       : aliased VkAttachmentReference                   := (attachment              => 1,
                                                                                layout                  => VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                                                                others                  => <>);
    Subpass_Description   : aliased VkSubpassDescription                    := (pipelineBindPoint       => VK_PIPELINE_BIND_POINT_GRAPHICS,
                                                                                colorAttachmentCount    => 1,
                                                                                pColorAttachments       => &colorAttachmentRef,
                                                                                pDepthStencilAttachment => &depthAttachmentRef,
                                                                                others                  => <>);
    Subpass_Dependency    : aliased VkSubpassDependency                     := (srcSubpass              => VK_SUBPASS_EXTERNAL,
                                                                                dstSubpass              => 0,
                                                                                srcStageMask            => VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                                                                                srcAccessMask           => 0,
                                                                                dstStageMask            => VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                                                                                dstAccessMask           => VK_ACCESS_COLOR_ATTACHMENT_READ_BIT or VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
                                                                                others                  => <>);
    Attachments           : aliased Array_VkAttachmentDescription           := (colorAttachment, depthAttachment);
    Render_Pass_Info      : aliased VkRenderPassCreateInfo                  := (attachmentCount         => attachments.size(),
                                                                                pAttachments            => attachments.data(),
                                                                                subpassCount            => 1,
                                                                                pSubpasses              => &subpass,
                                                                                dependencyCount         => 1,
                                                                                pDependencies           => &dependency,
                                                                                others                  => <>);
    Vertex_Shader_Model    : aliased VkShaderModule                         := Load_Shader ("shaders/vert.spv");
    Fragment_Shader_Model  : aliased VkShaderModule                         := Load_Shader ("shaders/frag.spv");
    Vertex_Stage_Info      : aliased VkPipelineShaderStageCreateInfo        := (stage                   => VK_SHADER_STAGE_VERTEX_BIT,
                                                                                module                  => Vertex_Shader_Model,
                                                                                pName                   => "main",
                                                                                others                  => <>);
    Fragment_Stage_Info    : aliased VkPipelineShaderStageCreateInfo        := (stage                   => VK_SHADER_STAGE_FRAGMENT_BIT,
                                                                                module                  => fragShaderModule,
                                                                                pName                   => "main",
                                                                                others                  => <>);
    Shader_Stages_Info     : aliased VkPipelineShaderStageCreateInfo        := (vertShaderStageInfo, fragShaderStageInfo);
    --auto bindingDescription = Vertex::getBindingDescription();
    --auto attributeDescriptions = Vertex::getAttributeDescriptions();
    Vertex_Input_Info      : aliased VkPipelineVertexInputStateCreateInfo   := (vertexBindingDescriptionCount   => 1,
                                                                                vertexAttributeDescriptionCount => attributeDescriptions.size(),
                                                                                pVertexBindingDescriptions      => &bindingDescription,
                                                                                pVertexAttributeDescriptions    => attributeDescriptions.data(),
                                                                                others                  => <>);
    Input_Assembly_Info    : aliased VkPipelineInputAssemblyStateCreateInfo := (topology                => VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
                                                                                primitiveRestartEnable  => VK_FALSE,
                                                                                others                  => <>);
    Viewport               : aliased VkViewport                             := (x                       => 0.0,
                                                                                y                       => 0.0,
                                                                                width                   => Real_C (swapChainExtent.width),
                                                                                height                  => Real_C (swapChainExtent.height),
                                                                                minDepth                => 0.0,
                                                                                maxDepth                => 1.0,
                                                                                others                  => <>);
    Scissor                : aliased VkRect2D                               := (offset                  => {0, 0},
                                                                                extent                  => swapChainExtent,
                                                                                others                  => <>);
    Viewport_State_Info    : aliased VkPipelineViewportStateCreateInfo      := (viewportCount           => 1,
                                                                                pViewports              => &viewport,
                                                                                scissorCount            => 1,
                                                                                pScissors               => &scissor,
                                                                                others                  => <>);
    Rasterizer_Info        : aliased VkPipelineRasterizationStateCreateInfo := (depthClampEnable        => VK_FALSE,
                                                                                rasterizerDiscardEnable => VK_FALSE,
                                                                                polygonMode             => VK_POLYGON_MODE_FILL,
                                                                                lineWidth               => 1.0,
                                                                                cullMode                => VK_CULL_MODE_BACK_BIT,
                                                                                frontFace               => VK_FRONT_FACE_COUNTER_CLOCKWISE,
                                                                                depthBiasEnable         => VK_FALSE,
                                                                                others                  => <>);
    Multisample_Info       : aliased VkPipelineMultisampleStateCreateInfo   := (sampleShadingEnable     => VK_FALSE,
                                                                                rasterizationSamples    => VK_SAMPLE_COUNT_1_BIT,
                                                                                others                  => <>);
    Depth_Stencil_Info     : aliased VkPipelineDepthStencilStateCreateInfo  := (depthTestEnable         => VK_TRUE,
                                                                                depthWriteEnable        => VK_TRUE,
                                                                                depthCompareOp          => VK_COMPARE_OP_LESS,
                                                                                depthBoundsTestEnable   => VK_FALSE,
                                                                                stencilTestEnable       => VK_FALSE,
                                                                                others                  => <>);
    Color_Blend_Attachment : aliased VkPipelineColorBlendAttachmentState    := (colorWriteMask          => VK_COLOR_COMPONENT_R_BIT or VK_COLOR_COMPONENT_G_BIT or
                                                                                                           VK_COLOR_COMPONENT_B_BIT or VK_COLOR_COMPONENT_A_BIT,
                                                                                blendEnable             => VK_FALSE
                                                                                others                  => <>);
    Color_Blend_Info       : aliased VkPipelineColorBlendStateCreateInfo    := (logicOpEnable           => VK_FALSE,
                                                                                logicOp                 => VK_LOGIC_OP_COPY,
                                                                                attachmentCount         => 1,
                                                                                pAttachments            => &colorBlendAttachment,
                                                                                blendConstants          => (others => 0.0),
                                                                                others                  => <>);
    Descriptor_Set_Layout  : aliased VkDescriptorSetLayout                  := (descriptorSetLayout); -- ???
    Pipeline_Layout_Info   : aliased VkPipelineLayoutCreateInfo             := (setLayoutCount          => 1,
                                                                                pSetLayouts             => setLayouts,
                                                                                others                  => <>);
    Pipeline_Info          : aliased VkGraphicsPipelineCreateInfo           := (stageCount              => 2,
                                                                                pStages                 => shaderStages,
                                                                                pVertexInputState       => &vertexInputInfo,
                                                                                pInputAssemblyState     => &inputAssembly,
                                                                                pViewportState          => &viewportState,
                                                                                pRasterizationState     => &rasterizer,
                                                                                pMultisampleState       => &multisampling,
                                                                                pDepthStencilState      => &depthStencil,
                                                                                pColorBlendState        => &colorBlending,
                                                                                layout                  => pipelineLayout,
                                                                                renderPass              => renderPass,
                                                                                subpass                 => 0,
                                                                                basePipelineHandle      => VK_NULL_HANDLE,
                                                                                others                  => <>);
    Frame_Buffer : aliased VkFramebufferCreateInfo := (others => <>);
    Render_Pass_Begin_Info : aliased VkRenderPassBeginInfo := (renderArea => (offset => (0, 0), extent => swapChainExtent),
                                                               others => <>);
    Allocate_Info : aliased VkCommandBufferAllocateInfo := (commandPool = commandPool;
                                                            level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
                                                            commandBufferCount = (uint32_t) commandBuffers.size(); 
                                                            others => <>);
    --VkBuffer vertexBuffers[] = {vertexBuffer};
    --VkDeviceSize offsets[] = {0};
    Clear_Values : aliased Array_VkClearValue := ((color => (0.0, 0.0, 0.0, 1.0),
                                             others => <>),
                                            (depthStencil => (1.0, 0.0),
                                             others => <>));
    begin

      -- Find the resolution of the swap chain images - some window managers force use to pick the resolution that best matches the window
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
                          clipped          => VK_TRUE,
                          others           => <>);

      -- Use multiple queues, one for graphics and one for presentation. If they differ use concurrent mode to avoid ownership... for now
      if Graphics_Family /= Present_Family then
        Swap_Chain_Info.imageSharingMode      := VK_SHARING_MODE_CONCURRENT;
        Swap_Chain_Info.queueFamilyIndexCount := 2;
        Swap_Chain_Info.pQueueFamilyIndices   := queueFamilyIndices;
      else Swap_Chain_Info.imageSharingMode := VK_SHARING_MODE_EXCLUSIVE; end if;

      -- Create the swapchain
      vkAssert (vkCreateSwapchainKHR (Device, Swap_Chain_Info'Unchecked_Access, NULL_PTR, Swap_Chain'Address));

      -- Explicitly query the number of desired images
      vkAssert (vkGetSwapchainImagesKHR (Device, Swap_Chain, Count'Unchecked_Access, null));
      Images := new Array_Ptr (1.. Int (Count));
      vkAssert (vkGetSwapchainImagesKHR (Device, Swap_Chain, Count'Unchecked_Access, Images));

      -- Create image views
      for I in 1..Images'Length loop
        Image_Views (I) := Make_Image_View (Images (I), swapChainImageFormat, VK_IMAGE_ASPECT_COLOR_BIT);
      end loop;

      -- Create the renderer
      vkAssert (vkCreateRenderPass (Device, &renderPassInfo, NULL_PTR, renderPass.replace()));
      vkAssert (vkCreatePipelineLayout (Device, &pipelineLayoutInfo, NULL_PTR, pipelineLayout.replace());
      vkAssert (vkCreateGraphicsPipelines (Device, VK_NULL_HANDLE, 1, &pipelineInfo, NULL_PTR, graphicsPipeline.replace());

      -- Create depth resource
      Create_Image (swapChainExtent.width, swapChainExtent.height, depthFormat, VK_IMAGE_TILING_OPTIMAL, VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, depthImage, depthImageMemory);
      createImageView (depthImage, depthFormat, VK_IMAGE_ASPECT_DEPTH_BIT, depthImageView);
      transitionImageLayout (depthImage, depthFormat, VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL);

      -- Create the image frame buffers
      for I in 1..Image_Views'Range loop
        Frame_Buffer := (renderPass      => renderPass;
                         attachmentCount => attachments.size();
                         pAttachments    => attachments.data();
                         width           => swapChainExtent.width;
                         height          => swapChainExtent.height;
                         layers          => 1;
                         others          => <>);
        vkAssert (vkCreateFramebuffer (device, &framebufferInfo, nullptr, swapChainFramebuffers[i].replace()));
      end loop;

      -- Create_Command_Buffers
      if (commandBuffers.size() > 0) { vkFreeCommandBuffers(device, commandPool, commandBuffers.size(), commandBuffers.data());
      commandBuffers.resize(swapChainFramebuffers.size());
      vkAssert (vkAllocateCommandBuffers(device, &allocInfo, commandBuffers.data()));
      for I in Command_Buffers'Range loop
        vkBeginCommandBuffer (Command_Buffers (I), &beginInfo);
        renderPassInfo.renderPass = renderPass;
        renderPassInfo.framebuffer = swapChainFramebuffers[i];
        renderPassInfo.clearValueCount = clearValues.size();
        renderPassInfo.pClearValues = clearValues.data();
        vkCmdBeginRenderPass         (Command_Buffers (I), &renderPassInfo, VK_SUBPASS_CONTENTS_INLINE);
        vkCmdBindPipeline            (Command_Buffers (I), VK_PIPELINE_BIND_POINT_GRAPHICS, graphicsPipeline);
        vkCmdBindVertexBuffers       (Command_Buffers (I), 0, 1, vertexBuffers, offsets);
        vkCmdBindIndexBuffer         (Command_Buffers (I), indexBuffer, 0, VK_INDEX_TYPE_UINT32);
        vkCmdBindDescriptorSets      (Command_Buffers (I), VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout, 0, 1, &descriptorSet, 0, nullptr);
        vkCmdDrawIndexed             (Command_Buffers (I), indices.size(), 1, 0, 0, 0);
        vkCmdEndRenderPass           (Command_Buffers (I));
        vkAssert (vkEndCommandBuffer (Command_Buffers (I)));
      end loop;
    end;

  -- Acquire the rendered image, show it in the window, and handle changes to the resolution
  procedure Present is
    Present_Info : aliased VkPresentInfoKHR := (swapchainCount     => 1,
                                                pSwapchains        => Swap_Chain'Address,
                                                pImageIndices      => Image_Index'Unchecked_Access,
                                                waitSemaphoreCount => 1,
                                                pWaitSemaphores    => Render_Status'Address,
                                                others             => <>);
    Submit_Info  : aliased VkSubmitInfo     := (commandBufferCount => 1,
                                                pCommandBuffers    => Setup_Buffer'Address,
                                                waitSemaphoreCount => 1,
                                                pWaitSemaphores    => Render_Status'Address,
                                                others             => <>);
    begin

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
                                  fence       => NULL_PTR,
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
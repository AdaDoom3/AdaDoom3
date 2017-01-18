
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

  -------------
  -- Globals --
  -------------

  -- Current recogonized width and height of the render surface
  Current_Width, Current_Height : aliased Natural := 0;

  -- Handles
  Instance, Surface, Swap_Chain, Command_Pool, Graphics_Queue, Present_Queue,
  Render_Status, Acquire_Status, Physical_Device, Device : aliased Ptr := NULL_PTR;
  Images : aliased Array_Ptr (1..3) := null;

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
  --     if newLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL then
  --       Barrier.subresourceRange.aspectMask := VK_IMAGE_ASPECT_DEPTH_BIT;
  --       if format = VK_FORMAT_D32_SFLOAT_S8_UINT or format = VK_FORMAT_D24_UNORM_S8_UINT then
  --         Barrier.subresourceRange.aspectMask := Barrier.subresourceRange.aspectMask or VK_IMAGE_ASPECT_STENCIL_BIT;
  --       end if;
  --     else Barrier.subresourceRange.aspectMask := VK_IMAGE_ASPECT_COLOR_BIT; end if;

  --     -- ???
  --     Barrier.subresourceRange.baseMipLevel   := 0;
  --     Barrier.subresourceRange.levelCount     := 1;
  --     Barrier.subresourceRange.baseArrayLayer := 0;
  --     Barrier.subresourceRange.layerCount     := 1;

  --     -- Set new image layout
  --     if oldLayout = VK_IMAGE_LAYOUT_PREINITIALIZED and newLayout = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL then
  --       Barrier.srcAccessMask := VK_ACCESS_HOST_WRITE_BIT;
  --       Barrier.dstAccessMask := VK_ACCESS_TRANSFER_READ_BIT;
  --     elsif oldLayout = VK_IMAGE_LAYOUT_PREINITIALIZED and newLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL then
  --       Barrier.srcAccessMask := VK_ACCESS_HOST_WRITE_BIT;
  --       Barrier.dstAccessMask := VK_ACCESS_TRANSFER_WRITE_BIT;
  --     elsif oldLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL and newLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL then
  --       Barrier.srcAccessMask := VK_ACCESS_TRANSFER_WRITE_BIT;
  --       Barrier.dstAccessMask := VK_ACCESS_SHADER_READ_BIT;
  --     elsif oldLayout = VK_IMAGE_LAYOUT_UNDEFINED and newLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL then
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
    Subresource : aliased VkImageSubresource := (aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
                                                mipLevel = 0;
                                                arrayLayer = 0;
                                                others => <>);
    VkSubresourceLayout stagingImageLayout;
    vkGetImageSubresourceLayout(Device, stagingImage, &subresource, &stagingImageLayout);
    void* data;
    VDeleter<VkImage> stagingImage{Device, vkDestroyImage};
    VDeleter<VkDeviceMemory> stagingImageMemory{Device, vkFreeMemory};
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
          uint8_t* dataBytes = reinterpret_cast<uint8_t*>(data);
          for (int y = 0; y < texHeight; y++) {
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
        if (Memory_Properties.memoryTypeBits and 2**I) > 0 and (Memory_Properties.memoryTypes (I).propertyFlags and Properties) = Properties then
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

  --------------
  -- Frontend --
  --------------

  -- 
  procedure Run_Light_Job is
    begin
      View.View_Entities.Append (Render_Lights.Shadow_Only_View_Entities);

      -- Evaluate light shader registers

      -- Additve lights with no stage in the light shader is positive, remove the light
      if Light.Kind = Fog_Light or Light.Kind = Blend_Light then
        Outter: for Stage of Light.Stages loop
          if Light.Stage.Registers.Condition_Register then

            -- Snap tiny values to zero
            for Register of Light.Stage.Registers loop
              if Register < 0.001 then Register := 0.0; end if;
            end loop;

            -- We found a light that add something
            for Register of Light.Stage.Registers loop
              exit Outter when Register > 0.0;
            end loop;              
          end if;
          if Stage = Light.Stages.Last then 

-- We didn't find any addition to the scene
goto Skip_Light;
          end if;
        end loop Outter;
      end if;

      -- Copy data to backend

      -- Create a "fog plane" to represent light far clip plane

      -- Calculate the matrix that projects the zero-to-one cube to exactly cover the light frustum in clip space

      -- Calculate the project bounds, either not clipped at all, near clipped, or fully clipped
        
      -- Build the scissor

      -- Create interactions with all entities the light may touch and add entities that may cast shadows
      for Reference of Light.References loop

        -- Some lights have their center of projection outside of the world so ignore areas not connected to the light center
        if not Light.World.Areas_Are_Connected (Light.Area, Reference.Area, PS_BLOCK_VIEW) then

          -- Check the models in this area
          for Model of Reference.Entities loop
            if not (vLight->entityInteractionState[ edef->index ] != viewLight_t::INTERACTION_UNCHECKED or else
                        
              -- A large fraction of static entity / light pairs will still have no interactions even though they are both present in the same area(s)
              eModel != NULL && !eModel->IsDynamicModel && inter == INTERACTION_EMPTY or else
                        
                -- We don't want the lights on weapons to illuminate anything else. There are two assumptions here - that
                -- allowLightInViewID is only used for weapon lights, and that all weapons will have weaponDepthHack. A more general
                -- solution would be to have an allowLightOnEntityID field.
                light->parms.allowLightInViewID && light->parms.pointLight && !eParms.weaponDepthHack or else

                --
                (eParms.noShadow || ( eModel && !eModel->ModelHasShadowCastingSurfaces ) ) && !edef->IsDirectlyVisible or else
                eModel && !eModel->ModelHasInteractingSurfaces && !eModel->ModelHasShadowCastingSurfaces or else
                inter == NULL and (eParms.noDynamicInteractions or R_CullModelBoundsToLight( light, edef->localReferenceBounds, edef->modelRenderMatrix )) or else 
                edef->IsDirectlyVisible 
                !lightCastsShadows 
            then

            end if;
          end loop;
        end if;
      end loop;
    end;
  
  -- The frontend - also performs the step of filling the depth buffer Visible_Surface_Determination
  procedure Run_Frontend is
    Area : ;
    BSP  : ;
    Pos  : ;

    -- Recursivly find lights and entities by flowing from an origin through visible portals that the view origin can see into
    procedure Flood_View_Through_Portals (Origin : Vector_3D; Planes : in out Vector_Plane_4D.Unsafe.Vector) is
      portalStack_t ps;
      ps.next = NULL;
      ps.p = NULL;
      idScreenRect r;
      idVec3 v;
      idVec3 ndc;
      begin
        Portals.Planes := Planes;
        Portals.Rectangle := View.Scissor;

        -- If we are outside the world, mark everything
        if View.Area < 0 then
          for I in 1..Length (Planes) loop
            Area_Screen_Rect (I) := View.Scissor;
            Portals.Add_Area_To_View (Portal)
          end loop;
        else
          portalArea_t * area = &portalAreas[ areaNum ];

          -- Cull models and lights to the current collection of planes
          AddAreaToView ( areaNum, ps );
          if Is_Empty (Area_Screen_Rect (Area)) then
            Area_Screen_Rect (Area) := Portals.Rect;
          else
            Area_Screen_Rect (Area) := Join (Portals.Rect, Area_Screen_Rect (Area));
          end if;

          -- Go through all the portals
          for Portal of Portals loop
            
            -- Check the portal isn't a closed door and that it is facing the view
            Distance := Portal.Plane.Distance (Origin);
            if not Portal.Double_Portal.Is_View_Block and Distance < -0.1 then

              -- If we are very close to the portal surface, avoid clipping that may cause areas to vanish
              if Distance < 1.0 then
                New_Portals := Portals;
                New_Portals.Append (Portal);
                Flood_View_Through_Portals (Origin, New_Portals);
              else

                -- Clip the portal winding to all of the planes
                for Plane of Portal.Planes loop
                  exit when not Is_Clip_In_Place (Portal.Winding, Plane);
                end loop;

                -- Continue only if the portal is visible and not fogged out
                if Portal.Winding.Points.Length /= 0 and Is_Fogged (Portal) then

                  -- Go through the portal
                  New_Portals.Append (Portal);

                  -- Scissor things outside the screen pixel bounding box
                  New_Portals.Rect := Screen_Rect_From_Winding (Portal.Winding, Identity_Space);
                  View_Width  := View.Port.X2 - View.Port.X1;
                  View_Height := View.Port.Y2 - View.Port.Y1;
                  for Point of Portal.Winding.Points loop
                    R_LocalPointToGlobal (space->modelMatrix, (*w)[i].ToVec3(), v);
                    R_GlobalToNormalizedDeviceCoordinates (v, ndc);
                    Screen_Rect.Points.Append (X => (ndc[0] * 0.5 + 0.5) * viewWidth,
                                               Y => (ndc[1] * 0.5 + 0.5) * viewHeight);
                  end loop;
                  r.Expand();

                  -- Slop might have spread it a pixel outside, so trim it back
                  New_Portals.Rect.Intersect (Portals.Rect);

                  -- Generate a set of clipping planes to restrict the view visibility beyond the scissor
                  for Point of Winding.Points loop
                    newStack.portalPlanes[newStack.numPortalPlanes].Normal().Cross( origin - w[j].ToVec3(), origin - w[i].ToVec3() );

                    -- Skip degenerates
                    if newStack.portalPlanes[newStack.numPortalPlanes].Normalize() >= 0.01f then
                      newStack.portalPlanes[newStack.numPortalPlanes].FitThroughPoint( origin );
                    end if;
                  end loop;
                end if;
              end if;
            end if;

            -- 
            newStack.portalPlanes[newStack.numPortalPlanes] = p->plane;
            Flood_View_Through_Portals (origin, Portal.Into_Area, New_Portals);
          end loop;
        end if;
      end;
    begin
      for View of Views loop

        -- Setup the view matrix
        Viewer := (Axis.XX, Axis.YX, Axis.ZX, -Origin.X * Axis.XX - Origin.Y * YX - Origin.Z * Axis.ZX,
                   Axis.XY, Axis.YY, Axis.ZY, -Origin.X * Axis.XY - Origin.Y * YY - Origin.Z * Axis.ZY,
                   Axis.XZ, Axis.YZ, Axis.ZZ, -Origin.X * Axis.XZ - Origin.Y * YZ - Origin.Z * Axis.ZZ);

--         -- Setup the projection matrix (use the ol' "infinite far z trick")
--         Jitter_Values := (if Jitter.Get then (Random_Float, Random_Float) else  (others => <>));
--         R_SetupProjectionMatrix

        -- Setup render matricies for faster culliong
        View.Render_Projection := Transpose (View.Projection);
        View.World_Space.MVP := View.Render_Projection * Transpose (View.World_Space.Model_View);
        View.Frustum := ((4)(3) => Z_Near.Get, others => -Get_Frustum_Planes (View.World_Space.MVP));

        -- Walk the BSP tree to find the area where the view origin is
        BSP := View.Level.Partitions.Get;
        Pos := BSP.Root;
        loop
          Node := Get (Pos);
          case Node.Kind is
            when Area_Partition   => Area := Node.Area_Id; exit;
            when Opaque_Partition => raise In_Solid;
            when Normal_Partition => Pos := (if View.Origin * Normal (Node.Plane) + Node.Plane.W > 0 then First (Pos) else Last (Pos));
          end case;
        end loop;

        -- Flood through portals
        Flood_View_Through_Portals (Origin, Area, View.Planes);

        -- Reset the amount of light jobs if changed (this will need tuning)
        if Length (View.Lights) < Length (Light_Jobs) then
          Light_Jobs.Append (new Vector_Lights.Unsafe.Vector (Length (View.Lights) - Length (Light_Jobs)));
        elsif Length (View.Lights) + MAX_IDLE_LIGHT_JOBS > Length (Light_Jobs) then
          Light_Jobs.Remove (1..Length (View.Lights) + MAX_IDLE_LIGHT_JOBS - Length (Light_Jobs));
        end if;

        -- Kick-off light projection and searching for lit models then wait for them to finish
        for I in 1..Length (View.Lights) loop
          Light_Jobs.Element (I).Do_Pass (View.Lights.Element (I));
        end loop;
        for Light_Job of Light_Jobs loop
          Light_Job.Confirm_Completion;
        end loop;

--         -- Determine all possible connected areas for light-behind-door culling
--         if Area = OUTSIDE_AREA then View.Connected_Areas := (others => True)
--         else View.Level.Portals.Iterate_Subtree (Flood_Portals); end if;

        -- R_SortDrawSurfs            // A simple C qsort call. C++ sort would have been faster thanks to inlining.       
        -- R_GenerateSubViews
        -- R_AddDrawViewCmd 
      end loop;
    end;
  package Task_Frontend is new Tasks (Run_Frontend);
  Frontend : Task_Frontend.Safe_Task;

  -----------------
  -- Run_Backend --
  -----------------

  procedure Run_Backend is
    VkRect2D scissor = {};
    VkViewport viewport = {};
        float parm[4];
    begin
      loop

        -- Update dynamic viewport state and window clipping
        viewport := (height   => View.Port.y2 + 1 - View.Port.y1,
                     width    => View.Port.x2 + 1 - View.Port.x1,
                     minDepth => 0.0,
                     maxDepth => 1.0);
        vkCmdSetViewport (drawCmdBuffers[i], 0, 1, Current_View_Port'Unchecked_Access);

        -- Update dynamic scissor which may be smaller than the viewport for subviews
        scissor.extent.width =  backEnd.View.Port.x1 + viewDef->scissor.x1;
        scissor.extent.height = backEnd.View.Port.y1 + viewDef->scissor.y1;
        scissor.offset.x = viewDef->scissor.x2 + 1 - viewDef->scissor.x1;
        scissor.offset.y = viewDef->scissor.y2 + 1 - viewDef->scissor.y1;
        vkCmdSetScissor(drawCmdBuffers[i], 0, 1, &scissor);

        -- Force face culling to set next time
        backEnd.glState.faceCulling = -1;   

        -- Ensure depth writes are enabled for the depth clear
        GL_State (GLS_DEFAULT);

        -- Clear the depth buffer and set the stencil to 128 for shadowing
        GL_Clear (false, true, true, STENCIL_SHADOW_TEST_VALUE, 0.0, 0.0, 0.0, 0.0);

        -- Normal face culling
        GL_Cull (CT_FRONT_SIDED);

        -- Bind one global Vertex Array Object (VAO)
        qglBindVertexArray (glConfig.global_vao);

        -- Set eye position shader parameter
        SetVertexParm (RENDERPARM_GLOBALEYEPOS, (View.Origin.X, View.Origin.Y, View.Origin.Z, 1.0));

        -- Sets overbright to make world brighter - this value is baked into the specularScale and diffuseScale values for lighting.glsl
        SetFragmentParm (RENDERPARM_OVERBRIGHT, (others => r_lightScale.GetFloat() * 0.5));

        -- Set projection Matrix prameter
        SetVertexParms (RENDERPARM_PROJMATRIX_X, R_MatrixTranspose(backEnd.viewDef->projectionMatrix), 4);

        -- Fill the depth buffer for surfaces - starting with subview surfaces, then opaque surfaces, and finally perforated surfaces
        for Surface_Sort in Surface_Sort_Kind'Range loop
          for Surfaces of View.Surfaces.Element (Surface_Sort) loop
            for Surface of Surfaces loop

            end loop;
          end loop;
        end loop;

        -- 
      end loop;
    end;
  package Task_Backend is new Tasks (Run_Backend);
  Backend : Task_Backend.Safe_Task;

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
      Shader_Module_Info : aliased VkShaderModuleCreateInfo := (codeSize => code.size;
                                                                pCode    => (uint32_t*) code.data,
                                                                others   => <>);
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

          -- Make a declare block to catch device verification exceptions
          declare begin

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
                  exit Inner when Extension = Required_Extension;
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
                if Present_Mode = VK_PRESENT_MODE_MAILBOX_KHR then Surface_Present_Mode := Present_Mode; end if;
              end loop;
            end;

            -- Get physical device and virtual device properties and compare to the current best
            vkGetPhysicalDeviceSurfaceCapabilitiesKHR (Current_Device, Surface, Current_Surface_Capabilities'Unchecked_Access);
            vkGetPhysicalDeviceProperties             (Current_Physical_Device, Current_Device_Properties'Unchecked_Access);
            vkGetPhysicalDeviceMemoryProperties       (Current_Physical_Device, Current_Memory_Properties'Unchecked_Access);

            -- Check if the current device is better than best found or if it is the first
            if Current_Physical_Device = NULL_PTR

            -- Prefer mailbox mode, but don't require it
            or ((Surface_Present_Mode = VK_PRESENT_MODE_FIFO_KHR or Current_Surface_Present_Mode = VK_PRESENT_MODE_MAILBOX_KHR)

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
      Frontend.Initalize;
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
    -- Swap_Chain_Info        : aliased VkSwapchainCreateInfoKHR               := (flags                   => VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT,
    --                                                                             others                  => <>);
    -- Memory_Barrier         : aliased VkImageMemoryBarrier                   := (srcQueueFamilyIndex     => VK_QUEUE_FAMILY_IGNORED,
    --                                                                             dstQueueFamilyIndex     => VK_QUEUE_FAMILY_IGNORED,
    --                                                                             oldLayout               => VK_IMAGE_LAYOUT_UNDEFINED,
    --                                                                             newLayout               => VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
    --                                                                             subresourceRange        => (VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1),
    --                                                                             others                  => <>);
    -- Setup_Buffer_Info      : aliased VkCommandBufferAllocateInfo            := (commandPool             => Command_Pool,
    --                                                                             level                   => VK_COMMAND_BUFFER_LEVEL_PRIMARY,
    --                                                                             commandBufferCount      => 1,
    --                                                                             others                  => <>);
    -- Submit_Info            : aliased VkSubmitInfo                           := (commandBufferCount      => 1,
    --                                                                             pCommandBuffers         => Setup_Buffer'Unchecked_Access,
    --                                                                             others                  => <>);
    -- Color_Attachment       : aliased VkAttachmentDescription                := (format                  => swapChainImageFormat,
    --                                                                             samples                 => VK_SAMPLE_COUNT_1_BIT,
    --                                                                             loadOp                  => VK_ATTACHMENT_LOAD_OP_CLEAR,
    --                                                                             storeOp                 => VK_ATTACHMENT_STORE_OP_STORE,
    --                                                                             stencilLoadOp           => VK_ATTACHMENT_LOAD_OP_DONT_CARE,
    --                                                                             stencilStoreOp          => VK_ATTACHMENT_STORE_OP_DONT_CARE,
    --                                                                             initialLayout           => VK_IMAGE_LAYOUT_UNDEFINED,
    --                                                                             finalLayout             => VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
    --                                                                             others                  => <>);
    -- Depth_Attachment       : aliased VkAttachmentDescription                := (format                  => findDepthFormat,
    --                                                                             samples                 => VK_SAMPLE_COUNT_1_BIT,
    --                                                                             loadOp                  => VK_ATTACHMENT_LOAD_OP_CLEAR,
    --                                                                             storeOp                 => VK_ATTACHMENT_STORE_OP_DONT_CARE,
    --                                                                             stencilLoadOp           => VK_ATTACHMENT_LOAD_OP_DONT_CARE,
    --                                                                             stencilStoreOp          => VK_ATTACHMENT_STORE_OP_DONT_CARE,
    --                                                                             initialLayout           => VK_IMAGE_LAYOUT_UNDEFINED,
    --                                                                             finalLayout             => VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
    --                                                                             others                  => <>);
    -- Color_Reference        : aliased VkAttachmentReference                  := (attachment              => 0,
    --                                                                             layout                  => VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
    --                                                                             others                  => <>);
    -- Depth_Reference        : aliased VkAttachmentReference                  := (attachment              => 1,
    --                                                                             layout                  => VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
    --                                                                             others                  => <>);
    -- Subpass_Description    : aliased VkSubpassDescription                   := (pipelineBindPoint       => VK_PIPELINE_BIND_POINT_GRAPHICS,
    --                                                                             colorAttachmentCount    => 1,
    --                                                                             pColorAttachments       => &colorAttachmentRef,
    --                                                                             pDepthStencilAttachment => &depthAttachmentRef,
    --                                                                             others                  => <>);
    -- Subpass_Dependency     : aliased VkSubpassDependency                    := (srcSubpass              => VK_SUBPASS_EXTERNAL,
    --                                                                             dstSubpass              => 0,
    --                                                                             srcStageMask            => VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
    --                                                                             srcAccessMask           => 0,
    --                                                                             dstStageMask            => VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
    --                                                                             dstAccessMask           => VK_ACCESS_COLOR_ATTACHMENT_READ_BIT or VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
    --                                                                             others                  => <>);
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

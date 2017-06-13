
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

with Neo.Data;       use Neo.Data;
with Neo.Data.Model; use Neo.Data.Model;

-- Renderer for the global engine state
separate (Neo.Engine) package body Renderer is

  DEBUG : constant Boolean := False;

  --------------
  -- Settings --
  --------------

  type Driver_State is record Version, Vendor_Id, Device_Id : Int_Unsigned_C := 0; end record;
  type Array_Driver_State is array (Positive range <>) of Driver_State;

  -- Lists of bad drivers used to exclude or prefer certain physical devices
  DRIVER_BLACKLIST : constant Array_Driver_State := (1 => (others => <>));
  DRIVER_GREYLIST  : constant Array_Driver_State := (1 => (others => <>));

  -- Required device extensions that must be present 
  REQUIRED_EXTENSIONS : aliased Array_Str_Unbound := (1 => U (To_Str (VK_KHR_SWAPCHAIN_EXTENSION_NAME)));

  -- List of acceptable depth and color for our physical device - we prefer ones higher in the list so order matters
  SUPPORTED_DEPTH_FORMATS : constant Array_Int_Unsigned_C := (VK_FORMAT_D32_SFLOAT,
                                                              VK_FORMAT_D32_SFLOAT_S8_UINT,
                                                              VK_FORMAT_D24_UNORM_S8_UINT);
  SUPPORTED_IMAGE_FORMATS : constant Array_VkSurfaceFormatKHR := (1 => (VK_FORMAT_B8G8R8A8_UNORM, VK_COLOR_SPACE_SRGB_NONLINEAR_KHR));

  -- Amount of idle frontend light job tasks that are allowed to sit idle until tasks start being destroyed
  MAX_IDLE_LIGHT_JOBS : constant Positive := 10;
  FOG_SCALE           : constant Float    := 0.001;

  -------------
  -- Globals --
  -------------
  
  -- Opaque handles
  Vertex_Shader_Model,
  Fragment_Shader_Model,
  Setup_Buffer,
  Instance,
  Surface,
  Swap_Chain,
  Command_Pool,
  Graphics_Queue,
  Present_Queue,
  Render_Status,
  Acquire_Status,
  Physical_Device,
  Graphics_Pipeline,
  Pipeline_Layout,
  Render_Pass,
  Descriptor_Set_Layout,
  Device : aliased Ptr := NULL_PTR;

  -- Instance information
  Graphics_Family, Present_Family, Image_Index : aliased Int_Unsigned_C := 0;
  
  -- Current recogonized width and height of the render surface
  Current_Width, Current_Height : Int_64_Positive := 1;

  -- Frame buffer, we demand triple buffering
  Images             : aliased Array_Ptr (1..3)         := (others => NULL_PTR);
  Image_Views        : aliased Array_Ptr (Images'Range) := (others => NULL_PTR);
  Commands           : aliased Array_Ptr (Images'Range) := (others => NULL_PTR);
  Swap_Chain_Buffers : aliased Array_Ptr (Images'Range) := (others => NULL_PTR);
  
  -- Settings and properties
  Depth_Format      : aliased VkSurfaceFormatKHR               := (others => <>);
  Image_Format      : aliased VkSurfaceFormatKHR               := (others => <>);
  Surface_Details   : aliased VkSurfaceCapabilitiesKHR         := (others => <>);
  Device_Properties : aliased VkPhysicalDeviceProperties       := (others => <>);
  Format_Properties : aliased VkFormatProperties               := (others => <>);
  Memory_Properties : aliased VkPhysicalDeviceMemoryProperties := (others => <>);
                                                                      
  -- Shader uniform globals. These must match up to the data structure in Shaders/globals.glsl
  type Global_State is record
      MVP                      : Matrix_4D := (others => <>);
      Model                    : Matrix_4D := (others => <>);
      Projection               : Matrix_4D := (others => <>);
      Model_View               : Matrix_4D := (others => <>);
      Texture_0                : Matrix_4D := (others => <>); -- S, T, Q, Enabled
      Texture_1                : Matrix_4D := (others => <>); -- S, T, Q, Enabled
      Light_Projection         : Matrix_3D := (others => <>); -- S, T, Q
      Bump                     : Matrix_2D := (others => <>); -- S, T
      Diffuse                  : Matrix_2D := (others => <>); -- S, T
      Specular                 : Matrix_2D := (others => <>); -- S, T
      Texture                  : Matrix_2D := (others => <>); -- S, T
      Light_Falloff            : Vector_4D := (others => <>); -- S
      Screen_Correction_Factor : Vector_4D := (others => <>);
      Window_Coordinate        : Vector_4D := (others => <>);
      Diffuse_Modifier         : Vector_4D := (others => <>);
      Specular_Modifier        : Vector_4D := (others => <>);
      Color                    : Vector_4D := (others => <>);
      View_Origin              : Vector_4D := (others => <>);
      Global_Eye_Position      : Vector_4D := (others => <>);
      Vertex_Color_Modulate    : Vector_4D := (others => <>);
      Vertex_Color_Add         : Vector_4D := (others => <>);
      Local_Light_Origin       : Vector_4D := (others => <>);
      Local_View_Origin        : Vector_4D := (others => <>);
      Overbright               : Vector_4D := (others => <>);
      Enable_Skinning          : Vector_4D := (others => <>);
      Alpha_Test               : Vector_4D := (others => <>);
    end record with Convention => C;
  Globals : Global_State := (others => <>);
  
  ------------
  -- Vertex --
  ------------
  
  -- Internal vertex type
  type Vertex_Internal_State is record
      Position : Vector_3D;
      Color    : Vector_3D;
      Texture  : Vector_2D;
    end record with Convention => C;
  
  ------------
  -- Shader --
  ------------
  
  -- Create shader model
  function Load_Shader (Path : Str) return Ptr is
    Result             : aliased Ptr        := NULL_PTR;
    Data               : aliased Array_Byte := Load (Path);
    Shader_Module_Info : aliased VkShaderModuleCreateInfo := (codeSize => Data'Length, pCode => Data'Address, others => <>);
    begin
      vkAssert (vkCreateShaderModule (Device, Shader_Module_Info'Unchecked_Access, null, Result'Unchecked_Access));
      return Result;
    end;
   
  --------------------
  -- Command Buffer --
  --------------------
 
  type Command_Buffer_State is new Controlled with record
      Data : aliased Ptr := NULL_PTR;
    end record;
    
  -- Controlled primitives
  procedure Initialize (Command_Buffer : in out Command_Buffer_State);
  procedure Finalize   (Command_Buffer : in out Command_Buffer_State);
  procedure Initialize (Command_Buffer : in out Command_Buffer_State) is
  
    -- 
    Begin_Info : aliased VkCommandBufferBeginInfo := (flags  => VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
                                                      others => <>);
 
    -- 
    Allocate_Info : aliased VkCommandBufferAllocateInfo := (level              => VK_COMMAND_BUFFER_LEVEL_PRIMARY,
                                                            commandPool        => Command_Pool,
                                                            commandBufferCount => 1,
                                                            others             => <>);
    begin
 
      -- 
      vkAssert (vkAllocateCommandBuffers (Device, Allocate_Info'Unchecked_Access, Command_Buffer.Data'Unchecked_Access));
      vkAssert (vkBeginCommandBuffer (Command_Buffer.Data, Begin_Info'Unchecked_Access));
    end;
  procedure Finalize (Command_Buffer : in out Command_Buffer_State) is
  
    -- 
    Submit_Info : aliased VkSubmitInfo := (commandBufferCount => 1,
                                           pCommandBuffers    => Command_Buffer.Data'Unchecked_Access,
                                           others             => <>);
    begin
  
      -- 
      vkAssert (vkEndCommandBuffer (Command_Buffer.Data));
      vkAssert (vkQueueSubmit      (Graphics_Queue, 1, Submit_Info'Unchecked_Access, NULL_PTR));
      vkAssert (vkQueueWaitIdle    (Graphics_Queue));
      vkFreeCommandBuffers (Device, Command_Pool, 1, Command_Buffer.Data'Unchecked_Access);
    end;
 
  -------------------
  -- Memory Buffer --
  -------------------

  type Memory_Buffer_State (Size : Int_64_Unsigned_C; Usage, Properties : Int_Unsigned_C) is new Controlled with record
      Data   : aliased Ptr := NULL_PTR;
      Memory : aliased Ptr := NULL_PTR;
    end record;
    
  -- Controlled primitives
  procedure Initialize (Memory_Buffer : in out Memory_Buffer_State);
  procedure Finalize   (Memory_Buffer : in out Memory_Buffer_State);
  procedure Initialize (Memory_Buffer : in out Memory_Buffer_State) is

    -- 
    Memory_Requirements : aliased VkMemoryRequirements := (others => <>);

    -- 
    Allocate_Info : aliased VkMemoryAllocateInfo := (others => <>);

    -- 
    Buffer_Info : aliased VkBufferCreateInfo := (size        => Memory_Buffer.Size,
                                                 usage       => Memory_Buffer.Usage,
                                                 sharingMode => VK_SHARING_MODE_EXCLUSIVE,
                                                 others      => <>);
    begin
    
      -- 
      vkAssert (vkCreateBuffer (Device, Buffer_Info'Unchecked_Access, null, Memory_Buffer.Data'Unchecked_Access));
      Allocate_Info.allocationSize := Memory_Requirements.size;

      -- 
      for I in 1..Memory_Properties.memoryTypeCount loop
        if (Memory_Requirements.memoryTypeBits and 2 ** Natural (I)) > 0 and then
           (Memory_Properties.memoryTypes (Int (I)).propertyFlags and Memory_Buffer.Properties) = Memory_Buffer.Properties
        then
          Allocate_Info.memoryTypeIndex := I;
          exit;
        elsif I = Memory_Properties.memoryTypeCount then raise Program_Error with "Failed to find suitable memory type!?"; end if;
      end loop;

      -- 
      vkAssert (vkAllocateMemory   (Device, Allocate_Info'Unchecked_Access, null, Memory_Buffer.Memory'Unchecked_Access));
      vkAssert (vkBindBufferMemory (Device, Memory_Buffer.Data, Memory_Buffer.Memory, 0));
    end;
  procedure Finalize (Memory_Buffer : in out Memory_Buffer_State) is
    begin
      vkDestroyBuffer (device, Memory_Buffer.Data,   null);
      vkFreeMemory    (device, Memory_Buffer.Memory, null);
    end;

  -- Copy for staging
  function Copy (Memory_Buffer : Memory_Buffer_State) return Memory_Buffer_State is

    --
    Command_Buffer : Command_Buffer_State;
    
    -- 
    Result : Memory_Buffer_State := Memory_Buffer;

    -- 
    Copy_Region : aliased VkBufferCopy := (size   => Memory_Buffer.Size,
                                           others => <>);
    begin
      
      -- 
      vkCmdCopyBuffer (Command_Buffer.Data, Memory_Buffer.Data, Result.Data, 1, Copy_Region'Unchecked_Access);
      return Result;
    end;

  -----------
  -- Image --
  -----------
  
  type Image_State (Width, Height, Tiling, Usage, Properties, Format : Int_Unsigned_C) is new Controlled with record
      Old_Layout : aliased Int_Unsigned_C := VK_IMAGE_LAYOUT_UNDEFINED; -- Do not touch this !!!
      Layout     : aliased Int_Unsigned_C := 0;
      Data       : aliased Ptr            := NULL_PTR;
      Memory     : aliased Ptr            := NULL_PTR;
    end record;

  -- Controlled primitives
  procedure Initialize (Image : in out Image_State);
  procedure Finalize   (Image : in out Image_State);
  procedure Adjust     (Image : in out Image_State);
  procedure Initialize (Image : in out Image_State) is
 
    -- 
    Memory_Requirements : aliased VkMemoryRequirements := (others => <>);
 
    -- 
    Image_Info : aliased VkImageCreateInfo := (imageType     => VK_IMAGE_TYPE_2D,
                                               extent        => (Image.Width, Image.Height, Depth => 1),
                                               mipLevels     => 1,
                                               arrayLayers   => 1,
                                               format        => Image.Format,
                                               tiling        => Image.Tiling,
                                               initialLayout => VK_IMAGE_LAYOUT_PREINITIALIZED,
                                               usage         => Image.Usage,
                                               samples       => VK_SAMPLE_COUNT_1_BIT,
                                               sharingMode   => VK_SHARING_MODE_EXCLUSIVE,
                                               others        => <>);
  
    -- 
    Allocate_Info : aliased VkMemoryAllocateInfo := (others => <>);
    begin
 
      -- 
      vkAssert (vkCreateImage (Device, Image_Info'Unchecked_Access, null, Image.Data'Unchecked_Access));
      
      -- 
      vkGetImageMemoryRequirements (Device, Image.Data, Memory_Requirements'Unchecked_Access);
      Allocate_Info.allocationSize := Memory_Requirements.size;
      
      -- 
      for I in 0..Memory_Properties.memoryTypeCount - 1 loop
        if (Memory_Requirements.memoryTypeBits and 2 ** Natural (I)) > 0
          and (Memory_Properties.memoryTypes (Int (I) + 1).propertyFlags and Image.Properties) = Image.Properties
        then
          Allocate_Info.memoryTypeIndex := I;
          exit;
        end if;
        Assert (I /= Memory_Properties.memoryTypeCount - 1);
      end loop;
      
      -- 
      vkAssert (vkAllocateMemory  (Device, Allocate_Info'Unchecked_Access, null, Image.Memory'Unchecked_Access));
      vkAssert (vkBindImageMemory (Device, Image.Data, Image.Memory, 0));
    end;
  procedure Finalize (Image : in out Image_State) is
    begin
      null; -- !!!
    end;
  procedure Adjust (Image : in out Image_State) is

    -- 
    Command_Buffer : Command_Buffer_State;

    --  
    Barrier : aliased VkImageMemoryBarrier := (oldLayout           => Image.Old_Layout,
                                               newLayout           => Image.Layout,
                                               srcQueueFamilyIndex => VK_QUEUE_FAMILY_IGNORED,
                                               dstQueueFamilyIndex => VK_QUEUE_FAMILY_IGNORED,
                                               image               => Image.Data,
                                               subresourceRange => (baseMipLevel   => 0,
                                                                    levelCount     => 1,
                                                                    baseArrayLayer => 0,
                                                                    layerCount     => 1,
                                                                    others         => <>), others => <>);
    begin

      -- 
      if Image.Layout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL then
        Barrier.subresourceRange.aspectMask := VK_IMAGE_ASPECT_DEPTH_BIT;
        if Image.Layout = VK_FORMAT_D32_SFLOAT_S8_UINT or Image.Layout = VK_FORMAT_D24_UNORM_S8_UINT then
          Barrier.subresourceRange.aspectMask := Barrier.subresourceRange.aspectMask or VK_IMAGE_ASPECT_STENCIL_BIT;
        end if;
      else Barrier.subresourceRange.aspectMask := VK_IMAGE_ASPECT_COLOR_BIT; end if;

      -- Set new image layout
      if Image.Old_Layout = VK_IMAGE_LAYOUT_PREINITIALIZED and Image.Layout = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL then
        Barrier.srcAccessMask := VK_ACCESS_HOST_WRITE_BIT;
        Barrier.dstAccessMask := VK_ACCESS_TRANSFER_READ_BIT;
      elsif Image.Old_Layout = VK_IMAGE_LAYOUT_PREINITIALIZED and Image.Layout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL then
        Barrier.srcAccessMask := VK_ACCESS_HOST_WRITE_BIT;
        Barrier.dstAccessMask := VK_ACCESS_TRANSFER_WRITE_BIT;
      elsif Image.Old_Layout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL and Image.Layout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL then
        Barrier.srcAccessMask := VK_ACCESS_TRANSFER_WRITE_BIT;
        Barrier.dstAccessMask := VK_ACCESS_SHADER_READ_BIT;
      elsif Image.Old_Layout = VK_IMAGE_LAYOUT_UNDEFINED and Image.Layout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL then
        Barrier.srcAccessMask := 0;
        Barrier.dstAccessMask := VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT or VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
      else raise Program_Error with "Unknown image layout transition?!"; end if;
      Image.Old_Layout := Image.Layout;

      -- Send transition commands
      vkCmdPipelineBarrier (commandBuffer            => Command_Buffer.Data,
                            srcStageMask             => VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT,
                            dstStageMask             => VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT,
                            dependencyFlags          => 0,
                            memoryBarrierCount       => 0,
                            pMemoryBarriers          => null,
                            bufferMemoryBarrierCount => 0,
                            pBufferMemoryBarriers    => null,
                            imageMemoryBarrierCount  => 1,
                            pImageMemoryBarriers     => Barrier'Unchecked_Access);
    end;
    
  function Make_View (Image_Data : Ptr; Format, Aspect_Flags : Int_Unsigned_C) return Ptr is
    Result : aliased Ptr := NULL_PTR;
  
    -- 
    Image_View_Info : aliased VkImageViewCreateInfo := (image            => Image_Data,
                                                        viewType         => VK_IMAGE_VIEW_TYPE_2D,
                                                        format           => Format,
                                                        subresourceRange => (aspectMask => Aspect_Flags,
                                                                             levelCount => 1,
                                                                             layerCount => 1,
                                                                             others     => <>), others => <>);
    begin
      vkAssert (vkCreateImageView (Device, Image_View_Info'Unchecked_Access, null, Result'Unchecked_Access));
      return Result;
    end;
    
  -------------
  -- Texture --
  -------------
--  
--    type Texture_State (Width, Height : Int_Unsigned_C) is new Controlled with record
--        Data : Ptr;
--      end record;
--  
--    -- Controlled primitives
--    procedure Initialize (Texture : in out Texture_State);
--    procedure Finalize   (Texture : in out Texture_State);
--    procedure Initialize (Texture : in out Texture_State) is
--  
--      -- 
--      Subresource : aliased VkImageSubresource := (VK_IMAGE_ASPECT_COLOR_BIT, others => <>);
--  
--      -- 
--      Staging_Image_Layout : aliased VkSubresourceLayout := (others => <>);
--      
--      -- 
--      region : VkBufferImageCopy := (imageExtent       => (Texture.Width, Texture.Height, 1),
--                                     imageSubresource  => (aspectMask     => VK_IMAGE_ASPECT_COLOR_BIT,
--                                                           mipLevel       => 0,
--                                                           baseArrayLayer => 0,
--                                                           layerCount     => 1), others => <>);
--      
--      -- 
--      Staging_Image_Memory, Free_Memory : aliased Buffer_State := (others => <>);
--      
--      --
--      Staging_Image : Image_State (Texture.Width,
--                                   Texture.Height,
--                                   VK_IMAGE_TILING_LINEAR,
--                                   VK_IMAGE_USAGE_TRANSFER_SRC_BIT,
--                                   VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT or VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
--      begin
--        vkGetImageSubresourceLayout (Device, Staging_Image.Data, Subresource'Unchecked_Access, Staging_Image_Layout'Unchecked_Access);
--        vkMapMemory (Device, Staging_Image.Memory, 0, imageSize, 0, data);
--  
--        --
--        vkUnmapMemory (Device, Staging_Image.Memory);
--        declare
--        
--        Create_Image (Texture.Width, Texture.Height, VK_FORMAT_R8G8B8A8_UNORM, VK_IMAGE_TILING_OPTIMAL, VK_IMAGE_USAGE_TRANSFER_DST_BIT or VK_IMAGE_USAGE_SAMPLED_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, Texture, textureImageMemory);
--        begin
--          Staging.Set_Properties (VK_FORMAT_R8G8B8A8_UNORM, VK_IMAGE_LAYOUT_PREINITIALIZED, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL);
--          vkCmdCopyBufferToImage(commandBuffer, buffer, image, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, &region);
--          Texture.Set_Properties  (VK_FORMAT_R8G8B8A8_UNORM, VK_IMAGE_LAYOUT_PREINITIALIZED, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
--          Copy_Image (stagingImage, Texture, texWidth, texHeight);
--        
--          vkCmdCopyBufferToImage(commandBuffer, buffer, image, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, &region);
--        
--          Texture.Set_Layout (Texture, VK_FORMAT_R8G8B8A8_UNORM, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
--        end;
--      end;
--    procedure Finalize (Texture : in out Texture_State) is
--      begin
--        null;
--      end;

--    --------------
--    -- Geometry --
--    --------------
--  
--    type Geometry_State is new Controlled with record
--        Mesh          : Mesh_State;
--        Index_Buffer  : aliased Ptr := NULL_ADDRESS;
--        Vertex_Buffer : aliased Ptr := NULL_ADDRESS;
--      end record;  
--    procedure Initialize (Geometry : in out Geometry_State);
--    procedure Adjust     (Geometry : in out Geometry_State);
--    procedure Finalize   (Geometry : in out Geometry_State);
--    procedure Initialize (Geometry : in out Geometry_State) is
--      Raw_Indicies   : aliased Array_Real_32_C (1..Indicies'Length * 3) := (others => 0.0); -- For-expression possible ???
--      Buffer_Size    : aliased VkDeviceSize                             := Raw_Indicies'Object_Size;
--      
--                 
--      begin
--  
--        -- Create vertex buffer
--        declare      
--        Staging_Buffer : Memory_Buffer_State (Size       => Raw_Indicies'Object_Size,
--                                              Usage      => VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
--                                              Properties => VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT or VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
--        begin
--          vkMapMemory (Device, Staging_Buffer.Memory, 0, Staging_Buffer.Size, 0, Data'Address);
--          vertices.data := Staging_Buffer.data;
--          vkUnmapMemory (Device, Staging_Buffer.Memory);
--          Create_Buffer (bufferSize, VK_BUFFER_USAGE_TRANSFER_DST_BIT or VK_BUFFER_USAGE_VERTEX_BUFFER_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, vertexBuffer, vertexBufferMemory);
--          vertexBuffer := Copy (Staging_Buffer);
--        end;
--  
--        -- Create index buffer
--        Create_Buffer (bufferSize, VK_BUFFER_USAGE_TRANSFER_SRC_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT or VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, stagingBuffer, stagingBufferMemory);
--        vkMapMemory   (Device, stagingBufferMemory, 0, bufferSize, 0, Data'Address);
--        memcpy        (data, indices.data, bufferSize'Object_Size / Byte'Size);
--        vkUnmapMemory (Device, stagingBufferMemory);
--        Create_Buffer (bufferSize, VK_BUFFER_USAGE_TRANSFER_DST_BIT or VK_BUFFER_USAGE_INDEX_BUFFER_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, indexBuffer, indexBufferMemory);
--        indexBuffer := Copy (Staging_Buffer);
--      end;
--    procedure Adjust (Geometry : in out Geometry_State) is
--      Stage_Flags : aliased Int_Unsigned_C := VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
--  
--      -- 
--      Submit_Info : aliased VkSubmitInfo := (waitSemaphoreCount   => 1,
--                                             pWaitSemaphores      => Acquire_Status'Unchecked_Access,
--                                             pWaitDstStageMask    => VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
--                                             commandBufferCount   => 1,
--                                             pCommandBuffers      => Commands (Int (Image_Index))'Unchecked_Access,
--                                             signalSemaphoreCount => 1,
--                                             pSignalSemaphores    => Render_Status'Unchecked_Access,
--                                             others               => <>);
--      begin
--        vkAssert (vkQueueSubmit (Graphics_Queue, 1, Submit_Info'Unchecked_Access, NULL_PTR));
--      end;

  ------------
  -- Update --
  ------------

  -- Recreate the swapchain among other things
  procedure Update is

    -- 
    Command_Begin_Info : aliased VkCommandBufferBeginInfo := (flags => VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT, others => <>);
    
    --
    Frame_Buffer_Info : aliased VkFramebufferCreateInfo  := (others => <>);

    -- 
    Swap_Chain_Info : aliased VkSwapchainCreateInfoKHR := (surface          => Surface,
                                                           imageArrayLayers => 1,
                                                           imageFormat      => Image_Format.format,
                                                           imageColorSpace  => Image_Format.colorSpace,
                                                           imageUsage       => VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
                                                           compositeAlpha   => VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
                                                           presentMode      => VK_PRESENT_MODE_MAILBOX_KHR,
                                                           clipped          => VK_TRUE,
                                                           oldSwapchain     => Swap_Chain,
                                                           preTransform     => Surface_Details.currentTransform,
                                                           
      -- Find the resolution of the swap chain images - some window managers force us to pick
      imageExtent => (Int_Unsigned_C (Current_Width), Int_Unsigned_C (Current_Height)),
                     --(if Surface_Details.currentExtent.width /= Int_Unsigned_C'Last then Surface_Details.currentExtent
                     -- else (Int_Unsigned_C'Max (Surface_Details.minImageExtent.width,  Int_Unsigned_C (Current_Width)),
                     --       Int_Unsigned_C'Max (Surface_Details.minImageExtent.height, Int_Unsigned_C (Current_Height)))),
                            
      -- Pick the queue length or number of images in the swap-chain - push for one more than the minimum for triple buffering
      minImageCount => (if Surface_Details.maxImageCount > 0 and Surface_Details.minImageCount + 1 > Surface_Details.maxImageCount
                        then Surface_Details.maxImageCount
                        else Surface_Details.minImageCount + 1), others => <>);

    --
    Memory_Barrier : aliased VkImageMemoryBarrier := (srcQueueFamilyIndex => VK_QUEUE_FAMILY_IGNORED,
                                                      dstQueueFamilyIndex => VK_QUEUE_FAMILY_IGNORED,
                                                      oldLayout           => VK_IMAGE_LAYOUT_UNDEFINED,
                                                      newLayout           => VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
                                                      subresourceRange    => (VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1),
                                                      others              => <>);

    -- 
    Submit_Info : aliased VkSubmitInfo := (commandBufferCount => 1,
                                           pCommandBuffers    => Setup_Buffer'Unchecked_Access,
                                           others             => <>);
  
    -- 
    Color_Reference : aliased VkAttachmentReference := (attachment => 0,
                                                        layout     => VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                                        others     => <>);
  
    -- 
    Depth_Reference : aliased VkAttachmentReference := (attachment => 1,
                                                        layout     => VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                                        others     => <>);
  
    -- 
    Subpasses_Description : aliased Array_VkSubpassDescription := (1 => (pipelineBindPoint       => VK_PIPELINE_BIND_POINT_GRAPHICS,
                                                                         colorAttachmentCount    => 1,
                                                                         pColorAttachments       => Color_Reference'Unchecked_Access,
                                                                         --pDepthStencilAttachment => Depth_Reference'Unchecked_Access,
                                                                         others                  => <>));
  
    -- 
    Subpass_Dependencies : aliased Array_VkSubpassDependency := (1 => (srcSubpass    => VK_SUBPASS_EXTERNAL,
                                                                       dstSubpass    => 0,
                                                                       srcStageMask  => VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                                                                       srcAccessMask => 0,
                                                                       dstStageMask  => VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                                                                       dstAccessMask => VK_ACCESS_COLOR_ATTACHMENT_READ_BIT or VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
                                                                       others        => <>));
 
    -- 
    Attachments : aliased Array_VkAttachmentDescription := ((format         => VK_FORMAT_B8G8R8A8_UNORM, -- Image_Format.format,
                                                             samples        => VK_SAMPLE_COUNT_1_BIT,
                                                             loadOp         => VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                             storeOp        => VK_ATTACHMENT_STORE_OP_STORE,
                                                             stencilLoadOp  => VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                             stencilStoreOp => VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                             initialLayout  => VK_IMAGE_LAYOUT_UNDEFINED,
                                                             finalLayout    => VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
                                                             others         => <>),
                                                            (format         => Depth_Format.format,
                                                             samples        => VK_SAMPLE_COUNT_1_BIT,
                                                             loadOp         => VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                             storeOp        => VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                             stencilLoadOp  => VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                             stencilStoreOp => VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                             initialLayout  => VK_IMAGE_LAYOUT_UNDEFINED,
                                                             finalLayout    => VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                                             others         => <>));
  
    -- 
    Render_Pass_Info : aliased VkRenderPassCreateInfo := (attachmentCount => 1, -- Attachments'Length,
                                                          pAttachments    => Attachments (1)'Unchecked_Access,
                                                          subpassCount    => Subpasses_Description'Length,
                                                          pSubpasses      => Subpasses_Description (1)'Unchecked_Access,
                                                          dependencyCount => Subpass_Dependencies'Length,
                                                          pDependencies   => Subpass_Dependencies (1)'Unchecked_Access,
                                                          others          => <>);
 
    -- 
    Vertex_Shader_Name : aliased Str_8_C := To_Str_8_C ("main" & NULL_STR_8);
    Vertex_Stage_Info  : aliased VkPipelineShaderStageCreateInfo := (stage  => VK_SHADER_STAGE_VERTEX_BIT,
                                                                     module => Vertex_Shader_Model,
                                                                     pName  => Vertex_Shader_Name (0)'Unchecked_Access,
                                                                     others => <>);

    -- 
    Fragment_Shader_Name : aliased Str_8_C := To_Str_8_C ("main" & NULL_STR_8);
    Fragment_Stage_Info  : aliased VkPipelineShaderStageCreateInfo := (stage  => VK_SHADER_STAGE_FRAGMENT_BIT,
                                                                       module => Fragment_Shader_Model,
                                                                       pName  => Fragment_Shader_Name (0)'Unchecked_Access,
                                                                       others => <>);

    -- 
    Shader_Stages_Info : aliased Array_VkPipelineShaderStageCreateInfo := (Vertex_Stage_Info, Fragment_Stage_Info);
    
    -- 
    Vertex_Binding_Descriptions : aliased Array_VkVertexInputBindingDescription := (1 => (binding   => 0,
                                                                                          stride    => Vertex_Internal_State'Object_Size / Byte'Object_Size,
                                                                                          inputRate => VK_VERTEX_INPUT_RATE_VERTEX));
        
    -- 
    Obj_Vertex_Internal_State : Vertex_Internal_State;
    Vertex_Attribute_Descriptions : aliased Array_VkVertexInputAttributeDescription := ((binding  => 0,
                                                                                         location => 0,
                                                                                         format   => VK_FORMAT_R32G32B32_SFLOAT,
                                                                                         offset   => Obj_Vertex_Internal_State.Position'Position / Byte'Object_Size),
                                                                                        (binding  => 0,
                                                                                         location => 1,
                                                                                         format   => VK_FORMAT_R32G32B32_SFLOAT,
                                                                                         offset   => Obj_Vertex_Internal_State.Color'Position / Byte'Object_Size),
                                                                                        (binding  => 0,
                                                                                         location => 2,
                                                                                         format   => VK_FORMAT_R32G32_SFLOAT,
                                                                                         offset   => Obj_Vertex_Internal_State.Texture'Position / Byte'Object_Size));
        
    -- 
    Vertex_Input_Info : aliased VkPipelineVertexInputStateCreateInfo := (vertexBindingDescriptionCount   => 0, --Vertex_Binding_Descriptions'Length,
                                                                         pVertexBindingDescriptions      => null, --Vertex_Binding_Descriptions (1)'Unchecked_Access,
                                                                         vertexAttributeDescriptionCount => 0, --Vertex_Attribute_Descriptions'Length,
                                                                         pVertexAttributeDescriptions    => null, --Vertex_Attribute_Descriptions (1)'Unchecked_Access,
                                                                         others                          => <>);

    --
    Input_Assembly_Info : aliased VkPipelineInputAssemblyStateCreateInfo := (topology => VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
                                                                             others   => <>);

    -- 
    Viewport : aliased VkViewport := (x        => 0.0,
                                      y        => 0.0,
                                      width    => Real_C (Swap_Chain_Info.imageExtent.width),
                                      height   => Real_C (Swap_Chain_Info.imageExtent.height),
                                      minDepth => 0.0,
                                      maxDepth => 1.0,
                                      others   => <>);

    -- 
    Scissor : aliased VkRect2D := (offset => (0, 0),
                                   extent => Swap_Chain_Info.imageExtent,
                                   others => <>);

    -- 
    Viewport_State_Info : aliased VkPipelineViewportStateCreateInfo := (viewportCount => 1,
                                                                        pViewports    => Viewport'Unchecked_Access,
                                                                        scissorCount  => 1,
                                                                        pScissors     => Scissor'Unchecked_Access,
                                                                        others        => <>);

    -- 
    Rasterizer_Info : aliased VkPipelineRasterizationStateCreateInfo := (polygonMode => VK_POLYGON_MODE_FILL,
                                                                         lineWidth   => 1.0,
                                                                         cullMode    => VK_CULL_MODE_BACK_BIT,
                                                                         frontFace   => VK_FRONT_FACE_COUNTER_CLOCKWISE,
                                                                         others      => <>);

    -- 
    Multisample_Info : aliased VkPipelineMultisampleStateCreateInfo := (sampleShadingEnable  => 0,
                                                                        rasterizationSamples => VK_SAMPLE_COUNT_1_BIT,
                                                                        others               => <>);

    -- 
    Depth_Stencil_Info : aliased VkPipelineDepthStencilStateCreateInfo := (depthTestEnable       => 1,
                                                                           depthWriteEnable      => 1,
                                                                           depthCompareOp        => VK_COMPARE_OP_LESS,
                                                                           depthBoundsTestEnable => 0,
                                                                           stencilTestEnable     => 0,
                                                                           others                => <>);

    -- 
    Color_Blend_Attachment : aliased VkPipelineColorBlendAttachmentState := (blendEnable    => 0,
                                                                             colorWriteMask => VK_COLOR_COMPONENT_R_BIT or VK_COLOR_COMPONENT_G_BIT or
                                                                                               VK_COLOR_COMPONENT_B_BIT or VK_COLOR_COMPONENT_A_BIT,
                                                                             others         => <>);

    -- 
    Color_Blend_Info : aliased VkPipelineColorBlendStateCreateInfo := (logicOpEnable   => 0,
                                                                       logicOp         => VK_LOGIC_OP_COPY,
                                                                       attachmentCount => 1,
                                                                       pAttachments    => Color_Blend_Attachment'Unchecked_Access,
                                                                       blendConstants  => (others => 0.0),
                                                                       others          => <>);

    -- 
    Pipeline_Layout_Info : aliased VkPipelineLayoutCreateInfo := (setLayoutCount => 0, -- 1,
                                                                  --pSetLayouts    => Descriptor_Set_Layout'Unchecked_Access,
                                                                  others         => <>);     
       
    -- 
    Pipeline_Info : aliased VkGraphicsPipelineCreateInfo := (stageCount          => 2,
                                                             pStages             => Shader_Stages_Info (1)'Unchecked_Access,
                                                             pVertexInputState   => Vertex_Input_Info'Unchecked_Access,
                                                             pInputAssemblyState => Input_Assembly_Info'Unchecked_Access,
                                                             pViewportState      => Viewport_State_Info'Unchecked_Access,
                                                             pRasterizationState => Rasterizer_Info'Unchecked_Access,
                                                             pMultisampleState   => Multisample_Info'Unchecked_Access,
                                                             pDepthStencilState  => Depth_Stencil_Info'Unchecked_Access,
                                                             pColorBlendState    => Color_Blend_Info'Unchecked_Access,
                                                             others              => <>);
  
    -- 
    Render_Pass_Begin_Info : aliased VkRenderPassBeginInfo := (renderArea => (Offset => (0, 0), Extent => Swap_Chain_Info.imageExtent),
                                                               others     => <>);

    -- 
    Allocate_Info : aliased VkCommandBufferAllocateInfo := (commandPool        => Command_Pool,
                                                            level              => VK_COMMAND_BUFFER_LEVEL_PRIMARY,
                                                            commandBufferCount => Commands'Length,
                                                            others             => <>);

    -- 
    Clear_Values : aliased Array_VkClearValue := ((color => (float32 => (0.0, 0.0, 0.0, 1.0)), others => <>),
                                                  (depthStencil => (1.0, 0), others => <>));
                                                  
    -- 
    Indexes : aliased Array_Int_Unsigned_C := (Graphics_Family, Present_Family);
    
    -- Start of Update
    begin
    
      -- Use multiple queues, if they differ use concurrent mode to avoid ownership
      if Graphics_Family /= Present_Family then 
        Swap_Chain_Info.imageSharingMode      := VK_SHARING_MODE_CONCURRENT;
        Swap_Chain_Info.queueFamilyIndexCount := Indexes'Length;
        Swap_Chain_Info.pQueueFamilyIndices   := Indexes (1)'Unchecked_Access;
      else Swap_Chain_Info.imageSharingMode := VK_SHARING_MODE_EXCLUSIVE; end if;
      vkAssert (vkCreateSwapchainKHR (Device, Swap_Chain_Info'Unchecked_Access, null, Swap_Chain'Unchecked_Access));
  
      -- Create image views
      vkAssert (vkGetSwapchainImagesKHR (device               => Device,
                                         swapchain            => Swap_Chain,
                                         pSwapchainImageCount => Swap_Chain_Info.minImageCount'Unchecked_Access,
                                         pSwapchainImages     => Images (1)'Unchecked_Access));
      for I in Images'Range loop
        declare
        Image_View_Info : aliased VkImageViewCreateInfo := (image            => Images (I),
                                                            viewType         => VK_IMAGE_VIEW_TYPE_2D,
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
          -- Image_Views (I) := Make_View (Images (I), Image_Format.format, VK_IMAGE_ASPECT_COLOR_BIT);
        end;
      end loop;
 
      -- Create renderer
      vkAssert (vkCreateRenderPass     (Device, Render_Pass_Info'Unchecked_Access,     null, Render_Pass'Unchecked_Access));
      vkAssert (vkCreatePipelineLayout (Device, Pipeline_Layout_Info'Unchecked_Access, null, Pipeline_Layout'Unchecked_Access));
      Pipeline_Info.layout     := Pipeline_Layout;
      Pipeline_Info.renderPass := Render_Pass;
      vkAssert (vkCreateGraphicsPipelines (Device, NULL_PTR, 1, Pipeline_Info'Unchecked_Access, null, Graphics_Pipeline'Unchecked_Access));
  
      -- Create depth resource
      declare
      Depth_Image : Image_State (Width      => Swap_Chain_Info.imageExtent.width,
                                 Height     => Swap_Chain_Info.imageExtent.height,
                                 Format     => Depth_Format.format,
                                 Tiling     => VK_IMAGE_TILING_OPTIMAL,
                                 Usage      => VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT,
                                   Properties => VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
      Depth_Image_View : aliased Ptr       := Make_View (Depth_Image.Data, Depth_Format.format, 2);--VK_IMAGE_ASPECT_DEPTH_BIT);
      Attachments      : aliased Array_Ptr := (NULL_PTR, Depth_Image_View);
      begin
--        Image_Layout (depthImage, depthFormat, VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL);

        -- Create frame buffers
        for I in Image_Views'Range loop
          Attachments (1) := Image_Views (I);
          Frame_Buffer_Info := (renderPass      => Render_Pass,
                                attachmentCount => 1, -- Attachments'Length,
                                pAttachments    => Attachments (1)'Unchecked_Access, -- Attachments (1)'Unchecked_Access,
                                width           => Swap_Chain_Info.imageExtent.width,
                                height          => Swap_Chain_Info.imageExtent.height,
                                layers          => 1,
                                others          => <>);
          vkAssert (vkCreateFramebuffer (Device, Frame_Buffer_Info'Unchecked_Access, null, Swap_Chain_Buffers (I)'Unchecked_Access));
        end loop;
      end;
      
      -- Destroy current command buffers if there are any
      if Commands (1) /= NULL_PTR then vkFreeCommandBuffers (Device, Command_Pool, Commands'Length, Commands (1)'Unchecked_Access); end if;
      
      -- Create command buffers
      vkAssert (vkAllocateCommandBuffers (Device, Allocate_Info'Unchecked_Access, Commands (1)'Unchecked_Access));
      for I in Commands'Range loop
        Render_Pass_Begin_Info.renderPass      := Render_Pass;
        Render_Pass_Begin_Info.framebuffer     := Swap_Chain_Buffers (I);
        Render_Pass_Begin_Info.clearValueCount := 1; -- Clear_Values'Length;
        Render_Pass_Begin_Info.pClearValues    := Clear_Values (1)'Unchecked_Access;
        vkAssert (vkBeginCommandBuffer (Commands (I), Command_Begin_Info'Unchecked_Access));
        vkCmdBeginRenderPass           (Commands (I), Render_Pass_Begin_Info'Unchecked_Access, VK_SUBPASS_CONTENTS_INLINE);
        vkCmdBindPipeline              (Commands (I), VK_PIPELINE_BIND_POINT_GRAPHICS, Graphics_Pipeline);
        --vkCmdBindVertexBuffers         (Commands (I), 0, 1, vertexBuffers, offsets);
        --vkCmdBindIndexBuffer           (Commands (I), indexBuffer, 0, VK_INDEX_TYPE_UINT32);
        --vkCmdBindDescriptorSets        (Commands (I), VK_PIPELINE_BIND_POINT_GRAPHICS, Pipeline_Layout, 0, 1, Descriptor_Set'Unchecked_Access, 0, NULL_PTR);
        --vkCmdDrawIndexed               (Commands (I), indices.size, 1, 0, 0, 0);
        vkCmdDraw (Commands (I), 3, 1, 0, 0);
        vkCmdEndRenderPass             (Commands (I));
        vkAssert (vkEndCommandBuffer   (Commands (I)));
      end loop;
    end;
     
  -------------
  -- Present --
  -------------
                                           
  -- Acquire the rendered image, show it in the window, and handle changes to the resolution
  procedure Present is
  
    -- 
    Present_Info : aliased VkPresentInfoKHR := (swapchainCount     => 1,
                                                pSwapchains        => Swap_Chain'Unchecked_Access,
                                                waitSemaphoreCount => 1,
                                                pWaitSemaphores    => Render_Status'Unchecked_Access,
                                                others             => <>);
    Wait_Stages : aliased Array_Int_Unsigned_C := (1 => VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT);
 
    -- 
    Submit_Info : aliased VkSubmitInfo := (commandBufferCount   => 1,
                                           pWaitDstStageMask    => Wait_Stages (1)'Unchecked_Access,
                                           waitSemaphoreCount   => 1,
                                           pWaitSemaphores      => Acquire_Status'Unchecked_Access,
                                           signalSemaphoreCount => 1,
                                           pSignalSemaphores    => Render_Status'Unchecked_Access,
                                           others               => <>);
    begin
    
      -- Recreate the swapchain and pipeline of the resolution changes
      if Current_Width /= Windowed_Width.Get or Current_Height /= Windowed_Height.Get then
        Line ("Update");
        Current_Width  := Windowed_Width.Get;
        Current_Height := Windowed_Height.Get;
        Update;
      end if;
 
      -- Fetch the next image from the swap chain
      case vkAcquireNextImageKHR (Device      => Device,
                                  swapchain   => Swap_Chain,
                                  timeout     => Int_64_Unsigned_C'Last,
                                  semaphore   => Acquire_Status,
                                  fence       => NULL_PTR,
                                  pImageIndex => Image_Index'Unchecked_Access) is
        when VK_SUCCESS | VK_SUBOPTIMAL_KHR => null;
        when VK_ERROR_OUT_OF_DATE_KHR => Update;
        when others => raise Program_Error with "Failed to acquire swap chain image!";
      end case;
      Present_Info.pImageIndices := Image_Index'Unchecked_Access;
      Submit_Info.pCommandBuffers := Commands (Int (Image_Index) + 1)'Unchecked_Access;
      vkAssert (vkQueueSubmit (Graphics_Queue, 1, Submit_Info'Unchecked_Access, NULL_PTR));
  
      -- Show the resulting image
      case vkQueuePresentKHR (Present_Queue, Present_Info'Unchecked_Access) is
        when VK_SUCCESS => null;
        when VK_ERROR_OUT_OF_DATE_KHR | VK_SUBOPTIMAL_KHR => Update;
        when others => raise Program_Error with "Failed to present swap chain image!";
      end case;
      vkAssert (vkQueueWaitIdle (Present_Queue));
    end;
     
  ----------------
  -- Initialize --
  ----------------
  
  procedure Initialize is

    -- Load all of the function pointers from a driver
    procedure Initialize_Vulkan_Subprograms is new Neo.API.Vulkan.Initialize (Get_Vulkan_Subprogram);
  
    -- Application details to allow tools and drivers to maintain compatibility
    Engine_Name      : aliased Str_8_C := To_Str_8_C (S (OS_Info.App_Name));
    Application_Name : aliased Str_8_C := To_Str_8_C (S (OS_Info.App_Name));
    Application_Info : aliased VkApplicationInfo := (pApplicationName   => Application_Name (Application_Name'First)'Unchecked_Access,
                                                     applicationVersion => VK_MAKE_VERSION (1, 0, 0),
                                                     pEngineName        => Engine_Name (Engine_Name'First)'Unchecked_Access,
                                                     apiVersion         => VK_MAKE_VERSION (1, 0, 0),
                                                     others             => <>);
                                                     
    -- Enabled extensions and layers for instance and device creation
    type Instance_Extensions_State is record
        One : Address := VK_KHR_SURFACE_EXTENSION_NAME'Address;
        Two : Address := VK_KHR_WIN32_SURFACE_EXTENSION_NAME'Address;
      end record with Convention => C;
    type Debug_Layers_State is record
        One : Address := VK_LAYER_LUNARG_API_DUMP_EXTENSION_NAME'Address;
      end record with Convention => C;
    Instance_Extensions : aliased Instance_Extensions_State := (others => <>); 
    Debug_Layers        : aliased Debug_Layers_State        := (others => <>);
    
    -- Creation details for the Vulkan instance - application info
    Instance_Info : aliased VkInstanceCreateInfo := (pApplicationInfo        => Application_Info'Unchecked_Access,
                                                     enabledExtensionCount   => 2,
                                                     ppenabledExtensionNames => Instance_Extensions'Address,
                                                     enabledLayerCount       => (if Debug then 1 else 0),
                                                     ppEnabledLayerNames     => (if Debug then Debug_Layers'Address else NULL_PTR),
                                                     others                  => <>);
    begin
      -- Load driver
      Initialize_Vulkan_Library;
      Initialize_Vulkan_Subprograms;

      -- Create instance and rendering surface
      vkAssert (vkCreateInstance (Instance_Info'Unchecked_Access, null, Instance'Unchecked_Access));
    end;
        
  --------------
  -- Finalize --
  --------------

  -- Kill the global instance state
  procedure Finalize is
    begin
      vkDestroySemaphore    (Device, Acquire_Status, null);
      vkDestroySemaphore    (Device, Render_Status,  null);
      vkDestroyCommandPool  (Device, Command_Pool,   null);
      vkDestroySwapchainKHR (Device, Swap_Chain,     null);
      vkDestroyDevice       (Device,   NULL_PTR);
      vkDestroyInstance     (Instance, NULL_PTR);
      Finalize_Vulkan_Library;
    end;
    
  ------------
  -- Adjust --
  ------------
    
  Is_First_Time : Bool := True;
  
  -- Get the game window ready for rendering and initialize the global variables in the spec
  procedure Adjust is 

    -- Temporary variables for fetching and testing physical devices  
    Current_Has_Debugging     : aliased Bool                             := False;
    Current_Device            : aliased Ptr                              := NULL_PTR;
    Count                     : aliased Int_Unsigned_C                   := 0;
    Current_Graphics_Family   : aliased Int_Unsigned_C                   := 0;
    Current_Present_Family    : aliased Int_Unsigned_C                   := 0;
    Current_Surface_Support   : aliased Int_Unsigned_C                   := 0;
    Current_Depth_Format      : aliased VkSurfaceFormatKHR               := (others => <>);
    Current_Image_Format      : aliased VkSurfaceFormatKHR               := (others => <>);
    Current_Format_Properties : aliased VkFormatProperties               := (others => <>);
    Current_Surface_Details   : aliased VkSurfaceCapabilitiesKHR         := (others => <>);
    Current_Device_Properties : aliased VkPhysicalDeviceProperties       := (others => <>);
    Current_Memory_Properties : aliased VkPhysicalDeviceMemoryProperties := (others => <>);

    -- Creation details for physical device queues - the count and priority
    Queue_Priority : aliased Real_C                        := 1.0;
    Queue_Infos    : aliased Array_VkDeviceQueueCreateInfo := (1..2 => (queueCount       => 1,
                                                                        pQueuePriorities => Queue_Priority'Unchecked_Access,
                                                                        others           => <>));
                                                                        
    -- List of enabled features used for Device_Info and device creation
    Device_Features : aliased VkPhysicalDeviceFeatures := (shaderClipDistance                     => VK_TRUE,
                                                           shaderCullDistance                     => VK_TRUE,
                                                           shaderTessellationAndGeometryPointSize => VK_TRUE,
                                                           geometryShader                         => VK_TRUE,
                                                           others                                 => <>);

    -- Enabled extensions and layers for instance and device creation
    type Device_Extensions_State is record
        One : Address := VK_KHR_SWAPCHAIN_EXTENSION_NAME'Address; -- VK_EXT_DEBUG_REPORT_EXTENSION_NAME'Address;
      end record with Convention => C;
    Device_Extensions : aliased Device_Extensions_State   := (others => <>);

    -- Creation details for a Vulkan non-physical device - features, queues, and debug layers
    Device_Info : aliased VkDeviceCreateInfo := (queueCreateInfoCount    => Queue_Infos'Length,
                                                 pQueueCreateInfos       => Queue_Infos (1)'Unchecked_Access,
                                                 pEnabledFeatures        => Device_Features'Unchecked_Access,
                                                 enabledExtensionCount   => (if Debug then 1 else 0),
                                                 ppenabledExtensionNames => (if Debug then Device_Extensions'Address else NULL_PTR),
                                                 others                  => <>);

    -- Creation details for the uniform buffer objects for global data used by shaders
    Layout_Bindings : aliased Array_VkDescriptorSetLayoutBinding := ((binding            => 0, -- UBO
                                                                      descriptorCount    => 1,
                                                                      descriptorType     => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                                                      stageFlags         => VK_SHADER_STAGE_VERTEX_BIT,
                                                                      others             => <>),
                                                                     (binding            => 1, -- Sampler
                                                                      descriptorCount    => 1,
                                                                      descriptorType     => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                                      stageFlags         => VK_SHADER_STAGE_FRAGMENT_BIT,
                                                                      others             => <>));
                                                                      
    -- Creation details for command pools are needed for the creation of command buffers
    Command_Pool_Info : aliased VkCommandPoolCreateInfo := (flags  => VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
                                                            others => <>);

    -- Descriptor for the command pool, descriptors are first bound into sets then these sets are bound to the pipeline
    Pool_Sizes : aliased Array_VkDescriptorPoolSize := ((typ             => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                                         descriptorCount => 1,
                                                         others          => <>),
                                                        (typ             => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                         descriptorCount => 1,
                                                         others          => <>));

    -- Creation details for the command pool descriptor
    Pool_Info : aliased VkDescriptorPoolCreateInfo := (poolSizeCount => Pool_Sizes'Length,
                                                       pPoolSizes    => Pool_Sizes'Unchecked_Access,
                                                       maxSets       => 1,
                                                       others        => <>);

    -- Creation details for atomic GPU queue semaphores 
    Semaphore_Info : aliased VkSemaphoreCreateInfo := (others => <>);

    -- Creation details for 
    Layout_Info : aliased VkDescriptorSetLayoutCreateInfo := (bindingCount => Layout_Bindings'Length,
                                                              pBindings    => Layout_Bindings (1)'Unchecked_Access,
                                                              others       => <>);

    -- 
    Allocate_Info : aliased VkDescriptorSetAllocateInfo := (--descriptorPool     => Pool_Info,
                                                            descriptorSetCount => 1,
                                                            --pSetLayouts        => Layout_Info,
                                                            others             => <>);

    -- 
    Buffer_Info : aliased VkDescriptorBufferInfo := (--buffer    => uniformBuffer,
                                                     offset    => 0,
                                                     --rangeSize => sizeof(UniformBufferObject),
                                                     others    => <>);

    -- 
    Image_Info : aliased VkDescriptorImageInfo := (imageLayout => VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                   --imageView   =>  ,
                                                   --sampler     => textureSampler,
                                                   others      => <>);

    -- 
    Write_Set : aliased array (1..2) of VkWriteDescriptorSet := ((--dstSet          => descriptorSet,
                                                                  dstBinding      => 0,
                                                                  descriptorType  => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                                                  descriptorCount => 1,
                                                                  pBufferInfo     => Buffer_Info'Unchecked_Access,
                                                                  others          => <>),
                                                                 (--dstSet          => descriptorSet,
                                                                  dstBinding      => 1,
                                                                  descriptorType  => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                                  descriptorCount => 1,
                                                                  pImageInfo      => Image_Info'Unchecked_Access,
                                                                  others          => <>));
    begin
    
      -- Create instance and rendering surface
      Surface := Create_Vulkan_Surface (Instance);
  
      -- Get physical devices
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
  
            -- Verify a queue exists with graphical surface support then aquire it
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
 
            -- We require mailbox mode so we can implement triple buffering
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
            Graphics_Family   := Current_Graphics_Family;
            Present_Family    := Current_Present_Family;
          end if;
 
        -- Ignore unsuitable devices and continue looping
        exception when others => null; end; end loop;
 
        -- We may have failed to find a device, so crash if thats the case
        Assert (Physical_Device);
        
        -- Log device information
        if Is_First_Time then
          Line ("Device Name: "       & To_Str (Device_Properties.deviceName));
          Line ("Driver Version:"     & Device_Properties.driverVersion'Wide_Image);
          Line ("Vulkan API Version:" & Device_Properties.apiVersion'Wide_Image);
          Is_First_Time := False;
        end if;
      exception when others => Line ("Your GPU adaptors are incompatible!"); raise Program_Error; end;

      -- Create the logical device based on the selected physical device
      Queue_Infos (2).queueFamilyIndex := Graphics_Family;
      Queue_Infos (1).queueFamilyIndex := Present_Family;
      if Graphics_Family = Present_Family then Device_Info.queueCreateInfoCount := 1; end if;
      vkAssert (vkCreateDevice (Physical_Device, Device_Info'Unchecked_Access, null, Device'Unchecked_Access));
      
      -- Fetch the appropriate queues
      vkGetDeviceQueue (Device, Graphics_Family, 0, Graphics_Queue'Unchecked_Access);
      vkGetDeviceQueue (Device, Present_Family, 0, Present_Queue'Unchecked_Access);
      
      -- Make descriptor set layout
      vkAssert (vkCreateDescriptorSetLayout (Device, Layout_Info'Unchecked_Access, null, Descriptor_Set_Layout'Unchecked_Access));
      
      -- Load the shaders
      Vertex_Shader_Model   := Load_Shader (S & ".." & S & ".." & S & "Shaders" & S & "depth-vert.spv");
      Fragment_Shader_Model := Load_Shader (S & ".." & S & ".." & S & "Shaders" & S & "depth-frag.spv");
        
      -- Create command pool
      Command_Pool_Info.queueFamilyIndex := Graphics_Family;
      vkAssert (vkCreateCommandPool (Device, Command_Pool_Info'Unchecked_Access, null, Command_Pool'Unchecked_Access));
      
      -- Create semaphores
      vkAssert (vkCreateSemaphore (Device, Semaphore_Info'Unchecked_Access, NULL_PTR, Acquire_Status'Unchecked_Access));
      vkAssert (vkCreateSemaphore (Device, Semaphore_Info'Unchecked_Access, NULL_PTR, Render_Status'Unchecked_Access));
      
      -- Create swap chain and ready the surface
      Update;

      -- Create descriptor sets
      --vkAssert (vkCreateDescriptorPool   (Device, Pool_Info'Unchecked_Access, null, descriptorPool.replace));
      --vkAssert (vkAllocateDescriptorSets (Device, Allocate_Info'Unchecked_Access, descriptorSet'Unchecked_Access));
      --vkAssert (vkUpdateDescriptorSets   (Device, Write_Set'Length, Write_Set'Unchecked_Access, 0, null));
      
      -- Kick off the tasks
      --Frontend_Task.Initialize;
      --Backend_Task.Initialize;
    exception when others => Line ("Vulkan driver not installed or up to date?"); raise Program_Error; end;
end;

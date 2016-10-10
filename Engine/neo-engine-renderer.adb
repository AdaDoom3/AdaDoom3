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

-- System independant code for rendering the global state
package body Neo.Engine.Renderer is

  -------------
  -- Globals --
  -------------

  -- Statuses
  Render_Status  : aliased VkSemaphore; 
  Acquire_Status : aliased VkSemaphore;

  -- Image ids
  VkImage       : Ptr;
  Desired_Images : aliased uint32_t = 3;
  Images      : aliased Vector_VkImage.Unsafe.Vector;
  Image_Index : aliased Int_32_Positive;

  -- Rendering ids
  VkQueue, VkSurface, VkInstance, VkRenderPass : Ptr;
  Swap_Chain             : aliased VkSwapchainKHR;
  Command_Pool           : aliased VkCommandPool;
  Swap_Chain_Create_Info : aliased VkSwapchainCreateInfoKHR;

  -- Device information
  Device                            : aliased VkDevice;
  Physical_Device                   : aliased VkPhysicalDevice;
  Physical_Device_Features          : aliased VkPhysicalDeviceFeatures
  Physical_Device_Properties        : aliased VkPhysicalDeviceProperties;
  Physical_Device_Memory_Properties : aliased VkPhysicalDeviceMemoryProperties;

  -------------------
  -- CVar Settings --
  -------------------

  -- Handle the setting of graphics cvars
  procedure Set_Vertical_Sync (Val : Bool) is
    Present_Modes VkPresentModeKHR presentMode = VK_PRESENT_MODE_FIFO_KHR;
    begin
      -- Call vkGetPhysicalDeviceSurfacePresentModesKHR
      vkGetPhysicalDeviceSurfacePresentModesKHR (Physical_Device, Surface, Count'Access, NULL_PTR);
      declare Present_Modes : VkPresentModeKHR_Array (1..Count); begin
        vkGetPhysicalDeviceSurfacePresentModesKHR (Physical_Device, Surface, Count'Access, Present_Modes'Access);
        for Present_Mode of Present_Modes loop
          if vsync && Present_Mode == VK_PRESENT_MODE_MAILBOX_KHR or !vsync && Present_Mode == VK_PRESENT_MODE_IMMEDIATE_KHR then
            Current_presentMode = Present_Mode;
            exit;
          end if;
        end loop
      end;
    end;

  --------------
  -- Resizing --
  --------------

  -- Set the sizes... comment here !!!
  procedure Resize_Vulkan (Initial_Sizing : Bool := True) is
    Pre_Transform        : aliased VkSurfaceTransformFlagBitsKHR;
    Surface_Capabilities : aliased VkSurfaceCapabilitiesKHR;
    Setup_Buffer         : aliased VkCommandBuffer;
    Begin_Info           : aliased VkCommandBufferBeginInfo    := (sType               => VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
                                                                   others              => <>);
    Memory_Barrier       : aliased VkImageMemoryBarrier        := (sType               => VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
                                                                   srcQueueFamilyIndex => VK_QUEUE_FAMILY_IGNORED;
                                                                   dstQueueFamilyIndex => VK_QUEUE_FAMILY_IGNORED;
                                                                   oldLayout           => VK_IMAGE_LAYOUT_UNDEFINED;
                                                                   newLayout           => VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
                                                                   subresourceRange    => (VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1),
                                                                   others              => <>);
    Setup_Buffer_Info    : aliased VkCommandBufferAllocateInfo := (sType               => VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
                                                                   commandPool         => Command_Pool;
                                                                   level               => VK_COMMAND_BUFFER_LEVEL_PRIMARY;
                                                                   commandBufferCount  => 1,
                                                                   others              => <>);
    Submit_Info          : aliased VkSubmitInfo                := (sType               => VK_STRUCTURE_TYPE_SUBMIT_INFO, 
                                                                   commandBufferCount  => 1,
                                                                   pCommandBuffers     => Setup_Buffer'Access,
                                                                   others              => <>);
    begin

      -- Set Surface_Capabilities
      vkGetPhysicalDeviceSurfaceCapabilitiesKHR (Physical_Device, Surface, Surface_Capabilities'Access);
      Desired_Images := (if Surface_Capabilities.maxImageCount > 0 and Desired_Images > Surface_Capabilities.maxImageCount then
                           Surface_Capabilities.maxImageCount
                         else Desired_Images);
 
      -- Take into account if this resize was called during initialization or execution
      if Initial_Sizing then
        if Swap_Chain_Create_Info.imageExtent = Surface_Capabilities.currentExtent then return; end if;
        Swap_Chain_Create_Info := (imageExtent  => Surface_Capabilities.currentExtent,
                                   oldSwapchain => Swap_Chain,
                                   others       => Swap_Chain_Create_Info);
        VkAssert (vkCreateSwapchainKHR (Device, Swap_Chain_Create_Info'Access, NULL_PTR, Swap_Chain'Access));
        vkDeviceWaitIdle               (Device);
        vkDestroySwapchainKHR          (Device, Swap_Chain_Create_Info.oldSwapchain, NULL_PTR);
      else
        Swap_Chain_Create_Info := (sType                 => VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR,
                                   surface               => Surface,
                                   minImageCount         => Desired_Images,
                                   imageFormat           => VK_FORMAT_B8G8R8A8_SRGB,
                                   imageColorSpace       => VK_COLORSPACE_SRGB_NONLINEAR_KHR,
                                   imageExtent           => Surface_Capabilities.currentExtent,
                                   imageUsage            => VK_IMAGE_USAGE_TRANSFER_DST_BIT,
                                   imageArrayLayers      => 1,
                                   imageSharingMode      => VK_SHARING_MODE_EXCLUSIVE,
                                   queueFamilyIndexCount => 0,
                                   pQueueFamilyIndices   => NULL_PTR,
                                   presentMode           => presentMode,
                                   compositeAlpha        => VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
                                   oldSwapchain          => VK_NULL_HANDLE;
                                   clipped               => true);

        -- Set vsync
        Set_Vertical_Sync (Vertical_Sync.Get)

        -- ???
        Pre_Transform := (if (Surface_Capabilities.supportedTransforms and VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR) > 0 then
                            VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
                          else Surface_Capabilities.currentTransform);
        VkAssert (vkCreateSwapchainKHR(Device, Swap_Chain_Create_Info'Access, NULL_PTR, Swap_Chain'Access));
      end if;

      -- Call vkGetSwapchainImagesKHR
      vkGetSwapchainImagesKHR (Device, Swap_Chain, Count'Access, NULL_PTR);
      declare Images : VkImage_Array (1..Count); begin
        Assert (Images'Length >= Count);
        vkGetSwapchainImagesKHR (Device, Swap_Chain, Count'Access, Images'Access);
        Images.Set (Images
      end;

      -- Present images
      VkAssert (vkAllocateCommandBuffers (Device, Setup_Buffer_Info'Access, Setup_Buffer'Access);
      VkAssert (vkBeginCommandBuffer     (Setup_Buffer, Begin_Info'Access);
      for Image of Images loop
        Memory_Barrier.image := Image;
        vkCmdPipelineBarrier (commandBuffer             => Setup_Buffer,
                              vkCmdPipelineBarrier      => VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT,
                              dstStageMask              => VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT,
                              dependencyFlags           => 0,
                              memoryBarrierCount        => 0,
                              pMemoryBarriers           => null,
                              bufferMemoryBarrierCount  => 0,
                              pBufferMemoryBarriers     => null,
                              imageMemoryBarrierCount   => 1,
                              pImageMemoryBarriers      => Memory_Barrier'Access);
      end loop;
      vkEndCommandBuffer   (Setup_Buffer);
      vkQueueSubmit        (Queue, 1, Submit_Info'Access, VK_NULL_HANDLE);
      vkQueueWaitIdle      (Queue);
      vkFreeCommandBuffers (Device, Command_Pool, 1, Setup_Buffer'Access);
    end;

  ----------------
  -- Initialize --
  ----------------

  -- Get the game window ready for rendering and initialize the global variables in the spec
  procedure Initialize_Vulkan is 

    -- Load all of the function pointers from a dll or lib
    procedure Import_Initialize_Vulkan_Functions is new Initialize_Vulkan_Functions (Load_Vulkan_Function);
    
    -- Structures and variables for configuration... so many
    Layers                   : aliased Array_Str_C := (To_Str_C (), To_Str_C ());
    Enabled_Extensions       : aliased  := Import.Get_Vulkan_Extensions;
    Surface_Support          : VkBool32  = VK_FALSE;
    Queue_Priority           : const float := 0.0f; 
    Pre_Transform            : VkSurfaceTransformFlagBitsKHR  
    Device_Extensions        : static const char *k[] =  VK_KHR_SWAPCHAIN_EXTENSION_NAME};
    Application_Info         : aliased VkApplicationInfo        := (sType                   => VK_STRUCTURE_TYPE_APPLICATION_INFO,
                                                                    pApplicationName        => Get_Information.Name,
                                                                    pEngineName             => NAME_ID & VERSION,
                                                                    apiVersion              => VK_MAKE_VERSION (1, 0, 0),
                                                                    others                  => <>);
    Instance_Create_Info     : aliased VkInstanceCreateInfo     := (sType                   => VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
                                                                    pApplicationInfo        => Application_Info'Access,
                                                                    enabledExtensionCount   => Enabled_Extensions'Length,
                                                                    ppenabledExtensionNames => Enabled_Extensions'Access,
                                                                    enabledLayerCount       => Layers'Length,
                                                                    ppEnabledLayerNames     => Layers'Access,
                                                                    others                  => <>);
    Queue_Create_Info        : aliased VkDeviceQueueCreateInfo  := (sType                   => VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
                                                                    queueCount              => 2, -- Double buffering
                                                                    pQueuePriorities        => Queue_Priority'Access,
                                                                    others                  => <>);
    Device_Features          : aliased VkPhysicalDeviceFeatures := (shaderClipDistance      => 1,
                                                                    shaderCullDistance      => 1,
                                                                    geometryShader          => 1,
                                                                    shaderTessellationAndGeometryPointSize => 1,
                                                                    others                  => <>);
    Device_Create_Info       : aliased VkDeviceCreateInfo       := (sType                   => VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
                                                                    queueCreateInfoCount    => 1,
                                                                    pQueueCreateInfos       => queueCreateInfo'Access,
                                                                    pEnabledFeatures        => Device_Features'Access,
                                                                    ppEnabledLayerNames     => kLayers,
                                                                    enabledExtensionCount   => sizeof kDevice_Extensions / sizeof *kDevice_Extensions,
                                                                    ppEnabledExtensionNames => kDevice_Extensions;
                                                                    enabledLayerCount       => sizeof kLayers / sizeof *kLayers,
                                                                    others                  => <>);
    Command_Pool_Create_Info : aliased VkCommandPoolCreateInfo  := (sType                   => VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
                                                                    flags                   => VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
                                                                    queueFamilyIndex        => Queue_Index,
                                                                    others                  => <>);
    Semaphore_Create_Info    : aliased VkSemaphoreCreateInfo    := (sType                   => VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO,
                                                                    flags                   => 0,
                                                                    others                  => <>);
    begin

      -- Initialize the system grap function pointers
      Import.Initialize_Vulkan;
      Import_Initialize_Vulkan_Functions;

      -- Create instance
      VkAssert (vkCreateInstance (Instance_Create_Info'Access, NULL_PTR, Instance'Access);

      -- Aquire a device
      vkEnumeratePhysicalDevices (Instance, Count'Access, NULL_PTR);
      declare Physical_Devices : VkPhysicalDevice_Array (1..Count); begin
        vkEnumeratePhysicalDevices (Instance, Count'Access, Physical_Devices'Access);
        Physical_Device := Physical_Devices (Physical_Devices'First);
      end;

      -- Call vkGetPhysicalDeviceQueueFamilyProperties
      vkGetPhysicalDeviceQueueFamilyProperties (Physical_Device, Count'Access, NULL_PTR);
      declare Queue_Family_Properties : VkQueueFamilyProperties_Array (1..Count); begin
        vkGetPhysicalDeviceQueueFamilyProperties (Physical_Device, Count'Access, Queue_Family_Properties'Access);
      end;
      
      -- ???
      for I in 1..Queues.Length loop 
        if (Queue.queueFlags and VK_QUEUE_GRAPHICS_BIT) > 0 then
          Queue_Create_Info.queueFamilyIndex := Queue_Index;
          exit;
        end if;
        Assert (I /= Queues.Legnth);
      end loop;      

      -- Create a Vulkan device
      Vk_Assert (vkCreateDevice           (Physical_Device, Device_Create_Info'Access, NULL_PTR, Device'Access);
      vkGetPhysicalDeviceProperties       (Physical_Device, Physical_DeviceProperties'Access);
      vkGetPhysicalDeviceMemoryProperties (Physical_Device, Physical_DeviceMemoryProperties'Access);
      vkGetDeviceQueue                    (Device, Queue_Index, 0, Queue'Access);

      -- Create the command pool
      Vk_Assert (vkCreateCommandPool(Device, Command_Pool_Create_Info'Access, NULL_PTR, Command_Pool'Access);

      -- Create surface
      Import.createSurface                 (Instance, Surface'Access))
      vkGetPhysicalDeviceSurfaceSupportKHR (Physical_Device, Queue_Index, Surface, Surface_Support'Access);
      VkAssert (Surface_Support);

      -- Find format
      vkGetPhysicalDeviceSurfaceFormatsKHR (Physical_Device, Surface, Count'Access, NULL_PTR);
      declare Surface_Formats : Array_VkSurfaceFormatKHR (1..Count);
        vkGetPhysicalDeviceSurfaceFormatsKHR (Physical_Device, Surface, Count'Access, Surface_Formats'Access);
        for I in 1..Surface_Formats.Length loop
          exit when Surface_Formats.Element (I).Format = VK_FORMAT_B8G8R8A8_SRGB;
          Assert (I /= Surface_Formats.Last_Index);
        end loop;
      end;

      -- Set the initial sizing
      Resize_Vulkan (Initial_Sizing => True);

      -- Create semaphores
      Vk_Assert (vkCreateSemaphore (Device, Semaphore_Create_Info'Access, NULL_PTR, Acquire_Status'Access);
      Vk_Assert (vkCreateSemaphore (Device, Semaphore_Create_Info'Access, NULL_PTR, Render_Status'Access);
    end;

  --------------
  -- Finalize --
  --------------

  -- Kill the globals
  procedure Finalize_Vulkan is
    begin    
      vkDeviceWaitIdle      (Device);
      vkDestroySemaphore    (Device,   Acquire_Status, NULL_PTR);
      vkDestroySemaphore    (Device,   Render_Status,  NULL_PTR);
      vkDestroyCommandPool  (Device,   Command_Pool,   NULL_PTR);
      vkDestroySwapchainKHR (Device,   Swap_Chain,     NULL_PTR);
      vkDestroyDevice       (Device,   NULL_PTR);
      vkDestroyInstance     (Instance, NULL_PTR);
      Import.Finalize_Vulkan;
    end;

  -------------
  -- Present --
  -------------

  -- Acquire the rendered image and present it
  procedure Render_Vulkan is
    Present_Info : aliased VkPresentInfoKHR := (sType              => VK_STRUCTURE_TYPE_PRESENT_INFO_KHR,
                                                swapchainCount     => 1,
                                                pSwapchains        => Swap_Chain'Access,
                                                pImageIndices      => Image_Index'Access,
                                                waitSemaphoreCount => 1,
                                                pWaitSemaphores    => Render_Status'Access);
    begin
      vkAcquireNextImageKHR (device      => Device,
                             swapchain   => Swap_Chain,
                             timeout     => UINT64_MAX,
                             semaphore   => Acquire_Status,
                             fence       => VK_NULL_HANDLE,
                             pImageIndex => Image_Index'Access);
      vkQueuePresentKHR (Queue, Present_Info'Access);
    end;

  ------------------
  -- Load_Texture --
  ------------------

  -- Load a KTX texture into GPU memory and return the id and 
  function Load_Texture (Path : Str) return Texture_State is

    -- KTX texture format: http:-- web.archive.org/web/20160811201320/https:-- www.khronos.org/opengles/sdk/tools/KTX/file_format_spec/
    type KTX_Header_State is record
        Id              : Str (1..12);
        Endianness      : Int_32_Unsigned;
        Kind            : Int_32_Unsigned;
        Image_Size      : Int_32_Unsigned;
        GL_Format       : Int_32_Unsigned;
        Internal_Format : Int_32_Unsigned;
        Base_Format     : Int_32_Unsigned;
        Width           : Int_32_Unsigned;
        Height          : Int_32_Unsigned;
        Depth           : Int_32_Unsigned;
        Array_Length    : Int_32_Unsigned;
        Faces           : Int_32_Unsigned;
        Mipmap_Levels   : Int_32_Unsigned;
        Key_Value_Bytes : Int_32_Unsigned;
      end record with Object_Size => 440;

    -- Open the file and load the header
    File   : File_Type        := Open_File (Path);
    Header : KTX_Header_State := KTX_Header_State'Read (File);

    -- Use the header to initialize a temp array for data parsing and result 
    Texture : array (1..Header.Mipmap_Levels, 1..Header.Array_Length, 1..Header.Faces, 1..Header.Face_Size) of Byte;
    Result  : Texture_State ();

    -- Vulkan configuration...
    Mappable_Image         : aliased VkImage;
    Device_Memory          : aliased VkDeviceMemory;
    Subresource_Layout     : aliased VkSubresourceLayout;
    Format_Properties      : aliased VkFormatProperties;
    VkMemoryAllocateInfo memAllocInfo = vkTools::initializers::memoryAllocateInfo();
    Memory_Requirements    : aliased VkMemoryRequirements;
    Subresource_Range      : aliased VkImageSubresourceRange := (aspectMask         => VK_IMAGE_ASPECT_COLOR_BIT;
                                                                 baseMipLevel       => 0;
                                                                 levelCount         => texture->mipLevels;
                                                                 layerCount         => 1;
                                                                 others             => <>);
    Buffer_Image_Copy      : aliased VkBufferImageCopy       := (imageSubresource.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
                                                                 imageSubresource.mipLevel = i;
                                                                 imageSubresource.baseArrayLayer = 0;
                                                                 imageSubresource.layerCount = 1;
                                                                 imageExtent.width = static_cast<uint32_t>(tex2D[i].dimensions().x);
                                                                 imageExtent.height = static_cast<uint32_t>(tex2D[i].dimensions().y);
                                                                 imageExtent.depth = 1;
                                                                 bufferOffset = offset;
                                                                 others             => <>);
    Buffer_Create_Info     : aliased VkBufferCreateInfo      := (size               => tex2D.size();
                                                                 usage              => VK_BUFFER_USAGE_TRANSFER_SRC_BIT;
                                                                 sharingMode        => VK_SHARING_MODE_EXCLUSIVE;
                                                                 others             => <>);
    Image_Create_Info      : aliased VkImageCreateInfo       := (imageType          => VK_IMAGE_TYPE_2D;
                                                                 format             => format;
                                                                 mipLevels          => texture->mipLevels;
                                                                 arrayLayers        => 1;
                                                                 samples            => VK_SAMPLE_COUNT_1_BIT;
                                                                 tiling             => VK_IMAGE_TILING_OPTIMAL;
                                                                 sharingMode        => VK_SHARING_MODE_EXCLUSIVE;
                                                                 initialLayout      => VK_IMAGE_LAYOUT_UNDEFINED;
                                                                 extent             => (texture->width, texture->height, 1);
                                                                 usage              => imageUsageFlags;
                                                                 others             => <>);
    Submit_Info            : aliased VkSubmitInfo            := (commandBufferCount => 1;
                                                                 waitSemaphoreCount => 0;
                                                                 pCommandBuffers    => &cmdBuffer;
                                                                 others             => <>);
    Subresource            : aliased VkImageSubresource      := (aspectMask         => VK_IMAGE_ASPECT_COLOR_BIT;
                                                                 mipLevel           => 0;
                                                                 others             => <>);
    Sampler_Create_Info    : aliased VkSamplerCreateInfo     := (sType              => VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
                                                                 magFilter          => VK_FILTER_LINEAR;
                                                                 minFilter          => VK_FILTER_LINEAR;
                                                                 mipmapMode         => VK_SAMPLER_MIPMAP_MODE_LINEAR;
                                                                 addressModeU       => VK_SAMPLER_ADDRESS_MODE_REPEAT;
                                                                 addressModeV       => VK_SAMPLER_ADDRESS_MODE_REPEAT;
                                                                 addressModeW       => VK_SAMPLER_ADDRESS_MODE_REPEAT;
                                                                 mipLodBias         => 0.0f;
                                                                 compareOp          => VK_COMPARE_OP_NEVER;
                                                                 minLod             => 0.0f;
                                                                 maxLod             => : 0.0f;
                                                                 maxAnisotropy      => 8;
                                                                 anisotropyEnable   => VK_TRUE;
                                                                 borderColor        => VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE;
                                                                 others             => <>);
    Image_View_Create_Info : aliased VkImageViewCreateInfo   := (sType              => VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
                                                                 image              => VK_NULL_HANDLE;
                                                                 viewType           => VK_IMAGE_VIEW_TYPE_2D;
                                                                 format             => format;
                                                                 image              => texture->image;
                                                                 components         => (VK_COMPONENT_SWIZZLE_R, VK_COMPONENT_SWIZZLE_G,
                                                                                        VK_COMPONENT_SWIZZLE_B, VK_COMPONENT_SWIZZLE_A),
                                                                 others             => <>);
    begin

      -- Use a separate command buffer for texture loading
      VkAssert (vkBeginCommandBuffer (cmdBuffer, &cmdBufInfo));

      -- Check if this support is supported for linear tiling Load mip map level 0 to linear tiling image
      vkGetPhysicalDeviceFormatProperties (vulkanDevice->physicalDevice, format, &formatProperties);
      Assert (formatProperties.linearTilingFeatures & VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT);

      -- Get memory requirements for this image like size and alignmentGet sub resources layout  Includes row pitch, size offsets, etc.
      VkAssert (vkCreateImage      (vulkanDevice->logicalDevice, &imageCreateInfo, nullptr, &mappableImage));
      vkGetImageMemoryRequirements (vulkanDevice->logicalDevice, mappableImage, &memReqs);
      memAllocInfo.allocationSize = memReqs.size;
      memAllocInfo.memoryTypeIndex = vulkanDevice->getMemoryType (memReqs.memoryTypeBits, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
      VkAssert (vkAllocateMemory   (vulkanDevice->logicalDevice, &memAllocInfo, nullptr, &mappableMemory));
      VkAssert (vkBindImageMemory  (vulkanDevice->logicalDevice, mappableImage, mappableMemory, 0));
      vkGetImageSubresourceLayout  (vulkanDevice->logicalDevice, mappableImage, &subRes, &subResLayout);

      -- Map image memory
      VkAssert (vkMapMemory (vulkanDevice->logicalDevice, mappableMemory, 0, memReqs.size, 0, &data));

      if(Header.NumberOfFaces > 1)
        if(Header.NumberOfArrayElements > 0) return TARGET_CUBE_ARRAY;
        else return TARGET_CUBE;
      elsif(Header.NumberOfArrayElements > 0)
        if(Header.PixelHeight == 0) return TARGET_1D_ARRAY;
        else return TARGET_2D_ARRAY;
      else if(Header.PixelHeight == 0) return TARGET_1D;
      else if(Header.PixelDepth > 0) return TARGET_3D;
      else return TARGET_2D;

      -- Assert the file is a valid KTX texture
      Assert ("KTX 11");
      Assert (Format != static_cast<format>(gli::FORMAT_INVALID));
      Assert ( this->extent().y >= 1 && this->extent().z == 1)
      Assert (Target != TARGET_2D         or (Target == TARGET_2D         && this->layers() == 1 && this->faces() == 1));
      Assert (Target != TARGET_2D_ARRAY   or (Target == TARGET_2D_ARRAY   && this->layers() >= 1 && this->faces() == 1));
      Assert (Target != TARGET_CUBE       or (Target == TARGET_CUBE       && this->layers() == 1 && this->faces() >= 1));
      Assert (Target != TARGET_CUBE_ARRAY or (Target == TARGET_CUBE_ARRAY && this->layers() >= 1 && this->faces() >= 1));

      -- Load the texture data
      for Level in 1..Header.Mipmap_Levels loop
        for Layer in 1..Header.Array_Length loop
          for Face in 1..Header.Faces loop
            for Element in 1..Header.Face_Size loop
              Texture_Data (Level, Layer, Face, Element) := Byte'Read (File);
            end loop;
          end loop;
        end loop;

        -- Skip the MIP padding
        Set_Position (File, 3 - ((Texture.Image_Size + 3) mod 4));
      end loop;

      vkUnmapMemory(vulkanDevice->logicalDevice, mappableMemory);

      -- Setup image memory barrier
      setImageLayout(
        cmdBuffer,
        texture->image, 
        VK_IMAGE_ASPECT_COLOR_BIT, 
        VK_IMAGE_LAYOUT_PREINITIALIZED, 
        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);

      -- Submit command buffer containing copy and image layout commands
      VkAssert (vkEndCommandBuffer(cmdBuffer));

      -- 
      VkAssert (vkQueueSubmit(queue, 1, &submitInfo, nullFence));
      VkAssert (vkQueueWaitIdle(queue));

      -- Create sampler
      VkAssert (vkCreateSampler(vulkanDevice->logicalDevice, &sampler, nullptr, &texture->sampler));
      
      -- Textures are not directly accessed by the shaders and are abstracted by image views containing additional information and sub resource ranges
      VkAssert (vkCreateImageView(vulkanDevice->logicalDevice, &view, nullptr, &texture->view));

      -- Fill descriptor image info that can be used for setting up descriptor sets
      texture->descriptor.imageLayout = VK_IMAGE_LAYOUT_GENERAL;
      return Texture;
    end;

  -----------------
  -- Load_Shader --
  -----------------

  function Load_Shader (Path : Str) return 

  ---------------
  -- Load_Mesh --
  ---------------

  function Load_Mesh (Path : Str) return Texture_State is
    Mesh : Mesh_State := Load (Path);
    begin
      
    end;
    
  -----------------------
  -- Task_Render_Light --
  -----------------------

  procedure Run_Render_Light is
    begin
    end;
  package Task_Render_Light is new Tasks (Run_Render_Light);
  package Vector_Render_Light is new Vectors (Task_Render_Light.Safe_Task);

  -------------------
  -- Task_Frontend --
  -------------------
  
  -- The frontend - also performs the step of filling the depth buffer Visible_Surface_Determination
  procedure Run_Frontend is
    Area : Int;
    procedure Point_In_Area is
      begin
      end;
    begin
      for View of Views loop

        -- Setup the view matrix
        Viewer := (Axis.XX, Axis.YX, Axis.ZX, -Origin.X * Axis.XX - Origin.Y * YX - Origin.Z * Axis.ZX,
                   Axis.XY, Axis.YY, Axis.ZY, -Origin.X * Axis.XY - Origin.Y * YY - Origin.Z * Axis.ZY,
                   Axis.XZ, Axis.YZ, Axis.ZZ, -Origin.X * Axis.XZ - Origin.Y * YZ - Origin.Z * Axis.ZZ);

        -- Setup the projection matrix (use the ol' "infinite far z trick")
        Jitter_Values := (if Jitter.Get then (Random_Float, Random_Float) else  (others => <>));
        R_SetupProjectionMatrix

        -- Setup render matricies for faster culliong
        View.Render_Projection := Transpose (View.Projection);
        View.World_Space.MVP := View.Render_Projection * Transpose (View.World_Space.Model_View);
        View.Frustum := ((4)(3) => Z_Near.Get, others => -Get_Frustum_Planes (View.World_Space.MVP));

        -- Walk the BSP tree to find the current area
        Iterate (View.Level, Point_In_Area);
        View.Area := Area;

        -- Determin all possible connected areas for light-behind-door culling
        

        -- Add or remove GUI surfaces

        -- Fill the depth buffer
        for Mesh of View.Meshes loop
          case Mesh.Material.Coverage is
            when Opaque_Coverage =>
            when Perferated_Coverage =>
              for Stage of Mesh.Material.Stages loop
              end loop;
          when others => null; end case;
        end loop;

        -- Trigger light render
      end loop;
    end;
  package Task_Frontend is new Tasks (Run_Frontend);
  Frontend : Task_Frontend.Safe_Task;

  -----------------
  -- Run_Backend --
  -----------------

  procedure Run_Backend is
    begin

      -- Post process

      -- Present

    end;
end;

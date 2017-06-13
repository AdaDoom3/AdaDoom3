
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

with Neo.Core.Arrays; use Neo.Core.Arrays;

-- Custom binding to the Vulkan API: http://web.archive.org/save/_embed/https://www.khronos.org/files/vulkan10-reference-guide.pdf
package Neo.API.Vulkan is

  -- Initialization of global function pointers with OS specific driver
  generic
    with function Get_Vulkan_Subprogram (Name : Str) return Ptr;
  procedure Initialize;

  -----------
  -- Types --
  -----------

  -- size_t                                  Int_Size_C
  -- int32_t                                 Int_C
  -- uint32_t                                Int_Unsigned_C
  -- uint64_t                                Int_64_Unsigned_C
  -- VkDeviceSize                            Int_64_Unsigned_C
  -- VkBool32                                Int_Unsigned_C
  -- VkFormat                                Int_Unsigned_C
  -- VkResult                                Int_Unsigned_C
  -- VkSurfaceTransformFlagBitsKHR           Int_Unsigned_C
  -- VkSurfaceTransformFlagsKHR              Int_Unsigned_C
  -- VkSemaphoreCreateFlagBits               Int_Unsigned_C
  -- VkCommandPoolCreateFlags                Int_Unsigned_C
  -- VkDeviceQueueCreateFlags                Int_Unsigned_C
  -- VkQueueFlags                            Int_Unsigned_C
  -- VkInstanceCreateFlags                   Int_Unsigned_C
  -- VkInstanceCreateFlags                   Int_Unsigned_C
  -- VkSampleCountFlags                      Int_Unsigned_C
  -- VkPipelineStageFlags                    Int_Unsigned_C
  -- VkMemoryPropertyFlags                   Int_Unsigned_C
  -- VkMemoryHeapFlags                       Int_Unsigned_C
  -- VkCommandBufferLevel                    Int_Unsigned_C
  -- VkAccessFlags                           Int_Unsigned_C
  -- VkImageAspectFlagBits                   Int_Unsigned_C
  -- VkImageUsageFlags                       Int_Unsigned_C
  -- VkImageLayout                           Int_Unsigned_C
  -- vkDependencyFlags                       Int_Unsigned_C
  -- VkCompositeAlphaFlagBitsKHR             Int_Unsigned_C
  -- VkSharingMode                           Int_Unsigned_C
  -- VkColorSpaceKHR                         Int_Unsigned_C
  -- VkSwapchainCreateFlagsKHR               Int_Unsigned_C
  -- VkPresentModeKHR                        Int_Unsigned_C
  -- VkWin32SurfaceCreateFlagsKHR            Int_Unsigned_C
  -- VkQueueFlagBits                         Int_Unsigned_C
  -- VkPipelineBindPoint                     Int_Unsigned_C
  -- VkSubpassContents                       Int_Unsigned_C
  -- VkFlags                                 Int_Unsigned_C
  -- VkPipelineCreateFlags                   Int_Unsigned_C
  -- VkPipelineShaderStageCreateFlags        Int_Unsigned_C
  -- VkPipelineVertexInputStateCreateFlags   Int_Unsigned_C
  -- VkVertexInputRate                       Int_Unsigned_C
  -- VkPipelineInputAssemblyStateCreateFlags Int_Unsigned_C
  -- VkPipelineTessellationStateCreateFlags  Int_Unsigned_C
  -- VkPipelineViewportStateCreateFlags      Int_Unsigned_C
  -- VkPipelineRasterizationStateCreateFlags Int_Unsigned_C
  -- VkCullModeFlags                         Int_Unsigned_C
  -- VkPipelineMultisampleStateCreateFlags   Int_Unsigned_C
  -- VkPipelineDepthStencilStateCreateFlags  Int_Unsigned_C
  -- VkPipelineColorBlendStateCreateFlags    Int_Unsigned_C
  -- VkColorComponentFlags                   Int_Unsigned_C
  -- VkPipelineDynamicStateCreateFlags       Int_Unsigned_C
  -- VkDynamicState                          Int_Unsigned_C
  -- VkImageAspectFlags                      Int_Unsigned_C
  -- VkBufferCreateFlags                     Int_Unsigned_C
  -- VkBufferUsageFlags                      Int_Unsigned_C
  -- VkSharingMode                           Int_Unsigned_C
  -- VkImageCreateFlags                      Int_Unsigned_C
  -- VkIndexType                             Int_Unsigned_C
  -- VkImageType                             Int_Unsigned_C
  -- VkSampleCountFlagBits                   Int_Unsigned_C
  -- VkImageTiling                           Int_Unsigned_C
  -- VkImageUsageFlags                       Int_Unsigned_C
  -- VkInternalAllocationType                Int_Unsigned_C
  -- VkDescriptorType                        Int_Unsigned_C
  -- VkSubpassDescriptionFlags               Int_Unsigned_C
  -- VkAttachmentStoreOp                     Int_Unsigned_C
  -- VkAttachmentLoadOp                      Int_Unsigned_C
  -- VkAttachmentDescriptionFlagBits         Int_Unsigned_C
  -- VkDescriptorSetLayoutCreateFlags        Int_Unsigned_C
  -- VkPipelineLayoutCreateFlagBits          Int_Unsigned_C
  -- VkImageViewCreateFlags                  Int_Unsigned_C
  -- VkImageViewType                         Int_Unsigned_C
  -- VkComponentSwizzle                      Int_Unsigned_C
  -- VkFramebufferCreateFlags                Int_Unsigned_C
  -- VkDescriptorSetLayout                   Ptr
  -- VkSampler                               Ptr
  -- VkFence                                 Ptr
  -- VkDeviceMemory                          Ptr
  -- VkImage                                 Ptr
  -- VkQueue                                 Ptr
  -- VkDevice                                Ptr
  -- VkInstance                              Ptr
  -- VkSemaphore                             Ptr
  -- VkRenderPass                            Ptr
  -- VkSurfaceKHR                            Ptr
  -- VkSwapchainKHR                          Ptr
  -- VkPhysicalDeviceType                    Ptr
  -- VkCommandPool                           Ptr
  -- VkCommandBuffer                         Ptr
  -- VkPhysicalDeviceType                    Ptr
  -- VkPipelineLayout                        Ptr
  -- VkBuffer                                Ptr
  -- VkPipeline                              Ptr
  -- VkFramebuffer                           Ptr
  -- VkShaderModule                          Ptr
  -- VkDescriptorPool                        Ptr
  -- VkDescriptorSet                         Ptr
  -- VkBufferView                            Ptr
  -- VkImageView                             Ptr
  -- VkPipelineCache                         Ptr

  ---------------
  -- Constants --
  ---------------
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkColorComponentFlagBits.html
  VK_COLOR_COMPONENT_R_BIT : constant Int_Unsigned_C := 16#0000_0001#; -- VkColorComponentFlagBits
  VK_COLOR_COMPONENT_G_BIT : constant Int_Unsigned_C := 16#0000_0002#; -- VkColorComponentFlagBits
  VK_COLOR_COMPONENT_B_BIT : constant Int_Unsigned_C := 16#0000_0004#; -- VkColorComponentFlagBits
  VK_COLOR_COMPONENT_A_BIT : constant Int_Unsigned_C := 16#0000_0008#; -- VkColorComponentFlagBits
    
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCullModeFlagBits.html
  VK_CULL_MODE_NONE           : constant Int_Unsigned_C := 16#0000_0000#; -- VkCullModeFlagBits
  VK_CULL_MODE_FRONT_BIT      : constant Int_Unsigned_C := 16#0000_0001#; -- VkCullModeFlagBits
  VK_CULL_MODE_BACK_BIT       : constant Int_Unsigned_C := 16#0000_0002#; -- VkCullModeFlagBits
  VK_CULL_MODE_FRONT_AND_BACK : constant Int_Unsigned_C := 16#0000_0003#; -- VkCullModeFlagBits

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkAttachmentStoreOp.html
  VK_ATTACHMENT_STORE_OP_STORE     : constant Int_Unsigned_C := 0; -- VkAttachmentStoreOp 
  VK_ATTACHMENT_STORE_OP_DONT_CARE : constant Int_Unsigned_C := 1; -- VkAttachmentStoreOp 

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkAttachmentLoadOp.html
  VK_ATTACHMENT_LOAD_OP_LOAD      : constant Int_Unsigned_C := 0; -- VkAttachmentLoadOp
  VK_ATTACHMENT_LOAD_OP_CLEAR     : constant Int_Unsigned_C := 1; -- VkAttachmentLoadOp
  VK_ATTACHMENT_LOAD_OP_DONT_CARE : constant Int_Unsigned_C := 2; -- VkAttachmentLoadOp 

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkAttachmentDescriptionFlagBits.html
  VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT : constant Int_Unsigned_C := 16#0000_0001#; -- VkAttachmentDescriptionFlagBits 

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDescriptorType.html
  VK_DESCRIPTOR_TYPE_SAMPLER                : constant Int_Unsigned_C := 0;  -- VkDescriptorType
  VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER : constant Int_Unsigned_C := 1;  -- VkDescriptorType
  VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE          : constant Int_Unsigned_C := 2;  -- VkDescriptorType
  VK_DESCRIPTOR_TYPE_STORAGE_IMAGE          : constant Int_Unsigned_C := 3;  -- VkDescriptorType
  VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER   : constant Int_Unsigned_C := 4;  -- VkDescriptorType
  VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER   : constant Int_Unsigned_C := 5;  -- VkDescriptorType
  VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER         : constant Int_Unsigned_C := 6;  -- VkDescriptorType
  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER         : constant Int_Unsigned_C := 7;  -- VkDescriptorType
  VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC : constant Int_Unsigned_C := 8;  -- VkDescriptorType
  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC : constant Int_Unsigned_C := 9;  -- VkDescriptorType
  VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT       : constant Int_Unsigned_C := 10; -- VkDescriptorType

  -- http://vulkan-spec-chunked.ahcox.com/apes09.html
  VK_WIN32_DLL_NAME                       : aliased Str_16_C := To_Str_16_C ("vulkan-1.dll");                -- ???
  VK_KHR_SWAPCHAIN_EXTENSION_NAME         : constant Str_8_C := To_Str_8_C (S ("VK_KHR_swapchain"));         -- ???
  VK_KHR_SURFACE_EXTENSION_NAME           : constant Str_8_C := To_Str_8_C (S ("VK_KHR_surface"));           -- ???
  VK_KHR_WIN32_SURFACE_EXTENSION_NAME     : constant Str_8_C := To_Str_8_C (S ("VK_KHR_win32_surface"));     -- ???
  VK_EXT_DEBUG_REPORT_EXTENSION_NAME      : constant Str_8_C := To_Str_8_C (S ("VK_EXT_debug_report"));      -- ???
  VK_LAYER_LUNARG_API_DUMP_EXTENSION_NAME : constant Str_8_C := To_Str_8_C (S ("VK_LAYER_LUNARG_api_dump")); -- ???
  --VK_LAYER_LUNARG_standard_validation     : constant Str_8_C := To_Str_8_C (S ("VK_LAYER_LUNARG_standard_validation")); -- ???

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VK_NULL_HANDLE.html
  VK_NULL_HANDLE : constant Int_Unsigned_C := 0; -- 0
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageMemoryBarrier.html
  VK_QUEUE_FAMILY_IGNORED : constant Int_Unsigned_C := 16#FFFF_FFFF#; -- (~0U)
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSubpassDependency.html
  VK_SUBPASS_EXTERNAL : constant Int_Unsigned_C := 16#FFFF_FFFF#; -- (~0U)

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExtensionProperties.html
  VK_MAX_EXTENSION_NAME_SIZE : constant Int_Size_C := 256; -- 0

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDependencyFlags.html
  VK_DEPENDENCY_BY_REGION_BIT : constant Int_Unsigned_C := 1; -- VkDependencyFlags

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryHeap.html
  VK_MEMORY_HEAP_DEVICE_LOCAL_BIT : constant Int_Unsigned_C := 1; -- VkMemoryHeapFlagBits

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkInternalAllocationType.html
  VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE : constant Int_Unsigned_C := 0; -- VkInternalAllocationType
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBool32.html
  VK_TRUE  : constant Int_Unsigned_C := 1; -- VkBool32
  VK_FALSE : constant Int_Unsigned_C := 0; -- VkBool32
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageTiling.html
  VK_IMAGE_TILING_OPTIMAL : constant Int_Unsigned_C := 0; -- VkImageTiling
  VK_IMAGE_TILING_LINEAR  : constant Int_Unsigned_C := 1; -- VkImageTiling

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSharingMode.html
  VK_SHARING_MODE_EXCLUSIVE  : constant Int_Unsigned_C := 0; -- VkSharingMode
  VK_SHARING_MODE_CONCURRENT : constant Int_Unsigned_C := 1; -- VkSharingMode

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkIndexType.html
  VK_INDEX_TYPE_UINT16 : constant Int_Unsigned_C := 0; -- VkIndexType
  VK_INDEX_TYPE_UINT32 : constant Int_Unsigned_C := 1; -- VkIndexType

  -- http://nopper.tv/Vulkan/1.0/VkColorSpaceKHR.html
  VK_COLOR_SPACE_SRGB_NONLINEAR_KHR : constant Int_Unsigned_C := 0; -- VkColorSpaceKHR
  VK_COLORSPACE_SRGB_NONLINEAR_KHR  : constant Int_Unsigned_C := 0; -- VkColorSpaceKHR

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandBufferLevel.html
  VK_COMMAND_BUFFER_LEVEL_PRIMARY   : constant Int_Unsigned_C := 0; -- VkCommandBufferLevel
  VK_COMMAND_BUFFER_LEVEL_SECONDARY : constant Int_Unsigned_C := 1; -- VkCommandBufferLevel

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandPoolCreateFlagBits.html
  VK_COMMAND_POOL_CREATE_TRANSIENT_BIT            : constant Int_Unsigned_C := 1; -- VkCommandPoolCreateFlagBits
  VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT : constant Int_Unsigned_C := 2; -- VkCommandPoolCreateFlagBits

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineBindPoint.html
  VK_PIPELINE_BIND_POINT_GRAPHICS : constant Int_Unsigned_C := 0; -- VkPipelineBindPoint
  VK_PIPELINE_BIND_POINT_COMPUTE  : constant Int_Unsigned_C := 1; -- VkPipelineBindPoint

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSubpassContents.html
  VK_SUBPASS_CONTENTS_INLINE                    : constant Int_Unsigned_C := 0; -- VkSubpassContents 
  VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS : constant Int_Unsigned_C := 1; -- VkSubpassContents 

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkVertexInputRate.html
  VK_VERTEX_INPUT_RATE_VERTEX   : constant Int_Unsigned_C := 0; -- VkVertexInputRate
  VK_VERTEX_INPUT_RATE_INSTANCE : constant Int_Unsigned_C := 1; -- VkVertexInputRate

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFrontFace.html
  VK_FRONT_FACE_COUNTER_CLOCKWISE : constant Int_Unsigned_C := 0; -- VkFrontFace
  VK_FRONT_FACE_CLOCKWISE         : constant Int_Unsigned_C := 1; -- VkFrontFace

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandBufferUsageFlagBits.html
  VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT      : constant Int_Unsigned_C := 1; -- VkCommandBufferUsageFlagBits
  VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT : constant Int_Unsigned_C := 2; -- VkCommandBufferUsageFlagBits
  VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT     : constant Int_Unsigned_C := 4; -- VkCommandBufferUsageFlagBits

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPolygonMode.html
  VK_POLYGON_MODE_FILL  : constant Int_Unsigned_C := 0; -- VkPolygonMode
  VK_POLYGON_MODE_LINE  : constant Int_Unsigned_C := 1; -- VkPolygonMode
  VK_POLYGON_MODE_POINT : constant Int_Unsigned_C := 2; -- VkPolygonMode

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageType.html
  VK_IMAGE_TYPE_1D : constant Int_Unsigned_C := 0; -- VkImageType
  VK_IMAGE_TYPE_2D : constant Int_Unsigned_C := 1; -- VkImageType
  VK_IMAGE_TYPE_3D : constant Int_Unsigned_C := 2; -- VkImageType

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSystemAllocationScope.html
  VK_SYSTEM_ALLOCATION_SCOPE_COMMAND  : constant Int_Unsigned_C := 0; -- VkSystemAllocationScope
  VK_SYSTEM_ALLOCATION_SCOPE_OBJECT   : constant Int_Unsigned_C := 1; -- VkSystemAllocationScope
  VK_SYSTEM_ALLOCATION_SCOPE_CACHE    : constant Int_Unsigned_C := 2; -- VkSystemAllocationScope
  VK_SYSTEM_ALLOCATION_SCOPE_DEVICE   : constant Int_Unsigned_C := 3; -- VkSystemAllocationScope
  VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE : constant Int_Unsigned_C := 4; -- VkSystemAllocationScope
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBlendOp.html
  VK_BLEND_OP_ADD              : constant Int_Unsigned_C := 0; -- VkBlendOp
  VK_BLEND_OP_SUBTRACT         : constant Int_Unsigned_C := 1; -- VkBlendOp
  VK_BLEND_OP_REVERSE_SUBTRACT : constant Int_Unsigned_C := 2; -- VkBlendOp
  VK_BLEND_OP_MIN              : constant Int_Unsigned_C := 3; -- VkBlendOp
  VK_BLEND_OP_MAX              : constant Int_Unsigned_C := 4; -- VkBlendOp

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkQueueFamilyProperties.html
  VK_QUEUE_GRAPHICS_BIT       : constant Int_Unsigned_C := 16#0000_0001#; -- VkQueueFlagBits
  VK_QUEUE_COMPUTE_BIT        : constant Int_Unsigned_C := 16#0000_0002#; -- VkQueueFlagBits
  VK_QUEUE_TRANSFER_BIT       : constant Int_Unsigned_C := 16#0000_0004#; -- VkQueueFlagBits
  VK_QUEUE_SPARSE_BINDING_BIT : constant Int_Unsigned_C := 16#0000_0008#; -- VkQueueFlagBits

  -- http://nopper.tv/Vulkan/1.0/VkCompositeAlphaFlagBitsKHR.html
  VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR          : constant Int_Unsigned_C := 1; -- VkCompositeAlphaFlagBitsKHR
  VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR  : constant Int_Unsigned_C := 2; -- VkCompositeAlphaFlagBitsKHR
  VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR : constant Int_Unsigned_C := 4; -- VkCompositeAlphaFlagBitsKHR
  VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR         : constant Int_Unsigned_C := 8; -- VkCompositeAlphaFlagBitsKHR

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageAspectFlagBits.html
  VK_IMAGE_ASPECT_COLOR_BIT    : constant Int_Unsigned_C := 16#0000_0001#; -- VkImageAspectFlagBits
  VK_IMAGE_ASPECT_DEPTH_BIT    : constant Int_Unsigned_C := 16#0000_0002#; -- VkImageAspectFlagBits
  VK_IMAGE_ASPECT_STENCIL_BIT  : constant Int_Unsigned_C := 16#0000_0004#; -- VkImageAspectFlagBits
  VK_IMAGE_ASPECT_METADATA_BIT : constant Int_Unsigned_C := 16#0000_0008#; -- VkImageAspectFlagBits

  -- http://nopper.tv/Vulkan/1.0/VkPresentModeKHR.html
  VK_PRESENT_MODE_IMMEDIATE_KHR    : constant Int_Unsigned_C := 0; -- VkPresentModeKHR
  VK_PRESENT_MODE_MAILBOX_KHR      : constant Int_Unsigned_C := 1; -- VkPresentModeKHR
  VK_PRESENT_MODE_FIFO_KHR         : constant Int_Unsigned_C := 2; -- VkPresentModeKHR
  VK_PRESENT_MODE_FIFO_RELAXED_KHR : constant Int_Unsigned_C := 3; -- VkPresentModeKHR 

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageViewType.html
  VK_IMAGE_VIEW_TYPE_1D         : constant Int_Unsigned_C := 0; -- VkImageViewType
  VK_IMAGE_VIEW_TYPE_2D         : constant Int_Unsigned_C := 1; -- VkImageViewType
  VK_IMAGE_VIEW_TYPE_3D         : constant Int_Unsigned_C := 2; -- VkImageViewType
  VK_IMAGE_VIEW_TYPE_CUBE       : constant Int_Unsigned_C := 3; -- VkImageViewType
  VK_IMAGE_VIEW_TYPE_1D_ARRAY   : constant Int_Unsigned_C := 4; -- VkImageViewType
  VK_IMAGE_VIEW_TYPE_2D_ARRAY   : constant Int_Unsigned_C := 5; -- VkImageViewType
  VK_IMAGE_VIEW_TYPE_CUBE_ARRAY : constant Int_Unsigned_C := 6; -- VkImageViewType
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceType.html
  VK_PHYSICAL_DEVICE_TYPE_OTHER          : constant Int_Unsigned_C := 0; -- VkPhysicalDeviceType
  VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU : constant Int_Unsigned_C := 1; -- VkPhysicalDeviceType
  VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU   : constant Int_Unsigned_C := 2; -- VkPhysicalDeviceType
  VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU    : constant Int_Unsigned_C := 3; -- VkPhysicalDeviceType
  VK_PHYSICAL_DEVICE_TYPE_CPU            : constant Int_Unsigned_C := 4; -- VkPhysicalDeviceType

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSampleCountFlagBits.html
  VK_SAMPLE_COUNT_1_BIT  : constant Int_Unsigned_C := 1;  -- VkSampleCountFlagBits
  VK_SAMPLE_COUNT_2_BIT  : constant Int_Unsigned_C := 2;  -- VkSampleCountFlagBits
  VK_SAMPLE_COUNT_4_BIT  : constant Int_Unsigned_C := 4;  -- VkSampleCountFlagBits
  VK_SAMPLE_COUNT_8_BIT  : constant Int_Unsigned_C := 8;  -- VkSampleCountFlagBits
  VK_SAMPLE_COUNT_16_BIT : constant Int_Unsigned_C := 16; -- VkSampleCountFlagBits
  VK_SAMPLE_COUNT_32_BIT : constant Int_Unsigned_C := 32; -- VkSampleCountFlagBits
  VK_SAMPLE_COUNT_64_BIT : constant Int_Unsigned_C := 64; -- VkSampleCountFlagBits

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryPropertyFlagBits.html
  VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT     : constant Int_Unsigned_C := 1;  -- VkMemoryPropertyFlagBits
  VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT     : constant Int_Unsigned_C := 2;  -- VkMemoryPropertyFlagBits
  VK_MEMORY_PROPERTY_HOST_COHERENT_BIT    : constant Int_Unsigned_C := 4;  -- VkMemoryPropertyFlagBits
  VK_MEMORY_PROPERTY_HOST_CACHED_BIT      : constant Int_Unsigned_C := 8;  -- VkMemoryPropertyFlagBits
  VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT : constant Int_Unsigned_C := 16; -- VkMemoryPropertyFlagBits
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkComponentSwizzle.html
  VK_COMPONENT_SWIZZLE_IDENTITY : constant Int_Unsigned_C := 0; -- VkComponentSwizzle
  VK_COMPONENT_SWIZZLE_ZERO     : constant Int_Unsigned_C := 1; -- VkComponentSwizzle
  VK_COMPONENT_SWIZZLE_ONE      : constant Int_Unsigned_C := 2; -- VkComponentSwizzle
  VK_COMPONENT_SWIZZLE_R        : constant Int_Unsigned_C := 3; -- VkComponentSwizzle
  VK_COMPONENT_SWIZZLE_G        : constant Int_Unsigned_C := 4; -- VkComponentSwizzle
  VK_COMPONENT_SWIZZLE_B        : constant Int_Unsigned_C := 5; -- VkComponentSwizzle
  VK_COMPONENT_SWIZZLE_A        : constant Int_Unsigned_C := 6; -- VkComponentSwizzle
    
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageUsageFlagBits.html
  VK_IMAGE_USAGE_TRANSFER_SRC_BIT             : constant Int_Unsigned_C := 1;   -- VkImageUsageFlagBits
  VK_IMAGE_USAGE_TRANSFER_DST_BIT             : constant Int_Unsigned_C := 2;   -- VkImageUsageFlagBits
  VK_IMAGE_USAGE_SAMPLED_BIT                  : constant Int_Unsigned_C := 4;   -- VkImageUsageFlagBits
  VK_IMAGE_USAGE_STORAGE_BIT                  : constant Int_Unsigned_C := 8;   -- VkImageUsageFlagBits
  VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT         : constant Int_Unsigned_C := 16;  -- VkImageUsageFlagBits
  VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT : constant Int_Unsigned_C := 32;  -- VkImageUsageFlagBits
  VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT     : constant Int_Unsigned_C := 64;  -- VkImageUsageFlagBits
  VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT         : constant Int_Unsigned_C := 128; -- VkImageUsageFlagBits

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageLayout.html
  VK_IMAGE_LAYOUT_UNDEFINED                        : constant Int_Unsigned_C := 0;          -- VkImageLayout
  VK_IMAGE_LAYOUT_GENERAL                          : constant Int_Unsigned_C := 1;          -- VkImageLayout
  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL         : constant Int_Unsigned_C := 2;          -- VkImageLayout
  VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL : constant Int_Unsigned_C := 3;          -- VkImageLayout
  VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL  : constant Int_Unsigned_C := 4;          -- VkImageLayout
  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL         : constant Int_Unsigned_C := 5;          -- VkImageLayout
  VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL             : constant Int_Unsigned_C := 6;          -- VkImageLayout
  VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL             : constant Int_Unsigned_C := 7;          -- VkImageLayout
  VK_IMAGE_LAYOUT_PREINITIALIZED                   : constant Int_Unsigned_C := 8;          -- VkImageLayout
  VK_IMAGE_LAYOUT_PRESENT_SRC_KHR                  : constant Int_Unsigned_C := 1000001002; -- VkImageLayout

  -- http://nopper.tv/Vulkan/1.0/VkSurfaceTransformFlagBitsKHR.html
  VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR                     : constant Int_Unsigned_C := 1;   -- VkSurfaceTransformFlagBitsKHR
  VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR                    : constant Int_Unsigned_C := 2;   -- VkSurfaceTransformFlagBitsKHR
  VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR                   : constant Int_Unsigned_C := 4;   -- VkSurfaceTransformFlagBitsKHR
  VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR                   : constant Int_Unsigned_C := 8;   -- VkSurfaceTransformFlagBitsKHR
  VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR            : constant Int_Unsigned_C := 16;  -- VkSurfaceTransformFlagBitsKHR
  VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR  : constant Int_Unsigned_C := 32;  -- VkSurfaceTransformFlagBitsKHR
  VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR : constant Int_Unsigned_C := 64;  -- VkSurfaceTransformFlagBitsKHR
  VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR : constant Int_Unsigned_C := 128; -- VkSurfaceTransformFlagBitsKHR
  VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR                      : constant Int_Unsigned_C := 256; -- VkSurfaceTransformFlagBitsKHR

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkAccessFlagBits.html
  VK_ACCESS_INDIRECT_COMMAND_READ_BIT          : constant Int_Unsigned_C := 1;     -- VkAccessFlagBits
  VK_ACCESS_INDEX_READ_BIT                     : constant Int_Unsigned_C := 2;     -- VkAccessFlagBits
  VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT          : constant Int_Unsigned_C := 4;     -- VkAccessFlagBits
  VK_ACCESS_UNIFORM_READ_BIT                   : constant Int_Unsigned_C := 8;     -- VkAccessFlagBits
  VK_ACCESS_INPUT_ATTACHMENT_READ_BIT          : constant Int_Unsigned_C := 16;    -- VkAccessFlagBits
  VK_ACCESS_SHADER_READ_BIT                    : constant Int_Unsigned_C := 32;    -- VkAccessFlagBits
  VK_ACCESS_SHADER_WRITE_BIT                   : constant Int_Unsigned_C := 64;    -- VkAccessFlagBits
  VK_ACCESS_COLOR_ATTACHMENT_READ_BIT          : constant Int_Unsigned_C := 128;   -- VkAccessFlagBits
  VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT         : constant Int_Unsigned_C := 256;   -- VkAccessFlagBits
  VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT  : constant Int_Unsigned_C := 512;   -- VkAccessFlagBits
  VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT : constant Int_Unsigned_C := 1024;  -- VkAccessFlagBits
  VK_ACCESS_TRANSFER_READ_BIT                  : constant Int_Unsigned_C := 2048;  -- VkAccessFlagBits
  VK_ACCESS_TRANSFER_WRITE_BIT                 : constant Int_Unsigned_C := 4096;  -- VkAccessFlagBits
  VK_ACCESS_HOST_READ_BIT                      : constant Int_Unsigned_C := 8192;  -- VkAccessFlagBits
  VK_ACCESS_HOST_WRITE_BIT                     : constant Int_Unsigned_C := 16384; -- VkAccessFlagBits
  VK_ACCESS_MEMORY_READ_BIT                    : constant Int_Unsigned_C := 32768; -- VkAccessFlagBits
  VK_ACCESS_MEMORY_WRITE_BIT                   : constant Int_Unsigned_C := 65536; -- VkAccessFlagBits

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDynamicState.html
  VK_DYNAMIC_STATE_VIEWPORT             : constant Int_Unsigned_C := 0; -- VkDynamicState
  VK_DYNAMIC_STATE_SCISSOR              : constant Int_Unsigned_C := 1; -- VkDynamicState
  VK_DYNAMIC_STATE_LINE_WIDTH           : constant Int_Unsigned_C := 2; -- VkDynamicState
  VK_DYNAMIC_STATE_DEPTH_BIAS           : constant Int_Unsigned_C := 3; -- VkDynamicState
  VK_DYNAMIC_STATE_BLEND_CONSTANTS      : constant Int_Unsigned_C := 4; -- VkDynamicState
  VK_DYNAMIC_STATE_DEPTH_BOUNDS         : constant Int_Unsigned_C := 5; -- VkDynamicState
  VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK : constant Int_Unsigned_C := 6; -- VkDynamicState
  VK_DYNAMIC_STATE_STENCIL_WRITE_MASK   : constant Int_Unsigned_C := 7; -- VkDynamicState
  VK_DYNAMIC_STATE_STENCIL_REFERENCE    : constant Int_Unsigned_C := 8; -- VkDynamicState
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCompareOp.html
  VK_COMPARE_OP_NEVER            : constant Int_Unsigned_C := 0; -- VkCompareOp
  VK_COMPARE_OP_LESS             : constant Int_Unsigned_C := 1; -- VkCompareOp
  VK_COMPARE_OP_EQUAL            : constant Int_Unsigned_C := 2; -- VkCompareOp
  VK_COMPARE_OP_LESS_OR_EQUAL    : constant Int_Unsigned_C := 3; -- VkCompareOp
  VK_COMPARE_OP_GREATER          : constant Int_Unsigned_C := 4; -- VkCompareOp
  VK_COMPARE_OP_NOT_EQUAL        : constant Int_Unsigned_C := 5; -- VkCompareOp
  VK_COMPARE_OP_GREATER_OR_EQUAL : constant Int_Unsigned_C := 6; -- VkCompareOp
  VK_COMPARE_OP_ALWAYS           : constant Int_Unsigned_C := 7; -- VkCompareOp

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkStencilOp.html
  VK_STENCIL_OP_KEEP                : constant Int_Unsigned_C := 0; -- VkStencilOp
  VK_STENCIL_OP_ZERO                : constant Int_Unsigned_C := 1; -- VkStencilOp
  VK_STENCIL_OP_REPLACE             : constant Int_Unsigned_C := 2; -- VkStencilOp
  VK_STENCIL_OP_INCREMENT_AND_CLAMP : constant Int_Unsigned_C := 3; -- VkStencilOp
  VK_STENCIL_OP_DECREMENT_AND_CLAMP : constant Int_Unsigned_C := 4; -- VkStencilOp
  VK_STENCIL_OP_INVERT              : constant Int_Unsigned_C := 5; -- VkStencilOp
  VK_STENCIL_OP_INCREMENT_AND_WRAP  : constant Int_Unsigned_C := 6; -- VkStencilOp
  VK_STENCIL_OP_DECREMENT_AND_WRAP  : constant Int_Unsigned_C := 7; -- VkStencilOp

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkShaderStageFlagBits.html
  VK_SHADER_STAGE_VERTEX_BIT                  : constant Int_Unsigned_C := 16#0000_0001#; -- VkShaderStageFlagBits
  VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT    : constant Int_Unsigned_C := 16#0000_0002#; -- VkShaderStageFlagBits
  VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT : constant Int_Unsigned_C := 16#0000_0004#; -- VkShaderStageFlagBits
  VK_SHADER_STAGE_GEOMETRY_BIT                : constant Int_Unsigned_C := 16#0000_0008#; -- VkShaderStageFlagBits
  VK_SHADER_STAGE_FRAGMENT_BIT                : constant Int_Unsigned_C := 16#0000_0010#; -- VkShaderStageFlagBits
  VK_SHADER_STAGE_COMPUTE_BIT                 : constant Int_Unsigned_C := 16#0000_0020#; -- VkShaderStageFlagBits
  VK_SHADER_STAGE_ALL_GRAPHICS                : constant Int_Unsigned_C := 16#0000_001F#; -- VkShaderStageFlagBits
  VK_SHADER_STAGE_ALL                         : constant Int_Unsigned_C := 16#7FFF_FFFF#; -- VkShaderStageFlagBits

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPrimitiveTopology.html
  VK_PRIMITIVE_TOPOLOGY_POINT_LIST                    : constant Int_Unsigned_C := 0;  -- VkPrimitiveTopology
  VK_PRIMITIVE_TOPOLOGY_LINE_LIST                     : constant Int_Unsigned_C := 1;  -- VkPrimitiveTopology
  VK_PRIMITIVE_TOPOLOGY_LINE_STRIP                    : constant Int_Unsigned_C := 2;  -- VkPrimitiveTopology
  VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST                 : constant Int_Unsigned_C := 3;  -- VkPrimitiveTopology
  VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP                : constant Int_Unsigned_C := 4;  -- VkPrimitiveTopology
  VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN                  : constant Int_Unsigned_C := 5;  -- VkPrimitiveTopology
  VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY      : constant Int_Unsigned_C := 6;  -- VkPrimitiveTopology
  VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY     : constant Int_Unsigned_C := 7;  -- VkPrimitiveTopology
  VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY  : constant Int_Unsigned_C := 8;  -- VkPrimitiveTopology
  VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY : constant Int_Unsigned_C := 9;  -- VkPrimitiveTopology
  VK_PRIMITIVE_TOPOLOGY_PATCH_LIST                    : constant Int_Unsigned_C := 10; -- VkPrimitiveTopology
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFormatFeatureFlagBits.html
  VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT                : constant Int_Unsigned_C := 16#0000_0001#; -- VkFormatFeatureFlags    
  VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT                : constant Int_Unsigned_C := 16#0000_0002#; -- VkFormatFeatureFlags    
  VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT         : constant Int_Unsigned_C := 16#0000_0004#; -- VkFormatFeatureFlags    
  VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT         : constant Int_Unsigned_C := 16#0000_0008#; -- VkFormatFeatureFlags    
  VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT         : constant Int_Unsigned_C := 16#0000_0010#; -- VkFormatFeatureFlags    
  VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT  : constant Int_Unsigned_C := 16#0000_0020#; -- VkFormatFeatureFlags    
  VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT                : constant Int_Unsigned_C := 16#0000_0040#; -- VkFormatFeatureFlags    
  VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT             : constant Int_Unsigned_C := 16#0000_0080#; -- VkFormatFeatureFlags    
  VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT       : constant Int_Unsigned_C := 16#0000_0100#; -- VkFormatFeatureFlags    
  VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT     : constant Int_Unsigned_C := 16#0000_0200#; -- VkFormatFeatureFlags    
  VK_FORMAT_FEATURE_BLIT_SRC_BIT                     : constant Int_Unsigned_C := 16#0000_0400#; -- VkFormatFeatureFlags    
  VK_FORMAT_FEATURE_BLIT_DST_BIT                     : constant Int_Unsigned_C := 16#0000_0800#; -- VkFormatFeatureFlags    
  VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT  : constant Int_Unsigned_C := 16#0000_1000#; -- VkFormatFeatureFlags    
    
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkLogicOp.html
  VK_LOGIC_OP_CLEAR         : constant Int_Unsigned_C := 0;  -- VkLogicOp
  VK_LOGIC_OP_AND           : constant Int_Unsigned_C := 1;  -- VkLogicOp
  VK_LOGIC_OP_AND_REVERSE   : constant Int_Unsigned_C := 2;  -- VkLogicOp
  VK_LOGIC_OP_COPY          : constant Int_Unsigned_C := 3;  -- VkLogicOp
  VK_LOGIC_OP_AND_INVERTED  : constant Int_Unsigned_C := 4;  -- VkLogicOp
  VK_LOGIC_OP_NO_OP         : constant Int_Unsigned_C := 5;  -- VkLogicOp
  VK_LOGIC_OP_XOR           : constant Int_Unsigned_C := 6;  -- VkLogicOp
  VK_LOGIC_OP_OR            : constant Int_Unsigned_C := 7;  -- VkLogicOp
  VK_LOGIC_OP_NOR           : constant Int_Unsigned_C := 8;  -- VkLogicOp
  VK_LOGIC_OP_EQUIVALENT    : constant Int_Unsigned_C := 9;  -- VkLogicOp
  VK_LOGIC_OP_INVERT        : constant Int_Unsigned_C := 10; -- VkLogicOp
  VK_LOGIC_OP_OR_REVERSE    : constant Int_Unsigned_C := 11; -- VkLogicOp
  VK_LOGIC_OP_COPY_INVERTED : constant Int_Unsigned_C := 12; -- VkLogicOp
  VK_LOGIC_OP_OR_INVERTED   : constant Int_Unsigned_C := 13; -- VkLogicOp
  VK_LOGIC_OP_NAND          : constant Int_Unsigned_C := 14; -- VkLogicOp
  VK_LOGIC_OP_SET           : constant Int_Unsigned_C := 15; -- VkLogicOp

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBlendFactor
  VK_BLEND_FACTOR_ZERO                     : constant Int_Unsigned_C := 0;  -- VkBlendFactor
  VK_BLEND_FACTOR_ONE                      : constant Int_Unsigned_C := 1;  -- VkBlendFactor
  VK_BLEND_FACTOR_SRC_COLOR                : constant Int_Unsigned_C := 2;  -- VkBlendFactor
  VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR      : constant Int_Unsigned_C := 3;  -- VkBlendFactor
  VK_BLEND_FACTOR_DST_COLOR                : constant Int_Unsigned_C := 4;  -- VkBlendFactor
  VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR      : constant Int_Unsigned_C := 5;  -- VkBlendFactor
  VK_BLEND_FACTOR_SRC_ALPHA                : constant Int_Unsigned_C := 6;  -- VkBlendFactor
  VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA      : constant Int_Unsigned_C := 7;  -- VkBlendFactor
  VK_BLEND_FACTOR_DST_ALPHA                : constant Int_Unsigned_C := 8;  -- VkBlendFactor
  VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA      : constant Int_Unsigned_C := 9;  -- VkBlendFactor
  VK_BLEND_FACTOR_CONSTANT_COLOR           : constant Int_Unsigned_C := 10; -- VkBlendFactor
  VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR : constant Int_Unsigned_C := 11; -- VkBlendFactor
  VK_BLEND_FACTOR_CONSTANT_ALPHA           : constant Int_Unsigned_C := 12; -- VkBlendFactor
  VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA : constant Int_Unsigned_C := 13; -- VkBlendFactor
  VK_BLEND_FACTOR_SRC_ALPHA_SATURATE       : constant Int_Unsigned_C := 14; -- VkBlendFactor
  VK_BLEND_FACTOR_SRC1_COLOR               : constant Int_Unsigned_C := 15; -- VkBlendFactor
  VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR     : constant Int_Unsigned_C := 16; -- VkBlendFactor
  VK_BLEND_FACTOR_SRC1_ALPHA               : constant Int_Unsigned_C := 17; -- VkBlendFactor
  VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA     : constant Int_Unsigned_C := 18; -- VkBlendFactor

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineStageFlagBits.html
  VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT                    : constant Int_Unsigned_C := 16#0000_0001#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT                  : constant Int_Unsigned_C := 16#0000_0002#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_VERTEX_INPUT_BIT                   : constant Int_Unsigned_C := 16#0000_0004#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_VERTEX_SHADER_BIT                  : constant Int_Unsigned_C := 16#0000_0008#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT    : constant Int_Unsigned_C := 16#0000_0010#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT : constant Int_Unsigned_C := 16#0000_0020#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT                : constant Int_Unsigned_C := 16#0000_0040#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT                : constant Int_Unsigned_C := 16#0000_0080#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT           : constant Int_Unsigned_C := 16#0000_0100#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT            : constant Int_Unsigned_C := 16#0000_0200#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT        : constant Int_Unsigned_C := 16#0000_0400#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT                 : constant Int_Unsigned_C := 16#0000_0800#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_TRANSFER_BIT                       : constant Int_Unsigned_C := 16#0000_1000#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT                 : constant Int_Unsigned_C := 16#0000_2000#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_HOST_BIT                           : constant Int_Unsigned_C := 16#0000_4000#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT                   : constant Int_Unsigned_C := 16#0000_8000#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_ALL_COMMANDS_BIT                   : constant Int_Unsigned_C := 16#0001_0000#; -- VkPipelineStageFlagBits 
    
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkResult.html
  VK_SUCCESS                        : constant Int_Unsigned_C := 0;                                  -- VkResult
  VK_NOT_READY                      : constant Int_Unsigned_C := 1;                                  -- VkResult
  VK_TIMEOUT                        : constant Int_Unsigned_C := 2;                                  -- VkResult
  VK_EVENT_SET                      : constant Int_Unsigned_C := 3;                                  -- VkResult
  VK_EVENT_RESET                    : constant Int_Unsigned_C := 4;                                  -- VkResult
  VK_INCOMPLETE                     : constant Int_Unsigned_C := 5;                                  -- VkResult
  VK_ERROR_OUT_OF_HOST_MEMORY       : constant Int_Unsigned_C := To_Int_32_Unsigned_C (-1);          -- VkResult
  VK_ERROR_OUT_OF_DEVICE_MEMORY     : constant Int_Unsigned_C := To_Int_32_Unsigned_C (-2);          -- VkResult
  VK_ERROR_INITIALIZATION_FAILED    : constant Int_Unsigned_C := To_Int_32_Unsigned_C (-3);          -- VkResult
  VK_ERROR_DEVICE_LOST              : constant Int_Unsigned_C := To_Int_32_Unsigned_C (-4);          -- VkResult
  VK_ERROR_MEMORY_MAP_FAILED        : constant Int_Unsigned_C := To_Int_32_Unsigned_C (-5);          -- VkResult
  VK_ERROR_LAYER_NOT_PRESENT        : constant Int_Unsigned_C := To_Int_32_Unsigned_C (-6);          -- VkResult
  VK_ERROR_EXTENSION_NOT_PRESENT    : constant Int_Unsigned_C := To_Int_32_Unsigned_C (-7);          -- VkResult
  VK_ERROR_FEATURE_NOT_PRESENT      : constant Int_Unsigned_C := To_Int_32_Unsigned_C (-8);          -- VkResult
  VK_ERROR_INCOMPATIBLE_DRIVER      : constant Int_Unsigned_C := To_Int_32_Unsigned_C (-9);          -- VkResult
  VK_ERROR_TOO_MANY_OBJECTS         : constant Int_Unsigned_C := To_Int_32_Unsigned_C (-10);         -- VkResult
  VK_ERROR_FORMAT_NOT_SUPPORTED     : constant Int_Unsigned_C := To_Int_32_Unsigned_C (-11);         -- VkResult
  VK_ERROR_SURFACE_LOST_KHR         : constant Int_Unsigned_C := To_Int_32_Unsigned_C (-1000000000); -- VkResult
  VK_ERROR_NATIVE_WINDOW_IN_USE_KHR : constant Int_Unsigned_C := To_Int_32_Unsigned_C (-1000000001); -- VkResult
  VK_SUBOPTIMAL_KHR                 : constant Int_Unsigned_C := 1000001003;                         -- VkResult
  VK_ERROR_OUT_OF_DATE_KHR          : constant Int_Unsigned_C := 3294966292; -- VkResult
  VK_ERROR_INCOMPATIBLE_DISPLAY_KHR : constant Int_Unsigned_C := To_Int_32_Unsigned_C (-1000003001); -- VkResult
  VK_ERROR_VALIDATION_FAILED_EXT    : constant Int_Unsigned_C := To_Int_32_Unsigned_C (-1000011001); -- VkResult
  VK_ERROR_INVALID_SHADER_NV        : constant Int_Unsigned_C := To_Int_32_Unsigned_C (-1000012000); -- VkResult

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkStructureType.html
  VK_STRUCTURE_TYPE_APPLICATION_INFO                          : constant Int_Unsigned_C := 0;          -- VkStructureType 
  VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO                      : constant Int_Unsigned_C := 1;          -- VkStructureType 
  VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO                  : constant Int_Unsigned_C := 2;          -- VkStructureType 
  VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO                        : constant Int_Unsigned_C := 3;          -- VkStructureType 
  VK_STRUCTURE_TYPE_SUBMIT_INFO                               : constant Int_Unsigned_C := 4;          -- VkStructureType 
  VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO                      : constant Int_Unsigned_C := 5;          -- VkStructureType 
  VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE                       : constant Int_Unsigned_C := 6;          -- VkStructureType 
  VK_STRUCTURE_TYPE_BIND_SPARSE_INFO                          : constant Int_Unsigned_C := 7;          -- VkStructureType 
  VK_STRUCTURE_TYPE_FENCE_CREATE_INFO                         : constant Int_Unsigned_C := 8;          -- VkStructureType 
  VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO                     : constant Int_Unsigned_C := 9;          -- VkStructureType 
  VK_STRUCTURE_TYPE_EVENT_CREATE_INFO                         : constant Int_Unsigned_C := 10;         -- VkStructureType 
  VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO                    : constant Int_Unsigned_C := 11;         -- VkStructureType 
  VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO                        : constant Int_Unsigned_C := 12;         -- VkStructureType 
  VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO                   : constant Int_Unsigned_C := 13;         -- VkStructureType 
  VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO                         : constant Int_Unsigned_C := 14;         -- VkStructureType 
  VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO                    : constant Int_Unsigned_C := 15;         -- VkStructureType 
  VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO                 : constant Int_Unsigned_C := 16;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO                : constant Int_Unsigned_C := 17;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO         : constant Int_Unsigned_C := 18;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO   : constant Int_Unsigned_C := 19;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO : constant Int_Unsigned_C := 20;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO   : constant Int_Unsigned_C := 21;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO       : constant Int_Unsigned_C := 22;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO  : constant Int_Unsigned_C := 23;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO    : constant Int_Unsigned_C := 24;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO  : constant Int_Unsigned_C := 25;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO    : constant Int_Unsigned_C := 26;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO        : constant Int_Unsigned_C := 27;         -- VkStructureType 
  VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO             : constant Int_Unsigned_C := 28;         -- VkStructureType 
  VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO              : constant Int_Unsigned_C := 29;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO               : constant Int_Unsigned_C := 30;         -- VkStructureType 
  VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO                       : constant Int_Unsigned_C := 31;         -- VkStructureType 
  VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO         : constant Int_Unsigned_C := 32;         -- VkStructureType 
  VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO               : constant Int_Unsigned_C := 33;         -- VkStructureType 
  VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO              : constant Int_Unsigned_C := 34;         -- VkStructureType 
  VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET                      : constant Int_Unsigned_C := 35;         -- VkStructureType 
  VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET                       : constant Int_Unsigned_C := 36;         -- VkStructureType 
  VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO                   : constant Int_Unsigned_C := 37;         -- VkStructureType 
  VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO                   : constant Int_Unsigned_C := 38;         -- VkStructureType 
  VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO                  : constant Int_Unsigned_C := 39;         -- VkStructureType 
  VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO              : constant Int_Unsigned_C := 40;         -- VkStructureType 
  VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO           : constant Int_Unsigned_C := 41;         -- VkStructureType 
  VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO                 : constant Int_Unsigned_C := 42;         -- VkStructureType 
  VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO                    : constant Int_Unsigned_C := 43;         -- VkStructureType 
  VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER                     : constant Int_Unsigned_C := 44;         -- VkStructureType 
  VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER                      : constant Int_Unsigned_C := 45;         -- VkStructureType 
  VK_STRUCTURE_TYPE_MEMORY_BARRIER                            : constant Int_Unsigned_C := 46;         -- VkStructureType 
  VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO               : constant Int_Unsigned_C := 47;         -- VkStructureType 
  VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO                 : constant Int_Unsigned_C := 48;         -- VkStructureType 
  VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR                 : constant Int_Unsigned_C := 1000001000; -- VkStructureType 
  VK_STRUCTURE_TYPE_PRESENT_INFO_KHR                          : constant Int_Unsigned_C := 1000001001; -- VkStructureType 
  VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR              : constant Int_Unsigned_C := 1000002000; -- VkStructureType 
  VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR           : constant Int_Unsigned_C := 1000002001; -- VkStructureType 
  VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR                  : constant Int_Unsigned_C := 1000003000; -- VkStructureType 
  VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR              : constant Int_Unsigned_C := 1000004000; -- VkStructureType 
  VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR               : constant Int_Unsigned_C := 1000005000; -- VkStructureType 
  VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR           : constant Int_Unsigned_C := 1000006000; -- VkStructureType 
  VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR               : constant Int_Unsigned_C := 1000007000; -- VkStructureType 
  VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR           : constant Int_Unsigned_C := 1000008000; -- VkStructureType 
  VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR             : constant Int_Unsigned_C := 1000009000; -- VkStructureType 
  VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT     : constant Int_Unsigned_C := 1000011000; -- VkStructureType

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFormat.html
  VK_FORMAT_UNDEFINED                  : constant Int_Unsigned_C := 0;   -- VkFormat
  VK_FORMAT_R4G4_UNORM_PACK8           : constant Int_Unsigned_C := 1;   -- VkFormat
  VK_FORMAT_R4G4B4A4_UNORM_PACK16      : constant Int_Unsigned_C := 2;   -- VkFormat
  VK_FORMAT_B4G4R4A4_UNORM_PACK16      : constant Int_Unsigned_C := 3;   -- VkFormat
  VK_FORMAT_R5G6B5_UNORM_PACK16        : constant Int_Unsigned_C := 4;   -- VkFormat
  VK_FORMAT_B5G6R5_UNORM_PACK16        : constant Int_Unsigned_C := 5;   -- VkFormat
  VK_FORMAT_R5G5B5A1_UNORM_PACK16      : constant Int_Unsigned_C := 6;   -- VkFormat
  VK_FORMAT_B5G5R5A1_UNORM_PACK16      : constant Int_Unsigned_C := 7;   -- VkFormat
  VK_FORMAT_A1R5G5B5_UNORM_PACK16      : constant Int_Unsigned_C := 8;   -- VkFormat
  VK_FORMAT_R8_UNORM                   : constant Int_Unsigned_C := 9;   -- VkFormat
  VK_FORMAT_R8_SNORM                   : constant Int_Unsigned_C := 10;  -- VkFormat
  VK_FORMAT_R8_USCALED                 : constant Int_Unsigned_C := 11;  -- VkFormat
  VK_FORMAT_R8_SSCALED                 : constant Int_Unsigned_C := 12;  -- VkFormat
  VK_FORMAT_R8_UINT                    : constant Int_Unsigned_C := 13;  -- VkFormat
  VK_FORMAT_R8_SINT                    : constant Int_Unsigned_C := 14;  -- VkFormat
  VK_FORMAT_R8_SRGB                    : constant Int_Unsigned_C := 15;  -- VkFormat
  VK_FORMAT_R8G8_UNORM                 : constant Int_Unsigned_C := 16;  -- VkFormat
  VK_FORMAT_R8G8_SNORM                 : constant Int_Unsigned_C := 17;  -- VkFormat
  VK_FORMAT_R8G8_USCALED               : constant Int_Unsigned_C := 18;  -- VkFormat
  VK_FORMAT_R8G8_SSCALED               : constant Int_Unsigned_C := 19;  -- VkFormat
  VK_FORMAT_R8G8_UINT                  : constant Int_Unsigned_C := 20;  -- VkFormat
  VK_FORMAT_R8G8_SINT                  : constant Int_Unsigned_C := 21;  -- VkFormat
  VK_FORMAT_R8G8_SRGB                  : constant Int_Unsigned_C := 22;  -- VkFormat
  VK_FORMAT_R8G8B8_UNORM               : constant Int_Unsigned_C := 23;  -- VkFormat
  VK_FORMAT_R8G8B8_SNORM               : constant Int_Unsigned_C := 24;  -- VkFormat
  VK_FORMAT_R8G8B8_USCALED             : constant Int_Unsigned_C := 25;  -- VkFormat
  VK_FORMAT_R8G8B8_SSCALED             : constant Int_Unsigned_C := 26;  -- VkFormat
  VK_FORMAT_R8G8B8_UINT                : constant Int_Unsigned_C := 27;  -- VkFormat
  VK_FORMAT_R8G8B8_SINT                : constant Int_Unsigned_C := 28;  -- VkFormat
  VK_FORMAT_R8G8B8_SRGB                : constant Int_Unsigned_C := 29;  -- VkFormat
  VK_FORMAT_B8G8R8_UNORM               : constant Int_Unsigned_C := 30;  -- VkFormat
  VK_FORMAT_B8G8R8_SNORM               : constant Int_Unsigned_C := 31;  -- VkFormat
  VK_FORMAT_B8G8R8_USCALED             : constant Int_Unsigned_C := 32;  -- VkFormat
  VK_FORMAT_B8G8R8_SSCALED             : constant Int_Unsigned_C := 33;  -- VkFormat
  VK_FORMAT_B8G8R8_UINT                : constant Int_Unsigned_C := 34;  -- VkFormat
  VK_FORMAT_B8G8R8_SINT                : constant Int_Unsigned_C := 35;  -- VkFormat
  VK_FORMAT_B8G8R8_SRGB                : constant Int_Unsigned_C := 36;  -- VkFormat
  VK_FORMAT_R8G8B8A8_UNORM             : constant Int_Unsigned_C := 37;  -- VkFormat
  VK_FORMAT_R8G8B8A8_SNORM             : constant Int_Unsigned_C := 38;  -- VkFormat
  VK_FORMAT_R8G8B8A8_USCALED           : constant Int_Unsigned_C := 39;  -- VkFormat
  VK_FORMAT_R8G8B8A8_SSCALED           : constant Int_Unsigned_C := 40;  -- VkFormat
  VK_FORMAT_R8G8B8A8_UINT              : constant Int_Unsigned_C := 41;  -- VkFormat
  VK_FORMAT_R8G8B8A8_SINT              : constant Int_Unsigned_C := 42;  -- VkFormat
  VK_FORMAT_R8G8B8A8_SRGB              : constant Int_Unsigned_C := 43;  -- VkFormat
  VK_FORMAT_B8G8R8A8_UNORM             : constant Int_Unsigned_C := 44;  -- VkFormat
  VK_FORMAT_B8G8R8A8_SNORM             : constant Int_Unsigned_C := 45;  -- VkFormat
  VK_FORMAT_B8G8R8A8_USCALED           : constant Int_Unsigned_C := 46;  -- VkFormat
  VK_FORMAT_B8G8R8A8_SSCALED           : constant Int_Unsigned_C := 47;  -- VkFormat
  VK_FORMAT_B8G8R8A8_UINT              : constant Int_Unsigned_C := 48;  -- VkFormat
  VK_FORMAT_B8G8R8A8_SINT              : constant Int_Unsigned_C := 49;  -- VkFormat
  VK_FORMAT_B8G8R8A8_SRGB              : constant Int_Unsigned_C := 50;  -- VkFormat
  VK_FORMAT_A8B8G8R8_UNORM_PACK32      : constant Int_Unsigned_C := 51;  -- VkFormat
  VK_FORMAT_A8B8G8R8_SNORM_PACK32      : constant Int_Unsigned_C := 52;  -- VkFormat
  VK_FORMAT_A8B8G8R8_USCALED_PACK32    : constant Int_Unsigned_C := 53;  -- VkFormat
  VK_FORMAT_A8B8G8R8_SSCALED_PACK32    : constant Int_Unsigned_C := 54;  -- VkFormat
  VK_FORMAT_A8B8G8R8_UINT_PACK32       : constant Int_Unsigned_C := 55;  -- VkFormat
  VK_FORMAT_A8B8G8R8_SINT_PACK32       : constant Int_Unsigned_C := 56;  -- VkFormat
  VK_FORMAT_A8B8G8R8_SRGB_PACK32       : constant Int_Unsigned_C := 57;  -- VkFormat
  VK_FORMAT_A2R10G10B10_UNORM_PACK32   : constant Int_Unsigned_C := 58;  -- VkFormat
  VK_FORMAT_A2R10G10B10_SNORM_PACK32   : constant Int_Unsigned_C := 59;  -- VkFormat
  VK_FORMAT_A2R10G10B10_USCALED_PACK32 : constant Int_Unsigned_C := 60;  -- VkFormat
  VK_FORMAT_A2R10G10B10_SSCALED_PACK32 : constant Int_Unsigned_C := 61;  -- VkFormat
  VK_FORMAT_A2R10G10B10_UINT_PACK32    : constant Int_Unsigned_C := 62;  -- VkFormat
  VK_FORMAT_A2R10G10B10_SINT_PACK32    : constant Int_Unsigned_C := 63;  -- VkFormat
  VK_FORMAT_A2B10G10R10_UNORM_PACK32   : constant Int_Unsigned_C := 64;  -- VkFormat
  VK_FORMAT_A2B10G10R10_SNORM_PACK32   : constant Int_Unsigned_C := 65;  -- VkFormat
  VK_FORMAT_A2B10G10R10_USCALED_PACK32 : constant Int_Unsigned_C := 66;  -- VkFormat
  VK_FORMAT_A2B10G10R10_SSCALED_PACK32 : constant Int_Unsigned_C := 67;  -- VkFormat
  VK_FORMAT_A2B10G10R10_UINT_PACK32    : constant Int_Unsigned_C := 68;  -- VkFormat
  VK_FORMAT_A2B10G10R10_SINT_PACK32    : constant Int_Unsigned_C := 69;  -- VkFormat
  VK_FORMAT_R16_UNORM                  : constant Int_Unsigned_C := 70;  -- VkFormat
  VK_FORMAT_R16_SNORM                  : constant Int_Unsigned_C := 71;  -- VkFormat
  VK_FORMAT_R16_USCALED                : constant Int_Unsigned_C := 72;  -- VkFormat
  VK_FORMAT_R16_SSCALED                : constant Int_Unsigned_C := 73;  -- VkFormat
  VK_FORMAT_R16_UINT                   : constant Int_Unsigned_C := 74;  -- VkFormat
  VK_FORMAT_R16_SINT                   : constant Int_Unsigned_C := 75;  -- VkFormat
  VK_FORMAT_R16_SFLOAT                 : constant Int_Unsigned_C := 76;  -- VkFormat
  VK_FORMAT_R16G16_UNORM               : constant Int_Unsigned_C := 77;  -- VkFormat
  VK_FORMAT_R16G16_SNORM               : constant Int_Unsigned_C := 78;  -- VkFormat
  VK_FORMAT_R16G16_USCALED             : constant Int_Unsigned_C := 79;  -- VkFormat
  VK_FORMAT_R16G16_SSCALED             : constant Int_Unsigned_C := 80;  -- VkFormat
  VK_FORMAT_R16G16_UINT                : constant Int_Unsigned_C := 81;  -- VkFormat
  VK_FORMAT_R16G16_SINT                : constant Int_Unsigned_C := 82;  -- VkFormat
  VK_FORMAT_R16G16_SFLOAT              : constant Int_Unsigned_C := 83;  -- VkFormat
  VK_FORMAT_R16G16B16_UNORM            : constant Int_Unsigned_C := 84;  -- VkFormat
  VK_FORMAT_R16G16B16_SNORM            : constant Int_Unsigned_C := 85;  -- VkFormat
  VK_FORMAT_R16G16B16_USCALED          : constant Int_Unsigned_C := 86;  -- VkFormat
  VK_FORMAT_R16G16B16_SSCALED          : constant Int_Unsigned_C := 87;  -- VkFormat
  VK_FORMAT_R16G16B16_UINT             : constant Int_Unsigned_C := 88;  -- VkFormat
  VK_FORMAT_R16G16B16_SINT             : constant Int_Unsigned_C := 89;  -- VkFormat
  VK_FORMAT_R16G16B16_SFLOAT           : constant Int_Unsigned_C := 90;  -- VkFormat
  VK_FORMAT_R16G16B16A16_UNORM         : constant Int_Unsigned_C := 91;  -- VkFormat
  VK_FORMAT_R16G16B16A16_SNORM         : constant Int_Unsigned_C := 92;  -- VkFormat
  VK_FORMAT_R16G16B16A16_USCALED       : constant Int_Unsigned_C := 93;  -- VkFormat
  VK_FORMAT_R16G16B16A16_SSCALED       : constant Int_Unsigned_C := 94;  -- VkFormat
  VK_FORMAT_R16G16B16A16_UINT          : constant Int_Unsigned_C := 95;  -- VkFormat
  VK_FORMAT_R16G16B16A16_SINT          : constant Int_Unsigned_C := 96;  -- VkFormat
  VK_FORMAT_R16G16B16A16_SFLOAT        : constant Int_Unsigned_C := 97;  -- VkFormat
  VK_FORMAT_R32_UINT                   : constant Int_Unsigned_C := 98;  -- VkFormat
  VK_FORMAT_R32_SINT                   : constant Int_Unsigned_C := 99;  -- VkFormat
  VK_FORMAT_R32_SFLOAT                 : constant Int_Unsigned_C := 100; -- VkFormat
  VK_FORMAT_R32G32_UINT                : constant Int_Unsigned_C := 101; -- VkFormat
  VK_FORMAT_R32G32_SINT                : constant Int_Unsigned_C := 102; -- VkFormat
  VK_FORMAT_R32G32_SFLOAT              : constant Int_Unsigned_C := 103; -- VkFormat
  VK_FORMAT_R32G32B32_UINT             : constant Int_Unsigned_C := 104; -- VkFormat
  VK_FORMAT_R32G32B32_SINT             : constant Int_Unsigned_C := 105; -- VkFormat
  VK_FORMAT_R32G32B32_SFLOAT           : constant Int_Unsigned_C := 106; -- VkFormat
  VK_FORMAT_R32G32B32A32_UINT          : constant Int_Unsigned_C := 107; -- VkFormat
  VK_FORMAT_R32G32B32A32_SINT          : constant Int_Unsigned_C := 108; -- VkFormat
  VK_FORMAT_R32G32B32A32_SFLOAT        : constant Int_Unsigned_C := 109; -- VkFormat
  VK_FORMAT_R64_UINT                   : constant Int_Unsigned_C := 110; -- VkFormat
  VK_FORMAT_R64_SINT                   : constant Int_Unsigned_C := 111; -- VkFormat
  VK_FORMAT_R64_SFLOAT                 : constant Int_Unsigned_C := 112; -- VkFormat
  VK_FORMAT_R64G64_UINT                : constant Int_Unsigned_C := 113; -- VkFormat
  VK_FORMAT_R64G64_SINT                : constant Int_Unsigned_C := 114; -- VkFormat
  VK_FORMAT_R64G64_SFLOAT              : constant Int_Unsigned_C := 115; -- VkFormat
  VK_FORMAT_R64G64B64_UINT             : constant Int_Unsigned_C := 116; -- VkFormat
  VK_FORMAT_R64G64B64_SINT             : constant Int_Unsigned_C := 117; -- VkFormat
  VK_FORMAT_R64G64B64_SFLOAT           : constant Int_Unsigned_C := 118; -- VkFormat
  VK_FORMAT_R64G64B64A64_UINT          : constant Int_Unsigned_C := 119; -- VkFormat
  VK_FORMAT_R64G64B64A64_SINT          : constant Int_Unsigned_C := 120; -- VkFormat
  VK_FORMAT_R64G64B64A64_SFLOAT        : constant Int_Unsigned_C := 121; -- VkFormat
  VK_FORMAT_B10G11R11_UFLOAT_PACK32    : constant Int_Unsigned_C := 122; -- VkFormat
  VK_FORMAT_E5B9G9R9_UFLOAT_PACK32     : constant Int_Unsigned_C := 123; -- VkFormat
  VK_FORMAT_D16_UNORM                  : constant Int_Unsigned_C := 124; -- VkFormat
  VK_FORMAT_X8_D24_UNORM_PACK32        : constant Int_Unsigned_C := 125; -- VkFormat
  VK_FORMAT_D32_SFLOAT                 : constant Int_Unsigned_C := 126; -- VkFormat
  VK_FORMAT_S8_UINT                    : constant Int_Unsigned_C := 127; -- VkFormat
  VK_FORMAT_D16_UNORM_S8_UINT          : constant Int_Unsigned_C := 128; -- VkFormat
  VK_FORMAT_D24_UNORM_S8_UINT          : constant Int_Unsigned_C := 129; -- VkFormat
  VK_FORMAT_D32_SFLOAT_S8_UINT         : constant Int_Unsigned_C := 130; -- VkFormat
  VK_FORMAT_BC1_RGB_UNORM_BLOCK        : constant Int_Unsigned_C := 131; -- VkFormat
  VK_FORMAT_BC1_RGB_SRGB_BLOCK         : constant Int_Unsigned_C := 132; -- VkFormat
  VK_FORMAT_BC1_RGBA_UNORM_BLOCK       : constant Int_Unsigned_C := 133; -- VkFormat
  VK_FORMAT_BC1_RGBA_SRGB_BLOCK        : constant Int_Unsigned_C := 134; -- VkFormat
  VK_FORMAT_BC2_UNORM_BLOCK            : constant Int_Unsigned_C := 135; -- VkFormat
  VK_FORMAT_BC2_SRGB_BLOCK             : constant Int_Unsigned_C := 136; -- VkFormat
  VK_FORMAT_BC3_UNORM_BLOCK            : constant Int_Unsigned_C := 137; -- VkFormat
  VK_FORMAT_BC3_SRGB_BLOCK             : constant Int_Unsigned_C := 138; -- VkFormat
  VK_FORMAT_BC4_UNORM_BLOCK            : constant Int_Unsigned_C := 139; -- VkFormat
  VK_FORMAT_BC4_SNORM_BLOCK            : constant Int_Unsigned_C := 140; -- VkFormat
  VK_FORMAT_BC5_UNORM_BLOCK            : constant Int_Unsigned_C := 141; -- VkFormat
  VK_FORMAT_BC5_SNORM_BLOCK            : constant Int_Unsigned_C := 142; -- VkFormat
  VK_FORMAT_BC6H_UFLOAT_BLOCK          : constant Int_Unsigned_C := 143; -- VkFormat
  VK_FORMAT_BC6H_SFLOAT_BLOCK          : constant Int_Unsigned_C := 144; -- VkFormat
  VK_FORMAT_BC7_UNORM_BLOCK            : constant Int_Unsigned_C := 145; -- VkFormat
  VK_FORMAT_BC7_SRGB_BLOCK             : constant Int_Unsigned_C := 146; -- VkFormat
  VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK    : constant Int_Unsigned_C := 147; -- VkFormat
  VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK     : constant Int_Unsigned_C := 148; -- VkFormat
  VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK  : constant Int_Unsigned_C := 149; -- VkFormat
  VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK   : constant Int_Unsigned_C := 150; -- VkFormat
  VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK  : constant Int_Unsigned_C := 151; -- VkFormat
  VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK   : constant Int_Unsigned_C := 152; -- VkFormat
  VK_FORMAT_EAC_R11_UNORM_BLOCK        : constant Int_Unsigned_C := 153; -- VkFormat
  VK_FORMAT_EAC_R11_SNORM_BLOCK        : constant Int_Unsigned_C := 154; -- VkFormat
  VK_FORMAT_EAC_R11G11_UNORM_BLOCK     : constant Int_Unsigned_C := 155; -- VkFormat
  VK_FORMAT_EAC_R11G11_SNORM_BLOCK     : constant Int_Unsigned_C := 156; -- VkFormat
  VK_FORMAT_ASTC_4x4_UNORM_BLOCK       : constant Int_Unsigned_C := 157; -- VkFormat
  VK_FORMAT_ASTC_4x4_SRGB_BLOCK        : constant Int_Unsigned_C := 158; -- VkFormat
  VK_FORMAT_ASTC_5x4_UNORM_BLOCK       : constant Int_Unsigned_C := 159; -- VkFormat
  VK_FORMAT_ASTC_5x4_SRGB_BLOCK        : constant Int_Unsigned_C := 160; -- VkFormat
  VK_FORMAT_ASTC_5x5_UNORM_BLOCK       : constant Int_Unsigned_C := 161; -- VkFormat
  VK_FORMAT_ASTC_5x5_SRGB_BLOCK        : constant Int_Unsigned_C := 162; -- VkFormat
  VK_FORMAT_ASTC_6x5_UNORM_BLOCK       : constant Int_Unsigned_C := 163; -- VkFormat
  VK_FORMAT_ASTC_6x5_SRGB_BLOCK        : constant Int_Unsigned_C := 164; -- VkFormat
  VK_FORMAT_ASTC_6x6_UNORM_BLOCK       : constant Int_Unsigned_C := 165; -- VkFormat
  VK_FORMAT_ASTC_6x6_SRGB_BLOCK        : constant Int_Unsigned_C := 166; -- VkFormat
  VK_FORMAT_ASTC_8x5_UNORM_BLOCK       : constant Int_Unsigned_C := 167; -- VkFormat
  VK_FORMAT_ASTC_8x5_SRGB_BLOCK        : constant Int_Unsigned_C := 168; -- VkFormat
  VK_FORMAT_ASTC_8x6_UNORM_BLOCK       : constant Int_Unsigned_C := 169; -- VkFormat
  VK_FORMAT_ASTC_8x6_SRGB_BLOCK        : constant Int_Unsigned_C := 170; -- VkFormat
  VK_FORMAT_ASTC_8x8_UNORM_BLOCK       : constant Int_Unsigned_C := 171; -- VkFormat
  VK_FORMAT_ASTC_8x8_SRGB_BLOCK        : constant Int_Unsigned_C := 172; -- VkFormat
  VK_FORMAT_ASTC_10x5_UNORM_BLOCK      : constant Int_Unsigned_C := 173; -- VkFormat
  VK_FORMAT_ASTC_10x5_SRGB_BLOCK       : constant Int_Unsigned_C := 174; -- VkFormat
  VK_FORMAT_ASTC_10x6_UNORM_BLOCK      : constant Int_Unsigned_C := 175; -- VkFormat
  VK_FORMAT_ASTC_10x6_SRGB_BLOCK       : constant Int_Unsigned_C := 176; -- VkFormat
  VK_FORMAT_ASTC_10x8_UNORM_BLOCK      : constant Int_Unsigned_C := 177; -- VkFormat
  VK_FORMAT_ASTC_10x8_SRGB_BLOCK       : constant Int_Unsigned_C := 178; -- VkFormat
  VK_FORMAT_ASTC_10x10_UNORM_BLOCK     : constant Int_Unsigned_C := 179; -- VkFormat
  VK_FORMAT_ASTC_10x10_SRGB_BLOCK      : constant Int_Unsigned_C := 180; -- VkFormat
  VK_FORMAT_ASTC_12x10_UNORM_BLOCK     : constant Int_Unsigned_C := 181; -- VkFormat
  VK_FORMAT_ASTC_12x10_SRGB_BLOCK      : constant Int_Unsigned_C := 182; -- VkFormat
  VK_FORMAT_ASTC_12x12_UNORM_BLOCK     : constant Int_Unsigned_C := 183; -- VkFormat
  VK_FORMAT_ASTC_12x12_SRGB_BLOCK      : constant Int_Unsigned_C := 184; -- VkFormat
  
  ------------
  -- Macros --
  ------------
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VK_MAKE_VERSION.html
  function VK_MAKE_VERSION (Major, Minor, Patch : Int_Unsigned) return Int_Unsigned_C is
    (Int_Unsigned_C (Shift_Left (Major, 22) or Shift_Left (Minor,12) or Patch));

  ----------------
  -- Allocation --
  ----------------

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/PFN_vkInternalFreeNotification.html
  type Ptr_vkInternalFreeNotification is access procedure (pUserData       : Ptr;            -- void*
                                                           size            : Int_Size_C;     -- size_t
                                                           allocationType  : Int_Unsigned_C; -- VkInternalAllocationType
                                                           allocationScope : Int_Unsigned_C) -- VkSystemAllocationScope
                                                           with Convention => C;
  function To_Ptr_vkInternalFreeNotification is new Unchecked_Conversion (Ptr, Ptr_vkInternalFreeNotification);
  vkInternalFreeNotification : Ptr_vkInternalFreeNotification := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/PFN_vkInternalAllocationNotificationFunction.html
  type Ptr_vkInternalAllocationNotification is access procedure (pUserData       : Ptr;            -- void*
                                                                 size            : Int_Size_C;     -- size_t
                                                                 allocationType  : Int_Unsigned_C; -- VkInternalAllocationType
                                                                 allocationScope : Int_Unsigned_C) -- VkSystemAllocationScope
                                                                 with Convention => C;
  function To_Ptr_vkInternalAllocationNotification is new Unchecked_Conversion (Ptr, Ptr_vkInternalAllocationNotification);
  vkInternalAllocationNotification : Ptr_vkInternalAllocationNotification := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/PFN_vkFreeFunction.html
  type Ptr_vkFreeFunction is access procedure (pUserData : Ptr; -- void*
                                               pMemory   : Ptr) -- void*
                                               with Convention => C;
  function To_Ptr_vkFreeFunction is new Unchecked_Conversion (Ptr, Ptr_vkFreeFunction);
  vkFreeFunction : Ptr_vkFreeFunction := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/PFN_vkReallocationFunction.html
  type Ptr_vkReallocationFunction is access procedure (pUserData       : Ptr;            -- void*
                                                       pOriginal       : Ptr;            -- void*
                                                       size            : Int_Size_C;     -- size_t
                                                       alignment       : Int_Size_C;     -- size_t
                                                       allocationScope : Int_Unsigned_C) -- VkSystemAllocationScope
                                                       with Convention => C;
  function To_Ptr_vkReallocationFunction is new Unchecked_Conversion (Ptr, Ptr_vkReallocationFunction);
  vkReallocationFunction : Ptr_vkReallocationFunction := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/PFN_vkAllocationFunction.html
  type Ptr_vkAllocationFunction is access procedure (pUserData       : Ptr;            -- void*
                                                     size            : Int_Size_C;     -- size_t
                                                     alignment       : Int_Size_C;     -- size_t
                                                     allocationScope : Int_Unsigned_C) -- VkSystemAllocationScope
                                                     with Convention => C;
  function To_Ptr_vkAllocationFunction is new Unchecked_Conversion (Ptr, Ptr_vkAllocationFunction);
  vkAllocationFunction : Ptr_vkAllocationFunction := null;

  ----------------
  -- Structures --
  ----------------

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExtent2D.html
  type VkExtent2D;
  type Ptr_VkExtent2D is access all VkExtent2D;
  type VkExtent2D is record
      width  : Int_Unsigned_C := 0; -- uint32_t
      height : Int_Unsigned_C := 0; -- uint32_t
    end record with Convention => C;
  type Array_VkExtent2D is array (Positive range <>) of aliased VkExtent2D;
  type Ptr_Array_VkExtent2D is access all Array_VkExtent2D;
 
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExtent3D.
  type VkExtent3D;
  type Ptr_VkExtent3D is access all VkExtent3D;
  type VkExtent3D is record
      width  : Int_Unsigned_C := 0; -- uint32_t
      height : Int_Unsigned_C := 0; -- uint32_t
      depth  : Int_Unsigned_C := 0; -- uint32_t
    end record with Convention => C;
  type Array_VkExtent3D is array (Positive range <>) of aliased VkExtent3D;
  type Ptr_Array_VkExtent3D is access all Array_VkExtent3D;
    
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBufferMemoryBarrier.html
  type VkBufferMemoryBarrier;
  type Ptr_VkBufferMemoryBarrier is access all VkBufferMemoryBarrier;
  type VkBufferMemoryBarrier is record
      sType               : Int_Unsigned_C := VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER; -- VkStructureType
      pNext               : Ptr_VkBufferMemoryBarrier := null;     -- const void*
      srcAccessMask       : Int_Unsigned_C            := 0;        -- VkAccessFlags
      dstAccessMask       : Int_Unsigned_C            := 0;        -- VkAccessFlags
      srcQueueFamilyIndex : Int_Unsigned_C            := 0;        -- uint32_t
      dstQueueFamilyIndex : Int_Unsigned_C            := 0;        -- uint32_t
      buffer              : Ptr                       := NULL_PTR; -- VkBuffer
      offset              : Int_64_Unsigned_C         := 0;        -- VkDeviceSize
      size                : Int_64_Unsigned_C         := 0;        -- VkDeviceSize
    end record with Convention => C;
  type Array_VkBufferMemoryBarrier is array (Positive range <>) of aliased VkBufferMemoryBarrier;
  type Ptr_Array_VkBufferMemoryBarrier is access all Array_VkBufferMemoryBarrier;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryBarrier.html
  type VkMemoryBarrier;
  type Ptr_VkMemoryBarrier is access all VkMemoryBarrier;
  type VkMemoryBarrier is record
      sType         : Int_Unsigned_C := VK_STRUCTURE_TYPE_MEMORY_BARRIER; -- VkStructureType
      pNext         : Ptr_VkMemoryBarrier := null; -- const void*
      srcAccessMask : Int_Unsigned_C      := 0;    -- VkAccessFlags
      dstAccessMask : Int_Unsigned_C      := 0;    -- VkAccessFlags
    end record with Convention => C;
  type Array_VkMemoryBarrier is array (Positive range <>) of aliased VkMemoryBarrier;
  type Ptr_Array_VkMemoryBarrier is access all Array_VkMemoryBarrier;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageSubresourceLayers.html
  type VkImageSubresourceLayers;
  type Ptr_VkImageSubresourceLayers is access all VkImageSubresourceLayers;
  type VkImageSubresourceLayers is record
      aspectMask     : Int_Unsigned_C := 0; -- VkImageAspectFlags
      mipLevel       : Int_Unsigned_C := 0; -- uint32_t
      baseArrayLayer : Int_Unsigned_C := 0; -- uint32_t
      layerCount     : Int_Unsigned_C := 0; -- uint32_t
    end record with Convention => C;
  type Array_VkImageSubresourceLayers is array (Positive range <>) of aliased VkImageSubresourceLayers;
  type Ptr_Array_VkImageSubresourceLayers is access all Array_VkImageSubresourceLayers;
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkOffset3D.html
  type VkOffset3D;
  type Ptr_VkOffset3D is access all VkOffset3D;
  type VkOffset3D is record
      x : Int_C := 0; -- int32_t
      y : Int_C := 0; -- int32_t
      z : Int_C := 0; -- int32_t
    end record with Convention => C;
  type Array_VkOffset3D is array (Positive range <>) of aliased VkOffset3D;
  type Ptr_Array_VkOffset3D is access all Array_VkOffset3D;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageCopy.html
  type VkImageCopy;
  type Ptr_VkImageCopy is access all VkImageCopy;
  type VkImageCopy is record
      srcSubresource : VkImageSubresourceLayers := (others => <>); -- VkImageSubresourceLayers
      srcOffset      : VkOffset3D               := (others => <>); -- VkOffset3D
      dstSubresource : VkImageSubresourceLayers := (others => <>); -- VkImageSubresourceLayers
      dstOffset      : VkOffset3D               := (others => <>); -- VkOffset3D
      extent         : VkExtent3D               := (others => <>); -- VkExtent3D
    end record with Convention => C;
  type Array_VkImageCopy is array (Positive range <>) of aliased VkImageCopy;
  type Ptr_Array_VkImageCopy is access all Array_VkImageCopy;
 
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBufferCreateInfo.html
  type VkBufferCreateInfo;
  type Ptr_VkBufferCreateInfo is access all VkBufferCreateInfo;
  type VkBufferCreateInfo is record
      sType                 : Int_Unsigned_C := VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO; -- VkStructureType
      pNext                 : Ptr_VkBufferCreateInfo := null; -- const void*
      flags                 : Int_Unsigned_C         := 0;    -- VkBufferCreateFlags
      size                  : Int_64_Unsigned_C      := 0;    -- VkDeviceSize
      usage                 : Int_Unsigned_C         := 0;    -- VkBufferUsageFlags
      sharingMode           : Int_Unsigned_C         := 0;    -- VkSharingMode
      queueFamilyIndexCount : Int_Unsigned_C         := 0;    -- uint32_t
      pQueueFamilyIndices   : Ptr_Int_Unsigned_C     := null; -- const uint32_t*
    end record with Convention => C;
  type Array_VkBufferCreateInfo is array (Positive range <>) of aliased VkBufferCreateInfo;
  type Ptr_Array_VkBufferCreateInfo is access all Array_VkBufferCreateInfo;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBufferCopy.html
  type VkBufferCopy;
  type Ptr_VkBufferCopy is access all VkBufferCopy;
  type VkBufferCopy is record
     srcOffset : Int_64_Unsigned_C := 0; -- VkDeviceSize
     dstOffset : Int_64_Unsigned_C := 0; -- VkDeviceSize
     size      : Int_64_Unsigned_C := 0; -- VkDeviceSize
    end record with Convention => C;
  type Array_VkBufferCopy is array (Positive range <>) of aliased VkBufferCopy;
  type Ptr_Array_VkBufferCopy is access all Array_VkBufferCopy;
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkShaderModuleCreateInfo.html
  type VkShaderModuleCreateInfo;
  type Ptr_VkShaderModuleCreateInfo is access all VkShaderModuleCreateInfo;
  type VkShaderModuleCreateInfo is record
      sType    : Int_Unsigned_C := VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO; -- VkStructureType
      pNext    : Ptr_VkShaderModuleCreateInfo := null; -- const void*
      flags    : Int_Unsigned_C               := 0;    -- VkShaderModuleCreateFlags
      codeSize : Int_Size_C                   := 0;    -- size_t
      pCode    : Ptr := NULL_PTR; -- Ptr_Int_Unsigned_C           := null; -- const uint32_t*
    end record with Convention => C;
  type Array_VkShaderModuleCreateInfo is array (Positive range <>) of aliased VkShaderModuleCreateInfo;
  type Ptr_Array_VkShaderModuleCreateInfo is access all Array_VkShaderModuleCreateInfo;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBufferImageCopy.html
  type VkBufferImageCopy;
  type Ptr_VkBufferImageCopy is access all VkBufferImageCopy;
  type VkBufferImageCopy is record
      bufferOffset      : Int_64_Unsigned_C        := 0;              -- VkDeviceSize                
      bufferRowLength   : Int_Unsigned_C           := 0;              -- uint32_t                    
      bufferImageHeight : Int_Unsigned_C           := 0;              -- uint32_t                    
      imageSubresource  : VkImageSubresourceLayers := (others => <>); -- VkImageSubresourceLayers    
      imageOffset       : VkOffset3D               := (others => <>); -- VkOffset3D                  
      imageExtent       : VkExtent3D               := (others => <>); -- VkExtent3D
    end record with Convention => C;
  type Array_VkBufferImageCopy is array (Positive range <>) of aliased VkBufferImageCopy;
  type Ptr_Array_VkBufferImageCopy is access all Array_VkBufferImageCopy;
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageCreateInfo.html
  type VkImageCreateInfo;
  type Ptr_VkImageCreateInfo is access all VkImageCreateInfo;
  type VkImageCreateInfo is record
      sType                 : Int_Unsigned_C  := VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO; -- VkStructureType
      pNext                 : Ptr_VkImageCreateInfo := null;           -- const void*
      flags                 : Int_Unsigned_C        := 0;              -- VkImageCreateFlags
      imageType             : Int_Unsigned_C        := 0;              -- VkImageType
      format                : Int_Unsigned_C        := 0;              -- VkFormat
      extent                : VkExtent3D            := (others => <>); -- VkExtent3D
      mipLevels             : Int_Unsigned_C        := 0;              -- uint32_t
      arrayLayers           : Int_Unsigned_C        := 0;              -- uint32_t
      samples               : Int_Unsigned_C        := 0;              -- VkSampleCountFlagBits
      tiling                : Int_Unsigned_C        := 0;              -- VkImageTiling
      usage                 : Int_Unsigned_C        := 0;              -- VkImageUsageFlags
      sharingMode           : Int_Unsigned_C        := 0;              -- VkSharingMode
      queueFamilyIndexCount : Int_Unsigned_C        := 0;              -- uint32_t
      pQueueFamilyIndices   : Ptr_Int_Unsigned_C    := null;           -- const uint32_t*
      initialLayout         : Int_Unsigned_C        := 0;              -- VkImageLayout
    end record with Convention => C;
  type Array_VkImageCreateInfo is array (Positive range <>) of aliased VkImageCreateInfo;
  type Ptr_Array_VkImageCreateInfo is access all Array_VkImageCreateInfo;
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/PFN_vkAllocationFunction.html
  type VkAllocationCallbacks;
  type Ptr_VkAllocationCallbacks is access all VkAllocationCallbacks;
  type VkAllocationCallbacks is record
      pUserData             : Ptr                                  := NULL_PTR; -- void*
      pfnAllocation         : Ptr_vkAllocationFunction             := null;     -- PFN_vkAllocationFunction
      pfnReallocation       : Ptr_vkReallocationFunction           := null;     -- PFN_vkReallocationFunction
      pfnFree               : Ptr_vkFreeFunction                   := null;     -- PFN_vkFreeFunction
      pfnInternalAllocation : Ptr_vkInternalAllocationNotification := null;     -- PFN_vkInternalAllocationNotification
      pfnInternalFree       : Ptr_vkInternalFreeNotification       := null;     -- PFN_vkInternalFreeNotification
    end record with Convention => C;
  type Array_VkAllocationCallbacks is array (Positive range <>) of aliased VkAllocationCallbacks;
  type Ptr_Array_VkAllocationCallbacks is access all Array_VkAllocationCallbacks;

  -- http://nopper.tv/Vulkan/1.0/VkWin32SurfaceCreateInfoKHR.html
  type VkWin32SurfaceCreateInfoKHR;
  type Ptr_VkWin32SurfaceCreateInfoKHR is access all VkWin32SurfaceCreateInfoKHR;
  type VkWin32SurfaceCreateInfoKHR is record
      sType     : Int_Unsigned_C := VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR; -- VkStructureType
      pNext     : Ptr_VkWin32SurfaceCreateInfoKHR := null;     -- const void*
      flags     : Int_Unsigned_C                  := 0;        -- VkWin32SurfaceCreateFlagsKHR
      hinstance : Ptr                             := NULL_PTR; -- HINSTANCE
      hwnd      : Ptr                             := NULL_PTR; -- HWND
    end record with Convention => C;
  type Array_VkWin32SurfaceCreateInfoKHR is array (Positive range <>) of aliased VkWin32SurfaceCreateInfoKHR;
  type Ptr_Array_VkWin32SurfaceCreateInfoKHR is access all Array_VkWin32SurfaceCreateInfoKHR;

  -- http://nopper.tv/Vulkan/1.0/VkSwapchainCreateInfoKHR.html
  type VkSwapchainCreateInfoKHR;
  type Ptr_VkSwapchainCreateInfoKHR is access all VkSwapchainCreateInfoKHR;
  type VkSwapchainCreateInfoKHR is record
      sType                 : Int_Unsigned_C := VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR; -- VkStructureType
      pNext                 : Ptr_VkSwapchainCreateInfoKHR := null; -- const void*
      flags                 : Int_Unsigned_C     := 0;              -- VkSwapchainCreateFlagsKHR
      surface               : Ptr                := NULL_PTR;       -- VkSurfaceKHR
      minImageCount         : aliased Int_Unsigned_C     := 0;              -- uint32_t
      imageFormat           : Int_Unsigned_C     := 0;              -- VkFormat
      imageColorSpace       : Int_Unsigned_C     := 0;              -- VkColorSpaceKHR
      imageExtent           : VkExtent2D         := (others => <>); -- VkExtent2D
      imageArrayLayers      : Int_Unsigned_C     := 0;              -- uint32_t
      imageUsage            : Int_Unsigned_C     := 0;              -- VkImageUsageFlags
      imageSharingMode      : Int_Unsigned_C     := 0;              -- VkSharingMode
      queueFamilyIndexCount : Int_Unsigned_C     := 0;              -- uint32_t
      pQueueFamilyIndices   : Ptr_Int_Unsigned_C := null;           -- const uint32_t*
      preTransform          : Int_Unsigned_C     := 0;              -- VkSurfaceTransformFlagBitsKHR
      compositeAlpha        : Int_Unsigned_C     := 0;              -- VkCompositeAlphaFlagBitsKHR
      presentMode           : Int_Unsigned_C     := 0;              -- VkPresentModeKHR
      clipped               : Int_Unsigned_C     := 0;              -- VkBool32
      oldSwapchain          : Ptr                := NULL_PTR;       -- VkSwapchainKHR
    end record with Convention => C;
  type Array_VkSwapchainCreateInfoKHR is array (Positive range <>) of aliased VkSwapchainCreateInfoKHR;
  type Ptr_Array_VkSwapchainCreateInfoKHR is access all Array_VkSwapchainCreateInfoKHR;
        
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceFeatures.html
  type VkPhysicalDeviceFeatures;
  type Ptr_VkPhysicalDeviceFeatures is access all VkPhysicalDeviceFeatures;
  type VkPhysicalDeviceFeatures is record
      robustBufferAccess                      : Int_Unsigned_C := 0; -- VkBool32
      fullDrawIndexUint32                     : Int_Unsigned_C := 0; -- VkBool32
      imageCubeArray                          : Int_Unsigned_C := 0; -- VkBool32
      independentBlend                        : Int_Unsigned_C := 0; -- VkBool32
      geometryShader                          : Int_Unsigned_C := 0; -- VkBool32
      tessellationShader                      : Int_Unsigned_C := 0; -- VkBool32
      sampleRateShading                       : Int_Unsigned_C := 0; -- VkBool32
      dualSrcBlend                            : Int_Unsigned_C := 0; -- VkBool32
      logicOp                                 : Int_Unsigned_C := 0; -- VkBool32
      multiDrawIndirect                       : Int_Unsigned_C := 0; -- VkBool32
      drawIndirectFirstInstance               : Int_Unsigned_C := 0; -- VkBool32
      depthClamp                              : Int_Unsigned_C := 0; -- VkBool32
      depthBiasClamp                          : Int_Unsigned_C := 0; -- VkBool32
      fillModeNonSolid                        : Int_Unsigned_C := 0; -- VkBool32
      depthBounds                             : Int_Unsigned_C := 0; -- VkBool32
      wideLines                               : Int_Unsigned_C := 0; -- VkBool32
      largePoints                             : Int_Unsigned_C := 0; -- VkBool32
      alphaToOne                              : Int_Unsigned_C := 0; -- VkBool32
      multiViewport                           : Int_Unsigned_C := 0; -- VkBool32
      samplerAnisotropy                       : Int_Unsigned_C := 0; -- VkBool32
      textureCompressionETC2                  : Int_Unsigned_C := 0; -- VkBool32
      textureCompressionASTC_LDR              : Int_Unsigned_C := 0; -- VkBool32
      textureCompressionBC                    : Int_Unsigned_C := 0; -- VkBool32
      occlusionQueryPrecise                   : Int_Unsigned_C := 0; -- VkBool32
      pipelineStatisticsQuery                 : Int_Unsigned_C := 0; -- VkBool32
      vertexPipelineStoresAndAtomics          : Int_Unsigned_C := 0; -- VkBool32
      fragmentStoresAndAtomics                : Int_Unsigned_C := 0; -- VkBool32
      shaderTessellationAndGeometryPointSize  : Int_Unsigned_C := 0; -- VkBool32
      shaderImageGatherExtended               : Int_Unsigned_C := 0; -- VkBool32
      shaderStorageImageExtendedFormats       : Int_Unsigned_C := 0; -- VkBool32
      shaderStorageImageMultisample           : Int_Unsigned_C := 0; -- VkBool32
      shaderStorageImageReadWithoutFormat     : Int_Unsigned_C := 0; -- VkBool32
      shaderStorageImageWriteWithoutFormat    : Int_Unsigned_C := 0; -- VkBool32
      shaderUniformBufferArrayDynamicIndexing : Int_Unsigned_C := 0; -- VkBool32
      shaderSampledImageArrayDynamicIndexing  : Int_Unsigned_C := 0; -- VkBool32
      shaderStorageBufferArrayDynamicIndexing : Int_Unsigned_C := 0; -- VkBool32
      shaderStorageImageArrayDynamicIndexing  : Int_Unsigned_C := 0; -- VkBool32
      shaderClipDistance                      : Int_Unsigned_C := 0; -- VkBool32
      shaderCullDistance                      : Int_Unsigned_C := 0; -- VkBool32
      shaderFloat64                           : Int_Unsigned_C := 0; -- VkBool32
      shaderInt64                             : Int_Unsigned_C := 0; -- VkBool32
      shaderInt16                             : Int_Unsigned_C := 0; -- VkBool32
      shaderResourceResidency                 : Int_Unsigned_C := 0; -- VkBool32
      shaderResourceMinLod                    : Int_Unsigned_C := 0; -- VkBool32
      sparseBinding                           : Int_Unsigned_C := 0; -- VkBool32
      sparseResidencyBuffer                   : Int_Unsigned_C := 0; -- VkBool32
      sparseResidencyImage2D                  : Int_Unsigned_C := 0; -- VkBool32
      sparseResidencyImage3D                  : Int_Unsigned_C := 0; -- VkBool32
      sparseResidency2Samples                 : Int_Unsigned_C := 0; -- VkBool32
      sparseResidency4Samples                 : Int_Unsigned_C := 0; -- VkBool32
      sparseResidency8Samples                 : Int_Unsigned_C := 0; -- VkBool32
      sparseResidency16Samples                : Int_Unsigned_C := 0; -- VkBool32
      sparseResidency                         : Int_Unsigned_C := 0; -- VkBool32
      variableMultisampleRate                 : Int_Unsigned_C := 0; -- VkBool32
      inheritedQueries                        : Int_Unsigned_C := 0; -- VkBool32
    end record with Convention => C;
  type Array_VkPhysicalDeviceFeatures is array (Positive range <>) of aliased VkPhysicalDeviceFeatures;
  type Ptr_Array_VkPhysicalDeviceFeatures is access all Array_VkPhysicalDeviceFeatures;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceLimits.html
  type VkPhysicalDeviceLimits;
  type Ptr_VkPhysicalDeviceLimits is access all VkPhysicalDeviceLimits;
  type VkPhysicalDeviceLimits is record
      maxImageDimension1D                             : Int_Unsigned_C              := 0;          -- uint32_t        
      maxImageDimension2D                             : Int_Unsigned_C              := 0;          -- uint32_t        
      maxImageDimension3D                             : Int_Unsigned_C              := 0;          -- uint32_t        
      maxImageDimensionCube                           : Int_Unsigned_C              := 0;          -- uint32_t        
      maxImageArrayLayers                             : Int_Unsigned_C              := 0;          -- uint32_t        
      maxTexelBufferElements                          : Int_Unsigned_C              := 0;          -- uint32_t        
      maxUniformBufferRange                           : Int_Unsigned_C              := 0;          -- uint32_t        
      maxStorageBufferRange                           : Int_Unsigned_C              := 0;          -- uint32_t        
      maxPushConstantsSize                            : Int_Unsigned_C              := 0;          -- uint32_t        
      maxMemoryAllocationCount                        : Int_Unsigned_C              := 0;          -- uint32_t        
      maxSamplerAllocationCount                       : Int_Unsigned_C              := 0;          -- uint32_t       
      bufferImageGranularity                          : Int_64_Unsigned_C           := 0;          -- VkDeviceSize
      sparseAddressSpaceSize                          : Int_64_Unsigned_C           := 0;          -- VkDeviceSize
      maxBoundDescriptorSets                          : Int_Unsigned_C              := 0;          -- uint32_t        
      maxPerStageDescriptorSamplers                   : Int_Unsigned_C              := 0;          -- uint32_t        
      maxPerStageDescriptorUniformBuffers             : Int_Unsigned_C              := 0;          -- uint32_t        
      maxPerStageDescriptorStorageBuffers             : Int_Unsigned_C              := 0;          -- uint32_t        
      maxPerStageDescriptorSampledImages              : Int_Unsigned_C              := 0;          -- uint32_t        
      maxPerStageDescriptorStorageImages              : Int_Unsigned_C              := 0;          -- uint32_t        
      maxPerStageDescriptorInputAttachments           : Int_Unsigned_C              := 0;          -- uint32_t        
      maxPerStageResources                            : Int_Unsigned_C              := 0;          -- uint32_t        
      maxDescriptorSetSamplers                        : Int_Unsigned_C              := 0;          -- uint32_t        
      maxDescriptorSetUniformBuffers                  : Int_Unsigned_C              := 0;          -- uint32_t        
      maxDescriptorSetUniformBuffersDynamic           : Int_Unsigned_C              := 0;          -- uint32_t        
      maxDescriptorSetStorageBuffers                  : Int_Unsigned_C              := 0;          -- uint32_t        
      maxDescriptorSetStorageBuffersDynamic           : Int_Unsigned_C              := 0;          -- uint32_t        
      maxDescriptorSetSampledImages                   : Int_Unsigned_C              := 0;          -- uint32_t        
      maxDescriptorSetStorageImages                   : Int_Unsigned_C              := 0;          -- uint32_t        
      maxDescriptorSetInputAttachments                : Int_Unsigned_C              := 0;          -- uint32_t        
      maxVertexInputAttributes                        : Int_Unsigned_C              := 0;          -- uint32_t        
      maxVertexInputBindings                          : Int_Unsigned_C              := 0;          -- uint32_t        
      maxVertexInputAttributeOffset                   : Int_Unsigned_C              := 0;          -- uint32_t        
      maxVertexInputBindingStride                     : Int_Unsigned_C              := 0;          -- uint32_t        
      maxVertexOutputComponents                       : Int_Unsigned_C              := 0;          -- uint32_t        
      maxTessellationGenerationLevel                  : Int_Unsigned_C              := 0;          -- uint32_t        
      maxTessellationPatchSize                        : Int_Unsigned_C              := 0;          -- uint32_t        
      maxTessellationControlPerVertexInputComponents  : Int_Unsigned_C              := 0;          -- uint32_t        
      maxTessellationControlPerVertexOutputComponents : Int_Unsigned_C              := 0;          -- uint32_t        
      maxTessellationControlPerPatchOutputComponents  : Int_Unsigned_C              := 0;          -- uint32_t        
      maxTessellationControlTotalOutputComponents     : Int_Unsigned_C              := 0;          -- uint32_t        
      maxTessellationEvaluationInputComponents        : Int_Unsigned_C              := 0;          -- uint32_t        
      maxTessellationEvaluationOutputComponents       : Int_Unsigned_C              := 0;          -- uint32_t        
      maxGeometryShaderInvocations                    : Int_Unsigned_C              := 0;          -- uint32_t        
      maxGeometryInputComponents                      : Int_Unsigned_C              := 0;          -- uint32_t        
      maxGeometryOutputComponents                     : Int_Unsigned_C              := 0;          -- uint32_t        
      maxGeometryOutputVertices                       : Int_Unsigned_C              := 0;          -- uint32_t        
      maxGeometryTotalOutputComponents                : Int_Unsigned_C              := 0;          -- uint32_t        
      maxFragmentInputComponents                      : Int_Unsigned_C              := 0;          -- uint32_t        
      maxFragmentOutputAttachments                    : Int_Unsigned_C              := 0;          -- uint32_t        
      maxFragmentDualSrcAttachments                   : Int_Unsigned_C              := 0;          -- uint32_t        
      maxFragmentCombinedOutputResources              : Int_Unsigned_C              := 0;          -- uint32_t        
      maxComputeSharedMemorySize                      : Int_Unsigned_C              := 0;          -- uint32_t     
      maxComputeWorkGroupCount                        : Array_Int_Unsigned_C (1..3) := (0, 0, 0);  -- uint32_t [3]
      maxComputeWorkGroupInvocations                  : Int_Unsigned_C              := 0;          -- uint32_t     
      maxComputeWorkGroupSize                         : Array_Int_Unsigned_C (1..3) := (0, 0, 0);  -- uint32_t [3]
      subPixelPrecisionBits                           : Int_Unsigned_C              := 0;          -- uint32_t     
      subTexelPrecisionBits                           : Int_Unsigned_C              := 0;          -- uint32_t     
      mipmapPrecisionBits                             : Int_Unsigned_C              := 0;          -- uint32_t     
      maxDrawIndexedIndexValue                        : Int_Unsigned_C              := 0;          -- uint32_t     
      maxDrawIndirectCount                            : Int_Unsigned_C              := 0;          -- uint32_t     
      maxSamplerLodBias                               : Real_C                      := 0.0;        -- float
      maxSamplerAnisotropy                            : Real_C                      := 0.0;        -- float
      maxViewports                                    : Int_Unsigned_C              := 0;          -- uint32_t     
      maxViewportDimensions                           : Array_Int_Unsigned_C (1..2) := (0, 0);     -- uint32_t [2]
      viewportBoundsRange                             : Array_Real_32 (1..2)        := (0.0, 0.0); -- float [2]
      viewportSubPixelBits                            : Int_Unsigned_C              := 0;          -- uint32_t     
      minMemoryMapAlignment                           : Int_Size_C                  := 0;          -- size_t
      minTexelBufferOffsetAlignment                   : Int_64_Unsigned_C           := 0;          -- VkDeviceSize
      minUniformBufferOffsetAlignment                 : Int_64_Unsigned_C           := 0;          -- VkDeviceSize
      minStorageBufferOffsetAlignment                 : Int_64_Unsigned_C           := 0;          -- VkDeviceSize
      minTexelOffset                                  : Int_C                       := 0;          -- int32_t
      maxTexelOffset                                  : Int_Unsigned_C              := 0;          -- uint32_t     
      minTexelGatherOffset                            : Int_C                       := 0;          -- int32_t
      maxTexelGatherOffset                            : Int_Unsigned_C              := 0;          -- uint32_t     
      minInterpolationOffset                          : Real_C                      := 0.0;        -- float
      maxInterpolationOffset                          : Real_C                      := 0.0;        -- float
      subPixelInterpolationOffsetBits                 : Int_Unsigned_C              := 0;          -- uint32_t     
      maxFramebufferWidth                             : Int_Unsigned_C              := 0;          -- uint32_t     
      maxFramebufferHeight                            : Int_Unsigned_C              := 0;          -- uint32_t     
      maxFramebufferLayers                            : Int_Unsigned_C              := 0;          -- uint32_t     
      framebufferColorSampleCounts                    : Int_Unsigned_C              := 0;          -- VkSampleCountFlags
      framebufferDepthSampleCounts                    : Int_Unsigned_C              := 0;          -- VkSampleCountFlags
      framebufferStencilSampleCounts                  : Int_Unsigned_C              := 0;          -- VkSampleCountFlags
      framebufferNoAttachmentsSampleCounts            : Int_Unsigned_C              := 0;          -- VkSampleCountFlags
      maxColorAttachments                             : Int_Unsigned_C              := 0;          -- uint32_t     
      sampledImageColorSampleCounts                   : Int_Unsigned_C              := 0;          -- VkSampleCountFlags
      sampledImageIntegerSampleCounts                 : Int_Unsigned_C              := 0;          -- VkSampleCountFlags
      sampledImageDepthSampleCounts                   : Int_Unsigned_C              := 0;          -- VkSampleCountFlags
      sampledImageStencilSampleCounts                 : Int_Unsigned_C              := 0;          -- VkSampleCountFlags
      storageImageSampleCounts                        : Int_Unsigned_C              := 0;          -- VkSampleCountFlags
      maxSampleMaskWords                              : Int_Unsigned_C              := 0;          -- uint32_t     
      timestampComputeAndGraphics                     : Int_Unsigned_C              := 0;          -- VkBool32
      timestampPeriod                                 : Real_C                      := 0.0;        -- float
      maxClipDistances                                : Int_Unsigned_C              := 0;          -- uint32_t     
      maxCullDistances                                : Int_Unsigned_C              := 0;          -- uint32_t     
      maxCombinedClipAndCullDistances                 : Int_Unsigned_C              := 0;          -- uint32_t     
      discreteQueuePriorities                         : Int_Unsigned_C              := 0;          -- uint32_t     
      pointSizeRange                                  : Array_Real_32 (1..2)        := (0.0, 0.0); -- float [2]
      lineWidthRange                                  : Array_Real_32 (1..2)        := (0.0, 0.0); -- float [2]
      pointSizeGranularity                            : Real_C                      := 0.0;        -- float
      lineWidthGranularity                            : Real_C                      := 0.0;        -- float
      strictLines                                     : Int_Unsigned_C              := 0;          -- VkBool32
      standardSampleLocations                         : Int_Unsigned_C              := 0;          -- VkBool32
      optimalBufferCopyOffsetAlignment                : Int_64_Unsigned_C           := 0;          -- VkDeviceSize
      optimalBufferCopyRowPitchAlignment              : Int_64_Unsigned_C           := 0;          -- VkDeviceSize
      nonCoherentAtomSize                             : Int_64_Unsigned_C           := 0;          -- VkDeviceSize
    end record with Convention => C;
  type Array_VkPhysicalDeviceLimits is array (Positive range <>) of aliased VkPhysicalDeviceLimits;
  type Ptr_Array_VkPhysicalDeviceLimits is access all Array_VkPhysicalDeviceLimits;
      
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceSparseProperties.html
  type VkPhysicalDeviceSparseProperties;
  type Ptr_VkPhysicalDeviceSparseProperties is access all VkPhysicalDeviceSparseProperties;
  type VkPhysicalDeviceSparseProperties is record
      residencyStandard2DBlockShape            : Int_Unsigned_C := 0; -- VkBool32
      residencyStandard2DMultisampleBlockShape : Int_Unsigned_C := 0; -- VkBool32
      residencyStandard3DBlockShape            : Int_Unsigned_C := 0; -- VkBool32
      residencyAlignedMipSize                  : Int_Unsigned_C := 0; -- VkBool32
      residencyNonResidentStrict               : Int_Unsigned_C := 0; -- VkBool32
    end record with Convention => C;
  type Array_VkPhysicalDeviceSparseProperties is array (Positive range <>) of aliased VkPhysicalDeviceSparseProperties;
  type Ptr_Array_VkPhysicalDeviceSparseProperties is access all Array_VkPhysicalDeviceSparseProperties;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceProperties.html
  type VkPhysicalDeviceProperties;
  type Ptr_VkPhysicalDeviceProperties is access all VkPhysicalDeviceProperties;
  type VkPhysicalDeviceProperties is record
      apiVersion        : Int_Unsigned_C                   := 0;                         -- uint32_t
      driverVersion     : Int_Unsigned_C                   := 0;                         -- uint32_t
      vendorID          : Int_Unsigned_C                   := 0;                         -- uint32_t
      deviceID          : Int_Unsigned_C                   := 0;                         -- uint32_t
      deviceType        : Int_Unsigned_C                   := 0;                         -- VkPhysicalDeviceType                
      deviceName        : Str_8_C (1..256)                 := (others => NULL_CHAR_8_C); -- char [VK_MAX_PHYSICAL_DEVICE_NAME_SIZE]
      pipelineCacheUUID : Array_Int_8_Unsigned_C (1..25)   := (others => 0);             -- uint8_t [VK_UUID_SIZE]
      limits            : VkPhysicalDeviceLimits           := (others => <>);            -- VkPhysicalDeviceLimits              
      sparseProperties  : VkPhysicalDeviceSparseProperties := (others => <>);            -- VkPhysicalDeviceSparseProperties
    end record with Convention => C;
  type Array_VkPhysicalDeviceProperties is array (Positive range <>) of aliased VkPhysicalDeviceProperties;
  type Ptr_Array_VkPhysicalDeviceProperties is access all Array_VkPhysicalDeviceProperties;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryType.html
  type VkMemoryType;
  type Ptr_VkMemoryType is access all VkMemoryType;
  type VkMemoryType is record
      propertyFlags : Int_Unsigned_C := 0; -- VkMemoryPropertyFlags    
      heapIndex     : Int_Unsigned_C := 0; -- uint32_t
    end record with Convention => C;
  type Array_VkMemoryType is array (1..32) of VkMemoryType; -- VkMemoryType [VK_MAX_MEMORY_TYPES]
  type Ptr_Array_VkMemoryType is access all Array_VkMemoryType;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryHeap.html
  type VkMemoryHeap;
  type Ptr_VkMemoryHeap is access all VkMemoryHeap;
  type VkMemoryHeap is record
      size  : Int_64_Unsigned_C := 0; -- VkDeviceSize
      flags : Int_Unsigned_C    := 0; -- VkMemoryHeapFlags
    end record with Convention => C;
  type Array_VkMemoryHeap is array (1..16) of VkMemoryHeap; -- VkMemoryHeap [VK_MAX_MEMORY_HEAPS]
  type Ptr_Array_VkMemoryHeap is access all Array_VkMemoryHeap;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceMemoryProperties.html
  type VkPhysicalDeviceMemoryProperties;
  type Ptr_VkPhysicalDeviceMemoryProperties is access all VkPhysicalDeviceMemoryProperties;
  type VkPhysicalDeviceMemoryProperties is record
      memoryTypeCount : Int_Unsigned_C     := 0;                           -- uint32_t
      memoryTypes     : Array_VkMemoryType := (others => (others => <>));  -- VkMemoryType [VK_MAX_MEMORY_TYPES]
      memoryHeapCount : Int_Unsigned_C     := 0;                           -- uint32_t
      memoryHeaps     : Array_VkMemoryHeap := (others => (others => <>));  -- VkMemoryHeap [VK_MAX_MEMORY_HEAPS]
    end record with Convention => C;
  type Array_VkPhysicalDeviceMemoryProperties is array (Positive range <>) of aliased VkPhysicalDeviceMemoryProperties;
  type Ptr_Array_VkPhysicalDeviceMemoryProperties is access all Array_VkPhysicalDeviceMemoryProperties;
        
  -- http://nopper.tv/Vulkan/1.0/VkSurfaceCapabilitiesKHR.html
  type VkSurfaceCapabilitiesKHR;
  type Ptr_VkSurfaceCapabilitiesKHR is access all VkSurfaceCapabilitiesKHR;
  type VkSurfaceCapabilitiesKHR is record
      minImageCount           : Int_Unsigned_C := 0;              -- uint32_t
      maxImageCount           : Int_Unsigned_C := 0;              -- uint32_t
      currentExtent           : VkExtent2D     := (others => <>); -- VkExtent2D
      minImageExtent          : VkExtent2D     := (others => <>); -- VkExtent2D
      maxImageExtent          : VkExtent2D     := (others => <>); -- VkExtent2D
      maxImageArrayLayers     : Int_Unsigned_C := 0;              -- uint32_t
      supportedTransforms     : Int_Unsigned_C := 0;              -- VkSurfaceTransformFlagsKHR
      currentTransform        : Int_Unsigned_C := 0;              -- VkSurfaceTransformFlagBitsKHR
      supportedCompositeAlpha : Int_Unsigned_C := 0;              -- VkCompositeAlphaFlagsKHR
      supportedUsageFlags     : Int_Unsigned_C := 0;              -- VkImageUsageFlags
    end record with Convention => C;
  type Array_VkSurfaceCapabilitiesKHR is array (Positive range <>) of aliased VkSurfaceCapabilitiesKHR;
  type Ptr_Array_VkSurfaceCapabilitiesKHR is access all Array_VkSurfaceCapabilitiesKHR;
        
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandBufferBeginInfo.html
  type VkCommandBufferBeginInfo;
  type Ptr_VkCommandBufferBeginInfo is access all VkCommandBufferBeginInfo;
  type VkCommandBufferBeginInfo is record
      sType            : Int_Unsigned_C := VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO; -- VkStructureType
      pNext            : Ptr_VkCommandBufferBeginInfo := null;     -- const void*
      flags            : Int_Unsigned_C               := 0;        -- VkCommandBufferUsageFlags
      pInheritanceInfo : Ptr                          := NULL_PTR; -- const VkCommandBufferInheritanceInfo*
    end record with Convention => C;
  type Array_VkCommandBufferBeginInfo is array (Positive range <>) of aliased VkCommandBufferBeginInfo;
  type Ptr_Array_VkCommandBufferBeginInfo is access all Array_VkCommandBufferBeginInfo;
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageSubresourceRange.html
  type VkImageSubresourceRange;
  type Ptr_VkImageSubresourceRange is access all VkImageSubresourceRange;
  type VkImageSubresourceRange is record
      aspectMask     : Int_Unsigned_C := 0; -- VkImageAspectFlags    
      baseMipLevel   : Int_Unsigned_C := 0; -- uint32_t
      levelCount     : Int_Unsigned_C := 0; -- uint32_t
      baseArrayLayer : Int_Unsigned_C := 0; -- uint32_t
      layerCount     : Int_Unsigned_C := 0; -- uint32_t              
    end record with Convention => C;
  type Array_VkImageSubresourceRange is array (Positive range <>) of aliased VkImageSubresourceRange;
  type Ptr_Array_VkImageSubresourceRange is access all Array_VkImageSubresourceRange;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageMemoryBarrier.html
  type VkImageMemoryBarrier;
  type Ptr_VkImageMemoryBarrier is access all VkImageMemoryBarrier;
  type VkImageMemoryBarrier is record
      sType               : Int_Unsigned_C := VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER; -- VkStructureType
      pNext               : Ptr_VkImageMemoryBarrier := null;           -- const void*
      srcAccessMask       : Int_Unsigned_C           := 0;              -- VkAccessFlags
      dstAccessMask       : Int_Unsigned_C           := 0;              -- VkAccessFlags
      oldLayout           : Int_Unsigned_C           := 0;              -- VkImageLayout
      newLayout           : Int_Unsigned_C           := 0;              -- VkImageLayout
      srcQueueFamilyIndex : Int_Unsigned_C           := 0;              -- uint32_t
      dstQueueFamilyIndex : Int_Unsigned_C           := 0;              -- uint32_t
      image               : Ptr                      := NULL_PTR;       -- VkImage
      subresourceRange    : VkImageSubresourceRange  := (others => <>); -- VkImageSubresourceRange
    end record with Convention => C;
  type Array_VkImageMemoryBarrier is array (Positive range <>) of aliased VkImageMemoryBarrier;
  type Ptr_Array_VkImageMemoryBarrier is access all Array_VkImageMemoryBarrier;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExtensionProperties.html
  type VkExtensionProperties;
  type Ptr_VkExtensionProperties is access all VkExtensionProperties;
  type VkExtensionProperties is record
      extensionName : Str_8_C (1..VK_MAX_EXTENSION_NAME_SIZE) := (others => NULL_CHAR_8_C); -- char [VK_MAX_EXTENSION_NAME_SIZE];
      specVersion   : Int_Unsigned_C                          := 0;                         -- uint32_t
    end record with Convention => C;
  type Array_VkExtensionProperties is array (Positive range <>) of aliased VkExtensionProperties;
  type Ptr_Array_VkExtensionProperties is access all Array_VkExtensionProperties;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandBufferAllocateInfo.html
  type VkCommandBufferAllocateInfo;
  type Ptr_VkCommandBufferAllocateInfo is access all VkCommandBufferAllocateInfo;
  type VkCommandBufferAllocateInfo is record
      sType              : Int_Unsigned_C := VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO; -- VkStructureType
      pNext              : Ptr            := NULL_PTR; -- const void*
      commandPool        : Ptr            := NULL_PTR; -- VkCommandPool
      level              : Int_Unsigned_C := 0;        -- VkCommandBufferLevel
      commandBufferCount : Int_Unsigned_C := 0;        -- uint32_t
    end record with Convention => C;
  type Array_VkCommandBufferAllocateInfo is array (Positive range <>) of aliased VkCommandBufferAllocateInfo;
  type Ptr_Array_VkCommandBufferAllocateInfo is access all Array_VkCommandBufferAllocateInfo;
        
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSubmitInfo.html
  type VkSubmitInfo;
  type Ptr_VkSubmitInfo is access all VkSubmitInfo;
  type VkSubmitInfo is record
      sType                : Int_Unsigned_C := VK_STRUCTURE_TYPE_SUBMIT_INFO; -- VkStructureType
      pNext                : Ptr_VkSubmitInfo   := null; -- const void*
      waitSemaphoreCount   : Int_Unsigned_C     := 0;    -- uint32_t
      pWaitSemaphores      : Ptr_Ptr            := null; -- const VkSemaphore*
      pWaitDstStageMask    : Ptr_Int_Unsigned_C := null; -- VkPipelineStageFlags
      commandBufferCount   : Int_Unsigned_C     := 0;    -- uint32_t
      pCommandBuffers      : Ptr_Ptr            := null; -- const VkCommandBuffer* 
      signalSemaphoreCount : Int_Unsigned_C     := 0;    -- uint32_t
      pSignalSemaphores    : Ptr_Ptr            := null; -- const VkSemaphore*
    end record with Convention => C;
  type Array_VkSubmitInfo is array (Positive range <>) of aliased VkSubmitInfo;
  type Ptr_Array_VkSubmitInfo is access all Array_VkSubmitInfo;
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageSubresource.html
  type VkImageSubresource;
  type Ptr_VkImageSubresource is access all VkImageSubresource;
  type VkImageSubresource is record
      aspectMask : Int_Unsigned_C := 0; -- VkImageAspectFlags
      mipLevel   : Int_Unsigned_C := 0; -- uint32_t
      arrayLayer : Int_Unsigned_C := 0; -- uint32_t
    end record;
  type Array_VkImageSubresource is array (Positive range <>) of aliased VkImageSubresource;
  type Ptr_Array_VkImageSubresource is access all Array_VkImageSubresource;
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSubresourceLayout.html
  type VkSubresourceLayout;
  type Ptr_VkSubresourceLayout is access all VkSubresourceLayout;
  type VkSubresourceLayout is record
      offset     : Int_64_Unsigned_C := 0; -- VkDeviceSize
      size       : Int_64_Unsigned_C := 0; -- VkDeviceSize
      rowPitch   : Int_64_Unsigned_C := 0; -- VkDeviceSize
      arrayPitch : Int_64_Unsigned_C := 0; -- VkDeviceSize
      depthPitch : Int_64_Unsigned_C := 0; -- VkDeviceSize
    end record;
  type Array_VkSubresourceLayout is array (Positive range <>) of aliased VkSubresourceLayout;
  type Ptr_Array_VkSubresourceLayout is access all Array_VkSubresourceLayout;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkApplicationInfo.html
  type VkApplicationInfo;
  type Ptr_VkApplicationInfo is access all VkApplicationInfo;
  type VkApplicationInfo is record
      sType              : Int_Unsigned_C := VK_STRUCTURE_TYPE_APPLICATION_INFO; -- VkStructureType
      pNext              : Ptr_VkApplicationInfo := null; -- const void*
      pApplicationName   : Ptr_Char_8_C           := null; -- const char*
      applicationVersion : Int_Unsigned_C        := 0;    -- uint32_t
      pEngineName        : Ptr_Char_8_C           := null; -- const char*
      engineVersion      : Int_Unsigned_C        := 0;    -- uint32_t
      apiVersion         : Int_Unsigned_C        := 0;    -- uint32_t
    end record with Convention => C;
  type Array_VkApplicationInfo is array (Positive range <>) of aliased VkApplicationInfo;
  type Ptr_Array_VkApplicationInfo is access all Array_VkApplicationInfo;
        
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkInstanceCreateInfo.html
  type Ptr_Ptr_Str_8_C is access all Ptr_Str_8_C with Convention => C;
  type VkInstanceCreateInfo;
  type Ptr_VkInstanceCreateInfo is access all VkInstanceCreateInfo;
  type VkInstanceCreateInfo is record
      sType                   : Int_Unsigned_C := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO; -- VkStructureType
      pNext                   : Ptr_VkInstanceCreateInfo := null;     -- const void* 
      flags                   : Int_Unsigned_C           := 0;        -- VkInstanceCreateFlags
      pApplicationInfo        : Ptr_VkApplicationInfo    := null;     -- const VkApplicationInfo*
      enabledLayerCount       : Int_Unsigned_C           := 0;        -- uint32_t
      ppEnabledLayerNames     : Ptr := NULL_PTR; -- Ptr_Ptr_Char_8_C              := null; -- const char* const* -- HACK !!!
      enabledExtensionCount   : Int_Unsigned_C           := 0;        -- uint32_t
      ppEnabledExtensionNames :  Ptr := NULL_PTR; -- Ptr_Ptr_Char_8_C              := null; -- const char* const* -- HACK !!!
    end record with Convention => C;
  type Array_VkInstanceCreateInfo is array (Positive range <>) of aliased VkInstanceCreateInfo;
  type Ptr_Array_VkInstanceCreateInfo is access all Array_VkInstanceCreateInfo;
    
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryAllocateInfo.html
  type VkMemoryAllocateInfo;
  type Ptr_VkMemoryAllocateInfo is access all VkMemoryAllocateInfo;
  type VkMemoryAllocateInfo  is record
      sType           : Int_Unsigned_C := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO; -- VkStructureType
      pNext           : Ptr_VkMemoryAllocateInfo := null; -- const void*
      allocationSize  : Int_64_Unsigned_C        := 0;    -- VkDeviceSize
      memoryTypeIndex : Int_Unsigned_C           := 0;    -- uint32_t 
    end record with Convention => C;
  type Array_VkMemoryAllocateInfo is array (Positive range <>) of aliased VkMemoryAllocateInfo;
  type Ptr_Array_VkMemoryAllocateInfo is access all Array_VkMemoryAllocateInfo;
        
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDeviceQueueCreateInfo.html
  type VkDeviceQueueCreateInfo;
  type Ptr_VkDeviceQueueCreateInfo is access all VkDeviceQueueCreateInfo with Convention => C;
  type VkDeviceQueueCreateInfo is record
      sType            : Int_Unsigned_C := VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO; -- VkStructureType
      pNext            : Ptr_VkDeviceQueueCreateInfo := null; -- const void*
      flags            : Int_Unsigned_C              := 0;    -- VkDeviceQueueCreateFlags
      queueFamilyIndex : Int_Unsigned_C              := 0;    -- uint32_t
      queueCount       : Int_Unsigned_C              := 0;    -- uint32_t
      pQueuePriorities : Ptr_Real_C                  := null; -- const float* 
    end record with Convention => C;
  type Array_VkDeviceQueueCreateInfo is array (Positive range <>) of aliased VkDeviceQueueCreateInfo with Convention => C;
  type Ptr_Array_VkDeviceQueueCreateInfo is access Array_VkDeviceQueueCreateInfo;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkQueueFamilyProperties.html
  type VkQueueFamilyProperties;
  type Ptr_VkQueueFamilyProperties is access all VkQueueFamilyProperties;
  type VkQueueFamilyProperties is record
      queueFlags                  : Int_Unsigned_C := 0;              -- VkQueueFlags    
      queueCount                  : Int_Unsigned_C := 0;              -- uint32_t        
      timestampValidBits          : Int_Unsigned_C := 0;              -- uint32_t        
      minImageTransferGranularity : VkExtent3D     := (others => <>); -- VkExtent3D      
    end record with Convention => C;
  type Array_VkQueueFamilyProperties is array (Positive range <>) of aliased VkQueueFamilyProperties;
  type Ptr_Array_VkQueueFamilyProperties is access all Array_VkQueueFamilyProperties;
    
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryRequirements.html
  type VkMemoryRequirements;
  type Ptr_VkMemoryRequirements is access all VkMemoryRequirements;
  type VkMemoryRequirements is record
      size           : Int_64_Unsigned_C := 0; -- VkDeviceSize
      alignment      : Int_64_Unsigned_C := 0; -- VkDeviceSize
      memoryTypeBits : Int_Unsigned_C    := 0; -- uint32_t
    end record with Convention => C;
  type Array_VkMemoryRequirements is array (Positive range <>) of aliased VkMemoryRequirements;
  type Ptr_Array_VkMemoryRequirements is access all Array_VkMemoryRequirements;
        
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDeviceCreateInfo.html
  type VkDeviceCreateInfo;
  type Ptr_VkDeviceCreateInfo is access all VkDeviceCreateInfo with Convention => C;
  type VkDeviceCreateInfo is record
      sType                   : Int_Unsigned_C := VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO; -- VkStructureType
      pNext                   : Ptr_VkDeviceCreateInfo       := null; -- const void* 
      flags                   : Int_Unsigned_C               := 0;    -- VkDeviceCreateFlags
      queueCreateInfoCount    : Int_Unsigned_C               := 0;    -- uint32_t
      pQueueCreateInfos       : Ptr_VkDeviceQueueCreateInfo  := null; -- const VkDeviceQueueCreateInfo*
      enabledLayerCount       : Int_Unsigned_C               := 0;    -- uint32_t
      ppEnabledLayerNames     : Ptr := NULL_PTR; -- Ptr_Str_8_C                  := null; -- const char* const*
      enabledExtensionCount   : Int_Unsigned_C               := 0;    -- uint32_t
      ppEnabledExtensionNames : Ptr := NULL_PTR; -- Ptr_Str_8_C                  := null; -- const char* const*
      pEnabledFeatures        : Ptr_VkPhysicalDeviceFeatures := null; -- const VkPhysicalDeviceFeatures*
    end record with Convention => C;
  type Array_VkDeviceCreateInfo is array (Positive range <>) of aliased VkDeviceCreateInfo;
  type Ptr_Array_VkDeviceCreateInfo is access all Array_VkDeviceCreateInfo;
        
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandPoolCreateInfo.html
  type VkCommandPoolCreateInfo;
  type Ptr_VkCommandPoolCreateInfo is access all VkCommandPoolCreateInfo;
  type VkCommandPoolCreateInfo is record
      sType            : Int_Unsigned_C := VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO; -- VkStructureType
      pNext            : Ptr            := NULL_PTR; -- const void*
      flags            : Int_Unsigned_C := 0;        -- VkCommandPoolCreateFlags
      queueFamilyIndex : Int_Unsigned_C := 0;        -- uint32_t
    end record with Convention => C;
  type Array_VkCommandPoolCreateInfo is array (Positive range <>) of aliased VkCommandPoolCreateInfo;
  type Ptr_Array_VkCommandPoolCreateInfo is access all Array_VkCommandPoolCreateInfo;
        
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSemaphoreCreateInfo.html
  type VkSemaphoreCreateInfo;
  type Ptr_VkSemaphoreCreateInfo is access all VkSemaphoreCreateInfo;
  type VkSemaphoreCreateInfo is record
      sType : Int_Unsigned_C := VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO; -- VkStructureType
      pNext : Ptr_VkSemaphoreCreateInfo := null; -- const void* 
      flags : Int_Unsigned_C            := 0;    -- VkSemaphoreCreateFlags
    end record with Convention => C;
  type Array_VkSemaphoreCreateInfo is array (Positive range <>) of aliased VkSemaphoreCreateInfo;
  type Ptr_Array_VkSemaphoreCreateInfo is access all Array_VkSemaphoreCreateInfo;
        
  -- http://nopper.tv/Vulkan/1.0/vkQueuePresentKHR.html
  type VkPresentInfoKHR;
  type Ptr_VkPresentInfoKHR is access all VkPresentInfoKHR;
  type VkPresentInfoKHR is record
      sType              : Int_Unsigned_C := VK_STRUCTURE_TYPE_PRESENT_INFO_KHR; -- VkStructureType
      pNext              : Ptr_VkPresentInfoKHR := null; -- const void*
      waitSemaphoreCount : Int_Unsigned_C       := 0;    -- uint32_t
      pWaitSemaphores    : Ptr_Ptr              := null; -- const VkSemaphore*
      swapchainCount     : Int_Unsigned_C       := 0;    -- uint32_t 
      pSwapchains        : Ptr_Ptr              := null; -- const VkSwapchainKHR* 
      pImageIndices      : Ptr_Int_Unsigned_C   := null; -- uint32_t
      pResults           : Ptr_Int_Unsigned_C   := null; -- VkResult
    end record with Convention => C;
  type Array_VkPresentInfoKHR is array (Positive range <>) of aliased VkPresentInfoKHR;
  type Ptr_Array_VkPresentInfoKHR is access all Array_VkPresentInfoKHR;

  -- https://vulkan.lunarg.com/doc/view/1.0.26.0/windows/vkspec.chunked/ch29s05.html#VkSurfaceFormatKHR
  type VkSurfaceFormatKHR;
  type Ptr_VkSurfaceFormatKHR is access all VkSurfaceFormatKHR;
  type VkSurfaceFormatKHR is record
      format     : Int_Unsigned_C := 0; -- VkFormat
      colorSpace : Int_Unsigned_C := 0; -- VkColorSpaceKHR
    end record with Convention => C;
  type Array_VkSurfaceFormatKHR is array (Positive range <>) of aliased VkSurfaceFormatKHR;
  type Ptr_Array_VkSurfaceFormatKHR is access all Array_VkSurfaceFormatKHR;
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkOffset2D.html
  type VkOffset2D;
  type Ptr_VkOffset2D is access all VkOffset2D;
  type VkOffset2D is record
      x : Int_C := 0; -- int32_t
      y : Int_C := 0; -- int32_t
    end record with Convention => C;
  type Array_VkOffset2D is array (Positive range <>) of aliased VkOffset2D;
  type Ptr_Array_VkOffset2D is access all Array_VkOffset2D;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkRect2D.html
  type VkRect2D;
  type Ptr_VkRect2D is access all VkRect2D;
  type VkRect2D is record
      offset : VkOffset2D := (others => <>); -- VkOffset2D
      extent : VkExtent2D := (others => <>); -- VkExtent2D
    end record with Convention => C;
  type Array_VkRect2D is array (Positive range <>) of aliased VkRect2D;
  type Ptr_Array_VkRect2D is access all Array_VkRect2D;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkClearColorValue.html
  type VkClearColorValue;
  type Ptr_VkClearColorValue is access all VkClearColorValue;
  type VkClearColorValue is record
      -- float32 float [4]
      -- int32 int32_t [4]
      -- uint32 uint32_t [4]
      float32 : Array_Real_C (1..4) := (others => 0.0); -- uint32_t[4]
    end record with Convention => C;
  type Array_VkClearColorValue is array (Positive range <>) of aliased VkClearColorValue;
  type Ptr_Array_VkClearColorValue is access all Array_VkClearColorValue;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkClearDepthStencilValue.html
  type VkClearDepthStencilValue;
  type Ptr_VkClearDepthStencilValue is access all VkClearDepthStencilValue;
  type VkClearDepthStencilValue is record
      depth   : Real_C         := 0.0; -- float
      stencil : Int_Unsigned_C := 0;   -- uint32_t
    end record with Convention => C;
  type Array_VkClearDepthStencilValue is array (Positive range <>) of aliased VkClearDepthStencilValue;
  type Ptr_Array_VkClearDepthStencilValue is access all Array_VkClearDepthStencilValue;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkClearValue.html
  type VkClearValue;
  type Ptr_VkClearValue is access all VkClearValue;
  type VkClearValue is record
      color        : VkClearColorValue        := (others => <>); -- VkClearColorValue
      depthStencil : VkClearDepthStencilValue := (others => <>); -- VkClearDepthStencilValue
    end record with Convention => C;
  type Array_VkClearValue is array (Positive range <>) of aliased VkClearValue;
  type Ptr_Array_VkClearValue is access all Array_VkClearValue;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkRenderPassBeginInfo.html
  type VkRenderPassBeginInfo;
  type Ptr_VkRenderPassBeginInfo is access all VkRenderPassBeginInfo;
  type VkRenderPassBeginInfo is record
      sType           : Int_Unsigned_C := VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO; -- VkStructureType
      pNext           : Ptr_VkRenderPassBeginInfo := null;           -- const void*
      renderPass      : Ptr                       := NULL_PTR;       -- VkRenderPass
      framebuffer     : Ptr                       := NULL_PTR;       -- VkFramebuffer
      renderArea      : VkRect2D                  := (others => <>); -- VkRect2D
      clearValueCount : Int_Unsigned_C            := 0;              -- uint32_t
      pClearValues    : Ptr_VkClearValue          := null;           -- const VkClearValue*
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSpecializationMapEntry.html
  type VkSpecializationMapEntry;
  type Ptr_VkSpecializationMapEntry is access all VkSpecializationMapEntry;
  type VkSpecializationMapEntry is record
      constantID : Int_Unsigned_C := 0; -- uint32_t
      offset     : Int_Unsigned_C := 0; -- uint32_t
      size       : Int_Size_C     := 0; -- size_t
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSpecializationInfo.html
  type VkSpecializationInfo;
  type Ptr_VkSpecializationInfo is access all VkSpecializationInfo;
  type VkSpecializationInfo is record
      mapEntryCount : Int_Unsigned_C               := 0;        -- uint32_t
      pMapEntries   : Ptr_VkSpecializationMapEntry := null;     -- const VkSpecializationMapEntry*
      dataSize      : Int_Size_C                   := 0;        -- size_t
      pData         : Ptr                          := NULL_PTR; -- const void*
    end record with Convention => C;

  -- VkPipelineShaderStageCreateInfo
  type VkPipelineShaderStageCreateInfo;
  type Ptr_VkPipelineShaderStageCreateInfo is access all VkPipelineShaderStageCreateInfo;
  type VkPipelineShaderStageCreateInfo is record
      sType               : Int_Unsigned_C := VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO; -- VkStructureType
      pNext               : Ptr_VkPipelineShaderStageCreateInfo := null; -- const void*
      flags               : Int_Unsigned_C           := 0;        -- VkPipelineShaderStageCreateFlags
      stage               : Int_Unsigned_C           := 0;        -- VkShaderStageFlagBits
      module              : Ptr                      := NULL_PTR; -- VkShaderModule
      pName               : Ptr_Char_8_C             := null;     -- const char*
      pSpecializationInfo : Ptr_VkSpecializationInfo := null;     -- const VkSpecializationInfo*
    end record with Convention => C;
  type Array_VkPipelineShaderStageCreateInfo is array (Positive range <>) of aliased VkPipelineShaderStageCreateInfo;
  type Ptr_Array_VkPipelineShaderStageCreateInfo is access all Array_VkPipelineShaderStageCreateInfo;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkVertexInputBindingDescription.html
  type VkVertexInputBindingDescription;
  type Ptr_VkVertexInputBindingDescription is access all VkVertexInputBindingDescription;
  type VkVertexInputBindingDescription is record
      binding   : Int_Unsigned_C := 0; -- uint32_t
      stride    : Int_Unsigned_C := 0; -- uint32_t
      inputRate : Int_Unsigned_C := 0; -- VkVertexInputRate
    end record with Convention => C;
  type Array_VkVertexInputBindingDescription is array (Positive range <>) of aliased VkVertexInputBindingDescription;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkVertexInputAttributeDescription.html
  type VkVertexInputAttributeDescription;
  type Ptr_VkVertexInputAttributeDescription is access all VkVertexInputAttributeDescription;
  type VkVertexInputAttributeDescription is record
      location : Int_Unsigned_C := 0; -- uint32_t
      binding  : Int_Unsigned_C := 0; -- uint32_t
      format   : Int_Unsigned_C := 0; -- VkFormat
      offset   : Int_Unsigned_C := 0; -- uint32_t
    end record with Convention => C;
  type Array_VkVertexInputAttributeDescription is array (Positive range <>) of aliased VkVertexInputAttributeDescription;
    
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineVertexInputStateCreateInfo.html
  type VkPipelineVertexInputStateCreateInfo;
  type Ptr_VkPipelineVertexInputStateCreateInfo is access all VkPipelineVertexInputStateCreateInfo;
  type VkPipelineVertexInputStateCreateInfo is record
      sType                           : Int_Unsigned_C := VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO; -- VkStructureType
      pNext                           : Ptr_VkPipelineVertexInputStateCreateInfo := null; -- const void*
      flags                           : Int_Unsigned_C                           := 0;    -- VkPipelineVertexInputStateCreateFlags
      vertexBindingDescriptionCount   : Int_Unsigned_C                           := 0;    -- uint32_t
      pVertexBindingDescriptions      : Ptr_VkVertexInputBindingDescription      := null; -- const VkVertexInputBindingDescription*
      vertexAttributeDescriptionCount : Int_Unsigned_C                           := 0;    -- uint32_t
      pVertexAttributeDescriptions    : Ptr_VkVertexInputAttributeDescription    := null; -- const VkVertexInputAttributeDescription*
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineInputAssemblyStateCreateInfo.html
  type VkPipelineInputAssemblyStateCreateInfo;
  type Ptr_VkPipelineInputAssemblyStateCreateInfo is access all VkPipelineInputAssemblyStateCreateInfo;
  type VkPipelineInputAssemblyStateCreateInfo is record
      sType                  : Int_Unsigned_C := VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO; -- VkStructureType
      pNext                  : Ptr_VkPipelineInputAssemblyStateCreateInfo := null; -- const void*
      flags                  : Int_Unsigned_C := 0; -- VkPipelineInputAssemblyStateCreateFlags
      topology               : Int_Unsigned_C := 0; -- VkPrimitiveTopology
      primitiveRestartEnable : Int_Unsigned_C := 0; -- VkBool32
    end record With Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineTessellationStateCreateFlags.html
  type VkPipelineTessellationStateCreateInfo;
  type Ptr_VkPipelineTessellationStateCreateInfo is access all VkPipelineTessellationStateCreateInfo;
  type VkPipelineTessellationStateCreateInfo is record
      sType              : Int_Unsigned_C := VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO; -- VkStructureType
      pNext              : Ptr_VkPipelineTessellationStateCreateInfo := null; -- const void*
      flags              : Int_Unsigned_C := 0; -- VkPipelineTessellationStateCreateFlags
      patchControlPoints : Int_Unsigned_C := 0; -- uint32_t
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkViewport.html
  type VkViewport;
  type Ptr_VkViewport is access all VkViewport;
  type VkViewport is record
      x        : Real_C := 0.0; -- float
      y        : Real_C := 0.0; -- float
      width    : Real_C := 0.0; -- float
      height   : Real_C := 0.0; -- float
      minDepth : Real_C := 0.0; -- float
      maxDepth : Real_C := 0.0; -- float
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineViewportStateCreateInfo.html
  type VkPipelineViewportStateCreateInfo;
  type Ptr_VkPipelineViewportStateCreateInfo is access all VkPipelineViewportStateCreateInfo;
  type VkPipelineViewportStateCreateInfo is record
      sType         : Int_Unsigned_C := VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO; -- VkStructureType
      pNext         : Ptr_VkPipelineViewportStateCreateInfo := null; -- const void*
      flags         : Int_Unsigned_C := 0;    -- VkPipelineViewportStateCreateFlags    
      viewportCount : Int_Unsigned_C := 0;    -- uint32_t
      pViewports    : Ptr_VkViewport := null; -- const VkViewport* 
      scissorCount  : Int_Unsigned_C := 0;    -- uint32_t
      pScissors     : Ptr_VkRect2D   := null; -- const VkRect2D*
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineRasterizationStateCreateInfo.html
  type VkPipelineRasterizationStateCreateInfo;
  type Ptr_VkPipelineRasterizationStateCreateInfo is access all VkPipelineRasterizationStateCreateInfo;
  type VkPipelineRasterizationStateCreateInfo is record
      sType                   : Int_Unsigned_C := VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO; -- VkStructureType
      pNext                   : Ptr_VkPipelineRasterizationStateCreateInfo := null; -- const void*
      flags                   : Int_Unsigned_C := 0;   -- VkPipelineRasterizationStateCreateFlags
      depthClampEnable        : Int_Unsigned_C := 0;   -- VkBool32
      rasterizerDiscardEnable : Int_Unsigned_C := 0;   -- VkBool32
      polygonMode             : Int_Unsigned_C := 0;   -- VkPolygonMode
      cullMode                : Int_Unsigned_C := 0;   -- VkCullModeFlags
      frontFace               : Int_Unsigned_C := 0;   -- VkFrontFace
      depthBiasEnable         : Int_Unsigned_C := 0;   -- VkBool32
      depthBiasConstantFactor : Real_C         := 0.0; -- float
      depthBiasClamp          : Real_C         := 0.0; -- float
      depthBiasSlopeFactor    : Real_C         := 0.0; -- float
      lineWidth               : Real_C         := 0.0; -- float
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineMultisampleStateCreateInfo.html
  type VkPipelineMultisampleStateCreateInfo;
  type Ptr_VkPipelineMultisampleStateCreateInfo is access all VkPipelineMultisampleStateCreateInfo;
  type VkPipelineMultisampleStateCreateInfo is record
      sType                 : Int_Unsigned_C :=  VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO; -- VkStructureType
      pNext                 : Ptr_VkPipelineRasterizationStateCreateInfo := null; -- const void*
      flags                 : Int_Unsigned_C     := 0;    -- VkPipelineMultisampleStateCreateFlags
      rasterizationSamples  : Int_Unsigned_C     := 0;    -- VkSampleCountFlagBits
      sampleShadingEnable   : Int_Unsigned_C     := 0;    -- VkBool32
      minSampleShading      : Real_C             := 0.0;  -- float
      pSampleMask           : Ptr_Int_Unsigned_C := null; -- const VkSampleMask*
      alphaToCoverageEnable : Int_Unsigned_C     := 0;    -- VkBool32
      alphaToOneEnable      : Int_Unsigned_C     := 0;    -- VkBool32
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkStencilOpState.html
  type VkStencilOpState;
  type Ptr_VkStencilOpState is access all VkStencilOpState;
  type VkStencilOpState is record
      failOp      : Int_Unsigned_C := 0; -- VkStencilOp
      passOp      : Int_Unsigned_C := 0; -- VkStencilOp
      depthFailOp : Int_Unsigned_C := 0; -- VkStencilOp
      compareOp   : Int_Unsigned_C := 0; -- VkCompareOp
      compareMask : Int_Unsigned_C := 0; -- uint32_t
      writeMask   : Int_Unsigned_C := 0; -- uint32_t
      reference   : Int_Unsigned_C := 0; -- uint32_t
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineDepthStencilStateCreateInfo.html
  type VkPipelineDepthStencilStateCreateInfo;
  type Ptr_VkPipelineDepthStencilStateCreateInfo is access all VkPipelineDepthStencilStateCreateInfo;
  type VkPipelineDepthStencilStateCreateInfo is record
      sType                 : Int_Unsigned_C :=  VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO; -- VkStructureType
      pNext                 : Ptr_VkPipelineDepthStencilStateCreateInfo := null;  -- const void*
      flags                 : Int_Unsigned_C   := 0;              -- VkPipelineDepthStencilStateCreateFlags
      depthTestEnable       : Int_Unsigned_C   := 0;              -- VkBool32
      depthWriteEnable      : Int_Unsigned_C   := 0;              -- VkBool32
      depthCompareOp        : Int_Unsigned_C   := 0;              -- VkCompareOp
      depthBoundsTestEnable : Int_Unsigned_C   := 0;              -- VkBool32
      stencilTestEnable     : Int_Unsigned_C   := 0;              -- VkBool32
      front                 : VkStencilOpState := (others => <>); -- VkStencilOpState
      back                  : VkStencilOpState := (others => <>); -- VkStencilOpState
      minDepthBounds        : Real_C           := 0.0;            -- float
      maxDepthBounds        : Real_C           := 0.0;            -- float
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineColorBlendAttachmentState.html
  type VkPipelineColorBlendAttachmentState;
  type Ptr_VkPipelineColorBlendAttachmentState is access all VkPipelineColorBlendAttachmentState;
  type VkPipelineColorBlendAttachmentState is record
      blendEnable         : Int_Unsigned_C := 0; -- VkBool32
      srcColorBlendFactor : Int_Unsigned_C := 0; -- VkBlendFactor
      dstColorBlendFactor : Int_Unsigned_C := 0; -- VkBlendFactor
      colorBlendOp        : Int_Unsigned_C := 0; -- VkBlendOp                
      srcAlphaBlendFactor : Int_Unsigned_C := 0; -- VkBlendFactor
      dstAlphaBlendFactor : Int_Unsigned_C := 0; -- VkBlendFactor
      alphaBlendOp        : Int_Unsigned_C := 0; -- VkBlendOp                
      colorWriteMask      : Int_Unsigned_C := 0; -- VkColorComponentFlags
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineColorBlendStateCreateInfo.html
  type VkPipelineColorBlendStateCreateInfo;
  type Ptr_VkPipelineColorBlendStateCreateInfo is access all VkPipelineColorBlendStateCreateInfo;
  type VkPipelineColorBlendStateCreateInfo is record
      sType           : Int_Unsigned_C := VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO; -- VkStructureType
      pNext           : Ptr_VkPipelineColorBlendStateCreateInfo := null;            -- const void*
      flags           : Int_Unsigned_C                          := 0;               -- VkPipelineColorBlendStateCreateFlags
      logicOpEnable   : Int_Unsigned_C                          := 0;               -- VkBool32
      logicOp         : Int_Unsigned_C                          := 0;               -- VkLogicOp
      attachmentCount : Int_Unsigned_C                          := 0;               -- uint32_t
      pAttachments    : Ptr_VkPipelineColorBlendAttachmentState := null;            -- const VkPipelineColorBlendAttachmentState*
      blendConstants  : Array_Real_32 (1..4)                    := (others => 0.0); -- float [4]
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineDynamicStateCreateInfo.html
  type VkPipelineDynamicStateCreateInfo;
  type Ptr_VkPipelineDynamicStateCreateInfo is access all VkPipelineDynamicStateCreateInfo;
  type VkPipelineDynamicStateCreateInfo is record
      sType             : Int_Unsigned_C := VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO; -- VkStructureType
      pNext             : Ptr_VkPipelineDynamicStateCreateInfo := null; -- const void*
      flags             : Int_Unsigned_C     := 0;    -- VkPipelineDynamicStateCreateFlags
      dynamicStateCount : Int_Unsigned_C     := 0;    -- uint32_t
      pDynamicStates    : Ptr_Int_Unsigned_C := null; -- const VkDynamicState*
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkGraphicsPipelineCreateInfo.html
  type VkGraphicsPipelineCreateInfo;
  type Ptr_VkGraphicsPipelineCreateInfo is access all VkGraphicsPipelineCreateInfo;
  type VkGraphicsPipelineCreateInfo is record
      sType               : Int_Unsigned_C := VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO; -- VkStructureType
      pNext               : Ptr_VkGraphicsPipelineCreateInfo           := null;     -- const void*
      flags               : Int_Unsigned_C                             := 0;        -- VkPipelineCreateFlags
      stageCount          : Int_Unsigned_C                             := 0;        -- uint32_t
      pStages             : Ptr_VkPipelineShaderStageCreateInfo        := null;     -- const VkPipelineShaderStageCreateInfo*
      pVertexInputState   : Ptr_VkPipelineVertexInputStateCreateInfo   := null;     -- const VkPipelineVertexInputStateCreateInfo*
      pInputAssemblyState : Ptr_VkPipelineInputAssemblyStateCreateInfo := null;     -- const VkPipelineInputAssemblyStateCreateInfo*
      pTessellationState  : Ptr_VkPipelineTessellationStateCreateInfo  := null;     -- const VkPipelineTessellationStateCreateInfo*
      pViewportState      : Ptr_VkPipelineViewportStateCreateInfo      := null;     -- const VkPipelineViewportStateCreateInfo*
      pRasterizationState : Ptr_VkPipelineRasterizationStateCreateInfo := null;     -- const VkPipelineRasterizationStateCreateInfo*
      pMultisampleState   : Ptr_VkPipelineMultisampleStateCreateInfo   := null;     -- const VkPipelineMultisampleStateCreateInfo* 
      pDepthStencilState  : Ptr_VkPipelineDepthStencilStateCreateInfo  := null;     -- const VkPipelineDepthStencilStateCreateInfo*
      pColorBlendState    : Ptr_VkPipelineColorBlendStateCreateInfo    := null;     -- const VkPipelineColorBlendStateCreateInfo*
      pDynamicState       : Ptr_VkPipelineDynamicStateCreateInfo       := null;     -- const VkPipelineDynamicStateCreateInfo*
      layout              : Ptr                                        := NULL_PTR; -- VkPipelineLayout
      renderPass          : Ptr                                        := NULL_PTR; -- VkRenderPass
      subpass             : Int_Unsigned_C                             := 0;        -- uint32_t
      basePipelineHandle  : Ptr                                        := NULL_PTR; -- VkPipeline
      basePipelineIndex   : Int_C                                      := 0;        -- int32_t
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkAttachmentDescription.html
  type VkAttachmentDescription;
  type Ptr_VkAttachmentDescription is access all VkAttachmentDescription;
  type VkAttachmentDescription is record
      flags          : Int_Unsigned_C := 0; -- VkAttachmentDescriptionFlags
      format         : Int_Unsigned_C := 0; -- VkFormat
      samples        : Int_Unsigned_C := 0; -- VkSampleCountFlagBits
      loadOp         : Int_Unsigned_C := 0; -- VkAttachmentLoadOp
      storeOp        : Int_Unsigned_C := 0; -- VkAttachmentStoreOp
      stencilLoadOp  : Int_Unsigned_C := 0; -- VkAttachmentLoadOp
      stencilStoreOp : Int_Unsigned_C := 0; -- VkAttachmentStoreOp
      initialLayout  : Int_Unsigned_C := 0; -- VkImageLayout
      finalLayout    : Int_Unsigned_C := 0; -- VkImageLayout
    end record with Convention => C;
  type Array_VkAttachmentDescription is array (Positive range <>) of aliased VkAttachmentDescription;
  type Ptr_Array_VkAttachmentDescription is access all Array_VkAttachmentDescription;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkAttachmentReference.html
  type VkAttachmentReference;
  type Ptr_VkAttachmentReference is access all VkAttachmentReference;
  type VkAttachmentReference is record
      attachment : Int_Unsigned_C := 0; -- uint32_t         
      layout     : Int_Unsigned_C := 0; -- VkImageLayout    
    end record with Convention => C;
  type Array_VkAttachmentReference is array (Positive range <>) of aliased VkAttachmentReference;
  type Ptr_Array_VkAttachmentReference is access all Array_VkAttachmentReference;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSubpassDescription.html
  type VkSubpassDescription;
  type Ptr_VkSubpassDescription is access all VkSubpassDescription;
  type VkSubpassDescription is record
      flags                   : Int_Unsigned_C            := 0;    -- VkSubpassDescriptionFlags       
      pipelineBindPoint       : Int_Unsigned_C            := 0;    -- VkPipelineBindPoint             
      inputAttachmentCount    : Int_Unsigned_C            := 0;    -- uint32_t                        
      pInputAttachments       : Ptr_VkAttachmentReference := null; -- const VkAttachmentReference*
      colorAttachmentCount    : Int_Unsigned_C            := 0;    -- uint32_t                        
      pColorAttachments       : Ptr_VkAttachmentReference := null; -- const VkAttachmentReference*
      pResolveAttachments     : Ptr_VkAttachmentReference := null; -- const VkAttachmentReference*
      pDepthStencilAttachment : Ptr_VkAttachmentReference := null; -- const VkAttachmentReference*
      preserveAttachmentCount : Int_Unsigned_C            := 0;    -- uint32_t                        
      pPreserveAttachments    : Ptr_Array_Int_Unsigned_C  := null; -- const uint32_t*
    end record with Convention => C;
  type Array_VkSubpassDescription is array (Positive range <>) of aliased VkSubpassDescription;
  type Ptr_Array_VkSubpassDescription is access all Array_VkSubpassDescription;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSubpassDependency.html
  type VkSubpassDependency;
  type Ptr_VkSubpassDependency is access all VkSubpassDependency;
  type VkSubpassDependency is record
      srcSubpass      : Int_Unsigned_C := 0; -- uint32_t
      dstSubpass      : Int_Unsigned_C := 0; -- uint32_t
      srcStageMask    : Int_Unsigned_C := 0; -- VkPipelineStageFlags
      dstStageMask    : Int_Unsigned_C := 0; -- VkPipelineStageFlags
      srcAccessMask   : Int_Unsigned_C := 0; -- VkAccessFlags
      dstAccessMask   : Int_Unsigned_C := 0; -- VkAccessFlags
      dependencyFlags : Int_Unsigned_C := 0; -- VkDependencyFlags
    end record with Convention => C;
  type Array_VkSubpassDependency is array (Positive range <>) of aliased VkSubpassDependency;
  type Ptr_Array_VkSubpassDependency is access all Array_VkSubpassDependency;
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFramebufferCreateInfo.html
  type VkFramebufferCreateInfo;
  type Ptr_VkFramebufferCreateInfo is access all VkFramebufferCreateInfo;
  type VkFramebufferCreateInfo is record
      sType           : Int_Unsigned_C := VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO; -- VkStructureType
      pNext           : Ptr_VkFramebufferCreateInfo := null;     -- const void*
      flags           : Int_Unsigned_C              := 0;        -- VkFramebufferCreateFlags
      renderPass      : Ptr                         := NULL_PTR; -- VkRenderPass                
      attachmentCount : Int_Unsigned_C              := 0;        -- uint32_t                    
      pAttachments    : Ptr_Ptr                     := null;     -- const VkImageView*
      width           : Int_Unsigned_C              := 0;        -- uint32_t                    
      height          : Int_Unsigned_C              := 0;        -- uint32_t                    
      layers          : Int_Unsigned_C              := 0;        -- uint32_t                    
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkRenderPassCreateInfo.html
  type VkRenderPassCreateInfo;
  type Ptr_VkRenderPassCreateInfo is access all VkRenderPassCreateInfo;
  type VkRenderPassCreateInfo is record
      sType           : Int_Unsigned_C := VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO; -- VkStructureType
      pNext           : Ptr_VkRenderPassCreateInfo  := null; -- const void*
      flags           : Int_Unsigned_C              := 0;    -- VkRenderPassCreateFlags
      attachmentCount : Int_Unsigned_C              := 0;    -- uint32_t                          
      pAttachments    : Ptr_VkAttachmentDescription := null; -- const VkAttachmentDescription*
      subpassCount    : Int_Unsigned_C              := 0;    -- uint32_t                          
      pSubpasses      : Ptr_VkSubpassDescription    := null; -- const VkSubpassDescription* 
      dependencyCount : Int_Unsigned_C              := 0;    -- uint32_t                          
      pDependencies   : Ptr_VkSubpassDependency     := null; -- const VkSubpassDependency*
    end record with Convention => C;
  type Array_VkRenderPassCreateInfo is array (Positive range <>) of aliased VkRenderPassCreateInfo;
  type Ptr_Array_VkRenderPassCreateInfo is access all Array_VkRenderPassCreateInfo;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFormatProperties.html
  type VkFormatProperties;
  type Ptr_VkFormatProperties is access all VkFormatProperties;
  type VkFormatProperties is record
      linearTilingFeatures  : Int_Unsigned_C := 0; -- VkFormatFeatureFlags    
      optimalTilingFeatures : Int_Unsigned_C := 0; -- VkFormatFeatureFlags    
      bufferFeatures        : Int_Unsigned_C := 0; -- VkFormatFeatureFlags    
    end record with Convention => C;
  type Array_VkFormatProperties is array (Positive range <>) of aliased VkFormatProperties;
  type Ptr_Array_VkFormatProperties is access all Array_VkFormatProperties;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDescriptorPoolSize.html
  type VkDescriptorPoolSize;
  type Ptr_VkDescriptorPoolSize is access all VkDescriptorPoolSize;
  type VkDescriptorPoolSize is record
      typ             : Int_Unsigned_C := 0; -- VkDescriptorType !!!
      descriptorCount : Int_Unsigned_C := 0; -- uint32_t            
    end record with Convention => C;
  type Array_VkDescriptorPoolSize is array (Positive range <>) of aliased VkDescriptorPoolSize;
  type Ptr_Array_VkDescriptorPoolSize is access all Array_VkDescriptorPoolSize;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDescriptorPoolCreateInfo.html
  type VkDescriptorPoolCreateInfo;
  type Ptr_VkDescriptorPoolCreateInfo is access all VkDescriptorPoolCreateInfo;
  type VkDescriptorPoolCreateInfo is record
      sType         : Int_Unsigned_C := VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO; -- VkStructureType                
      pNext         : Ptr_VkDescriptorPoolCreateInfo := null; -- const void*
      flags         : Int_Unsigned_C                 := 0;    -- VkDescriptorPoolCreateFlags    
      maxSets       : Int_Unsigned_C                 := 0;    -- uint32_t
      poolSizeCount : Int_Unsigned_C                 := 0;    -- uint32_t
      pPoolSizes    : Ptr_Array_VkDescriptorPoolSize := null; -- const VkDescriptorPoolSize*
    end record with Convention => C;
  type Array_VkDescriptorPoolCreateInfo is array (Positive range <>) of aliased VkDescriptorPoolCreateInfo;
  type Ptr_Array_VkDescriptorPoolCreateInfo is access all Array_VkDescriptorPoolCreateInfo;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDescriptorSetLayoutBinding.html
  type VkDescriptorSetLayoutBinding;
  type Ptr_VkDescriptorSetLayoutBinding is access all VkDescriptorSetLayoutBinding;
  type VkDescriptorSetLayoutBinding is record
      binding            : Int_Unsigned_C := 0;    -- uint32_t              
      descriptorType     : Int_Unsigned_C := 0;    -- VkDescriptorType      
      descriptorCount    : Int_Unsigned_C := 0;    -- uint32_t              
      stageFlags         : Int_Unsigned_C := 0;    -- VkShaderStageFlags    
      pImmutableSamplers : Ptr_Ptr        := null; -- const VkSampler*
    end record with Convention => C;
  type Array_VkDescriptorSetLayoutBinding is array (Positive range <>) of aliased VkDescriptorSetLayoutBinding;
  type Ptr_Array_VkDescriptorSetLayoutBinding is access all Array_VkDescriptorSetLayoutBinding;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDescriptorSetLayoutCreateInfo.html
  type VkDescriptorSetLayoutCreateInfo;
  type Ptr_VkDescriptorSetLayoutCreateInfo is access all VkDescriptorSetLayoutCreateInfo;
  type VkDescriptorSetLayoutCreateInfo is record
      sType        : Int_Unsigned_C := VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO; -- VkStructureType                        
      pNext        : Ptr_VkDescriptorSetLayoutCreateInfo := null; -- const void*
      flags        : Int_Unsigned_C                      := 0;    -- VkDescriptorSetLayoutCreateFlags       
      bindingCount : Int_Unsigned_C                      := 0;    -- uint32_t                               
      pBindings    : Ptr_VkDescriptorSetLayoutBinding    := null; -- const VkDescriptorSetLayoutBinding*
    end record with Convention => C;
  type Array_VkDescriptorSetLayoutCreateInfo is array (Positive range <>) of aliased VkDescriptorSetLayoutCreateInfo;
  type Ptr_Array_VkDescriptorSetLayoutCreateInfo is access all Array_VkDescriptorSetLayoutCreateInfo;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDescriptorBufferInfo.html
  type VkDescriptorBufferInfo;
  type Ptr_VkDescriptorBufferInfo is access all VkDescriptorBufferInfo;
  type VkDescriptorBufferInfo is record
      buffer : Ptr               := NULL_PTR; -- VkBuffer
      offset : Int_64_Unsigned_C := 0;        -- VkDeviceSize
      rang   : Int_64_Unsigned_C := 0;        -- VkDeviceSize !!!
    end record with Convention => C;
  type Array_VkDescriptorBufferInfo is array (Positive range <>) of aliased VkDescriptorBufferInfo;
  type Ptr_Array_VkDescriptorBufferInfo is access all Array_VkDescriptorBufferInfo;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDescriptorImageInfo.html
  type VkDescriptorImageInfo;
  type Ptr_VkDescriptorImageInfo is access all VkDescriptorImageInfo;
  type VkDescriptorImageInfo is record
      sampler     : Ptr            := NULL_PTR; -- VkSampler
      imageView   : Ptr            := NULL_PTR; -- VkImageView
      imageLayout : Int_Unsigned_C := 0;        -- VkImageLayout
    end record with Convention => C;
  type Array_VkDescriptorImageInfo is array (Positive range <>) of aliased VkDescriptorImageInfo;
  type Ptr_Array_VkDescriptorImageInfo is access all Array_VkDescriptorImageInfo;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDescriptorSetAllocateInfo.html
  type VkDescriptorSetAllocateInfo;
  type Ptr_VkDescriptorSetAllocateInfo is access all VkDescriptorSetAllocateInfo;
  type VkDescriptorSetAllocateInfo is record
      sType              : Int_Unsigned_C := VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO; -- VkStructureType                 
      pNext              : Ptr_VkDescriptorSetAllocateInfo := null;     -- const void*
      descriptorPool     : Ptr                             := NULL_PTR; -- VkDescriptorPool                
      descriptorSetCount : Int_Unsigned_C                  := 0;        -- uint32_t                        
      pSetLayouts        : Ptr_Array_Ptr                   := null;     -- const VkDescriptorSetLayout*
    end record with Convention => C;
  type Array_VkDescriptorSetAllocateInfo is array (Positive range <>) of aliased VkDescriptorSetAllocateInfo;
  type Ptr_Array_VkDescriptorSetAllocateInfo is access all Array_VkDescriptorSetAllocateInfo;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkWriteDescriptorSet.html
  type VkWriteDescriptorSet;
  type Ptr_VkWriteDescriptorSet is access all VkWriteDescriptorSet;
  type VkWriteDescriptorSet is record
      sType            : Int_Unsigned_C := VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET; -- VkStructureType                  
      pNext            : Ptr_VkWriteDescriptorSet   := null;     -- const void*
      dstSet           : Ptr                        := NULL_PTR; -- VkDescriptorSet                  
      dstBinding       : Int_Unsigned_C             := 0;        -- uint32_t                         
      dstArrayElement  : Int_Unsigned_C             := 0;        -- uint32_t                         
      descriptorCount  : Int_Unsigned_C             := 0;        -- uint32_t                         
      descriptorType   : Int_Unsigned_C             := 0;        -- VkDescriptorType                 
      pImageInfo       : Ptr_VkDescriptorImageInfo  := null;     -- const VkDescriptorImageInfo*
      pBufferInfo      : Ptr_VkDescriptorBufferInfo := null;     -- const VkDescriptorBufferInfo*
      pTexelBufferView : Ptr_Ptr                    := null;     -- const VkBufferView* 
    end record with Convention => C;
  type Array_VkWriteDescriptorSet is array (Positive range <>) of aliased VkWriteDescriptorSet;
  type Ptr_Array_VkWriteDescriptorSet is access all Array_VkWriteDescriptorSet;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCopyDescriptorSet.html
  type VkCopyDescriptorSet;
  type Ptr_VkCopyDescriptorSet is access all VkCopyDescriptorSet;
  type VkCopyDescriptorSet is record
      sType           : Int_Unsigned_C := VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET; -- VkStructureType    
      pNext           : Ptr_VkCopyDescriptorSet := null;     -- const void*
      srcSet          : Ptr                     := NULL_PTR; -- VkDescriptorSet    
      srcBinding      : Int_Unsigned_C          := 0;        -- uint32_t           
      srcArrayElement : Int_Unsigned_C          := 0;        -- uint32_t           
      dstSet          : Ptr                     := NULL_PTR; -- VkDescriptorSet    
      dstBinding      : Int_Unsigned_C          := 0;        -- uint32_t           
      dstArrayElement : Int_Unsigned_C          := 0;        -- uint32_t           
      descriptorCount : Int_Unsigned_C          := 0;        -- uint32_t           
    end record with Convention => C;
  type Array_VkCopyDescriptorSet is array (Positive range <>) of aliased VkCopyDescriptorSet;
  type Ptr_Array_VkCopyDescriptorSet is access all Array_VkCopyDescriptorSet;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPushConstantRange.html
  type VkPushConstantRange;
  type Ptr_VkPushConstantRange is access all VkPushConstantRange;
  type VkPushConstantRange is record
      stageFlags : Int_Unsigned_C := 0; -- VkShaderStageFlags
      offset     : Int_Unsigned_C := 0; -- uint32_t
      size       : Int_Unsigned_C := 0; -- uint32_t
    end record with Convention => C;
  type Array_VkPushConstantRange is array (Positive range <>) of aliased VkPushConstantRange;
  type Ptr_Array_VkPushConstantRange is access all Array_VkPushConstantRange;
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineLayoutCreateInfo.html
  type VkPipelineLayoutCreateInfo;
  type Ptr_VkPipelineLayoutCreateInfo is access all VkPipelineLayoutCreateInfo;
  type VkPipelineLayoutCreateInfo is record
      sType                  : Int_Unsigned_C := VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO; -- VkStructureType
      pNext                  : Ptr_VkPipelineLayoutCreateInfo := null; -- const void*
      flags                  : Int_Unsigned_C                 := 0;    -- VkPipelineLayoutCreateFlags
      setLayoutCount         : Int_Unsigned_C                 := 0;    -- uint32_t
      pSetLayouts            : Ptr_Ptr                        := null; -- const VkDescriptorSetLayout*
      pushConstantRangeCount : Int_Unsigned_C                 := 0;    -- uint32_t
      pPushConstantRanges    : Ptr_VkPushConstantRange        := null; -- const VkPushConstantRange*
    end record with Convention => C;
  type Array_VkPipelineLayoutCreateInfo is array (Positive range <>) of aliased VkPipelineLayoutCreateInfo;
  type Ptr_Array_VkPipelineLayoutCreateInfo is access all Array_VkPipelineLayoutCreateInfo;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkComponentMapping.html
  type VkComponentMapping;
  type Ptr_VkComponentMapping is access all VkComponentMapping;
  type VkComponentMapping is record
      r : Int_Unsigned_C := 0; -- VkComponentSwizzle
      g : Int_Unsigned_C := 0; -- VkComponentSwizzle
      b : Int_Unsigned_C := 0; -- VkComponentSwizzle
      a : Int_Unsigned_C := 0; -- VkComponentSwizzle
    end record with Convention => C;
  type Array_VkComponentMapping is array (Positive range <>) of aliased VkComponentMapping;
  type Ptr_Array_VkComponentMapping is access all Array_VkComponentMapping;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageViewCreateInfo.html
  type VkImageViewCreateInfo;
  type Ptr_VkImageViewCreateInfo is access all VkImageViewCreateInfo;
  type VkImageViewCreateInfo is record
      sType            : Int_Unsigned_C := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO; -- VkStructureType
      pNext            : Ptr_VkImageViewCreateInfo := null;           -- const void*
      flags            : Int_Unsigned_C            := 0;              -- VkImageViewCreateFlags
      image            : Ptr                       := NULL_PTR;       -- VkImage
      viewType         : Int_Unsigned_C            := 0;              -- VkImageViewType
      format           : Int_Unsigned_C            := 0;              -- VkFormat
      components       : VkComponentMapping        := (others => <>); -- VkComponentMapping
      subresourceRange : VkImageSubresourceRange   := (others => <>); -- VkImageSubresourceRange
    end record with Convention => C;
  type Array_VkImageViewCreateInfo is array (Positive range <>) of aliased VkImageViewCreateInfo;
  type Ptr_Array_VkImageViewCreateInfo is access all Array_VkImageViewCreateInfo;
  
  ---------------
  -- Functions --
  ---------------
  
  -- Template
  -- type Ptr_ is access function ( : ; -- 
  --                                : ; -- 
  --                                : ; -- 
  --                                : ; -- 
  --                                : ; -- 
  --                                : ) --
  --                               return  -- 
  --                               with Convention => C;
  --function To_Ptr_ is new Unchecked_Conversion (Ptr, Ptr_);
  -- : Ptr_ := null;
  
  -- Assert VK_SUCCESS
  procedure vkAssert (result : Int_Unsigned_C); -- VkResult
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdCopyBufferToImage.html
  type Ptr_vkCmdCopyBufferToImage is access procedure (commandBuffer  : Ptr;                   -- VkCommandBuffer                             
                                                       srcBuffer      : Ptr;                   -- VkBuffer                                    
                                                       dstImage       : Ptr;                   -- VkImage                                     
                                                       dstImageLayout : Int_Unsigned_C;        -- VkImageLayout                               
                                                       regionCount    : Int_Unsigned_C;        -- uint32_t                                    
                                                       pRegions       : Ptr_VkBufferImageCopy) -- const VkBufferImageCopy*
                                                       with Convention => C;
  function To_Ptr_vkCmdCopyBufferToImage is new Unchecked_Conversion (Ptr, Ptr_vkCmdCopyBufferToImage);
  vkCmdCopyBufferToImage : Ptr_vkCmdCopyBufferToImage := null;
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateShaderModule.html
  type Ptr_vkCreateShaderModule is access function (device        : Ptr;                          -- VkDevice                                    
                                                    pCreateInfo   : Ptr_VkShaderModuleCreateInfo; -- const VkShaderModuleCreateInfo*
                                                    pAllocator    : Ptr_VkAllocationCallbacks;    -- const VkAllocationCallbacks*
                                                    pShaderModule : Ptr_Ptr)                      -- VkShaderModule*
                                                    return Int_Unsigned_C                         -- VkResult 
                                                    with Convention => C;
  function To_Ptr_vkCreateShaderModule is new Unchecked_Conversion (Ptr, Ptr_vkCreateShaderModule);
  vkCreateShaderModule : Ptr_vkCreateShaderModule := null;
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateFramebuffer.html
  type Ptr_vkCreateFramebuffer is access function (device       : Ptr;                         -- VkDevice
                                                   pCreateInfo  : Ptr_VkFramebufferCreateInfo; -- const VkFramebufferCreateInfo*
                                                   pAllocator   : Ptr_VkAllocationCallbacks;   -- const VkAllocationCallbacks*
                                                   pFramebuffer : Ptr_Ptr)                     -- VkFramebuffer*
                                                   return Int_Unsigned_C                       -- VkResult 
                                                   with Convention => C;
  function To_Ptr_vkCreateFramebuffer is new Unchecked_Conversion (Ptr, Ptr_vkCreateFramebuffer);
  vkCreateFramebuffer : Ptr_vkCreateFramebuffer := null;
                                   
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkUnmapMemory.html
  type Ptr_vkUnmapMemory is access procedure (device : Ptr; -- VkDevice
                                              memory : Ptr) -- VkDeviceMemory
                                              with Convention => C;
  function To_Ptr_vkUnmapMemory is new Unchecked_Conversion (Ptr, Ptr_vkUnmapMemory);
  vkUnmapMemory : Ptr_vkUnmapMemory := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkMapMemory.html
  type Ptr_vkMapMemory is access function (device : Ptr;               -- VkDevice
                                           memory : Ptr;               -- VkDeviceMemory
                                           offset : Int_64_Unsigned_C; -- VkDeviceSize
                                           size   : Int_64_Unsigned_C; -- VkDeviceSize
                                           flags  : Int_Unsigned_C;    -- VkMemoryMapFlags
                                           ppData : Ptr_Ptr)           -- void**
                                           return Int_Unsigned_C       -- VkResult
                                           with Convention => C;
  function To_Ptr_vkMapMemory is new Unchecked_Conversion (Ptr, Ptr_vkMapMemory);
  vkMapMemory : Ptr_vkMapMemory := null;
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetImageSubresourceLayout.html
  type Ptr_vkGetImageSubresourceLayout is access procedure (device       : Ptr;                     -- VkDevice
                                                            image        : Ptr;                     -- VkImage
                                                            pSubresource : Ptr_VkImageSubresource;  -- const VkImageSubresource*
                                                            pLayout      : Ptr_VkSubresourceLayout) -- VkSubresourceLayout*
                                                            with Convention => C;
  function To_Ptr_vkGetImageSubresourceLayout is new Unchecked_Conversion (Ptr, Ptr_vkGetImageSubresourceLayout);
  vkGetImageSubresourceLayout : Ptr_vkGetImageSubresourceLayout := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateImageView.html
  type Ptr_vkCreateImageView is access function (device      : Ptr;                       -- VkDevice
                                                 pCreateInfo : Ptr_VkImageViewCreateInfo; -- const VkImageViewCreateInfo*
                                                 pAllocator  : Ptr_VkAllocationCallbacks; -- const VkAllocationCallbacks*
                                                 pView       : Ptr_Ptr)                   -- VkImageView* 
                                                 return Int_Unsigned_C                    -- VkResult
                                                 with Convention => C;
  function To_Ptr_vkCreateImageView is new Unchecked_Conversion (Ptr, Ptr_vkCreateImageView);
  vkCreateImageView : Ptr_vkCreateImageView := null;  
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdCopyImage.html
  type Ptr_vkCmdCopyImage is access procedure (commandBuffer  : Ptr;            -- VkCommandBuffer
                                               srcImage       : Ptr;            -- VkImage
                                               srcImageLayout : Int_Unsigned_C; -- VkImageLayout
                                               dstImage       : Ptr;            -- VkImage
                                               dstImageLayout : Int_Unsigned_C; -- VkImageLayout
                                               regionCount    : Int_Unsigned_C; -- uint32_t
                                               pRegions       : Ptr_Ptr)        -- const VkImageCopy*
                                               with Convention => C;
  function To_Ptr_vkCmdCopyImage is new Unchecked_Conversion (Ptr, Ptr_vkCmdCopyImage);
  vkCmdCopyImage : Ptr_vkCmdCopyImage := null;
    
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetBufferMemoryRequirements.html
  type Ptr_vkGetBufferMemoryRequirements is access procedure (device              : Ptr;                      -- VkDevice
                                                              buffer              : Ptr;                      -- VkBuffer
                                                              pMemoryRequirements : Ptr_VkMemoryRequirements) -- VkMemoryRequirements* 
                                                              with Convention => C;
  function To_Ptr_vkGetBufferMemoryRequirements is new Unchecked_Conversion (Ptr, Ptr_vkGetBufferMemoryRequirements);
  vkGetBufferMemoryRequirements : Ptr_vkGetBufferMemoryRequirements := null;
   
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkBindBufferMemory.html
  type Ptr_vkBindBufferMemory is access function (device       : Ptr;               -- VkDevice
                                                  buffer       : Ptr;               -- VkBuffer
                                                  memory       : Ptr;               -- VkDeviceMemory
                                                  memoryOffset : Int_64_Unsigned_C) -- VkDeviceSize
                                                  return Int_Unsigned_C             -- VkResult
                                                  with Convention => C;
  function To_Ptr_vkBindBufferMemory is new Unchecked_Conversion (Ptr, Ptr_vkBindBufferMemory);
  vkBindBufferMemory : Ptr_vkBindBufferMemory := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateBuffer.html
  type Ptr_vkCreateBuffer is access function (device      : Ptr;                       -- VkDevice
                                              pCreateInfo : Ptr_VkBufferCreateInfo;    -- const VkBufferCreateInfo*
                                              pAllocator  : Ptr_VkAllocationCallbacks; -- const VkAllocationCallbacks* 
                                              pBuffer     : Ptr_Ptr)                   -- VkBuffer*
                                              return Int_Unsigned_C                    -- VkResult
                                              with Convention => C;
  function To_Ptr_vkCreateBuffer is new Unchecked_Conversion (Ptr, Ptr_vkCreateBuffer);
  vkCreateBuffer : Ptr_vkCreateBuffer := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceFeatures.html
  type Ptr_vkGetPhysicalDeviceFeatures is access procedure (physicalDevice : Ptr;                          -- VkPhysicalDevice
                                                            pFeatures      : Ptr_VkPhysicalDeviceFeatures) -- VkPhysicalDeviceFeatures*
                                                            with Convention => C;
  function To_Ptr_vkGetPhysicalDeviceFeatures is new Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceFeatures);
  vkGetPhysicalDeviceFeatures : Ptr_vkGetPhysicalDeviceFeatures := null;                                 

  -- https://harrylovescode.gitbooks.io/vulkan-api/content/chap05/chap05-windows.html
  type Ptr_vkCreateWin32SurfaceKHR is access function (instance    : Ptr;                             -- VkInstance
                                                       pCreateInfo : Ptr_VkWin32SurfaceCreateInfoKHR; -- const VkWin32SurfaceCreateInfoKHR*
                                                       pAllocator  : Ptr_VkAllocationCallbacks;       -- const VkAllocationCallbacks*
                                                       pSurface    : Ptr_Ptr)                         -- VkSurfaceKHR*
                                                       return Int_Unsigned_C                          -- VkResult
                                                       with Convention => C;
  function To_Ptr_vkCreateWin32SurfaceKHR is new Unchecked_Conversion (Ptr, Ptr_vkCreateWin32SurfaceKHR);
  vkCreateWin32SurfaceKHR : Ptr_vkCreateWin32SurfaceKHR := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceProperties.html
  type Ptr_vkGetPhysicalDeviceProperties is access procedure (physicalDevice : Ptr;                            -- VkPhysicalDevice
                                                              pProperties    : Ptr_VkPhysicalDeviceProperties) -- VkPhysicalDeviceProperties*
                                                              with Convention => C;
  function To_Ptr_vkGetPhysicalDeviceProperties is new Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceProperties);
  vkGetPhysicalDeviceProperties : Ptr_vkGetPhysicalDeviceProperties := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceMemoryProperties.html
  type Ptr_vkGetPhysicalDeviceMemoryProperties is access procedure (physicalDevice    : Ptr;                                  -- VkPhysicalDevice
                                                                    pMemoryProperties : Ptr_VkPhysicalDeviceMemoryProperties) -- VkPhysicalDeviceMemoryProperties*
                                                                    with Convention => C;
  function To_Ptr_vkGetPhysicalDeviceMemoryProperties is new Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceMemoryProperties);
  vkGetPhysicalDeviceMemoryProperties : Ptr_vkGetPhysicalDeviceMemoryProperties := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceQueueFamilyProperties.html
  type Ptr_vkGetPhysicalDeviceQueueFamilyProperties is access procedure (physicalDevice            : Ptr;                               -- VkPhysicalDevice 
                                                                         pQueueFamilyPropertyCount : Ptr_Int_Unsigned_C;                -- uint32_t*
                                                                         pQueueFamilyProperties    : Ptr_Array_VkQueueFamilyProperties) -- VkQueueFamilyProperties*  
                                                                         with Convention => C;
  function To_Ptr_vkGetPhysicalDeviceQueueFamilyProperties is new Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceQueueFamilyProperties);
  vkGetPhysicalDeviceQueueFamilyProperties : Ptr_vkGetPhysicalDeviceQueueFamilyProperties;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDeviceQueue.html
  type Ptr_vkGetDeviceQueue is access procedure (device           : Ptr;            -- VkDevice
                                                 queueFamilyIndex : Int_Unsigned_C; -- uint32_t 
                                                 queueIndex       : Int_Unsigned_C; -- uint32_t 
                                                 pQueue           : Ptr_Ptr)        -- VkQueue*
                                                 with Convention => C;
  function To_Ptr_vkGetDeviceQueue is new Unchecked_Conversion (Ptr, Ptr_vkGetDeviceQueue);
  vkGetDeviceQueue : Ptr_vkGetDeviceQueue := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateInstance.html
  type Ptr_vkCreateInstance is access function (pCreateInfo : Ptr_VkInstanceCreateInfo;  -- const VkInstanceCreateInfo*
                                                pAllocator  : Ptr_VkAllocationCallbacks; -- const VkAllocationCallbacks*
                                                pInstance   : Ptr_Ptr)                   -- VkInstance*
                                                return Int_Unsigned_C                    -- VkResult
                                                with Convention => C;
  function To_Ptr_vkCreateInstance is new Unchecked_Conversion (Ptr, Ptr_vkCreateInstance);
  vkCreateInstance : Ptr_vkCreateInstance := null; 

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkEnumeratePhysicalDevices.html
  type Ptr_vkEnumeratePhysicalDevices is access function (instance             : Ptr;                -- VkInstance
                                                          pPhysicalDeviceCount : Ptr_Int_Unsigned_C; -- uint32_t*
                                                          pPhysicalDevices     : Ptr_Array_Ptr)      -- VkPhysicalDevice*
                                                          return Int_Unsigned_C                      -- VkResult
                                                          with Convention => C;
  function To_Ptr_vkEnumeratePhysicalDevices is new Unchecked_Conversion (Ptr, Ptr_vkEnumeratePhysicalDevices);
  vkEnumeratePhysicalDevices : Ptr_vkEnumeratePhysicalDevices := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateDevice.html
  type Ptr_vkCreateDevice is access function (physicalDevice : Ptr;                       -- VkPhysicalDevice
                                              pCreateInfo    : Ptr_VkDeviceCreateInfo;    -- const VkDeviceCreateInfo*
                                              pAllocator     : Ptr_VkAllocationCallbacks; -- const VkAllocationCallbacks*
                                              pDevice        : Ptr_Ptr)                   -- VkDevice*
                                              return Int_Unsigned_C                       -- VkResult
                                              with Convention => C;
  function To_Ptr_vkCreateDevice is new Unchecked_Conversion (Ptr,   Ptr_vkCreateDevice);
  vkCreateDevice : Ptr_vkCreateDevice := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateCommandPool.html
  type Ptr_vkCreateCommandPool is access function (device       : Ptr;                         -- VkDevice 
                                                   pCreateInfo  : Ptr_VkCommandPoolCreateInfo; -- const VkCommandPoolCreateInfo* 
                                                   pAllocator   : Ptr_VkAllocationCallbacks;   -- const VkAllocationCallbacks*
                                                   pCommandPool : Ptr_Ptr)                     -- VkCommandPool* 
                                                   return Int_Unsigned_C                       -- VkResult
                                                   with Convention => C;
  function To_Ptr_vkCreateCommandPool is new Unchecked_Conversion (Ptr, Ptr_vkCreateCommandPool);
  vkCreateCommandPool : Ptr_vkCreateCommandPool := null;

  -- http://nopper.tv/Vulkan/1.0/vkGetPhysicalDeviceSurfaceSupportKHR.html
  type Ptr_vkGetPhysicalDeviceSurfaceSupportKHR is access function (physicalDevice   : Ptr;                -- VkPhysicalDevice
                                                                    queueFamilyIndex : Int_Unsigned_C;     -- uint32_t
                                                                    surface          : Ptr;                -- VkSurfaceKHR
                                                                    pSupported       : Ptr_Int_Unsigned_C) -- VkBool32*
                                                                    return Int_Unsigned_C                  -- VkResult
                                                                    with Convention => C;
  function To_Ptr_vkGetPhysicalDeviceSurfaceSupportKHR is new Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceSurfaceSupportKHR);
  vkGetPhysicalDeviceSurfaceSupportKHR : Ptr_vkGetPhysicalDeviceSurfaceSupportKHR;    

  -- http://nopper.tv/Vulkan/1.0/vkGetPhysicalDeviceSurfaceFormatsKHR.html
  type Ptr_vkGetPhysicalDeviceSurfaceFormatsKHR is access function (physicalDevice      : Ptr;                          -- VkPhysicalDevice
                                                                    surface             : Ptr;                          -- VkSurfaceKHR
                                                                    pSurfaceFormatCount : Ptr_Int_Unsigned_C;           -- uint32_t* 
                                                                    pSurfaceFormats     : Ptr_Array_VkSurfaceFormatKHR) -- VkSurfaceFormatKHR*
                                                                    return Int_Unsigned_C                               -- VkResult 
                                                                    with Convention => C;
  function To_Ptr_vkGetPhysicalDeviceSurfaceFormatsKHR is new Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceSurfaceFormatsKHR);
  vkGetPhysicalDeviceSurfaceFormatsKHR : Ptr_vkGetPhysicalDeviceSurfaceFormatsKHR := null; 

  -- http://nopper.tv/Vulkan/1.0/vkGetPhysicalDeviceSurfaceCapabilitiesKHR.html
  type Ptr_vkGetPhysicalDeviceSurfaceCapabilitiesKHR is access function (physicalDevice       : Ptr;                          -- VkPhysicalDevice
                                                                         surface              : Ptr;                          -- VkSurfaceKHR
                                                                         pSurfaceCapabilities : Ptr_VkSurfaceCapabilitiesKHR) -- VkSurfaceCapabilitiesKHR*
                                                                         return Int_Unsigned_C                                -- VkResult
                                                                         with Convention => C;
  function To_Ptr_vkGetPhysicalDeviceSurfaceCapabilitiesKHR is new Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceSurfaceCapabilitiesKHR);
  vkGetPhysicalDeviceSurfaceCapabilitiesKHR : Ptr_vkGetPhysicalDeviceSurfaceCapabilitiesKHR := null;

  -- http://nopper.tv/Vulkan/1.0/vkGetPhysicalDeviceSurfacePresentModesKHR.html
  type Ptr_vkGetPhysicalDeviceSurfacePresentModesKHR is access function (physicalDevice    : Ptr;                      -- VkPhysicalDevice
                                                                         surface           : Ptr;                      -- VkSurfaceKHR
                                                                         pPresentModeCount : Ptr_Int_Unsigned_C;       -- uint32_t* 
                                                                         pPresentModes     : Ptr_Array_Int_Unsigned_C) -- VkPresentModeKHR*
                                                                         return Int_Unsigned_C                         -- VkResult 
                                                                         with Convention => C;
  function To_Ptr_vkGetPhysicalDeviceSurfacePresentModesKHR is new Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceSurfacePresentModesKHR);
  vkGetPhysicalDeviceSurfacePresentModesKHR : Ptr_vkGetPhysicalDeviceSurfacePresentModesKHR;

  -- http://nopper.tv/Vulkan/1.0/vkCreateSwapchainKHR.html
  type Ptr_vkCreateSwapchainKHR is access function (device      : Ptr;                          -- VkDevice
                                                    pCreateInfo : Ptr_VkSwapchainCreateInfoKHR; -- const VkSwapchainCreateInfoKHR*
                                                    pAllocator  : Ptr_VkAllocationCallbacks;    -- const VkAllocationCallbacks*
                                                    pSwapchain  : Ptr_Ptr)                      -- VkSwapchainKHR*
                                                    return Int_Unsigned_C                       -- VkResult
                                                    with Convention => C;
  function To_Ptr_vkCreateSwapchainKHR is new Unchecked_Conversion (Ptr, Ptr_vkCreateSwapchainKHR);
  vkCreateSwapchainKHR : Ptr_vkCreateSwapchainKHR := null;

  -- http://nopper.tv/Vulkan/1.0/vkGetSwapchainImagesKHR.html 
  type Ptr_vkGetSwapchainImagesKHR is access function (device               : Ptr;                -- VkDevice
                                                       swapchain            : Ptr;                -- VkSwapchainKHR
                                                       pSwapchainImageCount : Ptr_Int_Unsigned_C; -- uint32_t*  
                                                       pSwapchainImages     : Ptr_Ptr)            -- VkImage*
                                                       return Int_Unsigned_C                      -- VkResult
                                                       with Convention => C;
  function To_Ptr_vkGetSwapchainImagesKHR is new Unchecked_Conversion (Ptr, Ptr_vkGetSwapchainImagesKHR);
  vkGetSwapchainImagesKHR : Ptr_vkGetSwapchainImagesKHR := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkAllocateCommandBuffers.html
  type Ptr_vkAllocateCommandBuffers is access function (device          : Ptr;                             -- VkDevice                                    
                                                        pAllocateInfo   : Ptr_VkCommandBufferAllocateInfo; -- const VkCommandBufferAllocateInfo*
                                                        pCommandBuffers : Ptr_Ptr)                         -- VkCommandBuffer*
                                                        return Int_Unsigned_C                              -- VkResult
                                                        with Convention => C;
  function To_Ptr_vkAllocateCommandBuffers is new Unchecked_Conversion (Ptr, Ptr_vkAllocateCommandBuffers);
  vkAllocateCommandBuffers : Ptr_vkAllocateCommandBuffers := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkBeginCommandBuffer.html
  type Ptr_vkBeginCommandBuffer is access function (commandBuffer : Ptr;                             -- VkCommandBuffer                             
                                                    pBeginInfo    : access VkCommandBufferBeginInfo) -- const VkCommandBufferBeginInfo*
                                                    return Int_Unsigned_C                            -- VkResult
                                                    with Convention => C;
  function To_Ptr_vkBeginCommandBuffer is new Unchecked_Conversion (Ptr, ptr_vkBeginCommandBuffer);
  vkBeginCommandBuffer : Ptr_vkBeginCommandBuffer := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkEndCommandBuffer.html
  type Ptr_vkCmdPipelineBarrier is access procedure (commandBuffer            : Ptr;                       -- VkCommandBuffer
                                                     srcStageMask             : Int_Unsigned_C;            -- VkPipelineStageFlags
                                                     dstStageMask             : Int_Unsigned_C;            -- VkPipelineStageFlags
                                                     dependencyFlags          : Int_Unsigned_C;            -- VkDependencyFlags
                                                     memoryBarrierCount       : Int_Unsigned_C;            -- uint32_t
                                                     pMemoryBarriers          : Ptr_VkMemoryBarrier;       -- const VkMemoryBarrier*
                                                     bufferMemoryBarrierCount : Int_Unsigned_C;            -- uint32_t
                                                     pBufferMemoryBarriers    : Ptr_VkBufferMemoryBarrier; -- const VkBufferMemoryBarrier*
                                                     imageMemoryBarrierCount  : Int_Unsigned_C;            -- uint32_t
                                                     pImageMemoryBarriers     : Ptr_VkImageMemoryBarrier)  -- const VkImageMemoryBarrier*
                                                     with Convention => C;
  function To_Ptr_vkCmdPipelineBarrier is new Unchecked_Conversion (Ptr, Ptr_vkCmdPipelineBarrier);
  vkCmdPipelineBarrier : Ptr_vkCmdPipelineBarrier := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkEndCommandBuffer.html
  type Ptr_vkEndCommandBuffer is access function (commandBuffer : Ptr)  -- VkCommandBuffer                             
                                                  return Int_Unsigned_C -- VkResult 
                                                  with Convention => C;
  function To_Ptr_vkEndCommandBuffer is new Unchecked_Conversion (Ptr, Ptr_vkEndCommandBuffer);
  vkEndCommandBuffer : Ptr_vkEndCommandBuffer := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkQueueSubmit.html
  type Ptr_vkQueueSubmit is access function (queue       : Ptr;              -- VkQueue                                     
                                             submitCount : Int_Unsigned_C;   -- uint32_t                                    
                                             pSubmits    : Ptr_VkSubmitInfo; -- const VkSubmitInfo*
                                             fence       : Ptr)              -- VkFence
                                             return Int_Unsigned_C           -- VkResult
                                             with Convention => C;
  function To_Ptr_vkQueueSubmit is new Unchecked_Conversion (Ptr, Ptr_vkQueueSubmit);
  vkQueueSubmit : Ptr_vkQueueSubmit := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkQueueWaitIdle.html
  type Ptr_vkQueueWaitIdle is access function (queue : Ptr)          -- VkQueue
                                               return Int_Unsigned_C -- VkResult
                                               with Convention => C;
  function To_Ptr_vkQueueWaitIdle is new Unchecked_Conversion (Ptr, Ptr_vkQueueWaitIdle);
  vkQueueWaitIdle : Ptr_vkQueueWaitIdle := null; 

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkFreeCommandBuffers.html
  type Ptr_vkFreeCommandBuffers is access procedure (device      : Ptr;            -- VkDevice
                                                     pCreateInfo : Ptr;            -- VkCommandPool                               
                                                     pAllocator  : Int_Unsigned_C; -- uint32_t                                    
                                                     pSemaphore  : Ptr_Ptr)        -- VkCommandBuffer*
                                                     with Convention => C;
  function To_Ptr_vkFreeCommandBuffers is new Unchecked_Conversion (Ptr, Ptr_vkFreeCommandBuffers);
  vkFreeCommandBuffers : Ptr_vkFreeCommandBuffers := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateSemaphore.html
  type Ptr_vkCreateSemaphore is access function (device      : Ptr;                       -- VkDevice                                    
                                                 pCreateInfo : Ptr_VkSemaphoreCreateInfo; -- const VkSemaphoreCreateInfo* 
                                                 pAllocator  : Ptr;                       -- const VkAllocationCallbacks* 
                                                 pSemaphore  : Ptr_Ptr)                   -- VkSemaphore*
                                                 return Int_Unsigned_C                    -- VkResult
                                                 with Convention => C;
  function To_Ptr_vkCreateSemaphore is new Unchecked_Conversion (Ptr, Ptr_vkCreateSemaphore);
  vkCreateSemaphore : Ptr_vkCreateSemaphore := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDeviceWaitIdle.html
  type Ptr_vkDeviceWaitIdle is access function (device : Ptr)          -- VkDevice
                                                return Int_Unsigned_C; -- VkResult
  function To_Ptr_vkDeviceWaitIdle is new Unchecked_Conversion (Ptr, Ptr_vkDeviceWaitIdle);
  vkDeviceWaitIdle : Ptr_vkDeviceWaitIdle := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroySemaphore.html
  type Ptr_vkDestroySemaphore is access procedure (device     : Ptr;                       -- VkDevice                                    
                                                   semaphore  : Ptr;                       -- VkSemaphore                                 
                                                   pAllocator : Ptr_VkAllocationCallbacks) -- const VkAllocationCallbacks* 
                                                   with Convention => C;
  function To_Ptr_vkDestroySemaphore is new Unchecked_Conversion (Ptr, Ptr_vkDestroySemaphore);
  vkDestroySemaphore : Ptr_vkDestroySemaphore := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyCommandPool.html
  type Ptr_vkDestroyCommandPool is access procedure (device      : Ptr;                       -- VkDevice
                                                     commandPool : Ptr;                       -- VkCommandPool
                                                     pAllocator  : Ptr_VkAllocationCallbacks) -- const VkAllocationCallbacks*    
                                                     with Convention => C;
  function To_Ptr_vkDestroyCommandPool is new Unchecked_Conversion (Ptr, Ptr_vkDestroyCommandPool);
  vkDestroyCommandPool : Ptr_vkDestroyCommandPool := null;

  -- http://nopper.tv/Vulkan/1.0/vkDestroySwapchainKHR.html
  type Ptr_vkDestroySwapchainKHR is access procedure (device     : Ptr;                       -- VkDevice 
                                                      swapchain  : Ptr;                       -- VkSwapchainKHR
                                                      pAllocator : Ptr_VkAllocationCallbacks) -- const VkAllocationCallbacks*
                                                      with Convention => C;
  function To_Ptr_vkDestroySwapchainKHR is new Unchecked_Conversion (Ptr, Ptr_vkDestroySwapchainKHR);
  vkDestroySwapchainKHR : Ptr_vkDestroySwapchainKHR := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyDevice.html
  type Ptr_vkDestroyDevice is access procedure (device     : Ptr; -- VkDevice                                    
                                                pAllocator : Ptr) -- const VkAllocationCallbacks*
                                                with Convention => C;
  function To_Ptr_vkDestroyDevice is new Unchecked_Conversion (Ptr,  Ptr_vkDestroyDevice);
  vkDestroyDevice : Ptr_vkDestroyDevice := null; 

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyInstance.html
  type Ptr_vkDestroyInstance is access procedure (instance   : Ptr; -- VkInstance
                                                  pAllocator : Ptr) -- const VkAllocationCallbacks*
                                                  with Convention => C;
  function To_Ptr_vkDestroyInstance is new Unchecked_Conversion (Ptr, Ptr_vkDestroyInstance);
  vkDestroyInstance : Ptr_vkDestroyInstance := null;

  -- http://web.archive.org/web/20160608223244/https://www.khronos.org/registry/vulkan/specs/1.0-wsi_extensions/xhtml/vkspec.html#vkAcquireNextImageKHR
  type Ptr_vkAcquireNextImageKHR is access function (device      : Ptr;                -- VkDevice                                    
                                                     swapchain   : Ptr;                -- VkSwapchainKHR                              
                                                     timeout     : Int_64_Unsigned_C;  -- uint64_t                                    
                                                     semaphore   : Ptr;                -- VkSemaphore                                 
                                                     fence       : Ptr;                -- VkFence                                     
                                                     pImageIndex : Ptr_Int_Unsigned_C) -- uint32_t*  
                                                     return Int_Unsigned_C             -- VkResult
                                                     with Convention => C;
  function To_Ptr_vkAcquireNextImageKHR is new Unchecked_Conversion (Ptr, Ptr_vkAcquireNextImageKHR);
  vkAcquireNextImageKHR : Ptr_vkAcquireNextImageKHR := null;

  -- http://nopper.tv/Vulkan/1.0/vkQueuePresentKHR.html
  type Ptr_vkQueuePresentKHR is access function (queue        : Ptr;                  -- VkQueue
                                                 pPresentInfo : Ptr_VkPresentInfoKHR) -- const VkPresentInfoKHR*
                                                 return Int_Unsigned_C                -- VkResult
                                                 with Convention => C;
  function To_Ptr_vkQueuePresentKHR is new Unchecked_Conversion (Ptr, Ptr_vkQueuePresentKHR);
  vkQueuePresentKHR : Ptr_vkQueuePresentKHR := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkWaitForFences.html
  type Ptr_vkWaitForFences is access function (device     : Ptr;               -- VkDevice
                                               fenceCount : Int_Unsigned_C;    -- uint32_t
                                               pFences    : Ptr;               -- const VkFence* 
                                               waitAll    : Int_Unsigned_C;    -- VkBool32
                                               timeout    : Int_64_Unsigned_C) -- uint64_t
                                               return Int_Unsigned_C           -- VkResult
                                               with Convention => C;
  function To_Ptr_vkWaitForFences is new Unchecked_Conversion (Ptr, Ptr_vkWaitForFences);
  vkWaitForFences : Ptr_vkWaitForFences := null; 

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkResetFences.html
  type Ptr_vkResetFences is access function (device     : Ptr;            -- VkDevice
                                             fenceCount : Int_Unsigned_C; -- uint32_t
                                             pFences    : Ptr)            -- const VkFence*  
                                             return Int_Unsigned_C        -- VkResul
                                             with Convention => C;
  function To_Ptr_vkResetFences is new Unchecked_Conversion (Ptr, Ptr_vkResetFences);
  vkResetFences : Ptr_vkResetFences := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetFenceStatus.html
  type Ptr_vkGetFenceStatus is access function (device : Ptr;         -- VkDevice
                                                fence  : Ptr)         -- VkFence
                                                return Int_Unsigned_C -- VkResult
                                                with Convention => C; 
  function To_Ptr_vkGetFenceStatus is new Unchecked_Conversion (Ptr, Ptr_vkGetFenceStatus);
  vkGetFenceStatus : Ptr_vkGetFenceStatus := null; 

  -- http://web.archive.org/web/https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyFence.html
  type Ptr_vkDestroyFence is access procedure (device     : Ptr; -- VkDevice
                                               fence      : Ptr; -- VkFence
                                               pAllocator : Ptr) -- const VkAllocationCallbacks*
                                               with Convention => C;
  function To_Ptr_vkDestroyFence is new Unchecked_Conversion (Ptr, Ptr_vkDestroyFence);
  vkDestroyFence : Ptr_vkDestroyFence := null;

  -- http://web.archive.org/web/20160324124456/https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateFence.html
  type Ptr_vkCreateFence is access function (device      : Ptr;    -- VkDevice
                                             pCreateInfo : Ptr;    -- const VkFenceCreateInfo*
                                             pAllocator  : Ptr;    -- const VkAllocationCallbacks*
                                             pFence      : Ptr)    -- VkFence*
                                             return Int_Unsigned_C -- VkResult 
                                             with Convention => C;
  function To_Ptr_vkCreateFence is new Unchecked_Conversion (Ptr, Ptr_vkCreateFence);  
  vkCreateFence : Ptr_vkCreateFence := null;
    
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDraw.html
  type Ptr_vkCmdDraw is access procedure (commandBuffer     : Ptr; -- VkCommandBuffer
                                               vertexCount      : Int_Unsigned_C; -- uint32_t
                                               instanceCount      : Int_Unsigned_C; -- uint32_t
                                               firstVertex      : Int_Unsigned_C; -- uint32_t
                                               firstInstance      : Int_Unsigned_C) -- uint32_t
                                               with Convention => C;
  function To_Ptr_vkCmdDraw is new Unchecked_Conversion (Ptr, Ptr_vkCmdDraw);
  vkCmdDraw : Ptr_vkCmdDraw := null;
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdEndRenderPass.html
  type Ptr_vkCmdEndRenderPass is access procedure (commandBuffer : Ptr) -- VkCommandBuffer
                                                   with Convention => C;
  function To_Ptr_vkCmdEndRenderPass is new Unchecked_Conversion (Ptr, Ptr_vkCmdEndRenderPass);
  vkCmdEndRenderPass : Ptr_vkCmdEndRenderPass := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDrawIndexed.html
  type Ptr_vkCmdDrawIndexed is access procedure (commandBuffer : Ptr;            -- VkCommandBuffer
                                                 indexCount    : Int_Unsigned_C; -- uint32_t
                                                 instanceCount : Int_Unsigned_C; -- uint32_t
                                                 firstIndex    : Int_Unsigned_C; -- uint32_t
                                                 vertexOffset  : Int_C;          -- int32_t
                                                 firstInstance : Int_Unsigned_C) -- uint32_t
                                                 with Convention => C;
  function To_Ptr_vkCmdDrawIndexed is new Unchecked_Conversion(Ptr, Ptr_vkCmdDrawIndexed);
  vkCmdDrawIndexed : Ptr_vkCmdDrawIndexed := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdBindDescriptorSets.html
  type Ptr_vkCmdBindDescriptorSets is access procedure (commandBuffer      : Ptr;                -- VkCommandBuffer
                                                        pipelineBindPoint  : Int_Unsigned_C;     -- VkPipelineBindPoint
                                                        layout             : Ptr;                -- VkPipelineLayout
                                                        firstSet           : Int_Unsigned_C;     -- uint32_t
                                                        descriptorSetCount : Int_Unsigned_C;     -- uint32_t
                                                        pDescriptorSets    : Ptr_Ptr;            -- const VkDescriptorSet*
                                                        dynamicOffsetCount : Int_Unsigned_C;     -- uint32_t
                                                        pDynamicOffsets    : Ptr_Int_Unsigned_C) -- const uint32_t*
                                                        with Convention => C;
  function To_Ptr_vkCmdBindDescriptorSets is new Unchecked_Conversion(Ptr, Ptr_vkCmdBindDescriptorSets);
  vkCmdBindDescriptorSets : Ptr_vkCmdBindDescriptorSets := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdBindIndexBuffer.html
  type Ptr_vkCmdBindIndexBuffer is access procedure (commandBuffer : Ptr;               -- VkCommandBuffer
                                                     buffer        : Ptr;               -- VkBuffer
                                                     offset        : Int_64_Unsigned_C; -- VkDeviceSize
                                                     indexType     : Int_64_Unsigned_C) -- VkDeviceSize
                                                     with Convention => C;
  function To_Ptr_vkCmdBindIndexBuffer is new Unchecked_Conversion(Ptr, Ptr_vkCmdBindIndexBuffer);
  vkCmdBindIndexBuffer : Ptr_vkCmdBindIndexBuffer := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdBindVertexBuffers.html
  type Ptr_vkCmdBindVertexBuffers is access procedure (commandBuffer : Ptr;            -- VkCommandBuffer 
                                                       firstBinding  : Int_Unsigned_C; -- uint32_t
                                                       bindingCount  : Int_Unsigned_C; -- uint32_t
                                                       pBuffers      : Ptr;            -- const VkBuffer*
                                                       pOffsets      : Ptr)            -- const VkDeviceSize*
                                                       with Convention => C;
  function To_Ptr_vkCmdBindVertexBuffers is new Unchecked_Conversion(Ptr, Ptr_vkCmdBindVertexBuffers);
  vkCmdBindVertexBuffers : Ptr_vkCmdBindVertexBuffers;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdBindPipeline.html
  type Ptr_vkCmdBindPipeline is access procedure (commandBuffer     : Ptr;            -- VkCommandBuffer
                                                  pipelineBindPoint : Int_Unsigned_C; -- VkPipelineBindPoint
                                                  pipeline          : Ptr)            -- VkPipeline
                                                  with Convention => C;
  function To_Ptr_vkCmdBindPipeline is new Unchecked_Conversion(Ptr, Ptr_vkCmdBindPipeline);
  vkCmdBindPipeline : Ptr_vkCmdBindPipeline := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdBeginRenderPass.html
  type Ptr_vkCmdBeginRenderPass is access procedure (commandBuffer    : Ptr;                       -- VkCommandBuffer
                                                     pRenderPassBegin : Ptr_VkRenderPassBeginInfo; -- const VkRenderPassBeginInfo*
                                                     contents         : Int_Unsigned_C)            -- VkSubpassContents
                                                     with Convention => C;
  function To_Ptr_vkCmdBeginRenderPass is new Unchecked_Conversion(Ptr, Ptr_vkCmdBeginRenderPass);
  vkCmdBeginRenderPass : Ptr_vkCmdBeginRenderPass := null;
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateImage.html
  type Ptr_vkCreateImage is access function (device      : Ptr;                       -- VkDevice
                                             pCreateInfo : Ptr_VkImageCreateInfo;     -- const VkImageCreateInfo*
                                             pAllocator  : Ptr_VkAllocationCallbacks; -- const VkAllocationCallbacks*
                                             pImage      : Ptr_Ptr)                   -- VkImage*
                                             return Int_Unsigned_C                    -- VkResult 
                                             with Convention => C;
  function To_Ptr_vkCreateImage is new Unchecked_Conversion (Ptr, Ptr_vkCreateImage);
  vkCreateImage : Ptr_vkCreateImage := null;
   
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetImageMemoryRequirements.html
  type Ptr_vkGetImageMemoryRequirements is access procedure (device              : Ptr;                      -- VkDevice
                                                             image               : Ptr;                      -- VkImage
                                                             pMemoryRequirements : Ptr_VkMemoryRequirements) -- VkMemoryRequirements*
                                                             with Convention => C;
  function To_Ptr_vkGetImageMemoryRequirements is new Unchecked_Conversion (Ptr, Ptr_vkGetImageMemoryRequirements);
  vkGetImageMemoryRequirements : Ptr_vkGetImageMemoryRequirements := null;
   
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkAllocateMemory.html
  type Ptr_vkAllocateMemory is access function (device        : Ptr;                       -- VkDevice                                    
                                                pAllocateInfo : Ptr_VkMemoryAllocateInfo;  -- const VkMemoryAllocateInfo*
                                                pAllocator    : Ptr_VkAllocationCallbacks; -- const VkAllocationCallbacks*
                                                pMemory       : Ptr_Ptr)                   -- VkDeviceMemory*
                                                return Int_Unsigned_C                      -- VkResult
                                                with Convention => C;
  function To_Ptr_vkAllocateMemory is new Unchecked_Conversion (Ptr, Ptr_vkAllocateMemory);
  vkAllocateMemory : Ptr_vkAllocateMemory := null;
   
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkBindImageMemory.html
  type Ptr_vkBindImageMemory is access function (device       : Ptr;               -- VkDevice                                    
                                                 image        : Ptr;               -- VkImage                                     
                                                 memory       : Ptr;               -- VkDeviceMemory                                                                  
                                                 memoryOffset : Int_64_Unsigned_C) -- VkDeviceSize                                
                                                 return Int_Unsigned_C             -- VkResult
                                                 with Convention => C;
  function To_Ptr_vkBindImageMemory is new Unchecked_Conversion (Ptr, Ptr_vkBindImageMemory);
  vkBindImageMemory : Ptr_vkBindImageMemory := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdCopyBuffer.html
  type Ptr_vkCmdCopyBuffer is access procedure (commandBuffer : Ptr;              -- VkCommandBuffer
                                                srcBuffer     : Ptr;              -- VkBuffer
                                                dstBuffer     : Ptr;              -- VkBuffer
                                                regionCount   : Int_Unsigned_C;   -- uint32_t
                                                pRegions      : Ptr_VkBufferCopy) -- const VkBufferCopy*
                                                with Convention => C;
  function To_Ptr_vkCmdCopyBuffer is new Unchecked_Conversion (Ptr, Ptr_vkCmdCopyBuffer);
  vkCmdCopyBuffer : Ptr_vkCmdCopyBuffer := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyBuffer.html
  type Ptr_vkDestroyBuffer is access procedure (device     : Ptr;                       -- VkDevice
                                                buffer     : Ptr;                       -- VkBuffer
                                                pAllocator : Ptr_VkAllocationCallbacks) -- const VkAllocationCallbacks*
                                                with Convention => C;
  function To_Ptr_vkDestroyBuffer is new Unchecked_Conversion (Ptr, Ptr_vkDestroyBuffer);
  vkDestroyBuffer : Ptr_vkDestroyBuffer := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkFreeMemory.html
  type Ptr_vkFreeMemory is access procedure (device     : Ptr;                       -- VkDevice
                                             memory     : Ptr;                       -- VkDeviceMemory
                                             pAllocator : Ptr_VkAllocationCallbacks) -- const VkAllocationCallbacks*
                                             with Convention => C; 
  function To_Ptr_vkFreeMemory is new Unchecked_Conversion (Ptr, Ptr_vkFreeMemory);
  vkFreeMemory : Ptr_vkFreeMemory := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateRenderPass.html
  type Ptr_vkCreateRenderPass is access function (device      : Ptr;                        -- VkDevice
                                                  pCreateInfo : Ptr_VkRenderPassCreateInfo; -- const VkRenderPassCreateInfo*
                                                  pAllocator  : Ptr_VkAllocationCallbacks;  -- const VkAllocationCallbacks* 
                                                  pRenderPass : Ptr_Ptr)                    -- VkRenderPass*
                                                  return Int_Unsigned_C                     -- VkResult
                                                  with Convention => C; 
  function To_Ptr_vkCreateRenderPass is new Unchecked_Conversion (Ptr, Ptr_vkCreateRenderPass);
  vkCreateRenderPass : Ptr_vkCreateRenderPass := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreatePipelineLayout.html
  type Ptr_vkCreatePipelineLayout is access function (device          : Ptr;                            -- VkDevice
                                                      pCreateInfo     : Ptr_VkPipelineLayoutCreateInfo; -- const VkPipelineLayoutCreateInfo*
                                                      pAllocator      : Ptr_VkAllocationCallbacks;      -- const VkAllocationCallbacks*
                                                      pPipelineLayout : Ptr_Ptr)                        -- VkPipelineLayout*
                                                      return Int_Unsigned_C                             -- VkResult
                                                      with Convention => C; 
  function To_Ptr_vkCreatePipelineLayout is new Unchecked_Conversion (Ptr, Ptr_vkCreatePipelineLayout);
  vkCreatePipelineLayout : Ptr_vkCreatePipelineLayout := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateGraphicsPipelines.html
  type Ptr_vkCreateGraphicsPipelines is access function (device          : Ptr;                              -- VkDevice                                    
                                                         pipelineCache   : Ptr;                              -- VkPipelineCache                             
                                                         createInfoCount : Int_Unsigned_C;                   -- uint32_t                                    
                                                         pCreateInfos    : Ptr_VkGraphicsPipelineCreateInfo; -- const VkGraphicsPipelineCreateInfo*
                                                         pAllocator      : Ptr_VkAllocationCallbacks;        -- const VkAllocationCallbacks*
                                                         pPipelines      : Ptr_Ptr)                          -- VkPipeline* 
                                                         return Int_Unsigned_C                               -- VkResult
                                                         with Convention => C; 
  function To_Ptr_vkCreateGraphicsPipelines is new Unchecked_Conversion (Ptr, Ptr_vkCreateGraphicsPipelines);
  vkCreateGraphicsPipelines : Ptr_vkCreateGraphicsPipelines := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateDescriptorSetLayout.html
  type Ptr_vkCreateDescriptorSetLayout is access function (device      : Ptr;                                 -- VkDevice 
                                                           pCreateInfo : Ptr_VkDescriptorSetLayoutCreateInfo; -- const VkDescriptorSetLayoutCreateInfo*
                                                           pAllocator  : Ptr_VkAllocationCallbacks;           -- const VkAllocationCallbacks*
                                                           pSetLayout  : Ptr_Ptr)                             -- VkDescriptorSetLayout*
                                                           return Int_Unsigned_C                              -- VkResult
                                                           with Convention => C; 
  function To_Ptr_vkCreateDescriptorSetLayout is new Unchecked_Conversion (Ptr, Ptr_vkCreateDescriptorSetLayout);
  vkCreateDescriptorSetLayout : Ptr_vkCreateDescriptorSetLayout := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateDescriptorPool.html
  type Ptr_vkCreateDescriptorPool is access function (device          : Ptr;                            -- VkDevice
                                                      pCreateInfo     : Ptr_VkDescriptorPoolCreateInfo; -- const VkDescriptorPoolCreateInfo*
                                                      pAllocator      : Ptr_VkAllocationCallbacks;      -- const VkAllocationCallbacks*
                                                      pDescriptorPool : Ptr_Ptr)                        -- VkDescriptorPool*
                                                      return Int_Unsigned_C                             -- VkResult
                                                      with Convention => C; 
  function To_Ptr_vkCreateDescriptorPool is new Unchecked_Conversion (Ptr, Ptr_vkCreateDescriptorPool);
  vkCreateDescriptorPool : Ptr_vkCreateDescriptorPool := null;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkAllocateDescriptorSets.html
  type Ptr_vkAllocateDescriptorSets is access function (device          : Ptr;                -- VkDevice                                    
                                                        pAllocateInfo   : Ptr_Ptr;            -- const VkDescriptorSetAllocateInfo*
                                                        pDescriptorSets : Ptr_Int_Unsigned_C) -- VkDescriptorSet*
                                                        return Int_Unsigned_C                 -- VkResult
                                                        with Convention => C; 
  function To_Ptr_vkAllocateDescriptorSets is new Unchecked_Conversion (Ptr, Ptr_vkAllocateDescriptorSets);
  vkAllocateDescriptorSets : Ptr_vkAllocateDescriptorSets := null;

  -- https://manned.org/vkUpdateDescriptorSets/4ca27893
  type Ptr_vkUpdateDescriptorSets is access procedure (device               : Ptr;                           -- VkDevice                                    
                                                       descriptorWriteCount : Int_Unsigned_C;                -- uint32_t                                                     
                                                       pDescriptorWrites    : Ptr_VkCopyDescriptorSet;       -- const VkWriteDescriptorSet*                 
                                                       descriptorCopyCount  : Int_Unsigned_C;                -- uint32_t
                                                       pDescriptorCopies    : Ptr_Array_VkCopyDescriptorSet) -- const VkCopyDescriptorSet*
                                                       with Convention => C; 
  function To_Ptr_vkUpdateDescriptorSets is new Unchecked_Conversion (Ptr, Ptr_vkUpdateDescriptorSets);
  vkUpdateDescriptorSets : Ptr_vkUpdateDescriptorSets := null;
    
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceFormatProperties.html
  type Ptr_vkGetPhysicalDeviceFormatProperties is access procedure (physicalDevice    : Ptr;                    -- VkPhysicalDevice
                                                                    format            : Int_Unsigned_C;         -- VkFormat
                                                                    pFormatProperties : Ptr_VkFormatProperties) -- VkFormatProperties* 
                                                                    with Convention => C;
  function To_Ptr_vkGetPhysicalDeviceFormatProperties is new Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceFormatProperties);
  vkGetPhysicalDeviceFormatProperties : Ptr_vkGetPhysicalDeviceFormatProperties := null;
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkEnumerateDeviceExtensionProperties.html
  type Ptr_vkEnumerateDeviceExtensionProperties is access function (physicalDevice : Ptr;                             -- VkPhysicalDevice
                                                                    pLayerName     : Ptr_Str_8_C;                     -- const char*
                                                                    pPropertyCount : Ptr_Int_Unsigned_C;              -- uint32_t*
                                                                    pProperties    : Ptr_VkExtensionProperties) -- VkExtensionProperties* 
                                                                    return Int_Unsigned_C                             -- VkResult
                                                                    with Convention => C;
  function To_Ptr_vkEnumerateDeviceExtensionProperties is new Unchecked_Conversion (Ptr, Ptr_vkEnumerateDeviceExtensionProperties);
  vkEnumerateDeviceExtensionProperties : Ptr_vkEnumerateDeviceExtensionProperties := null;
end;

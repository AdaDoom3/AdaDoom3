
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

  -- VkDeviceSize                  Int_64_Unsigned_C
  -- uint64_t                      Int_64_Unsigned_C
  -- size_t                        Int_Size_C
  -- int32_t                       Int_32_Signed_C
  -- uint32_t                      Int_32_Unsigned_C
  -- VkBool32                      Int_32_Unsigned_C
  -- VkFormat                      Int_32_Unsigned_C
  -- VkResult                      Int_32_Unsigned_C
  -- VkSurfaceTransformFlagBitsKHR Int_32_Unsigned_C
  -- VkSurfaceTransformFlagsKHR    Int_32_Unsigned_C
  -- VkSemaphoreCreateFlagBits     Int_32_Unsigned_C
  -- VkCommandPoolCreateFlags      Int_32_Unsigned_C
  -- VkDeviceQueueCreateFlags      Int_32_Unsigned_C
  -- VkQueueFlags                  Int_32_Unsigned_C
  -- VkInstanceCreateFlags         Int_32_Unsigned_C
  -- VkInstanceCreateFlags         Int_32_Unsigned_C
  -- VkSampleCountFlags            Int_32_Unsigned_C
  -- VkPipelineStageFlags          Int_32_Unsigned_C
  -- VkMemoryPropertyFlags         Int_32_Unsigned_C
  -- VkMemoryHeapFlags             Int_32_Unsigned_C
  -- VkCommandBufferLevel          Int_32_Unsigned_C
  -- VkAccessFlags                 Int_32_Unsigned_C
  -- VkImageAspectFlagBits         Int_32_Unsigned_C
  -- VkImageUsageFlags             Int_32_Unsigned_C
  -- VkImageLayout                 Int_32_Unsigned_C
  -- vkDependencyFlags             Int_32_Unsigned_C
  -- VkFence                       Ptr
  -- VkImage                       Ptr
  -- VkQueue                       Ptr
  -- VkDevice                      Ptr
  -- VkInstance                    Ptr
  -- VkSemaphore                   Ptr
  -- VkRenderPass                  Ptr
  -- VkSurfaceKHR                  Ptr
  -- VkSwapchainKHR                Ptr
  -- VkPhysicalDevice              Ptr
  -- VkCommandPool                 Ptr
  -- VkCommandBuffer               Ptr
  -- VkPhysicalDeviceType          Ptr
  -- VkCompositeAlphaFlagBitsKHR   Int_32_Unsigned_C
  -- VkSharingMode                 Int_32_Unsigned_C
  -- VkColorSpaceKHR               Int_32_Unsigned_C
  -- VkSwapchainCreateFlagsKHR     Int_32_Unsigned_C
  -- VkPresentModeKHR              Int_32_Unsigned_C
  -- VkWin32SurfaceCreateFlagsKHR  Int_32_Unsigned_C
  -- VkQueueFlagBits               Int_32_Unsigned_C

  ---------------
  -- Constants --
  ---------------

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageMemoryBarrier.html
  VK_QUEUE_FAMILY_IGNORED : constant Int_32_Unsigned_C := 16#FFFF_FFFF#; -- (~0U)

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDependencyFlags.html
  VK_DEPENDENCY_BY_REGION_BIT : constant Int_32_Unsigned_C := 1; -- VkDependencyFlags

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryHeap.html
  VK_MEMORY_HEAP_DEVICE_LOCAL_BIT : constant Int_32_Unsigned_C := 1; -- VkMemoryHeapFlagBits

  -- http://nopper.tv/Vulkan/1.0/VkColorSpaceKHR.html
  VK_COLOR_SPACE_SRGB_NONLINEAR_KHR : constant Int_32_Unsigned_C := 0; -- VkColorSpaceKHR
  VK_COLORSPACE_SRGB_NONLINEAR_KHR  : constant Int_32_Unsigned_C := 0; -- VkColorSpaceKHR

  -- http://vulkan-spec-chunked.ahcox.com/apes09.html
  VK_KHR_SURFACE_EXTENSION_NAME       : constant Str_8_C := "VK_KHR_surface";       -- ???
  VK_KHR_WIN32_SURFACE_EXTENSION_NAME : constant Str_8_C := "VK_KHR_win32_surface"; -- ???

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSharingMode.html
  VK_SHARING_MODE_EXCLUSIVE  : constant Int_32_Unsigned_C := 0; -- VkSharingMode
  VK_SHARING_MODE_CONCURRENT : constant Int_32_Unsigned_C := 1; -- VkSharingMode

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandBufferLevel.html
  VK_COMMAND_BUFFER_LEVEL_PRIMARY   : constant Int_32_Unsigned_C := 0; -- VkCommandBufferLevel
  VK_COMMAND_BUFFER_LEVEL_SECONDARY : constant Int_32_Unsigned_C := 1; -- VkCommandBufferLevel

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandPoolCreateFlagBits.html
  VK_COMMAND_POOL_CREATE_TRANSIENT_BIT            : constant Int_32_Unsigned_C := 1; -- VkCommandPoolCreateFlagBits
  VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT : constant Int_32_Unsigned_C := 2; -- VkCommandPoolCreateFlagBits

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandBufferUsageFlagBits.html
  VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT      : constant Int_32_Unsigned_C := 1; -- VkCommandBufferUsageFlagBits
  VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT : constant Int_32_Unsigned_C := 2; -- VkCommandBufferUsageFlagBits
  VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT     : constant Int_32_Unsigned_C := 4; -- VkCommandBufferUsageFlagBits
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkQueueFamilyProperties.html
  VK_QUEUE_GRAPHICS_BIT       : constant Int_32_Unsigned_C := 16#0000_0001#; -- VkQueueFlagBits
  VK_QUEUE_COMPUTE_BIT        : constant Int_32_Unsigned_C := 16#0000_0002#; -- VkQueueFlagBits
  VK_QUEUE_TRANSFER_BIT       : constant Int_32_Unsigned_C := 16#0000_0004#; -- VkQueueFlagBits
  VK_QUEUE_SPARSE_BINDING_BIT : constant Int_32_Unsigned_C := 16#0000_0008#; -- VkQueueFlagBits

  -- http://nopper.tv/Vulkan/1.0/VkCompositeAlphaFlagBitsKHR.html
  VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR          : constant Int_32_Unsigned_C := 1; -- VkCompositeAlphaFlagBitsKHR
  VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR  : constant Int_32_Unsigned_C := 2; -- VkCompositeAlphaFlagBitsKHR
  VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR : constant Int_32_Unsigned_C := 4; -- VkCompositeAlphaFlagBitsKHR
  VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR         : constant Int_32_Unsigned_C := 8; -- VkCompositeAlphaFlagBitsKHR

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageAspectFlagBits.html
  VK_IMAGE_ASPECT_COLOR_BIT    : constant Int_32_Unsigned_C := 1; -- VkImageAspectFlagBits
  VK_IMAGE_ASPECT_DEPTH_BIT    : constant Int_32_Unsigned_C := 2; -- VkImageAspectFlagBits
  VK_IMAGE_ASPECT_STENCIL_BIT  : constant Int_32_Unsigned_C := 4; -- VkImageAspectFlagBits
  VK_IMAGE_ASPECT_METADATA_BIT : constant Int_32_Unsigned_C := 8; -- VkImageAspectFlagBits

  -- http://nopper.tv/Vulkan/1.0/VkPresentModeKHR.html
  VK_PRESENT_MODE_IMMEDIATE_KHR    : constant Int_32_Unsigned_C := 0; -- VkPresentModeKHR
  VK_PRESENT_MODE_MAILBOX_KHR      : constant Int_32_Unsigned_C := 1; -- VkPresentModeKHR
  VK_PRESENT_MODE_FIFO_KHR         : constant Int_32_Unsigned_C := 2; -- VkPresentModeKHR
  VK_PRESENT_MODE_FIFO_RELAXED_KHR : constant Int_32_Unsigned_C := 3; -- VkPresentModeKHR 

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceType.html
  VK_PHYSICAL_DEVICE_TYPE_OTHER          : constant Int_32_Unsigned_C := 0; -- VkPhysicalDeviceType
  VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU : constant Int_32_Unsigned_C := 1; -- VkPhysicalDeviceType
  VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU   : constant Int_32_Unsigned_C := 2; -- VkPhysicalDeviceType
  VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU    : constant Int_32_Unsigned_C := 3; -- VkPhysicalDeviceType
  VK_PHYSICAL_DEVICE_TYPE_CPU            : constant Int_32_Unsigned_C := 4; -- VkPhysicalDeviceType

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSampleCountFlagBits.html
  VK_SAMPLE_COUNT_1_BIT  : constant Int_32_Unsigned_C := 1;  -- VkSampleCountFlagBits
  VK_SAMPLE_COUNT_2_BIT  : constant Int_32_Unsigned_C := 2;  -- VkSampleCountFlagBits
  VK_SAMPLE_COUNT_4_BIT  : constant Int_32_Unsigned_C := 4;  -- VkSampleCountFlagBits
  VK_SAMPLE_COUNT_8_BIT  : constant Int_32_Unsigned_C := 8;  -- VkSampleCountFlagBits
  VK_SAMPLE_COUNT_16_BIT : constant Int_32_Unsigned_C := 16; -- VkSampleCountFlagBits
  VK_SAMPLE_COUNT_32_BIT : constant Int_32_Unsigned_C := 32; -- VkSampleCountFlagBits
  VK_SAMPLE_COUNT_64_BIT : constant Int_32_Unsigned_C := 64; -- VkSampleCountFlagBits

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryPropertyFlagBits.html
  VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT     : constant Int_32_Unsigned_C := 1;  -- VkMemoryPropertyFlagBits
  VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT     : constant Int_32_Unsigned_C := 2;  -- VkMemoryPropertyFlagBits
  VK_MEMORY_PROPERTY_HOST_COHERENT_BIT    : constant Int_32_Unsigned_C := 4;  -- VkMemoryPropertyFlagBits
  VK_MEMORY_PROPERTY_HOST_CACHED_BIT      : constant Int_32_Unsigned_C := 8;  -- VkMemoryPropertyFlagBits
  VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT : constant Int_32_Unsigned_C := 16; -- VkMemoryPropertyFlagBits

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageUsageFlagBits.html
  VK_IMAGE_USAGE_TRANSFER_SRC_BIT             : constant Int_32_Unsigned_C := 1;   -- VkImageUsageFlagBits
  VK_IMAGE_USAGE_TRANSFER_DST_BIT             : constant Int_32_Unsigned_C := 2;   -- VkImageUsageFlagBits
  VK_IMAGE_USAGE_SAMPLED_BIT                  : constant Int_32_Unsigned_C := 4;   -- VkImageUsageFlagBits
  VK_IMAGE_USAGE_STORAGE_BIT                  : constant Int_32_Unsigned_C := 8;   -- VkImageUsageFlagBits
  VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT         : constant Int_32_Unsigned_C := 16;  -- VkImageUsageFlagBits
  VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT : constant Int_32_Unsigned_C := 32;  -- VkImageUsageFlagBits
  VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT     : constant Int_32_Unsigned_C := 64;  -- VkImageUsageFlagBits
  VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT         : constant Int_32_Unsigned_C := 128; -- VkImageUsageFlagBits

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageLayout.html
  VK_IMAGE_LAYOUT_UNDEFINED                        : constant Int_32_Unsigned_C := 0;          -- VkImageLayout
  VK_IMAGE_LAYOUT_GENERAL                          : constant Int_32_Unsigned_C := 1;          -- VkImageLayout
  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL         : constant Int_32_Unsigned_C := 2;          -- VkImageLayout
  VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL : constant Int_32_Unsigned_C := 3;          -- VkImageLayout
  VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL  : constant Int_32_Unsigned_C := 4;          -- VkImageLayout
  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL         : constant Int_32_Unsigned_C := 5;          -- VkImageLayout
  VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL             : constant Int_32_Unsigned_C := 6;          -- VkImageLayout
  VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL             : constant Int_32_Unsigned_C := 7;          -- VkImageLayout
  VK_IMAGE_LAYOUT_PREINITIALIZED                   : constant Int_32_Unsigned_C := 8;          -- VkImageLayout
  VK_IMAGE_LAYOUT_PRESENT_SRC_KHR                  : constant Int_32_Unsigned_C := 1000001002; -- VkImageLayout

  -- http://nopper.tv/Vulkan/1.0/VkSurfaceTransformFlagBitsKHR.html
  VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR                     : constant Int_32_Unsigned_C := 1;   -- VkSurfaceTransformFlagBitsKHR
  VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR                    : constant Int_32_Unsigned_C := 2;   -- VkSurfaceTransformFlagBitsKHR
  VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR                   : constant Int_32_Unsigned_C := 4;   -- VkSurfaceTransformFlagBitsKHR
  VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR                   : constant Int_32_Unsigned_C := 8;   -- VkSurfaceTransformFlagBitsKHR
  VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR            : constant Int_32_Unsigned_C := 16;  -- VkSurfaceTransformFlagBitsKHR
  VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR  : constant Int_32_Unsigned_C := 32;  -- VkSurfaceTransformFlagBitsKHR
  VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR : constant Int_32_Unsigned_C := 64;  -- VkSurfaceTransformFlagBitsKHR
  VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR : constant Int_32_Unsigned_C := 128; -- VkSurfaceTransformFlagBitsKHR
  VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR                      : constant Int_32_Unsigned_C := 256; -- VkSurfaceTransformFlagBitsKHR

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkAccessFlagBits.html
  VK_ACCESS_INDIRECT_COMMAND_READ_BIT          : constant Int_32_Unsigned_C := 1;     -- VkAccessFlagBits
  VK_ACCESS_INDEX_READ_BIT                     : constant Int_32_Unsigned_C := 2;     -- VkAccessFlagBits
  VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT          : constant Int_32_Unsigned_C := 4;     -- VkAccessFlagBits
  VK_ACCESS_UNIFORM_READ_BIT                   : constant Int_32_Unsigned_C := 8;     -- VkAccessFlagBits
  VK_ACCESS_INPUT_ATTACHMENT_READ_BIT          : constant Int_32_Unsigned_C := 16;    -- VkAccessFlagBits
  VK_ACCESS_SHADER_READ_BIT                    : constant Int_32_Unsigned_C := 32;    -- VkAccessFlagBits
  VK_ACCESS_SHADER_WRITE_BIT                   : constant Int_32_Unsigned_C := 64;    -- VkAccessFlagBits
  VK_ACCESS_COLOR_ATTACHMENT_READ_BIT          : constant Int_32_Unsigned_C := 128;   -- VkAccessFlagBits
  VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT         : constant Int_32_Unsigned_C := 256;   -- VkAccessFlagBits
  VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT  : constant Int_32_Unsigned_C := 512;   -- VkAccessFlagBits
  VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT : constant Int_32_Unsigned_C := 1024;  -- VkAccessFlagBits
  VK_ACCESS_TRANSFER_READ_BIT                  : constant Int_32_Unsigned_C := 2048;  -- VkAccessFlagBits
  VK_ACCESS_TRANSFER_WRITE_BIT                 : constant Int_32_Unsigned_C := 4096;  -- VkAccessFlagBits
  VK_ACCESS_HOST_READ_BIT                      : constant Int_32_Unsigned_C := 8192;  -- VkAccessFlagBits
  VK_ACCESS_HOST_WRITE_BIT                     : constant Int_32_Unsigned_C := 16384; -- VkAccessFlagBits
  VK_ACCESS_MEMORY_READ_BIT                    : constant Int_32_Unsigned_C := 32768; -- VkAccessFlagBits
  VK_ACCESS_MEMORY_WRITE_BIT                   : constant Int_32_Unsigned_C := 65536; -- VkAccessFlagBits

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineStageFlagBits.html
  VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT                    : constant Int_32_Unsigned_C := 16#0000_0001#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT                  : constant Int_32_Unsigned_C := 16#0000_0002#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_VERTEX_INPUT_BIT                   : constant Int_32_Unsigned_C := 16#0000_0004#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_VERTEX_SHADER_BIT                  : constant Int_32_Unsigned_C := 16#0000_0008#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT    : constant Int_32_Unsigned_C := 16#0000_0010#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT : constant Int_32_Unsigned_C := 16#0000_0020#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT                : constant Int_32_Unsigned_C := 16#0000_0040#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT                : constant Int_32_Unsigned_C := 16#0000_0080#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT           : constant Int_32_Unsigned_C := 16#0000_0100#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT            : constant Int_32_Unsigned_C := 16#0000_0200#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT        : constant Int_32_Unsigned_C := 16#0000_0400#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT                 : constant Int_32_Unsigned_C := 16#0000_0800#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_TRANSFER_BIT                       : constant Int_32_Unsigned_C := 16#0000_1000#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT                 : constant Int_32_Unsigned_C := 16#0000_2000#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_HOST_BIT                           : constant Int_32_Unsigned_C := 16#0000_4000#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT                   : constant Int_32_Unsigned_C := 16#0000_8000#; -- VkPipelineStageFlagBits
  VK_PIPELINE_STAGE_ALL_COMMANDS_BIT                   : constant Int_32_Unsigned_C := 16#0001_0000#; -- VkPipelineStageFlagBits 

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkResult.html
  VK_SUCCESS                        : constant Int_32_Unsigned_C := 0;                                  -- VkResult
  VK_NOT_READY                      : constant Int_32_Unsigned_C := 1;                                  -- VkResult
  VK_TIMEOUT                        : constant Int_32_Unsigned_C := 2;                                  -- VkResult
  VK_EVENT_SET                      : constant Int_32_Unsigned_C := 3;                                  -- VkResult
  VK_EVENT_RESET                    : constant Int_32_Unsigned_C := 4;                                  -- VkResult
  VK_INCOMPLETE                     : constant Int_32_Unsigned_C := 5;                                  -- VkResult
  VK_ERROR_OUT_OF_HOST_MEMORY       : constant Int_32_Unsigned_C := To_Int_32_Unsigned_C (-1);          -- VkResult
  VK_ERROR_OUT_OF_DEVICE_MEMORY     : constant Int_32_Unsigned_C := To_Int_32_Unsigned_C (-2);          -- VkResult
  VK_ERROR_INITIALIZATION_FAILED    : constant Int_32_Unsigned_C := To_Int_32_Unsigned_C (-3);          -- VkResult
  VK_ERROR_DEVICE_LOST              : constant Int_32_Unsigned_C := To_Int_32_Unsigned_C (-4);          -- VkResult
  VK_ERROR_MEMORY_MAP_FAILED        : constant Int_32_Unsigned_C := To_Int_32_Unsigned_C (-5);          -- VkResult
  VK_ERROR_LAYER_NOT_PRESENT        : constant Int_32_Unsigned_C := To_Int_32_Unsigned_C (-6);          -- VkResult
  VK_ERROR_EXTENSION_NOT_PRESENT    : constant Int_32_Unsigned_C := To_Int_32_Unsigned_C (-7);          -- VkResult
  VK_ERROR_FEATURE_NOT_PRESENT      : constant Int_32_Unsigned_C := To_Int_32_Unsigned_C (-8);          -- VkResult
  VK_ERROR_INCOMPATIBLE_DRIVER      : constant Int_32_Unsigned_C := To_Int_32_Unsigned_C (-9);          -- VkResult
  VK_ERROR_TOO_MANY_OBJECTS         : constant Int_32_Unsigned_C := To_Int_32_Unsigned_C (-10);         -- VkResult
  VK_ERROR_FORMAT_NOT_SUPPORTED     : constant Int_32_Unsigned_C := To_Int_32_Unsigned_C (-11);         -- VkResult
  VK_ERROR_SURFACE_LOST_KHR         : constant Int_32_Unsigned_C := To_Int_32_Unsigned_C (-1000000000); -- VkResult
  VK_ERROR_NATIVE_WINDOW_IN_USE_KHR : constant Int_32_Unsigned_C := To_Int_32_Unsigned_C (-1000000001); -- VkResult
  VK_SUBOPTIMAL_KHR                 : constant Int_32_Unsigned_C := 1000001003;                         -- VkResult
  VK_ERROR_OUT_OF_DATE_KHR          : constant Int_32_Unsigned_C := To_Int_32_Unsigned_C (-1000001004); -- VkResult
  VK_ERROR_INCOMPATIBLE_DISPLAY_KHR : constant Int_32_Unsigned_C := To_Int_32_Unsigned_C (-1000003001); -- VkResult
  VK_ERROR_VALIDATION_FAILED_EXT    : constant Int_32_Unsigned_C := To_Int_32_Unsigned_C (-1000011001); -- VkResult
  VK_ERROR_INVALID_SHADER_NV        : constant Int_32_Unsigned_C := To_Int_32_Unsigned_C (-1000012000); -- VkResult

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkStructureType.html
  VK_STRUCTURE_TYPE_APPLICATION_INFO                          : constant Int_32_Unsigned_C := 0;          -- VkStructureType 
  VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO                      : constant Int_32_Unsigned_C := 1;          -- VkStructureType 
  VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO                  : constant Int_32_Unsigned_C := 2;          -- VkStructureType 
  VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO                        : constant Int_32_Unsigned_C := 3;          -- VkStructureType 
  VK_STRUCTURE_TYPE_SUBMIT_INFO                               : constant Int_32_Unsigned_C := 4;          -- VkStructureType 
  VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO                      : constant Int_32_Unsigned_C := 5;          -- VkStructureType 
  VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE                       : constant Int_32_Unsigned_C := 6;          -- VkStructureType 
  VK_STRUCTURE_TYPE_BIND_SPARSE_INFO                          : constant Int_32_Unsigned_C := 7;          -- VkStructureType 
  VK_STRUCTURE_TYPE_FENCE_CREATE_INFO                         : constant Int_32_Unsigned_C := 8;          -- VkStructureType 
  VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO                     : constant Int_32_Unsigned_C := 9;          -- VkStructureType 
  VK_STRUCTURE_TYPE_EVENT_CREATE_INFO                         : constant Int_32_Unsigned_C := 10;         -- VkStructureType 
  VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO                    : constant Int_32_Unsigned_C := 11;         -- VkStructureType 
  VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO                        : constant Int_32_Unsigned_C := 12;         -- VkStructureType 
  VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO                   : constant Int_32_Unsigned_C := 13;         -- VkStructureType 
  VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO                         : constant Int_32_Unsigned_C := 14;         -- VkStructureType 
  VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO                    : constant Int_32_Unsigned_C := 15;         -- VkStructureType 
  VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO                 : constant Int_32_Unsigned_C := 16;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO                : constant Int_32_Unsigned_C := 17;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO         : constant Int_32_Unsigned_C := 18;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO   : constant Int_32_Unsigned_C := 19;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO : constant Int_32_Unsigned_C := 20;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO   : constant Int_32_Unsigned_C := 21;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO       : constant Int_32_Unsigned_C := 22;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO  : constant Int_32_Unsigned_C := 23;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO    : constant Int_32_Unsigned_C := 24;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO  : constant Int_32_Unsigned_C := 25;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO    : constant Int_32_Unsigned_C := 26;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO        : constant Int_32_Unsigned_C := 27;         -- VkStructureType 
  VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO             : constant Int_32_Unsigned_C := 28;         -- VkStructureType 
  VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO              : constant Int_32_Unsigned_C := 29;         -- VkStructureType 
  VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO               : constant Int_32_Unsigned_C := 30;         -- VkStructureType 
  VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO                       : constant Int_32_Unsigned_C := 31;         -- VkStructureType 
  VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO         : constant Int_32_Unsigned_C := 32;         -- VkStructureType 
  VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO               : constant Int_32_Unsigned_C := 33;         -- VkStructureType 
  VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO              : constant Int_32_Unsigned_C := 34;         -- VkStructureType 
  VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET                      : constant Int_32_Unsigned_C := 35;         -- VkStructureType 
  VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET                       : constant Int_32_Unsigned_C := 36;         -- VkStructureType 
  VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO                   : constant Int_32_Unsigned_C := 37;         -- VkStructureType 
  VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO                   : constant Int_32_Unsigned_C := 38;         -- VkStructureType 
  VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO                  : constant Int_32_Unsigned_C := 39;         -- VkStructureType 
  VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO              : constant Int_32_Unsigned_C := 40;         -- VkStructureType 
  VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO           : constant Int_32_Unsigned_C := 41;         -- VkStructureType 
  VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO                 : constant Int_32_Unsigned_C := 42;         -- VkStructureType 
  VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO                    : constant Int_32_Unsigned_C := 43;         -- VkStructureType 
  VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER                     : constant Int_32_Unsigned_C := 44;         -- VkStructureType 
  VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER                      : constant Int_32_Unsigned_C := 45;         -- VkStructureType 
  VK_STRUCTURE_TYPE_MEMORY_BARRIER                            : constant Int_32_Unsigned_C := 46;         -- VkStructureType 
  VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO               : constant Int_32_Unsigned_C := 47;         -- VkStructureType 
  VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO                 : constant Int_32_Unsigned_C := 48;         -- VkStructureType 
  VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR                 : constant Int_32_Unsigned_C := 1000001000; -- VkStructureType 
  VK_STRUCTURE_TYPE_PRESENT_INFO_KHR                          : constant Int_32_Unsigned_C := 1000001001; -- VkStructureType 
  VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR              : constant Int_32_Unsigned_C := 1000002000; -- VkStructureType 
  VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR           : constant Int_32_Unsigned_C := 1000002001; -- VkStructureType 
  VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR                  : constant Int_32_Unsigned_C := 1000003000; -- VkStructureType 
  VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR              : constant Int_32_Unsigned_C := 1000004000; -- VkStructureType 
  VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR               : constant Int_32_Unsigned_C := 1000005000; -- VkStructureType 
  VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR           : constant Int_32_Unsigned_C := 1000006000; -- VkStructureType 
  VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR               : constant Int_32_Unsigned_C := 1000007000; -- VkStructureType 
  VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR           : constant Int_32_Unsigned_C := 1000008000; -- VkStructureType 
  VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR             : constant Int_32_Unsigned_C := 1000009000; -- VkStructureType 
  VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT     : constant Int_32_Unsigned_C := 1000011000; -- VkStructureType

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFormat.html
  VK_FORMAT_UNDEFINED                  : constant Int_32_Unsigned_C := 0;   -- VkFormat
  VK_FORMAT_R4G4_UNORM_PACK8           : constant Int_32_Unsigned_C := 1;   -- VkFormat
  VK_FORMAT_R4G4B4A4_UNORM_PACK16      : constant Int_32_Unsigned_C := 2;   -- VkFormat
  VK_FORMAT_B4G4R4A4_UNORM_PACK16      : constant Int_32_Unsigned_C := 3;   -- VkFormat
  VK_FORMAT_R5G6B5_UNORM_PACK16        : constant Int_32_Unsigned_C := 4;   -- VkFormat
  VK_FORMAT_B5G6R5_UNORM_PACK16        : constant Int_32_Unsigned_C := 5;   -- VkFormat
  VK_FORMAT_R5G5B5A1_UNORM_PACK16      : constant Int_32_Unsigned_C := 6;   -- VkFormat
  VK_FORMAT_B5G5R5A1_UNORM_PACK16      : constant Int_32_Unsigned_C := 7;   -- VkFormat
  VK_FORMAT_A1R5G5B5_UNORM_PACK16      : constant Int_32_Unsigned_C := 8;   -- VkFormat
  VK_FORMAT_R8_UNORM                   : constant Int_32_Unsigned_C := 9;   -- VkFormat
  VK_FORMAT_R8_SNORM                   : constant Int_32_Unsigned_C := 10;  -- VkFormat
  VK_FORMAT_R8_USCALED                 : constant Int_32_Unsigned_C := 11;  -- VkFormat
  VK_FORMAT_R8_SSCALED                 : constant Int_32_Unsigned_C := 12;  -- VkFormat
  VK_FORMAT_R8_UINT                    : constant Int_32_Unsigned_C := 13;  -- VkFormat
  VK_FORMAT_R8_SINT                    : constant Int_32_Unsigned_C := 14;  -- VkFormat
  VK_FORMAT_R8_SRGB                    : constant Int_32_Unsigned_C := 15;  -- VkFormat
  VK_FORMAT_R8G8_UNORM                 : constant Int_32_Unsigned_C := 16;  -- VkFormat
  VK_FORMAT_R8G8_SNORM                 : constant Int_32_Unsigned_C := 17;  -- VkFormat
  VK_FORMAT_R8G8_USCALED               : constant Int_32_Unsigned_C := 18;  -- VkFormat
  VK_FORMAT_R8G8_SSCALED               : constant Int_32_Unsigned_C := 19;  -- VkFormat
  VK_FORMAT_R8G8_UINT                  : constant Int_32_Unsigned_C := 20;  -- VkFormat
  VK_FORMAT_R8G8_SINT                  : constant Int_32_Unsigned_C := 21;  -- VkFormat
  VK_FORMAT_R8G8_SRGB                  : constant Int_32_Unsigned_C := 22;  -- VkFormat
  VK_FORMAT_R8G8B8_UNORM               : constant Int_32_Unsigned_C := 23;  -- VkFormat
  VK_FORMAT_R8G8B8_SNORM               : constant Int_32_Unsigned_C := 24;  -- VkFormat
  VK_FORMAT_R8G8B8_USCALED             : constant Int_32_Unsigned_C := 25;  -- VkFormat
  VK_FORMAT_R8G8B8_SSCALED             : constant Int_32_Unsigned_C := 26;  -- VkFormat
  VK_FORMAT_R8G8B8_UINT                : constant Int_32_Unsigned_C := 27;  -- VkFormat
  VK_FORMAT_R8G8B8_SINT                : constant Int_32_Unsigned_C := 28;  -- VkFormat
  VK_FORMAT_R8G8B8_SRGB                : constant Int_32_Unsigned_C := 29;  -- VkFormat
  VK_FORMAT_B8G8R8_UNORM               : constant Int_32_Unsigned_C := 30;  -- VkFormat
  VK_FORMAT_B8G8R8_SNORM               : constant Int_32_Unsigned_C := 31;  -- VkFormat
  VK_FORMAT_B8G8R8_USCALED             : constant Int_32_Unsigned_C := 32;  -- VkFormat
  VK_FORMAT_B8G8R8_SSCALED             : constant Int_32_Unsigned_C := 33;  -- VkFormat
  VK_FORMAT_B8G8R8_UINT                : constant Int_32_Unsigned_C := 34;  -- VkFormat
  VK_FORMAT_B8G8R8_SINT                : constant Int_32_Unsigned_C := 35;  -- VkFormat
  VK_FORMAT_B8G8R8_SRGB                : constant Int_32_Unsigned_C := 36;  -- VkFormat
  VK_FORMAT_R8G8B8A8_UNORM             : constant Int_32_Unsigned_C := 37;  -- VkFormat
  VK_FORMAT_R8G8B8A8_SNORM             : constant Int_32_Unsigned_C := 38;  -- VkFormat
  VK_FORMAT_R8G8B8A8_USCALED           : constant Int_32_Unsigned_C := 39;  -- VkFormat
  VK_FORMAT_R8G8B8A8_SSCALED           : constant Int_32_Unsigned_C := 40;  -- VkFormat
  VK_FORMAT_R8G8B8A8_UINT              : constant Int_32_Unsigned_C := 41;  -- VkFormat
  VK_FORMAT_R8G8B8A8_SINT              : constant Int_32_Unsigned_C := 42;  -- VkFormat
  VK_FORMAT_R8G8B8A8_SRGB              : constant Int_32_Unsigned_C := 43;  -- VkFormat
  VK_FORMAT_B8G8R8A8_UNORM             : constant Int_32_Unsigned_C := 44;  -- VkFormat
  VK_FORMAT_B8G8R8A8_SNORM             : constant Int_32_Unsigned_C := 45;  -- VkFormat
  VK_FORMAT_B8G8R8A8_USCALED           : constant Int_32_Unsigned_C := 46;  -- VkFormat
  VK_FORMAT_B8G8R8A8_SSCALED           : constant Int_32_Unsigned_C := 47;  -- VkFormat
  VK_FORMAT_B8G8R8A8_UINT              : constant Int_32_Unsigned_C := 48;  -- VkFormat
  VK_FORMAT_B8G8R8A8_SINT              : constant Int_32_Unsigned_C := 49;  -- VkFormat
  VK_FORMAT_B8G8R8A8_SRGB              : constant Int_32_Unsigned_C := 50;  -- VkFormat
  VK_FORMAT_A8B8G8R8_UNORM_PACK32      : constant Int_32_Unsigned_C := 51;  -- VkFormat
  VK_FORMAT_A8B8G8R8_SNORM_PACK32      : constant Int_32_Unsigned_C := 52;  -- VkFormat
  VK_FORMAT_A8B8G8R8_USCALED_PACK32    : constant Int_32_Unsigned_C := 53;  -- VkFormat
  VK_FORMAT_A8B8G8R8_SSCALED_PACK32    : constant Int_32_Unsigned_C := 54;  -- VkFormat
  VK_FORMAT_A8B8G8R8_UINT_PACK32       : constant Int_32_Unsigned_C := 55;  -- VkFormat
  VK_FORMAT_A8B8G8R8_SINT_PACK32       : constant Int_32_Unsigned_C := 56;  -- VkFormat
  VK_FORMAT_A8B8G8R8_SRGB_PACK32       : constant Int_32_Unsigned_C := 57;  -- VkFormat
  VK_FORMAT_A2R10G10B10_UNORM_PACK32   : constant Int_32_Unsigned_C := 58;  -- VkFormat
  VK_FORMAT_A2R10G10B10_SNORM_PACK32   : constant Int_32_Unsigned_C := 59;  -- VkFormat
  VK_FORMAT_A2R10G10B10_USCALED_PACK32 : constant Int_32_Unsigned_C := 60;  -- VkFormat
  VK_FORMAT_A2R10G10B10_SSCALED_PACK32 : constant Int_32_Unsigned_C := 61;  -- VkFormat
  VK_FORMAT_A2R10G10B10_UINT_PACK32    : constant Int_32_Unsigned_C := 62;  -- VkFormat
  VK_FORMAT_A2R10G10B10_SINT_PACK32    : constant Int_32_Unsigned_C := 63;  -- VkFormat
  VK_FORMAT_A2B10G10R10_UNORM_PACK32   : constant Int_32_Unsigned_C := 64;  -- VkFormat
  VK_FORMAT_A2B10G10R10_SNORM_PACK32   : constant Int_32_Unsigned_C := 65;  -- VkFormat
  VK_FORMAT_A2B10G10R10_USCALED_PACK32 : constant Int_32_Unsigned_C := 66;  -- VkFormat
  VK_FORMAT_A2B10G10R10_SSCALED_PACK32 : constant Int_32_Unsigned_C := 67;  -- VkFormat
  VK_FORMAT_A2B10G10R10_UINT_PACK32    : constant Int_32_Unsigned_C := 68;  -- VkFormat
  VK_FORMAT_A2B10G10R10_SINT_PACK32    : constant Int_32_Unsigned_C := 69;  -- VkFormat
  VK_FORMAT_R16_UNORM                  : constant Int_32_Unsigned_C := 70;  -- VkFormat
  VK_FORMAT_R16_SNORM                  : constant Int_32_Unsigned_C := 71;  -- VkFormat
  VK_FORMAT_R16_USCALED                : constant Int_32_Unsigned_C := 72;  -- VkFormat
  VK_FORMAT_R16_SSCALED                : constant Int_32_Unsigned_C := 73;  -- VkFormat
  VK_FORMAT_R16_UINT                   : constant Int_32_Unsigned_C := 74;  -- VkFormat
  VK_FORMAT_R16_SINT                   : constant Int_32_Unsigned_C := 75;  -- VkFormat
  VK_FORMAT_R16_SFLOAT                 : constant Int_32_Unsigned_C := 76;  -- VkFormat
  VK_FORMAT_R16G16_UNORM               : constant Int_32_Unsigned_C := 77;  -- VkFormat
  VK_FORMAT_R16G16_SNORM               : constant Int_32_Unsigned_C := 78;  -- VkFormat
  VK_FORMAT_R16G16_USCALED             : constant Int_32_Unsigned_C := 79;  -- VkFormat
  VK_FORMAT_R16G16_SSCALED             : constant Int_32_Unsigned_C := 80;  -- VkFormat
  VK_FORMAT_R16G16_UINT                : constant Int_32_Unsigned_C := 81;  -- VkFormat
  VK_FORMAT_R16G16_SINT                : constant Int_32_Unsigned_C := 82;  -- VkFormat
  VK_FORMAT_R16G16_SFLOAT              : constant Int_32_Unsigned_C := 83;  -- VkFormat
  VK_FORMAT_R16G16B16_UNORM            : constant Int_32_Unsigned_C := 84;  -- VkFormat
  VK_FORMAT_R16G16B16_SNORM            : constant Int_32_Unsigned_C := 85;  -- VkFormat
  VK_FORMAT_R16G16B16_USCALED          : constant Int_32_Unsigned_C := 86;  -- VkFormat
  VK_FORMAT_R16G16B16_SSCALED          : constant Int_32_Unsigned_C := 87;  -- VkFormat
  VK_FORMAT_R16G16B16_UINT             : constant Int_32_Unsigned_C := 88;  -- VkFormat
  VK_FORMAT_R16G16B16_SINT             : constant Int_32_Unsigned_C := 89;  -- VkFormat
  VK_FORMAT_R16G16B16_SFLOAT           : constant Int_32_Unsigned_C := 90;  -- VkFormat
  VK_FORMAT_R16G16B16A16_UNORM         : constant Int_32_Unsigned_C := 91;  -- VkFormat
  VK_FORMAT_R16G16B16A16_SNORM         : constant Int_32_Unsigned_C := 92;  -- VkFormat
  VK_FORMAT_R16G16B16A16_USCALED       : constant Int_32_Unsigned_C := 93;  -- VkFormat
  VK_FORMAT_R16G16B16A16_SSCALED       : constant Int_32_Unsigned_C := 94;  -- VkFormat
  VK_FORMAT_R16G16B16A16_UINT          : constant Int_32_Unsigned_C := 95;  -- VkFormat
  VK_FORMAT_R16G16B16A16_SINT          : constant Int_32_Unsigned_C := 96;  -- VkFormat
  VK_FORMAT_R16G16B16A16_SFLOAT        : constant Int_32_Unsigned_C := 97;  -- VkFormat
  VK_FORMAT_R32_UINT                   : constant Int_32_Unsigned_C := 98;  -- VkFormat
  VK_FORMAT_R32_SINT                   : constant Int_32_Unsigned_C := 99;  -- VkFormat
  VK_FORMAT_R32_SFLOAT                 : constant Int_32_Unsigned_C := 100; -- VkFormat
  VK_FORMAT_R32G32_UINT                : constant Int_32_Unsigned_C := 101; -- VkFormat
  VK_FORMAT_R32G32_SINT                : constant Int_32_Unsigned_C := 102; -- VkFormat
  VK_FORMAT_R32G32_SFLOAT              : constant Int_32_Unsigned_C := 103; -- VkFormat
  VK_FORMAT_R32G32B32_UINT             : constant Int_32_Unsigned_C := 104; -- VkFormat
  VK_FORMAT_R32G32B32_SINT             : constant Int_32_Unsigned_C := 105; -- VkFormat
  VK_FORMAT_R32G32B32_SFLOAT           : constant Int_32_Unsigned_C := 106; -- VkFormat
  VK_FORMAT_R32G32B32A32_UINT          : constant Int_32_Unsigned_C := 107; -- VkFormat
  VK_FORMAT_R32G32B32A32_SINT          : constant Int_32_Unsigned_C := 108; -- VkFormat
  VK_FORMAT_R32G32B32A32_SFLOAT        : constant Int_32_Unsigned_C := 109; -- VkFormat
  VK_FORMAT_R64_UINT                   : constant Int_32_Unsigned_C := 110; -- VkFormat
  VK_FORMAT_R64_SINT                   : constant Int_32_Unsigned_C := 111; -- VkFormat
  VK_FORMAT_R64_SFLOAT                 : constant Int_32_Unsigned_C := 112; -- VkFormat
  VK_FORMAT_R64G64_UINT                : constant Int_32_Unsigned_C := 113; -- VkFormat
  VK_FORMAT_R64G64_SINT                : constant Int_32_Unsigned_C := 114; -- VkFormat
  VK_FORMAT_R64G64_SFLOAT              : constant Int_32_Unsigned_C := 115; -- VkFormat
  VK_FORMAT_R64G64B64_UINT             : constant Int_32_Unsigned_C := 116; -- VkFormat
  VK_FORMAT_R64G64B64_SINT             : constant Int_32_Unsigned_C := 117; -- VkFormat
  VK_FORMAT_R64G64B64_SFLOAT           : constant Int_32_Unsigned_C := 118; -- VkFormat
  VK_FORMAT_R64G64B64A64_UINT          : constant Int_32_Unsigned_C := 119; -- VkFormat
  VK_FORMAT_R64G64B64A64_SINT          : constant Int_32_Unsigned_C := 120; -- VkFormat
  VK_FORMAT_R64G64B64A64_SFLOAT        : constant Int_32_Unsigned_C := 121; -- VkFormat
  VK_FORMAT_B10G11R11_UFLOAT_PACK32    : constant Int_32_Unsigned_C := 122; -- VkFormat
  VK_FORMAT_E5B9G9R9_UFLOAT_PACK32     : constant Int_32_Unsigned_C := 123; -- VkFormat
  VK_FORMAT_D16_UNORM                  : constant Int_32_Unsigned_C := 124; -- VkFormat
  VK_FORMAT_X8_D24_UNORM_PACK32        : constant Int_32_Unsigned_C := 125; -- VkFormat
  VK_FORMAT_D32_SFLOAT                 : constant Int_32_Unsigned_C := 126; -- VkFormat
  VK_FORMAT_S8_UINT                    : constant Int_32_Unsigned_C := 127; -- VkFormat
  VK_FORMAT_D16_UNORM_S8_UINT          : constant Int_32_Unsigned_C := 128; -- VkFormat
  VK_FORMAT_D24_UNORM_S8_UINT          : constant Int_32_Unsigned_C := 129; -- VkFormat
  VK_FORMAT_D32_SFLOAT_S8_UINT         : constant Int_32_Unsigned_C := 130; -- VkFormat
  VK_FORMAT_BC1_RGB_UNORM_BLOCK        : constant Int_32_Unsigned_C := 131; -- VkFormat
  VK_FORMAT_BC1_RGB_SRGB_BLOCK         : constant Int_32_Unsigned_C := 132; -- VkFormat
  VK_FORMAT_BC1_RGBA_UNORM_BLOCK       : constant Int_32_Unsigned_C := 133; -- VkFormat
  VK_FORMAT_BC1_RGBA_SRGB_BLOCK        : constant Int_32_Unsigned_C := 134; -- VkFormat
  VK_FORMAT_BC2_UNORM_BLOCK            : constant Int_32_Unsigned_C := 135; -- VkFormat
  VK_FORMAT_BC2_SRGB_BLOCK             : constant Int_32_Unsigned_C := 136; -- VkFormat
  VK_FORMAT_BC3_UNORM_BLOCK            : constant Int_32_Unsigned_C := 137; -- VkFormat
  VK_FORMAT_BC3_SRGB_BLOCK             : constant Int_32_Unsigned_C := 138; -- VkFormat
  VK_FORMAT_BC4_UNORM_BLOCK            : constant Int_32_Unsigned_C := 139; -- VkFormat
  VK_FORMAT_BC4_SNORM_BLOCK            : constant Int_32_Unsigned_C := 140; -- VkFormat
  VK_FORMAT_BC5_UNORM_BLOCK            : constant Int_32_Unsigned_C := 141; -- VkFormat
  VK_FORMAT_BC5_SNORM_BLOCK            : constant Int_32_Unsigned_C := 142; -- VkFormat
  VK_FORMAT_BC6H_UFLOAT_BLOCK          : constant Int_32_Unsigned_C := 143; -- VkFormat
  VK_FORMAT_BC6H_SFLOAT_BLOCK          : constant Int_32_Unsigned_C := 144; -- VkFormat
  VK_FORMAT_BC7_UNORM_BLOCK            : constant Int_32_Unsigned_C := 145; -- VkFormat
  VK_FORMAT_BC7_SRGB_BLOCK             : constant Int_32_Unsigned_C := 146; -- VkFormat
  VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK    : constant Int_32_Unsigned_C := 147; -- VkFormat
  VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK     : constant Int_32_Unsigned_C := 148; -- VkFormat
  VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK  : constant Int_32_Unsigned_C := 149; -- VkFormat
  VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK   : constant Int_32_Unsigned_C := 150; -- VkFormat
  VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK  : constant Int_32_Unsigned_C := 151; -- VkFormat
  VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK   : constant Int_32_Unsigned_C := 152; -- VkFormat
  VK_FORMAT_EAC_R11_UNORM_BLOCK        : constant Int_32_Unsigned_C := 153; -- VkFormat
  VK_FORMAT_EAC_R11_SNORM_BLOCK        : constant Int_32_Unsigned_C := 154; -- VkFormat
  VK_FORMAT_EAC_R11G11_UNORM_BLOCK     : constant Int_32_Unsigned_C := 155; -- VkFormat
  VK_FORMAT_EAC_R11G11_SNORM_BLOCK     : constant Int_32_Unsigned_C := 156; -- VkFormat
  VK_FORMAT_ASTC_4x4_UNORM_BLOCK       : constant Int_32_Unsigned_C := 157; -- VkFormat
  VK_FORMAT_ASTC_4x4_SRGB_BLOCK        : constant Int_32_Unsigned_C := 158; -- VkFormat
  VK_FORMAT_ASTC_5x4_UNORM_BLOCK       : constant Int_32_Unsigned_C := 159; -- VkFormat
  VK_FORMAT_ASTC_5x4_SRGB_BLOCK        : constant Int_32_Unsigned_C := 160; -- VkFormat
  VK_FORMAT_ASTC_5x5_UNORM_BLOCK       : constant Int_32_Unsigned_C := 161; -- VkFormat
  VK_FORMAT_ASTC_5x5_SRGB_BLOCK        : constant Int_32_Unsigned_C := 162; -- VkFormat
  VK_FORMAT_ASTC_6x5_UNORM_BLOCK       : constant Int_32_Unsigned_C := 163; -- VkFormat
  VK_FORMAT_ASTC_6x5_SRGB_BLOCK        : constant Int_32_Unsigned_C := 164; -- VkFormat
  VK_FORMAT_ASTC_6x6_UNORM_BLOCK       : constant Int_32_Unsigned_C := 165; -- VkFormat
  VK_FORMAT_ASTC_6x6_SRGB_BLOCK        : constant Int_32_Unsigned_C := 166; -- VkFormat
  VK_FORMAT_ASTC_8x5_UNORM_BLOCK       : constant Int_32_Unsigned_C := 167; -- VkFormat
  VK_FORMAT_ASTC_8x5_SRGB_BLOCK        : constant Int_32_Unsigned_C := 168; -- VkFormat
  VK_FORMAT_ASTC_8x6_UNORM_BLOCK       : constant Int_32_Unsigned_C := 169; -- VkFormat
  VK_FORMAT_ASTC_8x6_SRGB_BLOCK        : constant Int_32_Unsigned_C := 170; -- VkFormat
  VK_FORMAT_ASTC_8x8_UNORM_BLOCK       : constant Int_32_Unsigned_C := 171; -- VkFormat
  VK_FORMAT_ASTC_8x8_SRGB_BLOCK        : constant Int_32_Unsigned_C := 172; -- VkFormat
  VK_FORMAT_ASTC_10x5_UNORM_BLOCK      : constant Int_32_Unsigned_C := 173; -- VkFormat
  VK_FORMAT_ASTC_10x5_SRGB_BLOCK       : constant Int_32_Unsigned_C := 174; -- VkFormat
  VK_FORMAT_ASTC_10x6_UNORM_BLOCK      : constant Int_32_Unsigned_C := 175; -- VkFormat
  VK_FORMAT_ASTC_10x6_SRGB_BLOCK       : constant Int_32_Unsigned_C := 176; -- VkFormat
  VK_FORMAT_ASTC_10x8_UNORM_BLOCK      : constant Int_32_Unsigned_C := 177; -- VkFormat
  VK_FORMAT_ASTC_10x8_SRGB_BLOCK       : constant Int_32_Unsigned_C := 178; -- VkFormat
  VK_FORMAT_ASTC_10x10_UNORM_BLOCK     : constant Int_32_Unsigned_C := 179; -- VkFormat
  VK_FORMAT_ASTC_10x10_SRGB_BLOCK      : constant Int_32_Unsigned_C := 180; -- VkFormat
  VK_FORMAT_ASTC_12x10_UNORM_BLOCK     : constant Int_32_Unsigned_C := 181; -- VkFormat
  VK_FORMAT_ASTC_12x10_SRGB_BLOCK      : constant Int_32_Unsigned_C := 182; -- VkFormat
  VK_FORMAT_ASTC_12x12_UNORM_BLOCK     : constant Int_32_Unsigned_C := 183; -- VkFormat
  VK_FORMAT_ASTC_12x12_SRGB_BLOCK      : constant Int_32_Unsigned_C := 184; -- VkFormat

  ----------------
  -- Structures --
  ----------------

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExtent2D.html
  type VkExtent2D is record
      width  : Int_32_Unsigned_C := 0;  -- uint32_t
      height : Int_32_Unsigned_C := 0;  -- uint32_t
    end record with Convention => C;

  -- http://nopper.tv/Vulkan/1.0/VkWin32SurfaceCreateInfoKHR.html
  type VkWin32SurfaceCreateInfoKHR is record
      sType     : Int_32_Unsigned_C := VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR; -- VkStructureType
      pNext     : Ptr;               -- const void*
      flags     : Int_32_Unsigned_C; -- VkWin32SurfaceCreateFlagsKHR
      hinstance : Ptr;               -- HINSTANCE
      hwnd      : Ptr;               -- HWND
    end record with Convention => C;

  -- http://nopper.tv/Vulkan/1.0/VkSwapchainCreateInfoKHR.html
  type VkSwapchainCreateInfoKHR is record
      sType                 : Int_32_Unsigned_C := VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR; -- VkStructureType
      pNext                 : Ptr := NULL_PTR;                   -- const void*
      flags                 : Int_32_Unsigned_C := 0;     -- VkSwapchainCreateFlagsKHR
      surface               : Ptr := NULL_PTR;                   -- VkSurfaceKHR
      minImageCount         : Int_32_Unsigned_C := 0;     -- uint32_t
      imageFormat           : Int_32_Unsigned_C := 0;     -- VkFormat
      imageColorSpace       : Int_32_Unsigned_C := 0;     -- VkColorSpaceKHR
      imageExtent           : VkExtent2D := (others => <>);            -- VkExtent2D
      imageArrayLayers      : Int_32_Unsigned_C := 0;     -- uint32_t
      imageUsage            : Int_32_Unsigned_C := 0;     -- VkImageUsageFlags
      imageSharingMode      : Int_32_Unsigned_C := 0;     -- VkSharingMode
      queueFamilyIndexCount : Int_32_Unsigned_C := 0;     -- uint32_t
      pQueueFamilyIndices   : Ptr_Int_32_Unsigned_C := null; -- const uint32_t*
      preTransform          : Int_32_Unsigned_C := 0;     -- VkSurfaceTransformFlagBitsKHR
      compositeAlpha        : Int_32_Unsigned_C := 0;     -- VkCompositeAlphaFlagBitsKHR
      presentMode           : Int_32_Unsigned_C := 0;     -- VkPresentModeKHR
      clipped               : Int_32_Unsigned_C := 0;     -- VkBool32
      oldSwapchain          : Ptr;                   -- VkSwapchainKHR
    end record with Convention => C;
        
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceFeatures.html
  type VkPhysicalDeviceFeatures is record
      robustBufferAccess                      : Int_32_Unsigned_C; -- VkBool32  
      fullDrawIndexUint32                     : Int_32_Unsigned_C; -- VkBool32  
      imageCubeArray                          : Int_32_Unsigned_C; -- VkBool32  
      independentBlend                        : Int_32_Unsigned_C; -- VkBool32  
      geometryShader                          : Int_32_Unsigned_C; -- VkBool32  
      tessellationShader                      : Int_32_Unsigned_C; -- VkBool32  
      sampleRateShading                       : Int_32_Unsigned_C; -- VkBool32  
      dualSrcBlend                            : Int_32_Unsigned_C; -- VkBool32  
      logicOp                                 : Int_32_Unsigned_C; -- VkBool32  
      multiDrawIndirect                       : Int_32_Unsigned_C; -- VkBool32  
      drawIndirectFirstInstance               : Int_32_Unsigned_C; -- VkBool32  
      depthClamp                              : Int_32_Unsigned_C; -- VkBool32  
      depthBiasClamp                          : Int_32_Unsigned_C; -- VkBool32  
      fillModeNonSolid                        : Int_32_Unsigned_C; -- VkBool32  
      depthBounds                             : Int_32_Unsigned_C; -- VkBool32  
      wideLines                               : Int_32_Unsigned_C; -- VkBool32  
      largePoints                             : Int_32_Unsigned_C; -- VkBool32  
      alphaToOne                              : Int_32_Unsigned_C; -- VkBool32  
      multiViewport                           : Int_32_Unsigned_C; -- VkBool32  
      samplerAnisotropy                       : Int_32_Unsigned_C; -- VkBool32  
      textureCompressionETC2                  : Int_32_Unsigned_C; -- VkBool32  
      textureCompressionASTC_LDR              : Int_32_Unsigned_C; -- VkBool32  
      textureCompressionBC                    : Int_32_Unsigned_C; -- VkBool32  
      occlusionQueryPrecise                   : Int_32_Unsigned_C; -- VkBool32  
      pipelineStatisticsQuery                 : Int_32_Unsigned_C; -- VkBool32  
      vertexPipelineStoresAndAtomics          : Int_32_Unsigned_C; -- VkBool32  
      fragmentStoresAndAtomics                : Int_32_Unsigned_C; -- VkBool32  
      shaderTessellationAndGeometryPointSize  : Int_32_Unsigned_C; -- VkBool32  
      shaderImageGatherExtended               : Int_32_Unsigned_C; -- VkBool32  
      shaderStorageImageExtendedFormats       : Int_32_Unsigned_C; -- VkBool32  
      shaderStorageImageMultisample           : Int_32_Unsigned_C; -- VkBool32  
      shaderStorageImageReadWithoutFormat     : Int_32_Unsigned_C; -- VkBool32  
      shaderStorageImageWriteWithoutFormat    : Int_32_Unsigned_C; -- VkBool32  
      shaderUniformBufferArrayDynamicIndexing : Int_32_Unsigned_C; -- VkBool32  
      shaderSampledImageArrayDynamicIndexing  : Int_32_Unsigned_C; -- VkBool32  
      shaderStorageBufferArrayDynamicIndexing : Int_32_Unsigned_C; -- VkBool32  
      shaderStorageImageArrayDynamicIndexing  : Int_32_Unsigned_C; -- VkBool32  
      shaderClipDistance                      : Int_32_Unsigned_C; -- VkBool32  
      shaderCullDistance                      : Int_32_Unsigned_C; -- VkBool32  
      shaderFloat64                           : Int_32_Unsigned_C; -- VkBool32  
      shaderInt64                             : Int_32_Unsigned_C; -- VkBool32  
      shaderInt16                             : Int_32_Unsigned_C; -- VkBool32  
      shaderResourceResidency                 : Int_32_Unsigned_C; -- VkBool32  
      shaderResourceMinLod                    : Int_32_Unsigned_C; -- VkBool32  
      sparseBinding                           : Int_32_Unsigned_C; -- VkBool32  
      sparseResidencyBuffer                   : Int_32_Unsigned_C; -- VkBool32  
      sparseResidencyImage2D                  : Int_32_Unsigned_C; -- VkBool32  
      sparseResidencyImage3D                  : Int_32_Unsigned_C; -- VkBool32  
      sparseResidency2Samples                 : Int_32_Unsigned_C; -- VkBool32  
      sparseResidency4Samples                 : Int_32_Unsigned_C; -- VkBool32  
      sparseResidency8Samples                 : Int_32_Unsigned_C; -- VkBool32  
      sparseResidency16Samples                : Int_32_Unsigned_C; -- VkBool32  
      sparseResidency                         : Int_32_Unsigned_C; -- VkBool32  
      variableMultisampleRate                 : Int_32_Unsigned_C; -- VkBool32  
      inheritedQueries                        : Int_32_Unsigned_C; -- VkBool32  
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceLimits.html
  type VkPhysicalDeviceLimits is record
      maxImageDimension1D                             : Int_32_Unsigned_C;              -- uint32_t     
      maxImageDimension2D                             : Int_32_Unsigned_C;              -- uint32_t     
      maxImageDimension3D                             : Int_32_Unsigned_C;              -- uint32_t     
      maxImageDimensionCube                           : Int_32_Unsigned_C;              -- uint32_t     
      maxImageArrayLayers                             : Int_32_Unsigned_C;              -- uint32_t     
      maxTexelBufferElements                          : Int_32_Unsigned_C;              -- uint32_t     
      maxUniformBufferRange                           : Int_32_Unsigned_C;              -- uint32_t     
      maxStorageBufferRange                           : Int_32_Unsigned_C;              -- uint32_t     
      maxPushConstantsSize                            : Int_32_Unsigned_C;              -- uint32_t     
      maxMemoryAllocationCount                        : Int_32_Unsigned_C;              -- uint32_t     
      maxSamplerAllocationCount                       : Int_32_Unsigned_C;              -- uint32_t    
      bufferImageGranularity                          : Int_64_Unsigned_C;              -- VkDeviceSize
      sparseAddressSpaceSize                          : Int_64_Unsigned_C;              -- VkDeviceSize
      maxBoundDescriptorSets                          : Int_32_Unsigned_C;              -- uint32_t     
      maxPerStageDescriptorSamplers                   : Int_32_Unsigned_C;              -- uint32_t     
      maxPerStageDescriptorUniformBuffers             : Int_32_Unsigned_C;              -- uint32_t     
      maxPerStageDescriptorStorageBuffers             : Int_32_Unsigned_C;              -- uint32_t     
      maxPerStageDescriptorSampledImages              : Int_32_Unsigned_C;              -- uint32_t     
      maxPerStageDescriptorStorageImages              : Int_32_Unsigned_C;              -- uint32_t     
      maxPerStageDescriptorInputAttachments           : Int_32_Unsigned_C;              -- uint32_t     
      maxPerStageResources                            : Int_32_Unsigned_C;              -- uint32_t     
      maxDescriptorSetSamplers                        : Int_32_Unsigned_C;              -- uint32_t     
      maxDescriptorSetUniformBuffers                  : Int_32_Unsigned_C;              -- uint32_t     
      maxDescriptorSetUniformBuffersDynamic           : Int_32_Unsigned_C;              -- uint32_t     
      maxDescriptorSetStorageBuffers                  : Int_32_Unsigned_C;              -- uint32_t     
      maxDescriptorSetStorageBuffersDynamic           : Int_32_Unsigned_C;              -- uint32_t     
      maxDescriptorSetSampledImages                   : Int_32_Unsigned_C;              -- uint32_t     
      maxDescriptorSetStorageImages                   : Int_32_Unsigned_C;              -- uint32_t     
      maxDescriptorSetInputAttachments                : Int_32_Unsigned_C;              -- uint32_t     
      maxVertexInputAttributes                        : Int_32_Unsigned_C;              -- uint32_t     
      maxVertexInputBindings                          : Int_32_Unsigned_C;              -- uint32_t     
      maxVertexInputAttributeOffset                   : Int_32_Unsigned_C;              -- uint32_t     
      maxVertexInputBindingStride                     : Int_32_Unsigned_C;              -- uint32_t     
      maxVertexOutputComponents                       : Int_32_Unsigned_C;              -- uint32_t     
      maxTessellationGenerationLevel                  : Int_32_Unsigned_C;              -- uint32_t     
      maxTessellationPatchSize                        : Int_32_Unsigned_C;              -- uint32_t     
      maxTessellationControlPerVertexInputComponents  : Int_32_Unsigned_C;              -- uint32_t     
      maxTessellationControlPerVertexOutputComponents : Int_32_Unsigned_C;              -- uint32_t     
      maxTessellationControlPerPatchOutputComponents  : Int_32_Unsigned_C;              -- uint32_t     
      maxTessellationControlTotalOutputComponents     : Int_32_Unsigned_C;              -- uint32_t     
      maxTessellationEvaluationInputComponents        : Int_32_Unsigned_C;              -- uint32_t     
      maxTessellationEvaluationOutputComponents       : Int_32_Unsigned_C;              -- uint32_t     
      maxGeometryShaderInvocations                    : Int_32_Unsigned_C;              -- uint32_t     
      maxGeometryInputComponents                      : Int_32_Unsigned_C;              -- uint32_t     
      maxGeometryOutputComponents                     : Int_32_Unsigned_C;              -- uint32_t     
      maxGeometryOutputVertices                       : Int_32_Unsigned_C;              -- uint32_t     
      maxGeometryTotalOutputComponents                : Int_32_Unsigned_C;              -- uint32_t     
      maxFragmentInputComponents                      : Int_32_Unsigned_C;              -- uint32_t     
      maxFragmentOutputAttachments                    : Int_32_Unsigned_C;              -- uint32_t     
      maxFragmentDualSrcAttachments                   : Int_32_Unsigned_C;              -- uint32_t     
      maxFragmentCombinedOutputResources              : Int_32_Unsigned_C;              -- uint32_t     
      maxComputeSharedMemorySize                      : Int_32_Unsigned_C;              -- uint32_t     
      maxComputeWorkGroupCount                        : Array_Int_32_Unsigned_C (1..3); -- uint32_t [3]
      maxComputeWorkGroupInvocations                  : Int_32_Unsigned_C;              -- uint32_t     
      maxComputeWorkGroupSize                         : Array_Int_32_Unsigned_C (1..3); -- uint32_t [3]
      subPixelPrecisionBits                           : Int_32_Unsigned_C;              -- uint32_t     
      subTexelPrecisionBits                           : Int_32_Unsigned_C;              -- uint32_t     
      mipmapPrecisionBits                             : Int_32_Unsigned_C;              -- uint32_t     
      maxDrawIndexedIndexValue                        : Int_32_Unsigned_C;              -- uint32_t     
      maxDrawIndirectCount                            : Int_32_Unsigned_C;              -- uint32_t     
      maxSamplerLodBias                               : Real;                           -- float
      maxSamplerAnisotropy                            : Real;                           -- float
      maxViewports                                    : Int_32_Unsigned_C;              -- uint32_t     
      maxViewportDimensions                           : Array_Int_32_Unsigned_C (1..2); -- uint32_t [2]
      viewportBoundsRange                             : Array_Real_32 (1..2);           -- float [2]
      viewportSubPixelBits                            : Int_32_Unsigned_C;              -- uint32_t     
      minMemoryMapAlignment                           : Int_Size_C;                     -- size_t
      minTexelBufferOffsetAlignment                   : Int_64_Unsigned_C;              -- VkDeviceSize
      minUniformBufferOffsetAlignment                 : Int_64_Unsigned_C;              -- VkDeviceSize
      minStorageBufferOffsetAlignment                 : Int_64_Unsigned_C;              -- VkDeviceSize
      minTexelOffset                                  : Int_C;                          -- int32_t
      maxTexelOffset                                  : Int_32_Unsigned_C;              -- uint32_t     
      minTexelGatherOffset                            : Int_C;                          -- int32_t
      maxTexelGatherOffset                            : Int_32_Unsigned_C;              -- uint32_t     
      minInterpolationOffset                          : Real;                           -- float
      maxInterpolationOffset                          : Real;                           -- float
      subPixelInterpolationOffsetBits                 : Int_32_Unsigned_C;              -- uint32_t     
      maxFramebufferWidth                             : Int_32_Unsigned_C;              -- uint32_t     
      maxFramebufferHeight                            : Int_32_Unsigned_C;              -- uint32_t     
      maxFramebufferLayers                            : Int_32_Unsigned_C;              -- uint32_t     
      framebufferColorSampleCounts                    : Int_32_Unsigned_C;              -- VkSampleCountFlags
      framebufferDepthSampleCounts                    : Int_32_Unsigned_C;              -- VkSampleCountFlags
      framebufferStencilSampleCounts                  : Int_32_Unsigned_C;              -- VkSampleCountFlags
      framebufferNoAttachmentsSampleCounts            : Int_32_Unsigned_C;              -- VkSampleCountFlags
      maxColorAttachments                             : Int_32_Unsigned_C;              -- uint32_t     
      sampledImageColorSampleCounts                   : Int_32_Unsigned_C;              -- VkSampleCountFlags
      sampledImageIntegerSampleCounts                 : Int_32_Unsigned_C;              -- VkSampleCountFlags
      sampledImageDepthSampleCounts                   : Int_32_Unsigned_C;              -- VkSampleCountFlags
      sampledImageStencilSampleCounts                 : Int_32_Unsigned_C;              -- VkSampleCountFlags
      storageImageSampleCounts                        : Int_32_Unsigned_C;              -- VkSampleCountFlags
      maxSampleMaskWords                              : Int_32_Unsigned_C;              -- uint32_t     
      timestampComputeAndGraphics                     : Int_32_Unsigned_C;              -- VkBool32
      timestampPeriod                                 : Real;                           -- float
      maxClipDistances                                : Int_32_Unsigned_C;              -- uint32_t     
      maxCullDistances                                : Int_32_Unsigned_C;              -- uint32_t     
      maxCombinedClipAndCullDistances                 : Int_32_Unsigned_C;              -- uint32_t     
      discreteQueuePriorities                         : Int_32_Unsigned_C;              -- uint32_t     
      pointSizeRange                                  : Array_Real_32 (1..2);           -- float [2]
      lineWidthRange                                  : Array_Real_32 (1..2);           -- float [2]
      pointSizeGranularity                            : Real;                           -- float
      lineWidthGranularity                            : Real;                           -- float
      strictLines                                     : Int_32_Unsigned_C;              -- VkBool32
      standardSampleLocations                         : Int_32_Unsigned_C;              -- VkBool32
      optimalBufferCopyOffsetAlignment                : Int_64_Unsigned_C;              -- VkDeviceSize
      optimalBufferCopyRowPitchAlignment              : Int_64_Unsigned_C;              -- VkDeviceSize
      nonCoherentAtomSize                             : Int_64_Unsigned_C;              -- VkDeviceSize
    end record with Convention => C;
      
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceSparseProperties.html
  type VkPhysicalDeviceSparseProperties is record
      residencyStandard2DBlockShape            : Int_32_Unsigned_C; -- VkBool32
      residencyStandard2DMultisampleBlockShape : Int_32_Unsigned_C; -- VkBool32
      residencyStandard3DBlockShape            : Int_32_Unsigned_C; -- VkBool32
      residencyAlignedMipSize                  : Int_32_Unsigned_C; -- VkBool32
      residencyNonResidentStrict               : Int_32_Unsigned_C; -- VkBool32
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceProperties.html
  type VkPhysicalDeviceProperties is record
      apiVersion        : Int_32_Unsigned_C := 0;                -- uint32_t
      driverVersion     : Int_32_Unsigned_C := 0;                -- uint32_t
      vendorID          : Int_32_Unsigned_C := 0;                -- uint32_t
      deviceID          : Int_32_Unsigned_C := 0;                -- uint32_t
      deviceType        : Int_32_Unsigned_C := 0;                -- VkPhysicalDeviceType                
      deviceName        : Str_8_C (1..256) := (others => NULL_CHAR_8_C);        -- char [VK_MAX_PHYSICAL_DEVICE_NAME_SIZE]
      pipelineCacheUUID : Array_Int_8_Unsigned_C (1..25) := (others => 0);   -- uint8_t [VK_UUID_SIZE]
      limits            : VkPhysicalDeviceLimits := (others => <>);           -- VkPhysicalDeviceLimits              
      sparseProperties  : VkPhysicalDeviceSparseProperties := (others => <>); -- VkPhysicalDeviceSparseProperties
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryType.html
  type VkMemoryType is record
      propertyFlags : Int_32_Unsigned_C; -- VkMemoryPropertyFlags    
      heapIndex     : Int_32_Unsigned_C; -- uint32_t
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryHeap.html
  type VkMemoryHeap is record
      size  : Int_64_Unsigned_C; -- VkDeviceSize
      flags : Int_32_Unsigned_C; -- VkMemoryHeapFlags
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExtent3D.html
  type VkExtent3D is record
      width  : Int_32_Unsigned_C; -- uint32_t
      height : Int_32_Unsigned_C; -- uint32_t
      depth  : Int_32_Unsigned_C; -- uint32_t
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceMemoryProperties.html
  type Array_VkMemoryType is array (1..32) of VkMemoryType; -- VkMemoryType [VK_MAX_MEMORY_TYPES]
  type Array_VkMemoryHeap is array (1..16) of VkMemoryHeap; -- VkMemoryHeap [VK_MAX_MEMORY_HEAPS]
  type VkPhysicalDeviceMemoryProperties is record
      memoryTypeCount : Int_32_Unsigned_C;   -- uint32_t
      memoryTypes     : Array_VkMemoryType;  -- VkMemoryType [VK_MAX_MEMORY_TYPES]
      memoryHeapCount : Int_32_Unsigned_C;   -- uint32_t
      memoryHeaps     : Array_VkMemoryHeap;  -- VkMemoryHeap [VK_MAX_MEMORY_HEAPS]
    end record with Convention => C;
        
  -- http://nopper.tv/Vulkan/1.0/VkSurfaceCapabilitiesKHR.html
  type VkSurfaceCapabilitiesKHR is record
      minImageCount           : Int_32_Unsigned_C; -- uint32_t
      maxImageCount           : Int_32_Unsigned_C; -- uint32_t
      currentExtent           : VkExtent2D;        -- VkExtent2D
      minImageExtent          : VkExtent2D;        -- VkExtent2D
      maxImageExtent          : VkExtent2D;        -- VkExtent2D
      maxImageArrayLayers     : Int_32_Unsigned_C; -- uint32_t
      supportedTransforms     : Int_32_Unsigned_C; -- VkSurfaceTransformFlagsKHR
      currentTransform        : Int_32_Unsigned_C; -- VkSurfaceTransformFlagBitsKHR
      supportedCompositeAlpha : Int_32_Unsigned_C; -- VkCompositeAlphaFlagsKHR
      supportedUsageFlags     : Int_32_Unsigned_C; -- VkImageUsageFlags
    end record with Convention => C;
        
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandBufferBeginInfo.html
  type VkCommandBufferBeginInfo is record
      sType            : Int_32_Unsigned_C := VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO; -- VkStructureType
      pNext            : Ptr := NULL_PTR;               -- const void*
      flags            : Int_32_Unsigned_C := 0; -- VkCommandBufferUsageFlags
      pInheritanceInfo : Ptr := NULL_PTR;               -- const VkCommandBufferInheritanceInfo*
    end record with Convention => C;
  
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageSubresourceRange.html
  type VkImageSubresourceRange is record
      aspectMask     : Int_32_Unsigned_C; -- VkImageAspectFlags    
      baseMipLevel   : Int_32_Unsigned_C; -- uint32_t
      levelCount     : Int_32_Unsigned_C; -- uint32_t
      baseArrayLayer : Int_32_Unsigned_C; -- uint32_t
      layerCount     : Int_32_Unsigned_C; -- uint32_t              
    end record;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageMemoryBarrier.html
  type VkImageMemoryBarrier is record
      sType               : Int_32_Unsigned_C;       -- VkStructureType
      pNext               : Ptr;                     -- const void*
      srcAccessMask       : Int_32_Unsigned_C;       -- VkAccessFlags
      dstAccessMask       : Int_32_Unsigned_C;       -- VkAccessFlags
      oldLayout           : Int_32_Unsigned_C;       -- VkImageLayout
      newLayout           : Int_32_Unsigned_C;       -- VkImageLayout
      srcQueueFamilyIndex : Int_32_Unsigned_C;       -- uint32_t
      dstQueueFamilyIndex : Int_32_Unsigned_C;       -- uint32_t
      image               : Ptr;                     -- VkImage
      subresourceRange    : VkImageSubresourceRange; -- VkImageSubresourceRange
    end record with Convention => C;
        
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandBufferAllocateInfo.html
  type VkCommandBufferAllocateInfo is record
      sType              : Int_32_Unsigned_C := VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO; -- VkStructureType
      pNext              : Ptr := NULL_PTR;               -- const void*
      commandPool        : Ptr := NULL_PTR;               -- VkCommandPool
      level              : Int_32_Unsigned_C := 0; -- VkCommandBufferLevel
      commandBufferCount : Int_32_Unsigned_C := 0; -- uint32_t
    end record with Convention => C;
        
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSubmitInfo.html
  type VkSubmitInfo is record
      sType                : Int_32_Unsigned_C := VK_STRUCTURE_TYPE_SUBMIT_INFO;     -- VkStructureType
      pNext                : Ptr := NULL_PTR;                   -- const void*
      waitSemaphoreCount   : Int_32_Unsigned_C := 0;     -- uint32_t
      pWaitSemaphores      : Ptr := NULL_PTR;                   -- const VkSemaphore*
      pWaitDstStageMask    : Ptr_Int_32_Unsigned_C := null; -- VkPipelineStageFlags
      commandBufferCount   : Int_32_Unsigned_C := 0;     -- uint32_t
      pCommandBuffers      : Ptr := NULL_PTR;                   -- const VkCommandBuffer* 
      signalSemaphoreCount : Int_32_Unsigned_C := 0;     -- uint32_t
      pSignalSemaphores    : Ptr := NULL_PTR;                   -- const VkSemaphore*
    end record with Convention => C;
        
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkApplicationInfo.html
  type VkApplicationInfo is record
      sType              : Int_32_Unsigned_C := VK_STRUCTURE_TYPE_APPLICATION_INFO; -- VkStructureType
      pNext              : Ptr := NULL_PTR;               -- const void*
      pApplicationName   : Ptr_Char_8_C := null;      -- const char*
      applicationVersion : Int_32_Unsigned_C := 0; -- uint32_t
      pEngineName        : Ptr_Char_8_C := null;      -- const char*
      engineVersion      : Int_32_Unsigned_C := 0; -- uint32_t
      apiVersion         : Int_32_Unsigned_C := 0; -- uint32_t
    end record with Convention => C;
        
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkInstanceCreateInfo.html
  type VkInstanceCreateInfo is record
      sType                   : Int_32_Unsigned_C := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO; -- VkStructureType
      pNext                   : Ptr := NULL_PTR;               -- const void* 
      flags                   : Int_32_Unsigned_C := 0; -- VkInstanceCreateFlags
      pApplicationInfo        : access VkApplicationInfo; -- const VkApplicationInfo*
      enabledLayerCount       : Int_32_Unsigned_C := 0; -- uint32_t
      ppEnabledLayerNames     : Ptr := NULL_PTR;               -- const char* const* 
      enabledExtensionCount   : Int_32_Unsigned_C := 0; -- uint32_t
      ppEnabledExtensionNames : Ptr_Char_8_C := null;               -- const char* const* 
    end record with Convention => C;
        
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDeviceQueueCreateInfo.html
  type VkDeviceQueueCreateInfo is record
      sType            :        Int_32_Unsigned_C := VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO; -- VkStructureType
      pNext            :        Ptr := NULL_PTR;               -- const void*
      flags            :        Int_32_Unsigned_C := 0; -- VkDeviceQueueCreateFlags
      queueFamilyIndex :        Int_32_Unsigned_C := 0; -- uint32_t
      queueCount       :        Int_32_Unsigned_C := 0; -- uint32_t
      pQueuePriorities : access Real_32_C;         -- const float* 
    end record with Convention => C;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkQueueFamilyProperties.html
  type VkQueueFamilyProperties is record
      queueFlags                  : Int_32_Unsigned_C; -- VkQueueFlags    
      queueCount                  : Int_32_Unsigned_C; -- uint32_t        
      timestampValidBits          : Int_32_Unsigned_C; -- uint32_t        
      minImageTransferGranularity : VkExtent3D;        -- VkExtent3D      
    end record with Convention => C;
        
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDeviceCreateInfo.html
  type VkDeviceCreateInfo is record
      sType                   : Int_32_Unsigned_C := VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO; -- VkStructureType
      pNext                   : Ptr := NULL_PTR;               -- const void* 
      flags                   : Int_32_Unsigned_C := 0; -- VkDeviceCreateFlags
      queueCreateInfoCount    : Int_32_Unsigned_C := 0; -- uint32_t
      pQueueCreateInfos       : access VkDeviceQueueCreateInfo := null;               -- const VkDeviceQueueCreateInfo*
      enabledLayerCount       : Int_32_Unsigned_C := 0; -- uint32_t
      ppEnabledLayerNames     : Ptr := NULL_PTR;               -- const char* const*
      enabledExtensionCount   : Int_32_Unsigned_C := 0; -- uint32_t
      ppEnabledExtensionNames : Ptr := NULL_PTR;               -- const char* const*
      pEnabledFeatures        : access VkPhysicalDeviceFeatures := null;               -- const VkPhysicalDeviceFeatures*
    end record with Convention => C;
        
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCommandPoolCreateInfo.html
  type VkCommandPoolCreateInfo is record
      sType            : Int_32_Unsigned_C := VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO; -- VkStructureType
      pNext            : Ptr := NULL_PTR;               -- const void*
      flags            : Int_32_Unsigned_C := 0; -- VkCommandPoolCreateFlags
      queueFamilyIndex : Int_32_Unsigned_C := 0; -- uint32_t
    end record with Convention => C;
        
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSemaphoreCreateInfo.html
  type VkSemaphoreCreateInfo is record
      sType : Int_32_Unsigned_C := VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO; -- VkStructureType
      pNext : Ptr := NULL_PTR;               -- const void* 
      flags : Int_32_Unsigned_C := 0; -- VkSemaphoreCreateFlags
    end record with Convention => C;
        
  -- http://nopper.tv/Vulkan/1.0/vkQueuePresentKHR.html
  type VkPresentInfoKHR is record
      sType              : Int_32_Unsigned_C := VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;     -- VkStructureType
      pNext              : Ptr := NULL_PTR;                   -- const void*
      waitSemaphoreCount : Int_32_Unsigned_C := 0;     -- uint32_t
      pWaitSemaphores    : Ptr := NULL_PTR;                   -- const VkSemaphore*
      swapchainCount     : Int_32_Unsigned_C := 0;     -- uint32_t 
      pSwapchains        : Ptr := NULL_PTR;                   -- const VkSwapchainKHR* 
      pImageIndices      : Ptr_Int_32_Unsigned_C := null; -- uint32_t
      pResults           : Ptr_Int_32_Unsigned_C := null; -- VkResult
    end record with Convention => C;

  -- https://vulkan.lunarg.com/doc/view/1.0.26.0/windows/vkspec.chunked/ch29s05.html#VkSurfaceFormatKHR
  type VkSurfaceFormatKHR is record
      format     : Int_32_Unsigned_C := 0; -- VkFormat
      colorSpace : Int_32_Unsigned_C := 0; -- VkColorSpaceKHR
    end record with Convention => C;

  ------------
  -- Arrays --
  ------------

  type Array_VkSurfaceFormatKHR is array (Positive range <>) of VkSurfaceFormatKHR;
  type Array_VkQueueFamilyProperties is array (Positive range <>) of VkQueueFamilyProperties;
  type Ptr_Array_VkSurfaceFormatKHR is access all Array_VkSurfaceFormatKHR;
  type Ptr_Array_VkQueueFamilyProperties is access all Array_VkQueueFamilyProperties;

  ---------------
  -- Functions --
  ---------------

  -- Assert VK_SUCCESS
  procedure vkAssert (result : Int_32_Unsigned_C); -- VkResult

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceFeatures.html
  --type Ptr_vkGetPhysicalDeviceFeatures is access procedure (VkPhysicalDevice                            physicalDevice,
  --                                                          VkPhysicalDeviceFeatures*                   pFeatures)
  --                                                     with Convention => C;
  --function To_Ptr_vkGetPhysicalDeviceFeatures is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceFeatures);
  --vkGetPhysicalDeviceFeatures : Ptr_vkGetPhysicalDeviceFeatures;                                 

  -- https://harrylovescode.gitbooks.io/vulkan-api/content/chap05/chap05-windows.html
  type Ptr_vkCreateWin32SurfaceKHR is access function (instance    :        Ptr;                         -- VkInstance
                                                       pCreateInfo : access VkWin32SurfaceCreateInfoKHR; -- const VkWin32SurfaceCreateInfoKHR*
                                                       pAllocator  :        Ptr;                         -- const VkAllocationCallbacks*
                                                       pSurface    : access Ptr)                         -- VkSurfaceKHR*
                                                       return Int_32_Unsigned_C                          -- VkResult
                                                       with Convention => C;
  function To_Ptr_vkCreateWin32SurfaceKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkCreateWin32SurfaceKHR);
  vkCreateWin32SurfaceKHR : Ptr_vkCreateWin32SurfaceKHR;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceProperties.html
  type Ptr_vkGetPhysicalDeviceProperties is access procedure (physicalDevice :        Ptr;           -- VkPhysicalDevice
                                                              pProperties    : access VkPhysicalDeviceProperties) -- VkPhysicalDeviceProperties*
                                                              with Convention => C;
  function To_Ptr_vkGetPhysicalDeviceProperties is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceProperties);
  vkGetPhysicalDeviceProperties : Ptr_vkGetPhysicalDeviceProperties;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceMemoryProperties.html
  type Ptr_vkGetPhysicalDeviceMemoryProperties is access procedure (physicalDevice    :        Ptr;                 -- VkPhysicalDevice
                                                                    pMemoryProperties : access VkPhysicalDeviceMemoryProperties) -- VkPhysicalDeviceMemoryProperties*
                                                                    with Convention => C;
  function To_Ptr_vkGetPhysicalDeviceMemoryProperties is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceMemoryProperties);
  vkGetPhysicalDeviceMemoryProperties : Ptr_vkGetPhysicalDeviceMemoryProperties;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceQueueFamilyProperties.html
  type Ptr_vkGetPhysicalDeviceQueueFamilyProperties is access procedure (physicalDevice            :        Ptr;        -- VkPhysicalDevice 
                                                                         pQueueFamilyPropertyCount :        Ptr_Int_32_Unsigned_C;   -- uint32_t*
                                                                         pQueueFamilyProperties    : Ptr_Array_VkQueueFamilyProperties) -- VkQueueFamilyProperties*  
                                                                         with Convention => C;
  function To_Ptr_vkGetPhysicalDeviceQueueFamilyProperties is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceQueueFamilyProperties);
  vkGetPhysicalDeviceQueueFamilyProperties : Ptr_vkGetPhysicalDeviceQueueFamilyProperties;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDeviceQueue.html
  type Ptr_vkGetDeviceQueue is access procedure (device           : Ptr;          -- VkDevice
                                                 queueFamilyIndex : Int_32_Unsigned_C; -- uint32_t 
                                                 queueIndex       : Int_32_Unsigned_C; -- uint32_t 
                                                 pQueue           : Ptr)               -- VkQueue*
                                                 with Convention => C;
  function To_Ptr_vkGetDeviceQueue is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetDeviceQueue);
  vkGetDeviceQueue : Ptr_vkGetDeviceQueue;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateInstance.html
  type Ptr_vkCreateInstance is access function (pCreateInfo : access VkInstanceCreateInfo; -- const VkInstanceCreateInfo*
                                                pAllocator  :        Ptr;                  -- const VkAllocationCallbacks*
                                                pInstance   :        Ptr)                  -- VkInstance*
                                                return Int_32_Unsigned_C                   -- VkResult
                                                with Convention => C;
  function To_Ptr_vkCreateInstance is new Ada.Unchecked_Conversion (Ptr, Ptr_vkCreateInstance);
  vkCreateInstance : Ptr_vkCreateInstance; 

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkEnumeratePhysicalDevices.html
  type Ptr_vkEnumeratePhysicalDevices is access function (instance             : Ptr;                   -- VkInstance
                                                          pPhysicalDeviceCount : Ptr_Int_32_Unsigned_C; -- uint32_t*
                                                          pPhysicalDevices     : Ptr_Array_Ptr)         -- VkPhysicalDevice*
                                                          return Int_32_Unsigned_C                      -- VkResult
                                                          with Convention => C;
  function To_Ptr_vkEnumeratePhysicalDevices is new Ada.Unchecked_Conversion (Ptr, Ptr_vkEnumeratePhysicalDevices);
  vkEnumeratePhysicalDevices : Ptr_vkEnumeratePhysicalDevices;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateDevice.html
  type Ptr_vkCreateDevice is access function (physicalDevice : Ptr; -- VkPhysicalDevice
                                              pCreateInfo    : access VkDeviceCreateInfo;              -- const VkDeviceCreateInfo*
                                              pAllocator     : Ptr;              -- const VkAllocationCallbacks*
                                              pDevice        : Ptr)              -- VkDevice*
                                              return Int_32_Unsigned_C           -- VkResult
                                              with Convention => C;
  function To_Ptr_vkCreateDevice is new Ada.Unchecked_Conversion (Ptr,   Ptr_vkCreateDevice);
  vkCreateDevice : Ptr_vkCreateDevice;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateCommandPool.html
  type Ptr_vkCreateCommandPool is access function (device       : Ptr; -- VkDevice 
                                                   pCreateInfo  : access VkCommandPoolCreateInfo;      -- const VkCommandPoolCreateInfo* 
                                                   pAllocator   : Ptr;      -- const VkAllocationCallbacks*
                                                   pCommandPool : Ptr)      -- VkCommandPool* 
                                                   return Int_32_Unsigned_C -- VkResult
                                                   with Convention => C;
  function To_Ptr_vkCreateCommandPool is new Ada.Unchecked_Conversion (Ptr, Ptr_vkCreateCommandPool);
  vkCreateCommandPool : Ptr_vkCreateCommandPool;

  -- http://nopper.tv/Vulkan/1.0/vkGetPhysicalDeviceSurfaceSupportKHR.html
  type Ptr_vkGetPhysicalDeviceSurfaceSupportKHR is access function (physicalDevice   : Ptr;      -- VkPhysicalDevice
                                                                    queueFamilyIndex : Int_32_Unsigned_C;     -- uint32_t
                                                                    surface          : Ptr;          -- VkSurfaceKHR
                                                                    pSupported       : Ptr_Int_32_Unsigned_C) -- VkBool32*
                                                                    return Int_32_Unsigned_C                  -- VkResult
                                                                    with Convention => C;
  function To_Ptr_vkGetPhysicalDeviceSurfaceSupportKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceSurfaceSupportKHR);
  vkGetPhysicalDeviceSurfaceSupportKHR : Ptr_vkGetPhysicalDeviceSurfaceSupportKHR;    

  -- http://nopper.tv/Vulkan/1.0/vkGetPhysicalDeviceSurfaceFormatsKHR.html
  type Ptr_vkGetPhysicalDeviceSurfaceFormatsKHR is access function (physicalDevice      :        Ptr;      -- VkPhysicalDevice
                                                                    surface             :        Ptr;          -- VkSurfaceKHR
                                                                    pSurfaceFormatCount :        Ptr_Int_32_Unsigned_C; -- uint32_t* 
                                                                    pSurfaceFormats     : Ptr_Array_VkSurfaceFormatKHR)    -- VkSurfaceFormatKHR*
                                                                    return Int_32_Unsigned_C                            -- VkResult 
                                                                    with Convention => C;
  function To_Ptr_vkGetPhysicalDeviceSurfaceFormatsKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceSurfaceFormatsKHR);
  vkGetPhysicalDeviceSurfaceFormatsKHR : Ptr_vkGetPhysicalDeviceSurfaceFormatsKHR; 

  -- http://nopper.tv/Vulkan/1.0/vkGetPhysicalDeviceSurfaceCapabilitiesKHR.html
  type Ptr_vkGetPhysicalDeviceSurfaceCapabilitiesKHR is access function (physicalDevice       :        Ptr;         -- VkPhysicalDevice
                                                                         surface              :        Ptr;             -- VkSurfaceKHR
                                                                         pSurfaceCapabilities : access VkSurfaceCapabilitiesKHR) -- VkSurfaceCapabilitiesKHR*
                                                                         return Int_32_Unsigned_C                                -- VkResult
                                                                         with Convention => C;
  function To_Ptr_vkGetPhysicalDeviceSurfaceCapabilitiesKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceSurfaceCapabilitiesKHR);
  vkGetPhysicalDeviceSurfaceCapabilitiesKHR : Ptr_vkGetPhysicalDeviceSurfaceCapabilitiesKHR;

  -- http://nopper.tv/Vulkan/1.0/vkGetPhysicalDeviceSurfacePresentModesKHR.html
  type Ptr_vkGetPhysicalDeviceSurfacePresentModesKHR is access function (physicalDevice    : Ptr;                   -- VkPhysicalDevice
                                                                         surface           : Ptr;                   -- VkSurfaceKHR
                                                                         pPresentModeCount : Ptr_Int_32_Unsigned_C; -- uint32_t* 
                                                                         pPresentModes     : Ptr_Int_32_Unsigned_C) -- VkPresentModeKHR*
                                                                         return Int_32_Unsigned_C                   -- VkResult 
                                                                         with Convention => C;
  function To_Ptr_vkGetPhysicalDeviceSurfacePresentModesKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceSurfacePresentModesKHR);
  vkGetPhysicalDeviceSurfacePresentModesKHR : Ptr_vkGetPhysicalDeviceSurfacePresentModesKHR;

  -- http://nopper.tv/Vulkan/1.0/vkCreateSwapchainKHR.html
  type Ptr_vkCreateSwapchainKHR is access function (device      : Ptr;  -- VkDevice
                                                    pCreateInfo : access VkSwapchainCreateInfoKHR;       -- const VkSwapchainCreateInfoKHR*
                                                    pAllocator  : Ptr;       -- const VkAllocationCallbacks*
                                                    pSwapchain  : Ptr)       -- VkSwapchainKHR*
                                                    return Int_32_Unsigned_C -- VkResult
                                                    with Convention => C;
  function To_Ptr_vkCreateSwapchainKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkCreateSwapchainKHR);
  vkCreateSwapchainKHR : Ptr_vkCreateSwapchainKHR;

  -- http://nopper.tv/Vulkan/1.0/vkGetSwapchainImagesKHR.html 
  type Ptr_vkGetSwapchainImagesKHR is access function (device               : Ptr;              -- VkDevice
                                                       swapchain            : Ptr;        -- VkSwapchainKHR
                                                       pSwapchainImageCount : Ptr_Int_32_Unsigned_C; -- uint32_t*  
                                                       pSwapchainImages     : Ptr_Array_Ptr)                   -- VkImage*
                                                       return Int_32_Unsigned_C                      -- VkResult
                                                       with Convention => C;
  function To_Ptr_vkGetSwapchainImagesKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetSwapchainImagesKHR);
  vkGetSwapchainImagesKHR : Ptr_vkGetSwapchainImagesKHR;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkAllocateCommandBuffers.html
  type Ptr_vkAllocateCommandBuffers is access function (device          : Ptr; -- VkDevice                                    
                                                        pAllocateInfo   : access VkCommandBufferAllocateInfo;      -- const VkCommandBufferAllocateInfo*
                                                        pCommandBuffers : Ptr)      -- VkCommandBuffer*
                                                        return Int_32_Unsigned_C    -- VkResult
                                                        with Convention => C;
  function To_Ptr_vkAllocateCommandBuffers is new Ada.Unchecked_Conversion (Ptr, Ptr_vkAllocateCommandBuffers);
  vkAllocateCommandBuffers : Ptr_vkAllocateCommandBuffers;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkBeginCommandBuffer.html
  type Ptr_vkBeginCommandBuffer is access function (commandBuffer : Ptr; -- VkCommandBuffer                             
                                                    pBeginInfo    : access VkCommandBufferBeginInfo)             -- const VkCommandBufferBeginInfo*
                                                    return Int_32_Unsigned_C         -- VkResult
                                                    with Convention => C;
  function To_Ptr_vkBeginCommandBuffer is new Ada.Unchecked_Conversion (Ptr, ptr_vkBeginCommandBuffer);
  vkBeginCommandBuffer : Ptr_vkBeginCommandBuffer;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkEndCommandBuffer.html
  type Ptr_vkCmdPipelineBarrier is access procedure (commandBuffer            : Ptr;      -- VkCommandBuffer
                                                     srcStageMask             : Int_32_Unsigned_C; -- VkPipelineStageFlags
                                                     dstStageMask             : Int_32_Unsigned_C; -- VkPipelineStageFlags
                                                     dependencyFlags          : Int_32_Unsigned_C;    -- VkDependencyFlags
                                                     memoryBarrierCount       : Int_32_Unsigned_C;    -- uint32_t
                                                     pMemoryBarriers          : Ptr;                  -- const VkMemoryBarrier*
                                                     bufferMemoryBarrierCount : Int_32_Unsigned_C;    -- uint32_t
                                                     pBufferMemoryBarriers    : Ptr;                  -- const VkBufferMemoryBarrier*
                                                     imageMemoryBarrierCount  : Int_32_Unsigned_C;    -- uint32_t
                                                     pImageMemoryBarriers     : access VkImageMemoryBarrier)                  -- const VkImageMemoryBarrier*
                                                     with Convention => C;
  function To_Ptr_vkCmdPipelineBarrier is new Ada.Unchecked_Conversion (Ptr, Ptr_vkCmdPipelineBarrier);
  vkCmdPipelineBarrier : Ptr_vkCmdPipelineBarrier;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkEndCommandBuffer.html
  type Ptr_vkEndCommandBuffer is access function (commandBuffer : Ptr) -- VkCommandBuffer                             
                                                  return Int_32_Unsigned_C         -- VkResult 
                                                  with Convention => C;
  function To_Ptr_vkEndCommandBuffer is new Ada.Unchecked_Conversion (Ptr, Ptr_vkEndCommandBuffer);
  vkEndCommandBuffer : Ptr_vkEndCommandBuffer;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkQueueSubmit.html
  type Ptr_vkQueueSubmit is access function (queue       : Ptr;           -- VkQueue                                     
                                             submitCount : Int_32_Unsigned_C; -- uint32_t                                    
                                             pSubmits    : access VkSubmitInfo;               -- const VkSubmitInfo*
                                             fence       : Ptr)           -- VkFence
                                             return Int_32_Unsigned_C         -- VkResult
                                             with Convention => C;
  function To_Ptr_vkQueueSubmit is new Ada.Unchecked_Conversion (Ptr, Ptr_vkQueueSubmit);
  vkQueueSubmit : Ptr_vkQueueSubmit;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkQueueWaitIdle.html
  type Ptr_vkQueueWaitIdle is access function (queue : Ptr)         -- VkQueue
                                               return Int_32_Unsigned_C -- VkResult
                                               with Convention => C;
  function To_Ptr_vkQueueWaitIdle is new Ada.Unchecked_Conversion (Ptr, Ptr_vkQueueWaitIdle);
  vkQueueWaitIdle : Ptr_vkQueueWaitIdle; 

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateSemaphore.html
  type Ptr_vkFreeCommandBuffers is access procedure (device      : Ptr;          -- VkDevice
                                                     pCreateInfo : Ptr;     -- VkCommandPool                               
                                                     pAllocator  : Int_32_Unsigned_C; -- uint32_t                                    
                                                     pSemaphore  : Ptr)               -- VkCommandBuffer*
                                                     with Convention => C;
  function To_Ptr_vkFreeCommandBuffers is new Ada.Unchecked_Conversion (Ptr, Ptr_vkFreeCommandBuffers);
  vkFreeCommandBuffers : Ptr_vkFreeCommandBuffers;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateSemaphore.html
  type Ptr_vkCreateSemaphore is access function (device      : Ptr;  -- VkDevice                                    
                                                 pCreateInfo : access VkSemaphoreCreateInfo;       -- const VkSemaphoreCreateInfo* 
                                                 pAllocator  : Ptr;       -- const VkAllocationCallbacks* 
                                                 pSemaphore  : Ptr)       -- VkSemaphore*
                                                 return Int_32_Unsigned_C -- VkResult
                                                 with Convention => C;
  function To_Ptr_vkCreateSemaphore is new Ada.Unchecked_Conversion (Ptr, Ptr_vkCreateSemaphore);
  vkCreateSemaphore : Ptr_vkCreateSemaphore;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDeviceWaitIdle.html
  type Ptr_vkDeviceWaitIdle is access function (device : Ptr)        -- VkDevice
                                                return Int_32_Unsigned_C; -- VkResult
  function To_Ptr_vkDeviceWaitIdle is new Ada.Unchecked_Conversion (Ptr, Ptr_vkDeviceWaitIdle);
  vkDeviceWaitIdle : Ptr_vkDeviceWaitIdle;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroySemaphore.html
  type Ptr_vkDestroySemaphore is access procedure (device     : Ptr; -- VkDevice                                    
                                                   semaphore  : Ptr;      -- VkSemaphore                                 
                                                   pAllocator : Ptr)      -- const VkAllocationCallbacks* 
                                                   with Convention => C;
  function To_Ptr_vkDestroySemaphore is new Ada.Unchecked_Conversion (Ptr, Ptr_vkDestroySemaphore);
  vkDestroySemaphore : Ptr_vkDestroySemaphore;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyCommandPool.html
  type Ptr_vkDestroyCommandPool is access procedure (device      : Ptr;      -- VkDevice
                                                     commandPool : Ptr; -- VkCommandPool
                                                     pAllocator  : Ptr)           -- const VkAllocationCallbacks*    
                                                     with Convention => C;
  function To_Ptr_vkDestroyCommandPool is new Ada.Unchecked_Conversion (Ptr, Ptr_vkDestroyCommandPool);
  vkDestroyCommandPool : Ptr_vkDestroyCommandPool;

  -- http://nopper.tv/Vulkan/1.0/vkDestroySwapchainKHR.html
  type Ptr_vkDestroySwapchainKHR is access procedure (device     : Ptr;       -- VkDevice 
                                                      swapchain  : Ptr; -- VkSwapchainKHR
                                                      pAllocator : Ptr)            -- const VkAllocationCallbacks*
                                                      with Convention => C;
  function To_Ptr_vkDestroySwapchainKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkDestroySwapchainKHR);
  vkDestroySwapchainKHR : Ptr_vkDestroySwapchainKHR;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyDevice.html
  type Ptr_vkDestroyDevice is access procedure (device     : Ptr; -- VkDevice                                    
                                                pAllocator : Ptr)      -- const VkAllocationCallbacks*
                                                with Convention => C;
  function To_Ptr_vkDestroyDevice is new Ada.Unchecked_Conversion (Ptr,  Ptr_vkDestroyDevice);
  vkDestroyDevice : Ptr_vkDestroyDevice; 

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyInstance.html
  type Ptr_vkDestroyInstance is access procedure (instance   : Ptr; -- VkInstance
                                                  pAllocator : Ptr) -- const VkAllocationCallbacks*
                                                  with Convention => C;
  function To_Ptr_vkDestroyInstance is new Ada.Unchecked_Conversion (Ptr, Ptr_vkDestroyInstance);
  vkDestroyInstance : Ptr_vkDestroyInstance;

  -- http://web.archive.org/web/20160608223244/https://www.khronos.org/registry/vulkan/specs/1.0-wsi_extensions/xhtml/vkspec.html#vkAcquireNextImageKHR
  type Ptr_vkAcquireNextImageKHR is access function (device      : Ptr;              -- VkDevice                                    
                                                     swapchain   : Ptr;        -- VkSwapchainKHR                              
                                                     timeout     : Int_64_Unsigned_C;     -- uint64_t                                    
                                                     semaphore   : Ptr;                   -- VkSemaphore                                 
                                                     fence       : Ptr;               -- VkFence                                     
                                                     pImageIndex : Ptr_Int_32_Unsigned_C) -- uint32_t*  
                                                     return Int_32_Unsigned_C             -- VkResult
                                                     with Convention => C;
  function To_Ptr_vkAcquireNextImageKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkAcquireNextImageKHR);
  vkAcquireNextImageKHR : Ptr_vkAcquireNextImageKHR;

  -- http://nopper.tv/Vulkan/1.0/vkQueuePresentKHR.html
  type Ptr_vkQueuePresentKHR is access function (queue        : Ptr;  -- VkQueue
                                                 pPresentInfo : access VkPresentInfoKHR)      -- const VkPresentInfoKHR*
                                                 return Int_32_Unsigned_C -- VkResult
                                                 with Convention => C;
  function To_Ptr_vkQueuePresentKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkQueuePresentKHR);
  vkQueuePresentKHR : Ptr_vkQueuePresentKHR;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkWaitForFences.html
  type Ptr_vkWaitForFences is access function (device     : Ptr;          -- VkDevice
                                               fenceCount : Int_32_Unsigned_C; -- uint32_t
                                               pFences    : Ptr;               -- const VkFence* 
                                               waitAll    : Int_32_Unsigned_C; -- VkBool32
                                               timeout    : Int_64_Unsigned_C) -- uint64_t
                                               return Int_32_Unsigned_C        -- VkResult
                                               with Convention => C;
  function To_Ptr_vkWaitForFences is new Ada.Unchecked_Conversion (Ptr, Ptr_vkWaitForFences);
  vkWaitForFences : Ptr_vkWaitForFences; 

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkResetFences.html
  type Ptr_vkResetFences is access function (device     : Ptr;          -- VkDevice
                                             fenceCount : Int_32_Unsigned_C; -- uint32_t
                                             pFences    : Ptr)               -- const VkFence*  
                                             return Int_32_Unsigned_C        -- VkResul
                                             with Convention => C;
  function To_Ptr_vkResetFences is new Ada.Unchecked_Conversion (Ptr, Ptr_vkResetFences);
  vkResetFences : Ptr_vkResetFences;

  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetFenceStatus.html
  type Ptr_vkGetFenceStatus is access function (device : Ptr;       -- VkDevice
                                                fence  : Ptr)        -- VkFence
                                                return Int_32_Unsigned_C -- VkResult
                                                with Convention => C; 
  function To_Ptr_vkGetFenceStatus is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetFenceStatus);
  vkGetFenceStatus : Ptr_vkGetFenceStatus; 

  -- http://web.archive.org/web/https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyFence.html
  type Ptr_vkDestroyFence is access procedure (device     : Ptr; -- VkDevice
                                               fence      : Ptr;  -- VkFence
                                               pAllocator : Ptr)      -- const VkAllocationCallbacks*
                                               with Convention => C;
  function To_Ptr_vkDestroyFence is new Ada.Unchecked_Conversion (Ptr, Ptr_vkDestroyFence);
  vkDestroyFence : Ptr_vkDestroyFence;

  -- http://web.archive.org/web/20160324124456/https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateFence.html
  type Ptr_vkCreateFence is access function (device      : Ptr;  -- VkDevice
                                             pCreateInfo : Ptr;       -- const VkFenceCreateInfo*
                                             pAllocator  : Ptr;       -- const VkAllocationCallbacks*
                                             pFence      : Ptr)       -- VkFence*
                                             return Int_32_Unsigned_C -- VkResult 
                                             with Convention => C;
  function To_Ptr_vkCreateFence is new Ada.Unchecked_Conversion (Ptr, Ptr_vkCreateFence);
  vkCreateFence : Ptr_vkCreateFence;
end;
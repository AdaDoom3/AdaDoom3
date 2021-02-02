
--                                                                                                                               --
--                                                      N E O  E N G I N E                                                       --
--                                                                                                                               --
--                                               Copyright (C) 2020 Justin Squirek                                               --
--                                                                                                                               --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published --
-- by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.                      --
--                                                                                                                               --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                     --
--                                                                                                                               --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                --
--                                                                                                                               --

with Neo.Core.Console; use Neo.Core.Console;
with Neo.Core.Strings; use Neo.Core.Strings;

package body Neo.API.Vulkan is

  -- Print the actual, readable error from a VkResult
  procedure VkAssert (Result : Int_Unsigned_C) is
    begin
      if Result /= VK_SUCCESS then
        Line ((case Result is
                 when VK_ERROR_OUT_OF_DATE_KHR          => "VK_ERROR_OUT_OF_DATE_KHR",
                 when VK_ERROR_SURFACE_LOST_KHR         => "VK_ERROR_SURFACE_LOST_KHR",
                 when VK_ERROR_NATIVE_WINDOW_IN_USE_KHR => "VK_ERROR_NATIVE_WINDOW_IN_USE_KHR",
                 when VK_ERROR_INCOMPATIBLE_DISPLAY_KHR => "VK_ERROR_INCOMPATIBLE_DISPLAY_KHR",
                 when VK_ERROR_VALIDATION_FAILED_EXT    => "VK_ERROR_VALIDATION_FAILED_EXT",
                 when VK_ERROR_INVALID_SHADER_NV        => "VK_ERROR_INVALID_SHADER_NV",
                 when VK_ERROR_OUT_OF_HOST_MEMORY       => "VK_ERROR_OUT_OF_HOST_MEMORY",
                 when VK_ERROR_OUT_OF_DEVICE_MEMORY     => "VK_ERROR_OUT_OF_DEVICE_MEMORY",
                 when VK_ERROR_INITIALIZATION_FAILED    => "VK_ERROR_INITIALIZATION_FAILED",
                 when VK_ERROR_DEVICE_LOST              => "VK_ERROR_DEVICE_LOST.. :[",
                 when VK_ERROR_MEMORY_MAP_FAILED        => "VK_ERROR_MEMORY_MAP_FAILED",
                 when VK_ERROR_LAYER_NOT_PRESENT        => "VK_ERROR_LAYER_NOT_PRESENT",
                 when VK_ERROR_EXTENSION_NOT_PRESENT    => "VK_ERROR_EXTENSION_NOT_PRESENT",
                 when VK_ERROR_FEATURE_NOT_PRESENT      => "VK_ERROR_FEATURE_NOT_PRESENT",
                 when VK_ERROR_INCOMPATIBLE_DRIVER      => "VK_ERROR_INCOMPATIBLE_DRIVER",
                 when VK_ERROR_TOO_MANY_OBJECTS         => "VK_ERROR_TOO_MANY_OBJECTS",
                 when VK_ERROR_FORMAT_NOT_SUPPORTED     => "VK_ERROR_FORMAT_NOT_SUPPORTED",
                 when VK_ERROR_OUT_OF_POOL_MEMORY       => "VK_ERROR_OUT_OF_POOL_MEMORY",
                 --when VK_ERROR_FRAGMENTED_POOL          => "VK_ERROR_FRAGMENTED_POOL",
                 --when VK_ERROR_INVALID_EXTERNAL_HANDLE  => "VK_ERROR_INVALID_EXTERNAL_HANDLE",
                 --when VK_ERROR_FRAGMENTATION_EXT        => "VK_ERROR_FRAGMENTATION_EXT",
                 --when VK_ERROR_NOT_PERMITTED_EXT        => "VK_ERROR_NOT_PERMITTED_EXT",
                 when others => "VK_ERROR " & To_Str_16_Int (Result, 16)));
         Assert (False);
      end if;
    end;
  
  -- Build a version integer from a string
  function VK_MAKE_VERSION (Val : Str) return Int_Unsigned_C is
    Result : Int_Unsigned := 0;
    I, J   : Int          := Val'First;
    begin
    
      -- Major
      while Val (I) /= '.' loop I := I + 1; if I = Val'Last then return Int_Unsigned_C (Result); end if; end loop;
      Result := Shift_Left (Int_Unsigned'Wide_Value (Val (Val'First..I - 1)), 22);
      I := I + 1;
      J := I;
      
      -- Minor
      while Val (I) /= '.' loop I := I + 1; if I = Val'Last then return Int_Unsigned_C (Result); end if; end loop;

      -- Revision
      return Int_Unsigned_C (Result or Shift_Left (Int_Unsigned'Wide_Value (Val (J..I - 1)), 12) or
                               Int_Unsigned'Wide_Value (Val (I + 1..Val'Last)));
    end;
  
  -- Load function pointers to the Vulkan dynamic library
  procedure Initialize is
    function Get (Name : Str) return Ptr renames Get_Vulkan_Subprogram;
    begin
      vkGetAccelerationStructureMemoryRequirementsNV := To_Ptr_vkGetAccelerationStructureMemoryRequirementsNV (Get ("vkGetAccelerationStructureMemoryRequirementsNV"));
      vkCreateAccelerationStructureNV                := To_Ptr_vkCreateAccelerationStructureNV                (Get ("vkCreateAccelerationStructureNV"));
      vkDestroyAccelerationStructureNV               := To_Ptr_vkDestroyAccelerationStructureNV               (Get ("vkDestroyAccelerationStructureNV"));
      vkBindAccelerationStructureMemoryNV            := To_Ptr_vkBindAccelerationStructureMemoryNV            (Get ("vkBindAccelerationStructureMemoryNV"));
      vkGetAccelerationStructureHandleNV             := To_Ptr_vkGetAccelerationStructureHandleNV             (Get ("vkGetAccelerationStructureHandleNV"));
      vkCmdBuildAccelerationStructureNV              := To_Ptr_vkCmdBuildAccelerationStructureNV              (Get ("vkCmdBuildAccelerationStructureNV"));
      vkCreateRayTracingPipelinesNV                  := To_Ptr_vkCreateRayTracingPipelinesNV                  (Get ("vkCreateRayTracingPipelinesNV"));
      vkGetRayTracingShaderGroupHandlesNV            := To_Ptr_vkGetRayTracingShaderGroupHandlesNV            (Get ("vkGetRayTracingShaderGroupHandlesNV"));
      vkCmdTraceRaysNV                               := To_Ptr_vkCmdTraceRaysNV                               (Get ("vkCmdTraceRaysNV"));
      vkResetDescriptorPool                          := To_Ptr_vkResetDescriptorPool                          (Get ("vkResetDescriptorPool"));
      vkFreeDescriptorSets                           := To_Ptr_vkFreeDescriptorSets                           (Get ("vkFreeDescriptorSets"));
      vkDestroySurfaceKHR                            := To_Ptr_vkDestroySurfaceKHR                            (Get ("vkDestroySurfaceKHR"));
      vkGetPhysicalDeviceImageFormatProperties       := To_Ptr_vkGetPhysicalDeviceImageFormatProperties       (Get ("vkGetPhysicalDeviceImageFormatProperties"));
      vkFlushMappedMemoryRanges                      := To_Ptr_vkFlushMappedMemoryRanges                      (Get ("vkFlushMappedMemoryRanges"));
      vkCreateSampler                                := To_Ptr_vkCreateSampler                                (Get ("vkCreateSampler"));
      vkDestroyImage                                 := To_Ptr_vkDestroyImage                                 (Get ("vkDestroyImage"));
      vkDestroyPipeline                              := To_Ptr_vkDestroyPipeline                              (Get ("vkDestroyPipeline"));
      vkDestroySampler                               := To_Ptr_vkDestroySampler                               (Get ("vkDestroySampler"));
      vkDestroyDescriptorPool                        := To_Ptr_vkDestroyDescriptorPool                        (Get ("vkDestroyDescriptorPool"));
      vkDestroyDescriptorSetLayout                   := To_Ptr_vkDestroyDescriptorSetLayout                   (Get ("vkDestroyDescriptorSetLayout"));
      vkDestroyPipelineLayout                        := To_Ptr_vkDestroyPipelineLayout                        (Get ("vkDestroyPipelineLayout"));
      vkDestroyRenderPass                            := To_Ptr_vkDestroyRenderPass                            (Get ("vkDestroyRenderPass"));
      vkDestroyImageView                             := To_Ptr_vkDestroyImageView                             (Get ("vkDestroyImageView"));
      vkDestroyFramebuffer                           := To_Ptr_vkDestroyFramebuffer                           (Get ("vkDestroyFramebuffer"));
      vkCmdDraw                                      := To_Ptr_vkCmdDraw                                      (Get ("vkCmdDraw"));
      vkCmdCopyBufferToImage                         := To_Ptr_vkCmdCopyBufferToImage                         (Get ("vkCmdCopyBufferToImage"));
      vkCreateShaderModule                           := To_Ptr_vkCreateShaderModule                           (Get ("vkCreateShaderModule"));
      vkCreateFramebuffer                            := To_Ptr_vkCreateFramebuffer                            (Get ("vkCreateFramebuffer"));
      vkUnmapMemory                                  := To_Ptr_vkUnmapMemory                                  (Get ("vkUnmapMemory"));
      vkMapMemory                                    := To_Ptr_vkMapMemory                                    (Get ("vkMapMemory"));
      vkGetImageSubresourceLayout                    := To_Ptr_vkGetImageSubresourceLayout                    (Get ("vkGetImageSubresourceLayout"));
      vkCreateImageView                              := To_Ptr_vkCreateImageView                              (Get ("vkCreateImageView"));
      vkCmdCopyImage                                 := To_Ptr_vkCmdCopyImage                                 (Get ("vkCmdCopyImage"));
      vkGetBufferMemoryRequirements                  := To_Ptr_vkGetBufferMemoryRequirements                  (Get ("vkGetBufferMemoryRequirements"));
      vkCreateBuffer                                 := To_Ptr_vkCreateBuffer                                 (Get ("vkCreateBuffer"));
      vkBindBufferMemory                             := To_Ptr_vkBindBufferMemory                             (Get ("vkBindBufferMemory"));
      vkGetPhysicalDeviceProperties                  := To_Ptr_vkGetPhysicalDeviceProperties                  (Get ("vkGetPhysicalDeviceProperties"));
      vkGetPhysicalDeviceMemoryProperties            := To_Ptr_vkGetPhysicalDeviceMemoryProperties            (Get ("vkGetPhysicalDeviceMemoryProperties"));
      vkGetPhysicalDeviceQueueFamilyProperties       := To_Ptr_vkGetPhysicalDeviceQueueFamilyProperties       (Get ("vkGetPhysicalDeviceQueueFamilyProperties"));
      vkGetDeviceQueue                               := To_Ptr_vkGetDeviceQueue                               (Get ("vkGetDeviceQueue"));
      vkCreateInstance                               := To_Ptr_vkCreateInstance                               (Get ("vkCreateInstance"));
      vkEnumeratePhysicalDevices                     := To_Ptr_vkEnumeratePhysicalDevices                     (Get ("vkEnumeratePhysicalDevices"));
      vkCreateDevice                                 := To_Ptr_vkCreateDevice                                 (Get ("vkCreateDevice"));
      vkCreateCommandPool                            := To_Ptr_vkCreateCommandPool                            (Get ("vkCreateCommandPool"));
      vkGetPhysicalDeviceSurfaceSupportKHR           := To_Ptr_vkGetPhysicalDeviceSurfaceSupportKHR           (Get ("vkGetPhysicalDeviceSurfaceSupportKHR"));
      vkGetPhysicalDeviceSurfaceFormatsKHR           := To_Ptr_vkGetPhysicalDeviceSurfaceFormatsKHR           (Get ("vkGetPhysicalDeviceSurfaceFormatsKHR"));
      vkGetPhysicalDeviceSurfaceCapabilitiesKHR      := To_Ptr_vkGetPhysicalDeviceSurfaceCapabilitiesKHR      (Get ("vkGetPhysicalDeviceSurfaceCapabilitiesKHR"));
      vkGetPhysicalDeviceSurfacePresentModesKHR      := To_Ptr_vkGetPhysicalDeviceSurfacePresentModesKHR      (Get ("vkGetPhysicalDeviceSurfacePresentModesKHR"));
      vkCreateSwapchainKHR                           := To_Ptr_vkCreateSwapchainKHR                           (Get ("vkCreateSwapchainKHR"));
      vkGetSwapchainImagesKHR                        := To_Ptr_vkGetSwapchainImagesKHR                        (Get ("vkGetSwapchainImagesKHR"));
      vkAllocateCommandBuffers                       := To_Ptr_vkAllocateCommandBuffers                       (Get ("vkAllocateCommandBuffers"));
      vkBeginCommandBuffer                           := To_Ptr_vkBeginCommandBuffer                           (Get ("vkBeginCommandBuffer"));
      vkCmdPipelineBarrier                           := To_Ptr_vkCmdPipelineBarrier                           (Get ("vkCmdPipelineBarrier"));
      vkEndCommandBuffer                             := To_Ptr_vkEndCommandBuffer                             (Get ("vkEndCommandBuffer"));
      vkQueueSubmit                                  := To_Ptr_vkQueueSubmit                                  (Get ("vkQueueSubmit"));
      vkQueueWaitIdle                                := To_Ptr_vkQueueWaitIdle                                (Get ("vkQueueWaitIdle"));
      vkFreeCommandBuffers                           := To_Ptr_vkFreeCommandBuffers                           (Get ("vkFreeCommandBuffers"));
      vkCreateSemaphore                              := To_Ptr_vkCreateSemaphore                              (Get ("vkCreateSemaphore"));
      vkDeviceWaitIdle                               := To_Ptr_vkDeviceWaitIdle                               (Get ("vkDeviceWaitIdle"));
      vkDestroySemaphore                             := To_Ptr_vkDestroySemaphore                             (Get ("vkDestroySemaphore"));
      vkDestroyCommandPool                           := To_Ptr_vkDestroyCommandPool                           (Get ("vkDestroyCommandPool"));
      vkDestroySwapchainKHR                          := To_Ptr_vkDestroySwapchainKHR                          (Get ("vkDestroySwapchainKHR"));
      vkDestroyDevice                                := To_Ptr_vkDestroyDevice                                (Get ("vkDestroyDevice"));
      vkDestroyInstance                              := To_Ptr_vkDestroyInstance                              (Get ("vkDestroyInstance"));
      vkAcquireNextImageKHR                          := To_Ptr_vkAcquireNextImageKHR                          (Get ("vkAcquireNextImageKHR"));
      vkQueuePresentKHR                              := To_Ptr_vkQueuePresentKHR                              (Get ("vkQueuePresentKHR"));
      vkWaitForFences                                := To_Ptr_vkWaitForFences                                (Get ("vkWaitForFences"));
      vkResetFences                                  := To_Ptr_vkResetFences                                  (Get ("vkResetFences"));
      vkGetFenceStatus                               := To_Ptr_vkGetFenceStatus                               (Get ("vkGetFenceStatus"));
      vkDestroyFence                                 := To_Ptr_vkDestroyFence                                 (Get ("vkDestroyFence"));
      vkCreateFence                                  := To_Ptr_vkCreateFence                                  (Get ("vkCreateFence"));
      vkCmdDrawIndexed                               := To_Ptr_vkCmdDrawIndexed                               (Get ("vkCmdDrawIndexed"));
      vkCmdEndRenderPass                             := To_Ptr_vkCmdEndRenderPass                             (Get ("vkCmdEndRenderPass"));
      vkCmdBindDescriptorSets                        := To_Ptr_vkCmdBindDescriptorSets                        (Get ("vkCmdBindDescriptorSets"));
      vkCmdBindIndexBuffer                           := To_Ptr_vkCmdBindIndexBuffer                           (Get ("vkCmdBindIndexBuffer"));
      vkCmdBindVertexBuffers                         := To_Ptr_vkCmdBindVertexBuffers                         (Get ("vkCmdBindVertexBuffers"));
      vkCmdBindPipeline                              := To_Ptr_vkCmdBindPipeline                              (Get ("vkCmdBindPipeline"));
      vkCmdBeginRenderPass                           := To_Ptr_vkCmdBeginRenderPass                           (Get ("vkCmdBeginRenderPass"));
      vkCreateImage                                  := To_Ptr_vkCreateImage                                  (Get ("vkCreateImage"));
      vkGetImageMemoryRequirements                   := To_Ptr_vkGetImageMemoryRequirements                   (Get ("vkGetImageMemoryRequirements"));
      vkAllocateMemory                               := To_Ptr_vkAllocateMemory                               (Get ("vkAllocateMemory"));
      vkBindImageMemory                              := To_Ptr_vkBindImageMemory                              (Get ("vkBindImageMemory"));
      vkCmdCopyBuffer                                := To_Ptr_vkCmdCopyBuffer                                (Get ("vkCmdCopyBuffer"));
      vkDestroyBuffer                                := To_Ptr_vkDestroyBuffer                                (Get ("vkDestroyBuffer"));
      vkFreeMemory                                   := To_Ptr_vkFreeMemory                                   (Get ("vkFreeMemory"));
      vkCreateRenderPass                             := To_Ptr_vkCreateRenderPass                             (Get ("vkCreateRenderPass"));
      vkCreatePipelineLayout                         := To_Ptr_vkCreatePipelineLayout                         (Get ("vkCreatePipelineLayout"));
      vkCreateGraphicsPipelines                      := To_Ptr_vkCreateGraphicsPipelines                      (Get ("vkCreateGraphicsPipelines"));
      vkCreateCommandPool                            := To_Ptr_vkCreateCommandPool                            (Get ("vkCreateCommandPool"));
      vkCreateDescriptorSetLayout                    := To_Ptr_vkCreateDescriptorSetLayout                    (Get ("vkCreateDescriptorSetLayout"));
      vkCreateDescriptorPool                         := To_Ptr_vkCreateDescriptorPool                         (Get ("vkCreateDescriptorPool"));
      vkAllocateDescriptorSets                       := To_Ptr_vkAllocateDescriptorSets                       (Get ("vkAllocateDescriptorSets"));
      vkUpdateDescriptorSets                         := To_Ptr_vkUpdateDescriptorSets                         (Get ("vkUpdateDescriptorSets"));
      vkGetPhysicalDeviceFormatProperties            := To_Ptr_vkGetPhysicalDeviceFormatProperties            (Get ("vkGetPhysicalDeviceFormatProperties"));
      vkEnumerateDeviceExtensionProperties           := To_Ptr_vkEnumerateDeviceExtensionProperties           (Get ("vkEnumerateDeviceExtensionProperties"));
    end;
end;

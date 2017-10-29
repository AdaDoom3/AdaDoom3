
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

package body Neo.API.Vulkan is
  procedure VkAssert (result : Int_Unsigned_C) is begin Assert (result = VK_SUCCESS); end;
  
  -- Load function pointers to the Vulkan dynamic library
  procedure Initialize is
    function Get (Name : Str) return Ptr renames Get_Vulkan_Subprogram;
    begin
      vkCmdDraw                                 := To_Ptr_vkCmdDraw                                 (Get ("vkCmdDraw"));
      vkCmdCopyBufferToImage                    := To_Ptr_vkCmdCopyBufferToImage                    (Get ("vkCmdCopyBufferToImage"));
      vkCreateShaderModule                      := To_Ptr_vkCreateShaderModule                      (Get ("vkCreateShaderModule"));
      vkCreateFramebuffer                       := To_Ptr_vkCreateFramebuffer                       (Get ("vkCreateFramebuffer"));
      vkUnmapMemory                             := To_Ptr_vkUnmapMemory                             (Get ("vkUnmapMemory"));
      vkMapMemory                               := To_Ptr_vkMapMemory                               (Get ("vkMapMemory"));
      vkGetImageSubresourceLayout               := To_Ptr_vkGetImageSubresourceLayout               (Get ("vkGetImageSubresourceLayout"));
      vkCreateImageView                         := To_Ptr_vkCreateImageView                         (Get ("vkCreateImageView"));
      vkCmdCopyImage                            := To_Ptr_vkCmdCopyImage                            (Get ("vkCmdCopyImage"));
      vkGetBufferMemoryRequirements             := To_Ptr_vkGetBufferMemoryRequirements             (Get ("vkGetBufferMemoryRequirements"));
      vkCreateBuffer                            := To_Ptr_vkCreateBuffer                            (Get ("vkCreateBuffer"));
      vkBindBufferMemory                        := To_Ptr_vkBindBufferMemory                        (Get ("vkBindBufferMemory"));
      vkGetPhysicalDeviceProperties             := To_Ptr_vkGetPhysicalDeviceProperties             (Get ("vkGetPhysicalDeviceProperties"));
      vkGetPhysicalDeviceMemoryProperties       := To_Ptr_vkGetPhysicalDeviceMemoryProperties       (Get ("vkGetPhysicalDeviceMemoryProperties"));
      vkGetPhysicalDeviceQueueFamilyProperties  := To_Ptr_vkGetPhysicalDeviceQueueFamilyProperties  (Get ("vkGetPhysicalDeviceQueueFamilyProperties"));
      vkGetDeviceQueue                          := To_Ptr_vkGetDeviceQueue                          (Get ("vkGetDeviceQueue"));
      vkCreateInstance                          := To_Ptr_vkCreateInstance                          (Get ("vkCreateInstance"));
      vkEnumeratePhysicalDevices                := To_Ptr_vkEnumeratePhysicalDevices                (Get ("vkEnumeratePhysicalDevices"));
      vkCreateDevice                            := To_Ptr_vkCreateDevice                            (Get ("vkCreateDevice"));
      vkCreateCommandPool                       := To_Ptr_vkCreateCommandPool                       (Get ("vkCreateCommandPool"));
      vkGetPhysicalDeviceSurfaceSupportKHR      := To_Ptr_vkGetPhysicalDeviceSurfaceSupportKHR      (Get ("vkGetPhysicalDeviceSurfaceSupportKHR"));
      vkGetPhysicalDeviceSurfaceFormatsKHR      := To_Ptr_vkGetPhysicalDeviceSurfaceFormatsKHR      (Get ("vkGetPhysicalDeviceSurfaceFormatsKHR"));
      vkGetPhysicalDeviceSurfaceCapabilitiesKHR := To_Ptr_vkGetPhysicalDeviceSurfaceCapabilitiesKHR (Get ("vkGetPhysicalDeviceSurfaceCapabilitiesKHR"));
      vkGetPhysicalDeviceSurfacePresentModesKHR := To_Ptr_vkGetPhysicalDeviceSurfacePresentModesKHR (Get ("vkGetPhysicalDeviceSurfacePresentModesKHR"));
      vkCreateSwapchainKHR                      := To_Ptr_vkCreateSwapchainKHR                      (Get ("vkCreateSwapchainKHR"));
      vkGetSwapchainImagesKHR                   := To_Ptr_vkGetSwapchainImagesKHR                   (Get ("vkGetSwapchainImagesKHR"));
      vkAllocateCommandBuffers                  := To_Ptr_vkAllocateCommandBuffers                  (Get ("vkAllocateCommandBuffers"));
      vkBeginCommandBuffer                      := To_Ptr_vkBeginCommandBuffer                      (Get ("vkBeginCommandBuffer"));
      vkCmdPipelineBarrier                      := To_Ptr_vkCmdPipelineBarrier                      (Get ("vkCmdPipelineBarrier"));
      vkEndCommandBuffer                        := To_Ptr_vkEndCommandBuffer                        (Get ("vkEndCommandBuffer"));
      vkQueueSubmit                             := To_Ptr_vkQueueSubmit                             (Get ("vkQueueSubmit"));
      vkQueueWaitIdle                           := To_Ptr_vkQueueWaitIdle                           (Get ("vkQueueWaitIdle"));
      vkFreeCommandBuffers                      := To_Ptr_vkFreeCommandBuffers                      (Get ("vkFreeCommandBuffers"));
      vkCreateSemaphore                         := To_Ptr_vkCreateSemaphore                         (Get ("vkCreateSemaphore"));
      vkDeviceWaitIdle                          := To_Ptr_vkDeviceWaitIdle                          (Get ("vkDeviceWaitIdle"));
      vkDestroySemaphore                        := To_Ptr_vkDestroySemaphore                        (Get ("vkDestroySemaphore"));
      vkDestroyCommandPool                      := To_Ptr_vkDestroyCommandPool                      (Get ("vkDestroyCommandPool"));
      vkDestroySwapchainKHR                     := To_Ptr_vkDestroySwapchainKHR                     (Get ("vkDestroySwapchainKHR"));
      vkDestroyDevice                           := To_Ptr_vkDestroyDevice                           (Get ("vkDestroyDevice"));
      vkDestroyInstance                         := To_Ptr_vkDestroyInstance                         (Get ("vkDestroyInstance"));
      vkAcquireNextImageKHR                     := To_Ptr_vkAcquireNextImageKHR                     (Get ("vkAcquireNextImageKHR"));
      vkQueuePresentKHR                         := To_Ptr_vkQueuePresentKHR                         (Get ("vkQueuePresentKHR"));
      vkWaitForFences                           := To_Ptr_vkWaitForFences                           (Get ("vkWaitForFences"));
      vkResetFences                             := To_Ptr_vkResetFences                             (Get ("vkResetFences"));
      vkGetFenceStatus                          := To_Ptr_vkGetFenceStatus                          (Get ("vkGetFenceStatus"));
      vkDestroyFence                            := To_Ptr_vkDestroyFence                            (Get ("vkDestroyFence"));
      vkCreateFence                             := To_Ptr_vkCreateFence                             (Get ("vkCreateFence"));
      vkCmdDrawIndexed                          := To_Ptr_vkCmdDrawIndexed                          (Get ("vkCmdDrawIndexed"));
      vkCmdEndRenderPass                        := To_Ptr_vkCmdEndRenderPass                        (Get ("vkCmdEndRenderPass"));
      vkCmdBindDescriptorSets                   := To_Ptr_vkCmdBindDescriptorSets                   (Get ("vkCmdBindDescriptorSets"));
      vkCmdBindIndexBuffer                      := To_Ptr_vkCmdBindIndexBuffer                      (Get ("vkCmdBindIndexBuffer"));
      vkCmdBindVertexBuffers                    := To_Ptr_vkCmdBindVertexBuffers                    (Get ("vkCmdBindVertexBuffers"));
      vkCmdBindPipeline                         := To_Ptr_vkCmdBindPipeline                         (Get ("vkCmdBindPipeline"));
      vkCmdBeginRenderPass                      := To_Ptr_vkCmdBeginRenderPass                      (Get ("vkCmdBeginRenderPass"));
      vkCreateImage                             := To_Ptr_vkCreateImage                             (Get ("vkCreateImage"));
      vkGetImageMemoryRequirements              := To_Ptr_vkGetImageMemoryRequirements              (Get ("vkGetImageMemoryRequirements"));
      vkAllocateMemory                          := To_Ptr_vkAllocateMemory                          (Get ("vkAllocateMemory"));
      vkBindImageMemory                         := To_Ptr_vkBindImageMemory                         (Get ("vkBindImageMemory"));
      vkCmdCopyBuffer                           := To_Ptr_vkCmdCopyBuffer                           (Get ("vkCmdCopyBuffer"));
      vkDestroyBuffer                           := To_Ptr_vkDestroyBuffer                           (Get ("vkDestroyBuffer"));
      vkFreeMemory                              := To_Ptr_vkFreeMemory                              (Get ("vkFreeMemory"));
      vkCreateRenderPass                        := To_Ptr_vkCreateRenderPass                        (Get ("vkCreateRenderPass"));
      vkCreatePipelineLayout                    := To_Ptr_vkCreatePipelineLayout                    (Get ("vkCreatePipelineLayout"));
      vkCreateGraphicsPipelines                 := To_Ptr_vkCreateGraphicsPipelines                 (Get ("vkCreateGraphicsPipelines"));
      vkCreateCommandPool                       := To_Ptr_vkCreateCommandPool                       (Get ("vkCreateCommandPool"));
      vkCreateDescriptorSetLayout               := To_Ptr_vkCreateDescriptorSetLayout               (Get ("vkCreateDescriptorSetLayout"));
      vkCreateDescriptorPool                    := To_Ptr_vkCreateDescriptorPool                    (Get ("vkCreateDescriptorPool"));
      vkAllocateDescriptorSets                  := To_Ptr_vkAllocateDescriptorSets                  (Get ("vkAllocateDescriptorSets"));
      vkUpdateDescriptorSets                    := To_Ptr_vkUpdateDescriptorSets                    (Get ("vkUpdateDescriptorSets"));
      vkGetPhysicalDeviceFormatProperties       := To_Ptr_vkGetPhysicalDeviceFormatProperties       (Get ("vkGetPhysicalDeviceFormatProperties"));
      vkEnumerateDeviceExtensionProperties      := To_Ptr_vkEnumerateDeviceExtensionProperties      (Get ("vkEnumerateDeviceExtensionProperties"));
    end;
end;

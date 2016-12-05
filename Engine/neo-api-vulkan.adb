
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

  -- Initialization of global function pointers with OS specific driver
  generic
    with function Get_Vulkan_Subprogram (Name : Str) return Ptr;
  procedure Initialize_Vulkan is

    -- Assert the pointer is not null
    function Get (Name : Str) return Ptr is
      Result : Ptr := Get_Vulkan_Subprogram (Name);
      begin
        Assert (Ptr);
        return Result;
      end;

    -- Load and convert the pointers
    begin
      vkGetPhysicalDeviceProperties             := To_Ptr_vkGetPhysicalDeviceProperties             (Get ("vkGetPhysicalDeviceProperties"));
      vkGetPhysicalDeviceMemoryProperties       := To_Ptr_vkGetPhysicalDeviceMemoryProperties       (Get ("vkGetPhysicalDeviceMemoryProperties"));
      vkGetPhysicalDeviceQueueFamilyProperties  := To_Ptr_vkGetPhysicalDeviceQueueFamilyProperties  (Get ("vkGetPhysicalDeviceQueueFamilyProperties");
      vkGetDeviceQueue                          := To_Ptr_vkGetDeviceQueue                          (Get ("vkGetDeviceQueue");
      vkCreateInstance                          := To_Ptr_vkCreateInstance                          (Get ("vkCreateInstance");
      vkEnumeratePhysicalDevices                := To_Ptr_vkEnumeratePhysicalDevices                (Get ("vkEnumeratePhysicalDevices");
      vkCreateDevice                            := To_Ptr_vkCreateDevice                            (Get ("vkCreateDevice");
      vkCreateCommandPool                       := To_Ptr_vkCreateCommandPool                       (Get ("vkCreateCommandPool");
      vkGetPhysicalDeviceSurfaceSupportKHR      := To_Ptr_vkGetPhysicalDeviceSurfaceSupportKHR      (Get ("vkGetPhysicalDeviceSurfaceSupportKHR");
      vkGetPhysicalDeviceSurfaceFormatsKHR      := To_Ptr_vkGetPhysicalDeviceSurfaceFormatsKHR      (Get ("vkGetPhysicalDeviceSurfaceFormatsKHR");
      vkGetPhysicalDeviceSurfaceCapabilitiesKHR := To_Ptr_vkGetPhysicalDeviceSurfaceCapabilitiesKHR (Get ("vkGetPhysicalDeviceSurfaceCapabilitiesKHR"));
      vkGetPhysicalDeviceSurfacePresentModesKHR := To_Ptr_vkGetPhysicalDeviceSurfacePresentModesKHR (Get ("vkGetPhysicalDeviceSurfacePresentModesKHR"));
      vkCreateSwapchainKHR                      := To_Ptr_vkCreateSwapchainKHR                      (Get ("vkCreateSwapchainKHR");
      vkGetSwapchainImagesKHR                   := To_Ptr_vkGetSwapchainImagesKHR                   (Get ("vkGetSwapchainImagesKHR");
      vkAllocateCommandBuffers                  := To_Ptr_vkAllocateCommandBuffers                  (Get ("vkAllocateCommandBuffers");
      vkBeginCommandBuffer                      := To_Ptr_vkBeginCommandBuffer                      (Get ("vkBeginCommandBuffer");
      vkCmdPipelineBarrier                      := To_Ptr_vkCmdPipelineBarrier                      (Get ("vkCmdPipelineBarrier");
      vkEndCommandBuffer                        := To_Ptr_vkEndCommandBuffer                        (Get ("vkEndCommandBuffer");
      vkQueueSubmit                             := To_Ptr_vkQueueSubmit                             (Get ("vkQueueSubmit");
      vkQueueWaitIdle                           := To_Ptr_vkQueueWaitIdle                           (Get ("vkQueueWaitIdle");
      vkFreeCommandBuffers                      := To_Ptr_vkFreeCommandBuffers                      (Get ("vkFreeCommandBuffers");
      vkCreateSemaphore                         := To_Ptr_vkCreateSemaphore                         (Get ("vkCreateSemaphore");
      vkDeviceWaitIdle                          := To_Ptr_vkDeviceWaitIdle                          (Get ("vkDeviceWaitIdle");
      vkDestroySemaphore                        := To_Ptr_vkDestroySemaphore                        (Get ("vkDestroySemaphore");
      vkDestroyCommandPool                      := To_Ptr_vkDestroyCommandPool                      (Get ("vkDestroyCommandPool");
      vkDestroySwapchainKHR                     := To_Ptr_vkDestroySwapchainKHR                     (Get ("vkDestroySwapchainKHR");
      vkDestroyDevice                           := To_Ptr_vkDestroyDevice                           (Get ("vkDestroyDevice");
      vkDestroyInstance                         := To_Ptr_vkDestroyInstance                         (Get ("vkDestroyInstance");
      vkAcquireNextImageKHR                     := To_Ptr_vkAcquireNextImageKHR                     (Get ("vkAcquireNextImageKHR");
      vkQueuePresentKHR                         := To_Ptr_vkQueuePresentKHR                         (Get ("vkQueuePresentKHR");
      vkWaitForFences                           := To_Ptr_vkWaitForFences                           (Get ("vkWaitForFences");
      vkResetFences                             := To_Ptr_vkResetFences                             (Get ("vkResetFences");
      vkGetFenceStatus                          := To_Ptr_vkGetFenceStatus                          (Get ("vkGetFenceStatus");
      vkDestroyFence                            := To_Ptr_vkDestroyFence                            (Get ("vkDestroyFence");
      vkCreateFence                             := To_Ptr_vkCreateFence                             (Get ("vkCreateFence");
    end;
end;
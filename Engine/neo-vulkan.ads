
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

-- Custom binding to the Vulkan API: http://web.archive.org/web/20160914120446/https://www.khronos.org/registry/vulkan/specs/1.0-extensions/pdf/vkspec.pdf
package Neo.Vulkan is

  -- Initialization of global function pointers with OS specific driver
  generic
    with Get_Vulkan_Subprogram (Name : Str);
  procedure Initialize_Vulkan;

  -----------
  -- Types --
  -----------

  -- uint32_t 

  ------------
  -- Macros --
  ------------

  VK_KHR_SURFACE_EXTENSION_NAME       : constant Str_8_C := "VK_KHR_surface";
  VK_KHR_WIN32_SURFACE_EXTENSION_NAME : constant Str_8_C := "VK_KHR_win32_surface";

  --------------
  -- Buffers --
  -------------

  type VkPhysicalDevice_Array is array (Int_32_Positive range <>) of VkPhysicalDevice with Convention => C;
  type VkQueueFamilyProperties_Array is array (Int_32_Positive range <>) of VkQueueFamilyProperties with Convention => C;

  ----------------
  -- Structures --
  ----------------

  -----------------
  -- Conversions --
  -----------------

  ---------------
  -- Functions --
  ---------------

  -- 
  type vkCreateWin32SurfaceKHR is access all function () -- ???
                                                       with Convention => C;
  vkCreateWin32SurfaceKHR : Ptr_vkCreateWin32SurfaceKHR;
  function To_Ptr_vkCreateWin32SurfaceKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkCreateWin32SurfaceKHR);

  -- 
  type Ptr_vkGetPhysicalDeviceProperties is access procedure (physicalDevice :        VkPhysicalDevice;           -- VkPhysicalDevice
                                                              pProperties    : access VkPhysicalDeviceProperties) -- VkPhysicalDeviceProperties*
                                                              with Convention => C;
  vkGetPhysicalDeviceProperties : Ptr_vkGetPhysicalDeviceProperties;
  function To_Ptr_vkGetPhysicalDeviceProperties is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceProperties);

  --
  type Ptr_vkGetPhysicalDeviceMemoryProperties is access procedure (physicalDevice    :        VkPhysicalDevice;                 -- VkPhysicalDevice
                                                                    pMemoryProperties : access VkPhysicalDeviceMemoryProperties) -- VkPhysicalDeviceMemoryProperties*
                                                                    with Convention => C;
  vkGetPhysicalDeviceMemoryProperties : Ptr_vkGetPhysicalDeviceMemoryProperties;
  function To_Ptr_vkGetPhysicalDeviceMemoryProperties is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceMemoryProperties);

  --
  type Ptr_vkGetPhysicalDeviceQueueFamilyProperties is access procedure (arg1 : VkPhysicalDevice; --
                                                                         arg2 : access stdint_h.uint32_t; --
                                                                         arg3 : access VkQueueFamilyProperties) --
                                                                         with Convention => C;
  vkGetPhysicalDeviceQueueFamilyProperties : Ptr_vkGetPhysicalDeviceQueueFamilyProperties;
  function To_Ptr_vkGetPhysicalDeviceQueueFamilyProperties is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceQueueFamilyProperties);

  -- 
  type Ptr_vkGetDeviceQueue is access procedure (device           : VkDevice; -- VkDevice
                                                 queueFamilyIndex : Int_32_Unsigned_C; -- uint32_t 
                                                 queueIndex       : Int_32_Unsigned_C; -- uint32_t 
                                                 pQueue           : Ptr)               -- VkQueue*
                                                 with Convention => C;
  vkGetDeviceQueue : Ptr_vkGetDeviceQueue;
  function To_Ptr_vkGetDeviceQueue is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetDeviceQueue);

  --
  type Ptr_vkCreateInstance is access function (arg1 : Ptr; -- 
                                                arg2 : Ptr; -- 
                                                arg3 : Ptr) -- 
                                                return VkResult --
                                                with Convention => C;
  Ptr_vkCreateInstance : Ptr_vkCreateInstance; 
  function To_Ptr_vkCreateInstance is new Ada.Unchecked_Conversion (Ptr, Ptr_vkCreateInstance);

  -- 
  type Ptr_vkEnumeratePhysicalDevices is access function (arg1 : VkInstance; -- 
                                                          arg2 : access stdint_h.uint32_t; -- 
                                                          arg3 : Ptr) -- 
                                                          return VkResult -- 
                                                          with Convention => C;
  vkEnumeratePhysicalDevices : Ptr_vkEnumeratePhysicalDevices;
  function To_Ptr_vkEnumeratePhysicalDevices is new Ada.Unchecked_Conversion (Ptr, Ptr_vkEnumeratePhysicalDevices);

  -- 
  type Ptr_vkCreateDevice is access function (arg1 : VkPhysicalDevice; -- 
                                              arg2 : Ptr; -- 
                                              arg3 : Ptr; -- 
                                              arg4 : Ptr) -- 
                                              return VkResult --
                                              with Convention => C;
  vkCreateDevice : Ptr_vkCreateDevice;
  function To_Ptr_vkCreateDevice is new Ada.Unchecked_Conversion (Ptr,   Ptr_vkCreateDevice);

  -- 
  type Ptr_vkCreateCommandPool is access function (arg1 : VkDevice; -- 
                                                   arg2 : Ptr; -- 
                                                   arg3 : Ptr; -- 
                                                   arg4 : Ptr) -- 
                                                   return VkResult -- 
                                                   with Convention => C;
  vkCreateCommandPool : Ptr_vkCreateCommandPool;
  function To_Ptr_vkCreateCommandPool is new Ada.Unchecked_Conversion (Ptr, Ptr_vkCreateCommandPool);

  -- 
  type Ptr_vkGetPhysicalDeviceSurfaceSupportKHR is access function (arg1 : VkPhysicalDevice; -- 
                                                                    arg2 : stdint_h.uint32_t; -- 
                                                                    arg3 : VkSurfaceKHR; -- 
                                                                    arg4 : access VkBool32) -- 
                                                                    return VkResult -- 
                                                                    with Convention => C;
  vkGetPhysicalDeviceSurfaceSupportKHR : Ptr_vkGetPhysicalDeviceSurfaceSupportKHR;    
  function To_Ptr_vkGetPhysicalDeviceSurfaceSupportKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceSurfaceSupportKHR);

  -- 
  type Ptr_vkGetPhysicalDeviceSurfaceFormatsKHR is access function (arg1 : VkPhysicalDevice; -- 
                                                                    arg2 : VkSurfaceKHR; --
                                                                    arg3 : access stdint_h.uint32_t; -- 
                                                                    arg4 : access VkSurfaceFormatKHR) -- 
                                                                    return VkResult -- 
                                                                    with Convention => C;
  vkGetPhysicalDeviceSurfaceFormatsKHR : Ptr_vkGetPhysicalDeviceSurfaceFormatsKHR; 
  function To_Ptr_vkGetPhysicalDeviceSurfaceFormatsKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceSurfaceFormatsKHR);

  -- 
  type Ptr_vkGetPhysicalDeviceSurfaceCapabilitiesKHR is access function (arg1 : VkPhysicalDevice; --
                                                                         arg2 : VkSurfaceKHR; -- 
                                                                         arg3 : access VkSurfaceCapabilitiesKHR) -- 
                                                                         return VkResult -- 
                                                                         with Convention => C;
  vkGetPhysicalDeviceSurfaceCapabilitiesKHR : Ptr_vkGetPhysicalDeviceSurfaceCapabilitiesKHR;
  function To_Ptr_vkGetPhysicalDeviceSurfaceCapabilitiesKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceSurfaceCapabilitiesKHR);

  -- 
  type Ptr_vkGetPhysicalDeviceSurfacePresentModesKHR is access function (arg1 : VkPhysicalDevice; -- 
                                                                         arg2 : VkSurfaceKHR; -- 
                                                                         arg3 : access stdint_h.uint32_t; -- 
                                                                         arg4 : access VkPresentModeKHR) -- 
                                                                         return VkResult -- 
                                                                         with Convention => C;
  vkGetPhysicalDeviceSurfacePresentModesKHR : Ptr_vkGetPhysicalDeviceSurfacePresentModesKHR;
  function To_Ptr_vkGetPhysicalDeviceSurfacePresentModesKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetPhysicalDeviceSurfacePresentModesKHR);

  -- 
  type Ptr_vkCreateSwapchainKHR is access function (arg1 : VkDevice; -- 
                                                    arg2 : Ptr; -- 
                                                    arg3 : Ptr; -- 
                                                    arg4 : Ptr) -- 
                                                    return VkResult -- 
                                                    with Convention => C;
  vkCreateSwapchainKHR : Ptr_vkCreateSwapchainKHR;
  function To_Ptr_vkCreateSwapchainKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkCreateSwapchainKHR);

  -- 
  type Ptr_vkGetSwapchainImagesKHR is access function (arg1 : VkDevice; --
                                                       arg2 : VkSwapchainKHR; --
                                                       arg3 : access stdint_h.uint32_t; --
                                                       arg4 : Ptr) -- 
                                                       return VkResult --
                                                       with Convention => C;
  vkGetSwapchainImagesKHR : Ptr_vkGetSwapchainImagesKHR;
  function To_Ptr_vkGetSwapchainImagesKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetSwapchainImagesKHR);

  -- 
  type Ptr_vkAllocateCommandBuffers is access function (arg1 : VkDevice; -- 
                                                        arg2 : Ptr; -- 
                                                        arg3 : Ptr) -- 
                                                        return VkResult -- 
                                                        with Convention => C;
  vkAllocateCommandBuffers : Ptr_vkAllocateCommandBuffers;
  function To_Ptr_vkAllocateCommandBuffers is new Ada.Unchecked_Conversion (Ptr, Ptr_vkAllocateCommandBuffers);

  -- 
  type Ptr_vkBeginCommandBuffer is access function (arg1 : VkCommandBuffer; -- 
                                                    arg2 : Ptr) -- 
                                                    return VkResult -- 
                                                    with Convention => C;
  vkBeginCommandBuffeR : Ptr_vkBeginCommandBuffer;
  function To_Ptr_vkBeginCommandBuffer is new Ada.Unchecked_Conversion (Ptr, ptr_vkBeginCommandBuffer);

  -- 
  type Ptr_vkCmdPipelineBarrier is access procedure (arg1 : VkCommandBuffer; -- 
                                                     arg2 : VkPipelineStageFlags; -- 
                                                     arg3 : VkPipelineStageFlags; -- 
                                                     arg4 : VkDependencyFlags; -- 
                                                     arg5 : stdint_h.uint32_t; -- 
                                                     arg6 : Ptr; -- 
                                                     arg7 : stdint_h.uint32_t; -- 
                                                     arg8 : Ptr; -- 
                                                     arg9 : stdint_h.uint32_t; -- 
                                                     arg10 : Ptr) -- 
                                                     with Convention => C;
  vkCmdPipelineBarrier : Ptr_vkCmdPipelineBarrier;
  function To_Ptr_vkCmdPipelineBarrier is new Ada.Unchecked_Conversion (Ptr, Ptr_vkCmdPipelineBarrier);

  -- 
  type Ptr_vkEndCommandBuffer is access function (arg1 : VkCommandBuffer) -- 
                                                  return VkResult -- 
                                                  with Convention => C;
  vkEndCommandBuffer : Ptr_vkEndCommandBuffer;
  function To_Ptr_vkEndCommandBuffer is new Ada.Unchecked_Conversion (Ptr, Ptr_vkEndCommandBuffer);

  -- 
  type Ptr_vkQueueSubmit is access function (arg1 : VkQueue; -- 
                                             arg2 : stdint_h.uint32_t; -- 
                                             arg3 : Ptr; -- 
                                             arg4 : VkFence) -- 
                                             return VkResult -- 
                                             with Convention => C;
  vkQueueSubmit : Ptr_vkQueueSubmit;
  function To_Ptr_vkQueueSubmit is new Ada.Unchecked_Conversion (Ptr, Ptr_vkQueueSubmit);

  -- 
  type Ptr_vkQueueWaitIdle is access function (arg1 : VkQueue) -- 
                                               return VkResult -- 
                                               with Convention => C;
  Ptr_vkQueueWaitIdle : Ptr_vkQueueWaitIdle; 
  function To_Ptr_vkQueueWaitIdle is new Ada.Unchecked_Conversion (Ptr, Ptr_vkQueueWaitIdle);

  -- 
  type Ptr_vkFreeCommandBuffers is access procedure (arg1 : VkDevice; -- 
                                                     arg2 : VkCommandPool; -- 
                                                     arg3 : stdint_h.uint32_t; -- 
                                                     arg4 : Ptr) -- 
                                                     with Convention => C;
  vkFreeCommandBuffers : Ptr_vkFreeCommandBuffers;
  function To_Ptr_vkFreeCommandBuffers is new Ada.Unchecked_Conversion (Ptr, Ptr_vkFreeCommandBuffers);

  -- 
  type Ptr_vkCreateSemaphore is access function (arg1 : VkDevice; -- 
                                                 arg2 : Ptr; -- 
                                                 arg3 : Ptr; -- 
                                                 arg4 : Ptr) -- 
                                                 return VkResult -- 
                                                 with Convention => C;
  vkCreateSemaphore : Ptr_vkCreateSemaphore;
  function To_Ptr_vkCreateSemaphore is new Ada.Unchecked_Conversion (Ptr, Ptr_vkCreateSemaphore);

  -- 
  type Ptr_vkDeviceWaitIdle is access function (arg1 : VkDevice) -- 
                                                return VkResult -- 
  vkDeviceWaitIdle : Ptr_vkDeviceWaitIdle;
  function To_Ptr_vkDeviceWaitIdle is new Ada.Unchecked_Conversion (Ptr, Ptr_vkDeviceWaitIdle);

  -- 
  type vkDestroySemaphore is access procedure vkDestroySemaphore (arg1 : VkDevice;
                                                                  arg2 : VkSemaphore;
                                                                  arg3 : Ptr; -- ???
                                                                  with Convention => C;
  vkDestroySemaphore : Ptr_vkDestroySemaphore;
  function To_Ptr_vkDestroySemaphore is new Ada.Unchecked_Conversion (Ptr, Ptr_vkDestroySemaphore);

  --   
  type Ptr_vkDestroyCommandPool is access procedure (arg1 : VkDevice; -- 
                                                     arg2 : VkCommandPool; -- 
                                                     arg3 : Ptr) -- 
                                                     with Convention => C;
  vkDestroyCommandPool : Ptr_vkDestroyCommandPool;
  function To_Ptr_vkDestroyCommandPool is new Ada.Unchecked_Conversion (Ptr, Ptr_vkDestroyCommandPool);

  -- 
  type Ptr_vkDestroySwapchainKHR is access procedure (arg1 : VkDevice; -- 
                                                      arg2 : VkSwapchainKHR; -- 
                                                      arg3 : Ptr) -- 
                                                      with Convention => C;
  vkDestroySwapchainKHR : Ptr_vkDestroySwapchainKHR;
  function To_Ptr_vkDestroySwapchainKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkDestroySwapchainKHR);

  -- 
  type Ptr_vkDestroyDevice is access procedure (arg1 : VkDevice --
                                                arg2 : Ptr) -- 
                                                with Convention => C;
  vkDestroyDevice : Ptr_vkDestroyDevice; 
  function To_Ptr_vkDestroyDevice is new Ada.Unchecked_Conversion (Ptr,  Ptr_vkDestroyDevice);

  -- 
  type Ptr_vkDestroyInstance is access procedure (arg1 : VkInstance; -- 
                                                  arg2 : Ptr; -- ???
                                                  with Convention => C;
  vkDestroyInstance : Ptr_vkDestroyInstance;
  function To_Ptr_vkDestroyInstance is new Ada.Unchecked_Conversion (Ptr, Ptr_vkDestroyInstance);

  -- 
  type Ptr_vkAcquireNextImageKHR is access function (arg1 : VkDevice; -- 
                                                     arg2 : VkSwapchainKHR; -- 
                                                     arg3 : stdint_h.uint64_t; -- 
                                                     arg4 : VkSemaphore; -- 
                                                     arg5 : VkFence; -- 
                                                     arg6 : access stdint_h.uint32_t) -- 
                                                     return VkResult -- 
                                                     with Convention => C;
  vkAcquireNextImageKHR : Ptr_vkAcquireNextImageKHR;
  function To_Ptr_vkAcquireNextImageKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkAcquireNextImageKHR);


  type Ptr_vkQueuePresentKHR is access function (arg1 : VkQueue;
                                                 arg2 : Ptr)
                                                 return VkResult
                                                 with Convention => C;
  vkQueuePresentKHR : Ptr_vkQueuePresentKHR;
  function To_Ptr_vkQueuePresentKHR is new Ada.Unchecked_Conversion (Ptr, Ptr_vkQueuePresentKHR);

  -- 
  type Ptr_vkWaitForFences is access function (arg1 : VkDevice; -- 
                                               arg2 : stdint_h.uint32_t; -- 
                                               arg3 : Ptr; -- 
                                               arg4 : VkBool32; -- 
                                               arg5 : stdint_h.uint64_t) -- 
                                               return VkResult -- 
                                               with Convention => C;
  vkWaitForFences : Ptr_vkWaitForFences; 
  function To_Ptr_vkWaitForFences is new Ada.Unchecked_Conversion (Ptr, Ptr_vkWaitForFences);

  -- 
  type Ptr_vkResetFences is access function (arg1 : VkDevice; -- 
                                             arg2 : stdint_h.uint32_t; -- 
                                             arg3 : Ptr) -- 
                                             return VkResult -- 
                                             with Convention => C;
  vkResetFences : Ptr_vkResetFences;
  function To_Ptr_vkResetFences is new Ada.Unchecked_Conversion (Ptr, Ptr_vkResetFences);

  -- 
  type Ptr_vkGetFenceStatus is access function (arg1 : VkDevice; -- 
                                                arg2 : VkFence)  -- 
                                                return VkResult  -- 
                                                with Convention => C; 
  vkGetFenceStatus : Ptr_vkGetFenceStatus; 
  function To_Ptr_vkGetFenceStatus is new Ada.Unchecked_Conversion (Ptr, Ptr_vkGetFenceStatus);

  -- 
  type Ptr_vkDestroyFence is access procedure (arg1 : VkDevice; -- 
                                               arg2 : VkFence; -- 
                                               arg3 : Ptr) -- 
                                               with Convention => C;
  vkDestroyFence : Ptr_vkDestroyFence;
  function To_Ptr_vkDestroyFence is new Ada.Unchecked_Conversion (Ptr, Ptr_vkDestroyFence);

  -- 
  type Ptr_vkCreateFence is access function (arg1 : VkDevice; -- 
                                             arg2 : Ptr; -- 
                                             arg3 : Ptr; -- 
                                             arg4 : Ptr) -- 
                                             return VkResult -- 
                                             with Convention => C;
  vkCreateFence : Ptr_vkCreateFence;
  function To_Ptr_vkCreateFence is new Ada.Unchecked_Conversion (Ptr, Ptr_vkCreateFence 
end;
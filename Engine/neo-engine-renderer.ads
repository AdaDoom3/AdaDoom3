
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
package Neo.Engine.Renderer is

  --------------
  -- Settings --
  --------------

  -- Size of the global joint buffer uniform size in bytes and shader accessability
  JOINT_BUFFER_BINDING    : constant Int_Unsigned_C    := 3;
  JOINT_BUFFER_LAYOUT_SET : constant Int_Unsigned_C    := 3;
  JOINT_BUFFER_SIZE       : constant Int_64_Unsigned_C := Int_64_Unsigned_C (kB);

  -- Frequency of garbage collection
  GARBAGE_POLLING_DURATION : constant Duration := 0.2;

  -- Acceptable image formats for our render targets
  SUPPORTED_SWAPCHAIN_FORMAT : constant VkSurfaceFormatKHR   := (VK_FORMAT_B8G8R8A8_UNORM,     VK_COLOR_SPACE_SRGB_NONLINEAR_KHR);
  SUPPORTED_DEPTH_FORMATS    : constant Array_Int_Unsigned_C := (VK_FORMAT_D32_SFLOAT_S8_UINT, VK_FORMAT_D24_UNORM_S8_UINT);

  -- Feature extensions
  DEVICE_EXTENSIONS   : constant Array_Ptr_Char_8_C := (C (VK_KHR_MAINTENANCE1_NAME),
                                                        C (VK_KHR_MAINTENANCE2_NAME),
                                                        C (VK_KHR_BIND_MEMORY2_NAME),
                                                        C (VK_KHR_IMAGE_FORMAT_LIST_NAME),
                                                        C (VK_KHR_SWAPCHAIN_EXTENSION_NAME),
                                                        C (VK_KHR_GET_MEMORY_REQUIREMENTS2_NAME));
  DEBUGING_EXTENSIONS : constant Array_Ptr_Char_8_C := (C (VK_LAYER_LUNARG_API_DUMP_EXTENSION_NAME),
                                                        C (VK_LAYER_LUNARG_CORE_VALIDATION_NAME),
                                                        C (VK_LAYER_LUNARG_OBJECT_TRACKER_NAME),
                                                        C (VK_LAYER_LUNARG_PARAMETER_VALIDATION_NAME));

  -- Must match Static_Vertex_State and Animated_Vertex_State from Neo.Data.Model
  STATIC_VERTEX_ATTRIBUTES : constant Array_VkVertexInputAttributeDescription := ((0, 0, VK_FORMAT_R32G32B32A32_SFLOAT, 0 / Byte'Size),  -- Position
                                                                                  (0, 0, VK_FORMAT_R32G32B32A32_SFLOAT, 0 / Byte'Size)); -- Velocity
  ANIMATED_VERTEX_ATTRIBUTES : constant Array_VkVertexInputAttributeDescription := ((0, 1, VK_FORMAT_R16G16_SFLOAT, 0),  -- Texture
                                                                                    (0, 1, VK_FORMAT_R16G16_SFLOAT, 0),  -- Point
                                                                                    (0, 1, VK_FORMAT_R16G16_SFLOAT, 0),  -- Normal
                                                                                    (0, 1, VK_FORMAT_R16G16_SFLOAT, 0)); -- Velocity

  -------------
  -- Handles --
  -------------

  Queue,           -- VkQueue
  Device,          -- VkDevice
  Surface,         -- VkSurfaceKHR
  Instance,        -- VkInstance
  Swapchain,       -- VkSwapchainKHR
  Render_Pass,     -- VkRenderPass
  Command_Pool,    -- VkCommandPool
  Pipeline_Cache,  -- VkPipelineCache
  Pipeline_Layout, -- VkPipelineLayout
  Descriptor_Pool, -- VkDescriptorPool
  Physical_Device, -- VkPhysicalDevice
  Current_Pipeline -- VkPipeline
    : aliased Ptr := NULL_PTR;

  -----------------
  -- Information --
  -----------------

  Queue_Family      : aliased Int_Unsigned_C                   := 0;
  Surface_Extent    : aliased VkExtent2D                       := (others => <>);
  Swapchain_Format  : aliased VkSurfaceFormatKHR               := (others => <>);
  Depth_Format      : aliased VkSurfaceFormatKHR               := (others => <>);
  Surface_Details   : aliased VkSurfaceCapabilitiesKHR         := (others => <>);
  Format_Properties : aliased VkFormatProperties               := (others => <>);
  Image_Properties  : aliased VkImageFormatProperties          := (others => <>);
  Device_Limits     : aliased VkPhysicalDeviceLimits           := (others => <>);
  Device_Properties : aliased VkPhysicalDeviceProperties       := (others => <>);
  Memory_Properties : aliased VkPhysicalDeviceMemoryProperties := (others => <>);

  ------------
  -- Memory --
  ------------
  --
  -- https://developer.nvidia.com/vulkan-memory-management
  --

  type Filter_Kind     is (Nearest_Filter,  Linear_Filter);
  type Buffer_Kind     is (Uniform_Buffer,  Image_Buffer,      Other_Buffer);
  type Usage_Kind      is (GPU_Usage,       CPU_Usage,         CPU_To_GPU_Usage,        GPU_To_CPU_Usage);
  type Allocation_Kind is (Free_Allocation, Buffer_Allocation, Linear_Image_Allocation, Optimal_Image_Allocation);

  -- Memory element within a Heap
  type Allocation_State;
  type Ptr_Allocation_State is access all Allocation_State;
  type Allocation_State is record
      ID             : Int_Unsigned_C       := 0;
      Offset         : Int_64_Unsigned_C    := 0;
      Size           : Int_64_Unsigned_C    := 0;
      Kind           : Allocation_Kind      := Free_Allocation;
      Next, Previous : Ptr_Allocation_State := null;
    end record;
  procedure Free is new Unchecked_Deallocation (Allocation_State, Ptr_Allocation_State);

  -- Dynamically allocatable list of memory allocations which share the same usage
  type Heap_State (Usage : Usage_Kind := GPU_Usage; Size : Int_64_Unsigned_C := 0; Index : Int_Unsigned_C := 0) is record
      Next_ID       :         Int_Unsigned_C       := 0;
      Offset        :         Int_64_Unsigned_C    := 0;
      Allocated     :         Int_64_Unsigned_C    := 0;
      First_Chunk   :         Ptr_Allocation_State := null;
      Device_Memory : aliased Ptr                  := NULL_PTR; -- VkDeviceMemory
      Data          : aliased Ptr                  := NULL_PTR;
    end record;
  package Vector_Heap is new Neo.Core.Vectors (Heap_State); use Vector_Heap.Unsafe;

  -- Internal data structure to aggregate VkBuffer and VkImage object states
  type Buffer_State (Usage : Usage_Kind := GPU_Usage; Kind : Buffer_Kind := Other_Buffer) is record
      Heap          :         Vector_Heap.Cursor := Vector_Heap.NO_ELEMENT;
      ID            :         Int_Unsigned_C     := 0;
      Count         :         Int_32_Unsigned_C  := 0;
      Size          :         Int_64_Unsigned_C  := 0; -- In bytes
      Offset        :         Int_64_Unsigned_C  := 0;
      Device_Memory : aliased Ptr                := NULL_PTR; -- VkDeviceMemory
      Data          : aliased Ptr                := NULL_PTR; -- VkBuffer or VkImage
      case Kind is
        when Image_Buffer =>
          References :         Int_Unsigned_C := 0;
          Filter     :         Filter_Kind    := Nearest_Filter;
          View       : aliased Ptr            := NULL_PTR; -- VkImageView
          Sampler    : aliased Ptr            := NULL_PTR; -- VkSampler
        when Uniform_Buffer =>
          Binding    : Int_Unsigned_C := 0;
          Layout_Set : Int_Unsigned_C := 0;
      when Other_Buffer => null; end case;
    end record;
  package Vector_Buffer is new Neo.Core.Vectors (Buffer_State);
  package Hashed_Buffer is new Neo.Core.Hashed (Buffer_State); use Hashed_Buffer.Unsafe;

  type Buffer_Surface_State (Is_Animated : Bool := False) is record
      Material : Str_Unbound  := NULL_STR_UNBOUND;
      Vertices : Buffer_State := (others => <>);
      Indicies : Buffer_State := (others => <>);
      case Is_Animated is when True =>
        Weights : Buffer_State := (others => <>);
      when False => null; end case;
    end record;
  package Vector_Buffer_Surface is new Neo.Core.Vectors (Buffer_Surface_State); use Vector_Buffer_Surface.Unsafe;
  package Hashed_Vector_Buffer_Surface is new Neo.Core.Hashed (Vector_Buffer_Surface.Unsafe.Vector);

  -------------
  -- Staging --
  -------------

  protected Safe_Staging is
      procedure Set_Buffer (Buffer : in out Buffer_State; Data : Ptr; Data_Size : Int_Ptr);
      procedure Initialize;
      procedure Finalize;
    private
      Staging_Device_Memory, -- VkDeviceMemory
      Staging_Command_Pool,  -- VkCommandPool
      Staging_Commands,      -- VkCommandBuffer
      Staging_Buffer,        -- VkBuffer
      Staging_Fence,         -- VkFence
      Staging_Data   : aliased Ptr     := NULL_PTR;
      Staging_Offset : aliased Int_Ptr := 0;
    end;

  ----------------
  -- Allocation --
  ----------------

  protected Safe_Allocation is
      procedure Throw_Away       (Buffer : Buffer_State);
      procedure Allocate_Buffer  (Buffer : in out Buffer_State);
      procedure Finalize_Buffers (Force_Total_Finalization : Bool := False);
    private
      Buffered_Garbage : Vector_Buffer.Unsafe.Vector;
      Heaps            : Vector_Heap.Unsafe.Vector;
    end;

  -- ???
  procedure Initialize_Buffer (Buffer : in out Buffer_State; Usage_Bits : Int_Unsigned_C; Data : Ptr; Data_Size : Int_Ptr; Count : Count_Type);
  function Find_Memory_Type_Index (Memory_Type_Bits : Int_Unsigned_C; Usage : Usage_Kind) return Int_Unsigned_C;

  ---------------
  -- Buffering --
  ---------------

  procedure Unbuffer_Mesh     (Path : Str);
  procedure Buffer_Mesh       (Path : Str);
  procedure Unbuffer_Material (Path : Str);
  procedure Buffer_Material   (Path : Str);

  Buffered_Uniforms  : Hashed_Buffer.Unsafe.Map; -- Unsafe, should only be used by the backend
  Buffered_Images    : Hashed_Buffer.Safe_Map;
  Buffered_Meshes    : Hashed_Vector_Buffer_Surface.Safe_Map;
  Buffered_Materials : Hashed_Material.Safe_Map;

  -- For user-defined marking and resource tracking
  procedure Free_Unreferenced_Materials;
  procedure Add_Material_Reference    (Path : Str);
  procedure Remove_Material_Reference (Path : Str);

  -----------------
  -- Framebuffer --
  -----------------

  type Framebuffer_State is record
      Fence,           -- VkFence
      Image,           -- VkImage
      Image_View,      -- VkImageView
      Commands,        -- VkCommandBuffer
      -- Descriptor_Pool, -- VkDescriptorPool
      Render_Status,   -- VkSemaphore
      Acquire_Status,  -- VkSemaphore
      Swapchain_Buffer -- VkFramebuffer
        : aliased Ptr := NULL_PTR;
    end record;
  package Vector_Framebuffer is new Neo.Core.Vectors (Framebuffer_State);

  procedure Initialize_Framebuffer;
  procedure Restart_Framebuffer;
  procedure Finalize_Framebuffer;

  Framebuffer_Status : Safe_Status;
  Framebuffer        : Vector_Framebuffer.Ptr_Unsafe_Array;
  Depth_Image        : Buffer_State := (GPU_Usage, Image_Buffer, others => <>);

  -------------
  -- Shaders --
  -------------

  type Stage_Kind is (Fragment_Stage, Vertex_Stage, Tesselation_Stage, Geometry_Stage);
  type Stage_State (Kind : Stage_Kind := Fragment_Stage) is record
      Program  : aliased Ptr := NULL_PTR;
      Uniforms : Vector_Str_Unbound.Unsafe.Vector;
      case Kind is when Fragment_Stage => -- Fragment stages are the only ones that access samplers... could be too rigid
        Domain : Domain_Kind := Surface_Domain;
      when others => null; end case;
    end record;
  package Vector_Stage is new Neo.Core.Vectors (Stage_State); use Vector_Stage;

  type Shader_State is record
      Stages                  :         Vector_Stage.Unsafe.Vector;
      Stages_Info             :         Vector_VkPipelineShaderStageCreateInfo.Unsafe.Vector;
      Vertex_Inputs           :         Vector_VkPipelineVertexInputStateCreateInfo.Unsafe.Vector;
      Vertex_Input_Attributes :         Vector_VkVertexInputAttributeDescription.Unsafe.Vector;
      Pipeline_Stages         : aliased VkPipelineShaderStageCreateInfo := (others => <>);
      Pipeline_Layout         : aliased Ptr                             := NULL_PTR; -- VkPipelineLayout
      Descriptor_Set_Layout   : aliased Ptr                             := NULL_PTR; -- VkDescriptorSetLayout
    end record;
  package Map_Shader is new Neo.Core.Hashed (Shader_State);

  -- "Samplers" are texture parameters for GPU shaders
  generic
    Binding, Layout_Set : Int_Unsigned_C;
    Usage : Usage_Kind := GPU_Usage;
  package Sampler is
      procedure Set (Val : Str);
      function Get return Str;
      function Get_Binding return Int_Unsigned_C is (Binding);
    end;

  -- Global parameters accessable to shaders
  generic
    Name_ID : Str;
    type Uniform_T is private; -- Will NOT work with array types due to Ada's additional "dope"
    Binding, Layout_Set : Int_Unsigned_C;
    Usage : Usage_Kind := GPU_Usage;
  package Uniform is
      procedure Set (Val : Uniform_T);
      function Get return Uniform_T;
    end;

  -- ???
  generic
    Path   : Str;
    Stages : Vector_Stage.Unsafe_Array;
  package Shader is procedure Commit; end;

  type Write_Descriptor_State (Is_Image : Bool := False) is record
      Set : VkWriteDescriptorSet := (others => <>);
      case Is_Image is
        when True  => Image_Info  : VkDescriptorImageInfo  := (others => <>);
        when False => Buffer_Info : VkDescriptorBufferInfo := (others => <>);
      end case;
    end record;
  package Vector_Write_Descriptor is new Vectors (Write_Descriptor_State);

  Writes            : Vector_Write_Descriptor.Unsafe.Vector;
  Shaders           : Map_Shader.Unsafe.Map;
  Shader_Entry_Name : aliased Str_8_C      := "main";
  Sampler_Count     :         Natural      := 0;

  --------------
  -- Pipeline --
  --------------
  --
  -- ???
  --

  type Pipeline_State is record
      Shader : Shader_State := (others => <>);

      -- Rasterization
      Cull_Mode          : Int_Unsigned_C := VK_CULL_MODE_FRONT_BIT;          -- VkCullModeFlagBits
      Front_Face         : Int_Unsigned_C := VK_FRONT_FACE_COUNTER_CLOCKWISE; -- VkFrontFace
      Has_Mirror_View    : Bool           := False;
      Has_Polygon_Offset : Bool           := False;

      -- Color blending
      Color_Blend              : Int_Unsigned_C := VK_BLEND_OP_ADD;      -- VkBlendOp
      Alpha_Blend              : Int_Unsigned_C := VK_BLEND_OP_ADD;      -- VkBlendOp
      Source_Blend_Factor      : Int_Unsigned_C := VK_BLEND_FACTOR_ZERO; -- VkBlendFactor
      Destination_Blend_Factor : Int_Unsigned_C := VK_BLEND_FACTOR_ZERO; -- VkBlendFactor
      Color_Mask               : Int_Unsigned_C := 0;                    -- VkColorComponentFlags

      -- Depth stencil
      Front_Fail         : Int_Unsigned_C := VK_STENCIL_OP_KEEP;  -- VkStencilOp
      Front_Pass         : Int_Unsigned_C := VK_STENCIL_OP_KEEP;  -- VkStencilOp
      Front_Depth_Fail   : Int_Unsigned_C := VK_STENCIL_OP_KEEP;  -- VkStencilOp
      Back_Fail          : Int_Unsigned_C := VK_STENCIL_OP_KEEP;  -- VkStencilOp
      Back_Pass          : Int_Unsigned_C := VK_STENCIL_OP_KEEP;  -- VkStencilOp
      Back_Depth_Fail    : Int_Unsigned_C := VK_STENCIL_OP_KEEP;  -- VkStencilOp
      Stencil_Compare    : Int_Unsigned_C := VK_COMPARE_OP_NEVER; -- VkCompareOp
      Depth_Compare      : Int_Unsigned_C := VK_COMPARE_OP_NEVER; -- VkCompareOp
      Compare_Mask       : Int_Unsigned_C := 0;
      Stencil_Reference  : Int_Unsigned_C := 0;
      Depth_Write_Enable : Bool           := False;
      Depths_Bounds_Test : Bool           := False;
      Has_Back_Stencil   : Bool           := False;
    end record;

  type Buffered_Pipeline_State is record
      Val  : Pipeline_State := (others => <>);
      Data : Ptr            := NULL_PTR; -- VkPipeline
    end record;
  package Vector_Buffered_Pipeline is new Neo.Core.Vectors (Buffered_Pipeline_State);

  -- Stored pipelines for reference/caching
  Pipeline           : Pipeline_State          := (others => <>);
  Previous_Pipeline  : Buffered_Pipeline_State := (others => <>);
  Buffered_Pipelines : Vector_Buffered_Pipeline.Unsafe.Vector;

  -------------
  -- Drawing --
  -------------

  type Light_Kind       is (Point_Light,        Blend_Light,        Fog_Light);
  type Rendering_Kind   is (Doom3_Rendering,    FGED2_Rendering,    Raytrace_Rendering);
  type Interaction_Kind is (Shadow_Interaction, Direct_Interaction, Translucent_Interaction);

  type Surface_Sort_Array is array (Sort_Kind) of Bool;

  type Space_State is record
      MVP             : Matrix_4D := ZERO_MATRIX_4D;
      Local_To_Global : Matrix_4D := ZERO_MATRIX_4D;
      Local_To_Eye    : Matrix_4D := ZERO_MATRIX_4D;
    end record;

  type Bottom_Level_State (Is_Animated : Bool := False) is record
      -- Materials_Map     : Hashed_Str_Unbound.Unsafe.Map;
      Mesh              : Str_Unbound := NULL_STR_UNBOUND; -- All of the mesh surfaces must also be animated
      Space             : Space_State := (others => <>);
      Scissor           : Matrix_4D   := ZERO_MATRIX_4D;
      Model_Depth_Hack  : Bool        := False;
      Weapon_Depth_Hack : Bool        := False;
      From_Menu         : Bool        := False;
      case Is_Animated is when True => Pose : Treed_Joint.Unsafe.Tree; when False => null; end case;
    end record;
  package Vector_Bottom_Level_State        is new Neo.Core.Vectors (Bottom_Level_State);
  package Vector_Cursor_Bottom_Level_State is new Neo.Core.Vectors (Vector_Bottom_Level_State.Cursor);
  type Interaction_Array is array (Interaction_Kind) of Vector_Cursor_Bottom_Level_State.Unsafe.Vector;

  type Light_State is record
      Material           : Str_Unbound := NULL_STR_UNBOUND;
      Inverse_Projection : Matrix_4D   := ZERO_MATRIX_4D;
      Projection         : Matrix_4D   := ZERO_MATRIX_4D;
      Scissor            : Matrix_4D   := ZERO_MATRIX_4D;
      Origin             : Matrix_4D   := ZERO_MATRIX_4D;
      Fog_Plane          : Matrix_4D   := ZERO_MATRIX_4D;
      Interactions       : Interaction_Array;
    end record;
  package Vector_Light is new Neo.Core.Vectors (Light_State);
  type Array_Light is array (Light_Kind) of Vector_Light.Unsafe.Vector;

  type View_State is record
      Time_Stamp  : Time        := Clock;
      Is_Mirror   : Bool        := False;
      Origin      : Matrix_4D   := ZERO_MATRIX_4D;
      Port        : Matrix_4D   := ZERO_MATRIX_4D;
      Scissor     : Matrix_4D   := ZERO_MATRIX_4D;
      World_Space : Space_State := (others => <>);
      Surfaces    : Vector_Bottom_Level_State.Unsafe.Vector; -- Sorted by distance from origin
      Lights      : Array_Light; -- Each light kind vector is sorted by distance from the light interaction
    end record;
  package Safe_View is new Safe (View_State, (others => <>));

  -- Global data shared between the frontend and backend
  View         : Safe_View.T;
  Joint_Buffer : Buffer_State := (Size => JOINT_BUFFER_SIZE, others => <>);

  procedure Draw (Data : Bottom_Level_State; Commands : in out Ptr; Surface_Sort : Surface_Sort_Array);
  procedure Initialize_Drawing;
  procedure Finalize_Drawing;

  ---------------------
  -- Implementations --
  ---------------------

  procedure Run_Backend;
  procedure Run_Frontend;

  package FGED2 is
      procedure Build_Frame (Frame : in out Framebuffer_State; View : View_State);
      function Build_View return View_State;
    end;
  package Raytrace is
      procedure Build_Frame (Frame : in out Framebuffer_State; View : View_State);
      function Build_View return View_State;
    end;
  package Doom3 is
      procedure Build_Frame (Frame : in out Framebuffer_State; View : View_State);
      function Build_View return View_State;
    end;
end;















































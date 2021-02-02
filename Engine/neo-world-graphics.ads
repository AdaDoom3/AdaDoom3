
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

with Neo.API.Vulkan; use Neo.API.Vulkan;
with Neo.Data.Game;  use Neo.Data.Game;

-- Renderer for the global engine state
package Neo.World.Graphics is

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
  DEVICE_EXTENSIONS   : constant Array_Ptr_Char_8_C := (C (VK_KHR_MAINTENANCE1),
                                                        C (VK_KHR_MAINTENANCE2),
                                                        C (VK_KHR_MAINTENANCE3),
                                                        C (VK_KHR_BIND_MEMORY2),
                                                        C (VK_KHR_IMAGE_FORMAT_LIST),
                                                        C (VK_KHR_SWAPCHAIN_EXTENSION),
                                                        C (VK_KHR_GET_MEMORY_REQUIREMENTS2));
  DEBUGING_EXTENSIONS : constant Array_Ptr_Char_8_C := (-- C (VK_EXT_DEBUG_UTILS),
                                                        -- C (VK_LAYER_LUNARG_API_DUMP_EXTENSION),
                                                        1 => C (VK_LAYER_KHRONOS_VALIDATION));

  -- Must match Static_Vertex_State and Animated_Vertex_State from Neo.Data.Model
  VERTEX_BINDING    : aliased VkVertexInputBindingDescription := (binding   => 0,
                                                                  stride    => Vertex_State'Object_Size / Byte'Size,
                                                                  inputRate => VK_VERTEX_INPUT_RATE_VERTEX);
  VERTEX_ATTRIBUTES : aliased Array_VkVertexInputAttributeDescription :=
                                                               ((format   => VK_FORMAT_R32G32B32_SFLOAT, -- Position
                                                                 location => 0,
                                                                 offset   => 0, others => <>),
                                                                (format   => VK_FORMAT_R32G32B32_SFLOAT, -- Normal
                                                                 location => 1,
                                                                 offset   => Vector_3D'Object_Size / Byte'Size, others => <>),
                                                                (format   => VK_FORMAT_R32G32_SFLOAT,    -- Texture_Coordiante
                                                                 location => 2,
                                                                 offset   => Vector_3D'Object_Size / Byte'Size * 2, others => <>));

  -------------
  -- Handles --
  -------------

  Queue,                 -- VkQueue
  Device,                -- VkDevice
  Surface,               -- VkSurfaceKHR
  Instance,              -- VkInstance
  Swapchain,             -- VkSwapchainKHR
  Render_Pass,           -- VkRenderPass
  Command_Pool,          -- VkCommandPool
  Pipeline_Cache,        -- VkPipelineCache
  Pipeline_Layout,       -- VkPipelineLayout
  Descriptor_Pool,       -- VkDescriptorPool
  Physical_Device,       -- VkPhysicalDevice
  Current_Pipeline,      -- VkPipeline
  Staging_Device_Memory, -- VkDeviceMemory
  Staging_Command_Pool,  -- VkCommandPool
  Staging_Commands,      -- VkCommandBuffer
  Staging_Buffer,        -- VkBuffer
  Staging_Fence,         -- VkFence
  Staging_Data
    : aliased Ptr := NULL_PTR;

  -----------------
  -- Information --
  -----------------

  Queue_Family      : aliased Int_Unsigned_C                   := 0;
  Swapchain_Info    : aliased VkSwapchainCreateInfoKHR         := (others => <>);
  Swapchain_Format  : aliased VkSurfaceFormatKHR               := (others => <>);
  Depth_Format      : aliased VkSurfaceFormatKHR               := (others => <>);
  Surface_Details   : aliased VkSurfaceCapabilitiesKHR         := (others => <>);
  Surface_Extent    : aliased VkExtent2D                       := (others => <>);
  Format_Properties : aliased VkFormatProperties               := (others => <>);
  Image_Properties  : aliased VkImageFormatProperties          := (others => <>);
  Device_Limits     : aliased VkPhysicalDeviceLimits           := (others => <>);
  Device_Properties : aliased VkPhysicalDeviceProperties       := (others => <>);
  Memory_Properties : aliased VkPhysicalDeviceMemoryProperties := (others => <>);

  ------------
  -- Memory --
  ------------
  -- https://developer.nvidia.com/vulkan-memory-management

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
          Width      :         Int_Unsigned_C := 0;
          Height     :         Int_Unsigned_C := 0;
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

  -- Buffered mesh table with segmenting
  type Buffered_Mesh_Segment_State is record
      Material : Str_Unbound  := NULL_STR_UNBOUND;
      Indices  : Buffer_State := (others => <>);
    end record;
  type Array_Buffered_Mesh_Segment is array (Positive range <>) of Buffered_Mesh_Segment_State;
  type Buffered_Mesh_State (Is_Animated : Bool; Segment_Count : Positive) is record
      Vertices : Buffer_State := (others => <>);
      Segments : Array_Buffered_Mesh_Segment (1..Segment_Count) := (others => <>);
      case Is_Animated is when True =>
        Weights : Buffer_State := (others => <>);
      when False => null; end case;
    end record;
  type Ptr_Buffered_Mesh_State is access all Buffered_Mesh_State;
  package Hashed_Buffer_Mesh is new Neo.Core.Hashed (Ptr_Buffered_Mesh_State);

  -----------
  -- Queue --
  -----------

  protected Safe_Queue is
      function vkQueuePresent (pPresentInfo : Ptr_VkPresentInfoKHR) -- const VkPresentInfoKHR*
                               return Int_Unsigned_C;               -- VkResult
      function vkQueueSubmit  (submitCount : Int_Unsigned_C;   -- uint32_t
                               pSubmits    : Ptr_VkSubmitInfo; -- const VkSubmitInfo*
                               fence       : Ptr)              -- VkFence
                               return Int_Unsigned_C;          -- VkResult
    end;

  ---------------
  -- Allocator --
  ---------------

  protected Safe_Allocation is
      procedure Throw_Away       (Buffer : Buffer_State);
      procedure Allocate_Buffer  (Buffer : in out Buffer_State);
      procedure Finalize_Buffers (Force_Total_Finalization : Bool := False);
    private
      Buffered_Garbage : Vector_Buffer.Unsafe.Vector;
      Heaps            : Vector_Heap.Unsafe.Vector;
    end;

  procedure Initialize_Buffer
    (Buffer : in out Buffer_State; Usage_Bits : Int_Unsigned_C; Data : Ptr; Data_Size : Int_Ptr; Count : Positive);
  procedure Set_Buffer (Buffer : in out Buffer_State; Data : Ptr; Data_Size : Int_Ptr);
  function Find_Memory_Type_Index (Memory_Type_Bits : Int_Unsigned_C; Usage : Usage_Kind) return Int_Unsigned_C;

  ---------------
  -- Buffering --
  ---------------

  procedure Unbuffer_Mesh     (Path : Str);
  procedure Buffer_Mesh       (Path : Str; Mesh : Ptr_Mesh_State);
  procedure Unbuffer_Material (Path : Str);
  procedure Buffer_Material   (Path : Str);

  Buffered_Uniforms  : Hashed_Buffer.Unsafe.Map; -- Unsafe, should only be used by the backend
  Buffered_Images    : Hashed_Buffer.Safe_Map;
  Buffered_Meshes    : Hashed_Buffer_Mesh.Safe_Map;
  Buffered_Materials : Hashed_Material.Safe_Map;

  -- For user-defined marking and resource tracking
  procedure Free_Unreferenced_Materials;
  procedure Add_Material_Reference    (Path : Str);
  procedure Remove_Material_Reference (Path : Str);

  -----------------
  -- Framebuffer --
  -----------------

  type Framebuffer_State is record
      Fence,            -- VkFence
      Image,            -- VkImage
      Image_View,       -- VkImageView
      Commands,         -- VkCommandBuffer
      Render_Status,    -- VkSemaphore
      Acquire_Status,   -- VkSemaphore
      Swapchain_Buffer, -- VkFramebuffer
      Descriptor_Set
        : aliased Ptr := NULL_PTR;
    end record;
  package Vector_Framebuffer is new Neo.Core.Vectors (Framebuffer_State);

  procedure Initialize_Framebuffer with Inline;
  procedure Restart_Framebuffer    with Inline;
  procedure Finalize_Framebuffer   with Inline;

  Framebuffer_Status : Safe_Status;
  Framebuffer        : Vector_Framebuffer.Ptr_Unsafe_Array;
  Depth_Image        : Buffer_State := (GPU_Usage, Image_Buffer, others => <>);

  -------------
  -- Shaders --
  -------------

  type Array_Uniforms is array (1..64) of Str_Unbound; -- Max uniforms is 64
  type Stage_Kind is (Fragment_Stage, Vertex_Stage, Tesselation_Stage, Geometry_Stage);
  type Stage_State (Kind : Stage_Kind := Fragment_Stage) is record
      Program  : aliased Ptr := NULL_PTR;
      Uniforms : Array_Uniforms := (others => NULL_STR_UNBOUND);
      --case Kind is when Fragment_Stage => -- Fragment stages are the only ones that access samplers... could be too rigid
      --  Domain : Domain_Kind := Surface_Domain;
      --when others => null; end case;
    end record;
  package Vector_Stage is new Neo.Core.Vectors (Stage_State); use Vector_Stage;

  type Shader_State is record
      Stages                :         Vector_Stage.Unsafe.Vector;
      Stages_Info           :         Vector_VkPipelineShaderStageCreateInfo.Unsafe.Vector;
      Pipeline_Layout       : aliased Ptr := NULL_PTR; -- VkPipelineLayout
      Descriptor_Set_Layout : aliased Ptr := NULL_PTR; -- VkDescriptorSetLayout
    end record;
  package Map_Shader is new Neo.Core.Hashed (Shader_State);

  -- "Samplers" are texture parameters for GPU shaders
  generic
    Binding : Int_Unsigned_C;
    Usage   : Usage_Kind := GPU_Usage;
  package Sampler is
      procedure Set (Val : Str);
      function Get return Str;
      function Get_Binding return Int_Unsigned_C is (Binding);
    end;

  -- Global parameters accessable to shaders
  generic
    Name_ID : Str;
    type Uniform_T is private; -- Will NOT work with array types due to Ada's additional "dope"
    Binding : Int_Unsigned_C;
    Usage   : Usage_Kind := GPU_Usage;
  package Uniform is
      procedure Set (Val : Uniform_T);
      function Get return Uniform_T;
      function Get_Binding return Int_Unsigned_C is (Binding);
    end;

  -- Shader stuff
  generic
    Path   : Str;
    Stages : Vector_Stage.Unsafe_Array;
  package Shader is procedure Commit; end;

  -- Writes to
  type Write_Descriptor_State (Is_Image : Bool := False) is record
      Set : VkWriteDescriptorSet := (others => <>);
      case Is_Image is
        when True  => Image_Info  : aliased VkDescriptorImageInfo  := (others => <>);
        when False => Buffer_Info : aliased VkDescriptorBufferInfo := (others => <>);
      end case;
    end record;
  package Vector_Write_Descriptor is new Vectors (Write_Descriptor_State);

  Writes            : Vector_Write_Descriptor.Unsafe.Vector;
  Shaders           : Map_Shader.Unsafe.Map;
  Shader_Entry_Name : aliased Str_8_C := "main";
  Sampler_Count     : Natural := 0;

  --------------
  -- Pipeline --
  --------------

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
  type Interaction_Kind is (Shadow_Interaction, Direct_Interaction, Translucent_Interaction);

  type Surface_Sort_Array is array (Sort_Kind) of Bool;

  type Space_State is record
      MVP             : Matrix_4D := ZERO_MATRIX_4D;
      Local_To_Global : Matrix_4D := ZERO_MATRIX_4D;
      Local_To_Eye    : Matrix_4D := ZERO_MATRIX_4D;
    end record;

  type Bottom_Level_State (Is_Animated : Bool := False) is record
      -- Materials_Map     : Hashed_Str_Unbound.Unsafe.Map;
      Mesh              : Ptr_Buffered_Mesh_State; --Str_Unbound := NULL_STR_UNBOUND; -- All of the mesh surfaces must also be animated
      Space             : Space_State := (others => <>);
      Origin            : Vector_3D   := (others => <>);
      Scissor           : Matrix_4D   := ZERO_MATRIX_4D;
      Model_Depth_Hack  : Bool        := False;
      Weapon_Depth_Hack : Bool        := False;
      From_Menu         : Bool        := False;
      --case Is_Animated is when True => Pose : Treed_Joint.Unsafe.Tree; when False => null; end case;
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

  procedure Draw (Data : Bottom_Level_State; Frame : in out Framebuffer_State; Surface_Sort : Surface_Sort_Array);
  procedure Initialize_Drawing;
  procedure Finalize_Drawing;

  --------------------
  -- Implementation --
  --------------------

  procedure Run_Backend;
  procedure Run_Frontend;

  package Renderer is
      procedure Build_Frame (Frame : in out Framebuffer_State; View : View_State);
      procedure Build_View  (View  : in out View_State);
    end;
end;















































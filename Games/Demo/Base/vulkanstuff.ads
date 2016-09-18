  generic
    type Vec_T is private;
  package Vectors is
      package Unsafe is new Ada.Containers.Indefinite_Vectors (Int_32_Positive, Vec_T); use Unsafe;
      subtype Cursor is Unsafe.Cursor;
      NO_ELEMENT : Cursor := Unsafe.NO_ELEMENT;
      protected type Vector is
          procedure Clear;
          procedure Set     (Data : Unsafe.Vector);
          procedure Next    (Pos : in out Cursor);
          procedure Replace (Pos :        Cursor;      New_Item : Vec_T);
          procedure Append                            (New_Item : Vec_T; Count : Int_32_Positive := 1);
          procedure Prepend                           (New_Item : Vec_T; Count : Int_32_Positive := 1);
          procedure Insert  (Before : Int_32_Positive; New_Item : Vec_T; Count : Int_32_Positive := 1);
          procedure Delete  (Index  : Int_32_Positive;                   Count : Int_32_Positive := 1);
          function Has      (Pos    : Cursor)          return Bool            is (Has (Pos));
          function Get      (Pos    : Cursor)          return Bool            is (Has (Pos));
          function Get      (Pos    : Cursor)          return Vec_T           is (Element (Pos));
          function Get      (Index  : Int_32_Positive) return Vec_T           is (Element (Data, Index));
          function Get                                 return Unsafe.Vector   is (Data);
          function First                               return Cursor          is (First (Data));
          function Length                              return Int_32_Positive is (Int_32_Positive (Length (Data)));
        private
          Data : Unsafe.Vector;
        end;
    end;
  generic
    type Map_T is private;
  package Hashed is
      package Unsafe is new Ada.Containers.Indefinite_Hashed_Maps (Str_16_Unbound, Map_T, Wide_Hash, "="); use Unsafe;
      subtype Cursor is Unsafe.Cursor;
      NO_ELEMENT : Cursor := Unsafe.NO_ELEMENT;
      protected type Map is
          procedure Clear;
          procedure Set     (Data : Unsafe.Map);
          procedure Next    (Pos : in out Cursor);
          procedure Delete  (Pos : in out Cursor);
          procedure Delete  (Key : Str_16);
          procedure Replace (Pos : Cursor; New_Item : Map_T);
          procedure Replace (Key : Str_16; New_Item : Map_T);
          procedure Insert  (Key : Str_16; New_Item : Map_T);
          function Has      (Key : Str_16) return Bool   is (Has (Find (Data, To_Str_16_Unbound (Key))));
          function Has      (Pos : Cursor) return Bool   is (Has (Pos));
          function Key      (Pos : Cursor) return Str_16 is (To_Str_16 (Key (Pos)));
          function Get      (Key : Str_16) return Map_T  is (Element (Data, To_Str_16_Unbound (Key)));
          function Get      (Pos : Cursor) return Map_T  is (Element (Pos));
          function Get                     return Map    is (Data);
          function First                   return Cursor is (First (Data));
        private
          Data : Unsafe.Map;
        end;
    end;
  generic
    type Key_T is (<>);
    type Map_T is private;
  package Ordered is
      package Unsafe is new Ada.Containers.Indefinite_Ordered_Maps (Key_T, Map_T, "<", "="); use Unsafe;
      subtype Cursor is Unsafe.Cursor;
      NO_ELEMENT : Cursor := Unsafe.NO_ELEMENT;
      protected type Map is
          procedure Clear;
          procedure Set     (Data : Unsafe.Map);
          procedure Next    (Pos : in out Cursor);
          procedure Delete  (Pos : in out Cursor);
          procedure Delete  (Key : Key_T);
          procedure Replace (Pos : Cursor; New_Item : Map_T);
          procedure Replace (Key : Key_T;  New_Item : Map_T);
          procedure Insert  (Key : Key_T;  New_Item : Map_T);
          function Has      (Key : Key_T)  return Bool   is (Has (Find (Data, Key)));
          function Has      (Pos : Cursor) return Bool   is (Has (Pos));
          function Key      (Pos : Cursor) return Key_T  is (Key (Pos));
          function Get      (Pos : Cursor) return Map_T  is (Element (Pos));
          function Get      (Key : Key_T)  return Map_T  is (Element (Data, Key));
          function Get                     return Map    is (Data);
          function First                   return Cursor is (First (Data));
        private
          Data : Unsafe.Map;
        end;
    end;

         procedure Clear
          procedure Iterate      (Parent : Cursor; Process : not null access procedure (Pos : Cursor));
          procedure Iterate_Back (Parent : Cursor; Process : not null access procedure (Pos : Cursor));
          procedure Prepend      (Parent : Cursor;         New_Item : Tree_T; Count : Int_32_Positive := 1);
          procedure Append       (Parent : Cursor;         New_Item : Tree_T; Count : Int_32_Positive := 1);
          procedure Insert       (Parent, Before : Cursor; New_Item : Tree_T; Count : Int_32_Positive := 1; Pos : out Cursor);
          procedure Copy_Subtree (Parent, Before, Source : Cursor);
          procedure Splice       (Parent, Before, Source : Cursor; Source_Tree : in out Tree);
          function Next          (Pos    : Cursor) return Cursor;
          function Previous      (Pos    : Cursor) return Cursor;
          function Parent        (Pos    : Cursor) return Cursor;
          function Is_Leaf       (Pos    : Cursor) return Bool;
          function Subtree_Nodes (Pos    : Cursor) return Int_32_Positive;
          function First         (Parent      : Cursor) return Cursor;
          function First         (Parent      : Cursor) return Tree_T;
          function Last          (Parent      : Cursor) return Tree_T;
          function Last          (Parent      : Cursor) return Cursor;
          function "="           (Left, Right : Cursor) return Bool;
          function "="           (Left, Right : Unsafe.Tree) return Bool;
          function Empty                                return Bool;
          function Nodes                                return Int_32_Positive;
          function Root                                 return Cursor; 

          function Has              (Pos : Cursor) return Bool;
          function Equal_Subtree    (Left_Pos  : Cursor; Right_Pos : Cursor) return Bool;
          function "="              (Left, Right : Tree) return Bool;
          function Is_Empty         (Container : Tree) return Bool;
          function Node_Count       (Container : Tree) return Count_Type;
          function Subtree_Nodes    (Pos : Cursor) return Count_Type;
          function Depth            (Pos : Cursor) return Count_Type;
          function Is_Root          (Pos : Cursor) return Bool;
          function Is_Leaf          (Pos : Cursor) return Bool;
          function Root             (Container : Tree) return Cursor;
          procedure Clear           (Container : in out Tree);
          function Element          (Pos : Cursor) return Element_Type;
          procedure Replace         (Container : in out Tree;  Pos  : Cursor; New_Item  : Element_Type);
          procedure Query           (Pos : Cursor; Process  : not null access procedure (Element : Element_Type));
          procedure Update          (Container : in out Tree; Pos  : Cursor; Process   : not null access procedure (Element : in out Element_Type));
          function Reference        (Container : aliased in out Tree; Pos  : Cursor) return Reference_Type;
          procedure Assign          (Target : in out Tree; Source : Tree);
          function Copy             (Source : Tree) return Tree;
          procedure Move            (Target : in out Tree; Source : in out Tree);
          procedure Delete_Leaf     (Container : in out Tree; Pos  : in out Cursor);
          procedure Delete_Subtree  (Container : in out Tree; Pos  : in out Cursor);
          procedure Swap            (Container : in out Tree; I, J      : Cursor);
          function Find             (Container : Tree; Item      : Element_Type) return Cursor;
          function Find_In_Subtree  (Pos : Cursor;Item     : Element_Type) return Cursor;
          function Ancestor_Find    (Pos : Cursor; Item     : Element_Type) return Cursor;
          function Contains         (Container : Tree; Item      : Element_Type) return Bool;
          procedure Iterate         (Container : Tree; Process   : not null access procedure (Pos : Cursor));
          procedure Iterate_Subtree (Pos  : Cursor; Process   : not null access procedure (Pos : Cursor));
          function Iterate          (Container : Tree) return Tree_Iterator_Interfaces.Forward_Iterator'Class;
          function Iterate_Subtree  (Pos : Cursor) return Tree_Iterator_Interfaces.Forward_Iterator'Class;
          function Iterate          (Container : Tree; Parent    : Cursor)  return Tree_Iterator_Interfaces.Reversible_Iterator'Class;
          function Child_Count      (Parent : Cursor) return Count_Type;
          function Child_Depth      (Parent, Child : Cursor) return Count_Type;
          procedure Insert          (Container : in out Tree; Parent        : Cursor; Before : Cursor; New_Item : Element_Type; Pos : out Cursor; Count : Count_Type := 1);
          procedure Insert          (Container : in out Tree; Parent        : Cursor; Before : Cursor; New_Item : Element_Type;                   Count : Count_Type := 1);
          procedure Prepend         (Container : in out Tree; Parent        : Cursor;                  New_Item : Element_Type;                   Count : Count_Type := 1);
          procedure Append          (Container : in out Tree; Parent        : Cursor;                  New_Item : Element_Type;                   Count : Count_Type := 1);
          procedure Delete          (Container : in out Tree; Parent        : Cursor);
          procedure Copy_Subtree    (Target    : in out Tree; Parent        : Cursor; Before : Cursor; Source : Cursor);
          procedure Splice_Subtree  (Target    : in out Tree; Parent        : Cursor; Before : Cursor; Source : in out Tree; Pos : in out Cursor);
          procedure Splice_Subtree  (Container : in out Tree; Parent        : Cursor; Before : Cursor;                       Pos : Cursor);
          procedure Splice          (Target    : in out Tree; Target_Parent : Cursor; Before : Cursor; Source : in out Tree; Source_Parent : Cursor);
          procedure Splice          (Container : in out Tree; Target_Parent : Cursor; Before : Cursor;                       Source_Parent : Cursor);
          function Parent           (Pos : Cursor) return Cursor;
          function First            (Parent : Cursor) return Cursor;
          function First            (Parent : Cursor) return Element_Type;
          function Last             (Parent : Cursor) return Cursor;
          function Last             (Parent : Cursor) return Element_Type;
          function Next             (Pos : Cursor) return Cursor;
          function Previous         (Pos : Cursor) return Cursor;
          procedure Next            (Pos : in out Cursor);
          procedure Previous        (Pos : in out Cursor);
          procedure Iterate         (Parent : Cursor; Process : not null access procedure (Pos : Cursor));
          procedure Reverse_Iterate (Parent : Cursor; Process : not null access procedure (Pos : Cursor));

  procedure Load_Mesh;
  procedure Load_Texture;
  procedure Load_Shader;
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBufferUsageFlags.html
  -- https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryPropertyFlags.html
  procedure Create_Buffer (Data                      : Address;
                           Device_Size               : Integer_Device_Size;
                           Buffer                    : Buffer_State;
                           Memory                    : Memory_State;
                           Information               : Information_State
                           Transfer_Kind             : Boolean := False
                           Texel_Kind                : Boolean := False
                           Buffer_Kind               : Boolean := False
                           Is_Index_Bindable         : Boolean := False
                           Is_Vertex_Bindable        : Boolean := False
                           Is_Indirectly_Commandable : Boolean := False);
  procedure Check_Command_Buffers;
  procedure Create_Setup_Command_Buffer;
  procedure Flush_Setup_Command_Buffer;
  procedure Create_Command_Buffers;
  procedure Creat


-- void VulkanExampleBase::setupSwapChain()
-- void VulkanExampleBase::setupFrameBuffer()
-- void VulkanExampleBase::setupDepthStencil()
-- uint32_t VulkanExampleBase::getMemoryType(uint32_t typeBits, VkFlags properties)
-- VkBool32 VulkanExampleBase::getMemoryType(uint32_t typeBits, VkFlags properties, uint32_t * typeIndex)

-- void VulkanExampleBase::buildPresentCommandBuffers()
-- VulkanExampleBase::~VulkanExampleBase()
-- VulkanExampleBase::VulkanExampleBase(bool enableValidation, PFN_GetEnabledFeatures enabledFeaturesFn)

-- void VulkanExampleBase::submitFrame()
-- void VulkanExampleBase::prepareFrame()
-- VkSubmitInfo VulkanExampleBase::prepareSubmitInfo(

-- VkPipelineShaderStageCreateInfo VulkanExampleBase::loadShader(std::string fileName, VkShaderStageFlagBits stage)
-- void VulkanExampleBase::loadMesh(
--   std::string filename,
--   vkMeshLoader::MeshBuffer * meshBuffer,
--   std::vector<vkMeshLoader::VertexLayout> vertexLayout,
--   float scale)
-- void VulkanExampleBase::prepare()
-- void VulkanExampleBase::createPipelineCache()
-- void VulkanExampleBase::flushCommandBuffer(VkCommandBuffer commandBuffer, VkQueue queue, bool free)
-- VkCommandBuffer VulkanExampleBase::createCommandBuffer(VkCommandBufferLevel level, bool begin)

-- void VulkanExampleBase::createSetupCommandBuffer()
-- void VulkanExampleBase::flushSetupCommandBuffer()
-- void VulkanExampleBase::createSetupCommandBuffer()
-- void VulkanExampleBase::createCommandBuffers()
-- bool VulkanExampleBase::checkCommandBuffers()

  vulkanExample = new VulkanExample();                              \
  vulkanExample->setupWindow(hInstance, WndProc);                         \
  vulkanExample->initSwapchain();                                 \
  vulkanExample->prepare();                                   \
  vulkanExample->renderLoop();                                  \
  delete(vulkanExample);  


    zoom = -150.0f;
    zoomSpeed = 2.5f;
    rotationSpeed = 0.5f;
    rotation = { -182.5f, -38.5f, 180.0f };
    enableTextOverlay = true;
    title = "Vulkan Example - Skeletal animation";
    cameraPos = { 0.0f, 0.0f, 12.0f };


  void setupDescriptorPool()

  void setupDescriptorSetLayout()

  void setupDescriptorSet()

  void preparePipelines()

  void prepareUniformBuffers()

  void updateUniformBuffers(bool viewChanged)

  package body Neo.Engine.Renderer is
  procedure Render is
    begin
      Prepare_Frame;
      Submit_Info := .commandBufferCount = 1 .pCommandBuffers = &drawCmdBuffers[currentBuffer];
      Queue_Submit (Queue, 1, Submit_Info, VK_NULL_HANDLE);
      if Menu.Get then
        runningTime += frameTimer * skinnedMesh->animationSpeed;
        vkDeviceWaitIdle(device);
        updateUniformBuffers(false);
      end if;
    end;
  procedure Prepare is
    begin
      Load ("textures/goblin_bc3.ktx", VK_FORMAT_BC3_UNORM_BLOCK, &textures.colorMap);

end;


class VulkanExample : public VulkanExampleBase
{
public:
  struct {
    vkTools::VulkanTexture colorMap;
    vkTools::VulkanTexture floor;
  } textures;

  struct {
    VkPipelineVertexInputStateCreateInfo inputState;
    std::vector<VkVertexInputBindingDescription> bindingDescriptions;
    std::vector<VkVertexInputAttributeDescription> attributeDescriptions;
  } vertices;

  SkinnedMesh *skinnedMesh = nullptr;

  struct {
    vkTools::UniformData vsScene;
    vkTools::UniformData floor;
  } uniformData;

  struct {
    glm::mat4 projection;
    glm::mat4 model;
    glm::mat4 bones[MAX_BONES];
    glm::vec4 lightPos = glm::vec4(0.0f, -250.0f, 250.0f, 1.0);
    glm::vec4 viewPos;
  } uboVS;

  struct {
    glm::mat4 projection;
    glm::mat4 model;
    glm::vec4 lightPos = glm::vec4(0.0, 0.0f, -25.0f, 1.0);
    glm::vec4 viewPos;
    glm::vec2 uvOffset;
  } uboFloor;

  struct {
    VkPipeline skinning;
    VkPipeline texture;
  } pipelines;

  struct {
    vkMeshLoader::MeshBuffer floor;
  } meshes;

  VkPipelineLayout pipelineLayout;
  VkDescriptorSet descriptorSet;
  VkDescriptorSetLayout descriptorSetLayout;

  struct {
    VkDescriptorSet skinning;
    VkDescriptorSet floor;
  } descriptorSets;

  float runningTime = 0.0f;

  VulkanExample() : VulkanExampleBase(ENABLE_VALIDATION)
  {
    zoom = -150.0f;
    zoomSpeed = 2.5f;
    rotationSpeed = 0.5f;
    rotation = { -182.5f, -38.5f, 180.0f };
    enableTextOverlay = true;
    title = "Vulkan Example - Skeletal animation";
    cameraPos = { 0.0f, 0.0f, 12.0f };
  }

  ~VulkanExample()
  {
  }

  void setupDescriptorSet()
  {
    VkDescriptorSetAllocateInfo allocInfo =
      vkTools::initializers::descriptorSetAllocateInfo(
        descriptorPool,
        &descriptorSetLayout,
        1);

    VK_CHECK_RESULT(vkAllocateDescriptorSets(device, &allocInfo, &descriptorSet));
    
    VkDescriptorImageInfo texDescriptor =
      vkTools::initializers::descriptorImageInfo(
        textures.colorMap.sampler,
        textures.colorMap.view,
        VK_IMAGE_LAYOUT_GENERAL);

    std::vector<VkWriteDescriptorSet> writeDescriptorSets =
    {
      -- Binding 0 : Vertex shader uniform buffer
      vkTools::initializers::writeDescriptorSet(
        descriptorSet,
        VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
        0,
        &uniformData.vsScene.descriptor),
      -- Binding 1 : Color map 
      vkTools::initializers::writeDescriptorSet(
        descriptorSet,
        VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
        1,
        &texDescriptor)
    };

    vkUpdateDescriptorSets(device, writeDescriptorSets.size(), writeDescriptorSets.data(), 0, NULL);

    -- Floor
    VK_CHECK_RESULT(vkAllocateDescriptorSets(device, &allocInfo, &descriptorSets.floor));

    texDescriptor.imageView = textures.floor.view;
    texDescriptor.sampler = textures.floor.sampler;

    writeDescriptorSets.clear();

    -- Binding 0 : Vertex shader uniform buffer
    writeDescriptorSets.push_back(
      vkTools::initializers::writeDescriptorSet(
        descriptorSets.floor,
        VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
        0,
        &uniformData.floor.descriptor));
    -- Binding 1 : Color map 
    writeDescriptorSets.push_back(
      vkTools::initializers::writeDescriptorSet(
        descriptorSets.floor,
        VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
        1,
        &texDescriptor));

    vkUpdateDescriptorSets(device, writeDescriptorSets.size(), writeDescriptorSets.data(), 0, NULL);
  }


  void updateUniformBuffers(bool viewChanged)
  {
    if (viewChanged)
    {
      uboVS.projection = glm::perspective(glm::radians(60.0f), (float)width / (float)height, 0.1f, 512.0f);

      glm::mat4 viewMatrix = glm::translate(glm::mat4(), glm::vec3(0.0f, 0.0f, zoom));
      viewMatrix = glm::rotate(viewMatrix, glm::radians(90.0f), glm::vec3(1.0f, 0.0f, 0.0f));
      viewMatrix = glm::scale(viewMatrix, glm::vec3(0.025f));

      uboVS.model = viewMatrix * glm::translate(glm::mat4(), glm::vec3(cameraPos.x, -cameraPos.z, cameraPos.y) * 100.0f);
      uboVS.model = glm::rotate(uboVS.model, glm::radians(rotation.x), glm::vec3(1.0f, 0.0f, 0.0f));
      uboVS.model = glm::rotate(uboVS.model, glm::radians(rotation.z), glm::vec3(0.0f, 1.0f, 0.0f));
      uboVS.model = glm::rotate(uboVS.model, glm::radians(-rotation.y), glm::vec3(0.0f, 0.0f, 1.0f));

      uboVS.viewPos = glm::vec4(0.0f, 0.0f, -zoom, 0.0f);

      uboFloor.projection = uboVS.projection;
      uboFloor.model = viewMatrix * glm::translate(glm::mat4(), glm::vec3(cameraPos.x, -cameraPos.z, cameraPos.y) * 100.0f);
      uboFloor.model = glm::rotate(uboFloor.model, glm::radians(rotation.x), glm::vec3(1.0f, 0.0f, 0.0f));
      uboFloor.model = glm::rotate(uboFloor.model, glm::radians(rotation.z), glm::vec3(0.0f, 1.0f, 0.0f));
      uboFloor.model = glm::rotate(uboFloor.model, glm::radians(-rotation.y), glm::vec3(0.0f, 0.0f, 1.0f));
      uboFloor.model = glm::translate(uboFloor.model, glm::vec3(0.0f, 0.0f, -1800.0f));
      uboFloor.viewPos = glm::vec4(0.0f, 0.0f, -zoom, 0.0f);
    }

    -- Update bones
    skinnedMesh->update(runningTime);
    for (uint32_t i = 0; i < skinnedMesh->boneTransforms.size(); i++)
    {
      uboVS.bones[i] = glm::transpose(glm::make_mat4(&skinnedMesh->boneTransforms[i].a1));
    }

    memcpy(uniformData.vsScene.mapped, &uboVS, sizeof(uboVS));

    -- Update floor animation
    uboFloor.uvOffset.t -= 0.5f * skinnedMesh->animationSpeed * frameTimer;
    memcpy(uniformData.floor.mapped, &uboFloor, sizeof(uboFloor));
  }

  virtual void render()
  {
    if (!prepared)
      return;
    VulkanExampleBase::prepareFrame();

    submitInfo.commandBufferCount = 1;
    submitInfo.pCommandBuffers = &drawCmdBuffers[currentBuffer];
    VK_CHECK_RESULT(vkQueueSubmit(queue, 1, &submitInfo, VK_NULL_HANDLE));

    VulkanExampleBase::submitFrame();
    if (!paused)
    {
      runningTime += frameTimer * skinnedMesh->animationSpeed;
      vkDeviceWaitIdle(device);
      updateUniformBuffers(false);
    }
  }

  virtual void viewChanged()
  {
    vkDeviceWaitIdle(device);
    updateUniformBuffers(true);
  }

  virtual void keyPressed(uint32_t keyCode)
  {
    switch (keyCode)
    {
    case 0x6B:
    case GAMEPAD_BUTTON_R1:
    skinnedMesh->animationSpeed += delta;
      break;
    case 0x6D:
    case GAMEPAD_BUTTON_L1:
    skinnedMesh->animationSpeed += delta;
      break;
    }
  }




  if (enableDebugMarkers)
  {
    vkDebug::DebugMarker::setup(device);
  }
  createCommandPool();
  createSetupCommandBuffer();
  setupSwapChain();
  createCommandBuffers();
  buildPresentCommandBuffers();
  setupDepthStencil();
  setupRenderPass();
  createPipelineCache();
  setupFrameBuffer();
  flushSetupCommandBuffer();
  // Recreate setup command buffer for derived class
  createSetupCommandBuffer();
  // Create a simple texture loader class
  textureLoader = new vkTools::VulkanTextureLoader(vulkanDevice, queue, cmdPool);

  -------------
  -- Prepare --
  -------------


    -- Load texture
    VulkanExampleBase::prepare();
    textureLoader->loadTexture(
      getAssetPath() + "textures/goblin_bc3.ktx",
      VK_FORMAT_BC3_UNORM_BLOCK,
      &textures.colorMap);

    textureLoader->loadTexture(
      getAssetPath() + "textures/pattern_35_bc3.ktx",
      VK_FORMAT_BC3_UNORM_BLOCK,
      &textures.floor);

    -- Load mesh
    skinnedMesh = new SkinnedMesh();
    skinnedMesh->meshLoader = new VulkanMeshLoader(vulkanDevice);
#if defined(__ANDROID__)
    skinnedMesh->meshLoader->assetManager = androidApp->activity->assetManager;
#endif
    skinnedMesh->meshLoader->LoadMesh(getAssetPath() + "models/goblin.dae", 0);
    skinnedMesh->setAnimation(0);

    -- Setup bones
    -- One vertex bone info structure per vertex
    skinnedMesh->bones.resize(skinnedMesh->meshLoader->numVertices);
    -- Store global inverse transform matrix of root node 
    skinnedMesh->globalInverseTransform = skinnedMesh->meshLoader->pScene->mRootNode->mTransformation;
    skinnedMesh->globalInverseTransform.Inverse();
    -- Load bones (weights and IDs)
    for (uint32_t m = 0; m < skinnedMesh->meshLoader->m_Entries.size(); m++)
    {
      aiMesh *paiMesh = skinnedMesh->meshLoader->pScene->mMeshes[m];
      if (paiMesh->mNumBones > 0)
      {
        skinnedMesh->loadBones(m, paiMesh, skinnedMesh->bones);
      }
    }

    -- Generate vertex buffer
    std::vector<Vertex> vertexBuffer;
    -- Iterate through all meshes in the file
    -- and extract the vertex information used in this demo
    for (uint32_t m = 0; m < skinnedMesh->meshLoader->m_Entries.size(); m++)
    {
      for (uint32_t i = 0; i < skinnedMesh->meshLoader->m_Entries[m].Vertices.size(); i++)
      {
        Vertex vertex;

        vertex.pos = skinnedMesh->meshLoader->m_Entries[m].Vertices[i].m_pos;
        vertex.pos.y = -vertex.pos.y;
        vertex.normal = skinnedMesh->meshLoader->m_Entries[m].Vertices[i].m_normal;
        vertex.uv = skinnedMesh->meshLoader->m_Entries[m].Vertices[i].m_tex;
        vertex.color = skinnedMesh->meshLoader->m_Entries[m].Vertices[i].m_color;

        -- Fetch bone weights and IDs
        for (uint32_t j = 0; j < MAX_BONES_PER_VERTEX; j++)
        {
          vertex.boneWeights[j] = skinnedMesh->bones[skinnedMesh->meshLoader->m_Entries[m].vertexBase + i].weights[j];
          vertex.boneIDs[j] = skinnedMesh->bones[skinnedMesh->meshLoader->m_Entries[m].vertexBase + i].IDs[j];
        }

        vertexBuffer.push_back(vertex);
      }
    }
    uint32_t vertexBufferSize = vertexBuffer.size() * sizeof(Vertex);

    -- Generate index buffer from loaded mesh file
    std::vector<uint32_t> indexBuffer;
    for (uint32_t m = 0; m < skinnedMesh->meshLoader->m_Entries.size(); m++)
    {
      uint32_t indexBase = indexBuffer.size();
      for (uint32_t i = 0; i < skinnedMesh->meshLoader->m_Entries[m].Indices.size(); i++)
      {
        indexBuffer.push_back(skinnedMesh->meshLoader->m_Entries[m].Indices[i] + indexBase);
      }
    }
    uint32_t indexBufferSize = indexBuffer.size() * sizeof(uint32_t);
    skinnedMesh->meshBuffer.indexCount = indexBuffer.size();

    bool useStaging = true;

    if (useStaging)
    {
      struct {
        VkBuffer buffer;
        VkDeviceMemory memory;
      } vertexStaging, indexStaging;

      -- Create staging buffers
      -- Vertex data
      createBuffer(
        VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT,
        vertexBufferSize,
        vertexBuffer.data(),
        &vertexStaging.buffer,
        &vertexStaging.memory);
      -- Index data
      createBuffer(
        VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT,
        indexBufferSize,
        indexBuffer.data(),
        &indexStaging.buffer,
        &indexStaging.memory);

      -- Create device local buffers
      -- Vertex buffer
      createBuffer(
        VK_BUFFER_USAGE_VERTEX_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
        vertexBufferSize,
        nullptr,
        &skinnedMesh->meshBuffer.vertices.buf,
        &skinnedMesh->meshBuffer.vertices.mem);
      -- Index buffer
      createBuffer(
        VK_BUFFER_USAGE_INDEX_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
        indexBufferSize,
        nullptr,
        &skinnedMesh->meshBuffer.indices.buf,
        &skinnedMesh->meshBuffer.indices.mem);

      -- Copy from staging buffers
      VkCommandBuffer copyCmd = VulkanExampleBase::createCommandBuffer(VK_COMMAND_BUFFER_LEVEL_PRIMARY, true);

      VkBufferCopy copyRegion = {};

      copyRegion.size = vertexBufferSize;
      vkCmdCopyBuffer(
        copyCmd,
        vertexStaging.buffer,
        skinnedMesh->meshBuffer.vertices.buf,
        1,
        &copyRegion);

      copyRegion.size = indexBufferSize;
      vkCmdCopyBuffer(
        copyCmd,
        indexStaging.buffer,
        skinnedMesh->meshBuffer.indices.buf,
        1,
        &copyRegion);

      VulkanExampleBase::flushCommandBuffer(copyCmd, queue, true);

      vkDestroyBuffer(device, vertexStaging.buffer, nullptr);
      vkFreeMemory(device, vertexStaging.memory, nullptr);
      vkDestroyBuffer(device, indexStaging.buffer, nullptr);
      vkFreeMemory(device, indexStaging.memory, nullptr);
    } 
    else
    {
      -- Vertex buffer
      createBuffer(
        VK_BUFFER_USAGE_VERTEX_BUFFER_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT,
        vertexBufferSize,
        vertexBuffer.data(),
        &skinnedMesh->meshBuffer.vertices.buf,
        &skinnedMesh->meshBuffer.vertices.mem);
      -- Index buffer
      createBuffer(
        VK_BUFFER_USAGE_INDEX_BUFFER_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT,
        indexBufferSize,
        indexBuffer.data(),
        &skinnedMesh->meshBuffer.indices.buf,
        &skinnedMesh->meshBuffer.indices.mem);
    }
    --loadMeshes();
    VulkanExampleBase::loadMesh(getAssetPath() + "models/plane_z.obj", &meshes.floor, vertexLayout, 512.0f);
    --setupVertexDescriptions();
    -- Binding description
    vertices.bindingDescriptions.resize(1);
    vertices.bindingDescriptions[0] =
      vkTools::initializers::vertexInputBindingDescription(
        VERTEX_BUFFER_BIND_ID,
        sizeof(Vertex),
        VK_VERTEX_INPUT_RATE_VERTEX);

    -- Attribute descriptions
    -- Describes memory layout and shader positions
    vertices.attributeDescriptions.resize(6);
    -- Location 0 : Position
    vertices.attributeDescriptions[0] =
      vkTools::initializers::vertexInputAttributeDescription(
        VERTEX_BUFFER_BIND_ID,
        0,
        VK_FORMAT_R32G32B32_SFLOAT,
        0);
    -- Location 1 : Normal
    vertices.attributeDescriptions[1] =
      vkTools::initializers::vertexInputAttributeDescription(
        VERTEX_BUFFER_BIND_ID,
        1,
        VK_FORMAT_R32G32B32_SFLOAT,
        sizeof(float) * 3);
    -- Location 2 : Texture coordinates
    vertices.attributeDescriptions[2] =
      vkTools::initializers::vertexInputAttributeDescription(
        VERTEX_BUFFER_BIND_ID,
        2,
        VK_FORMAT_R32G32_SFLOAT,
        sizeof(float) * 6);
    -- Location 3 : Color
    vertices.attributeDescriptions[3] =
      vkTools::initializers::vertexInputAttributeDescription(
        VERTEX_BUFFER_BIND_ID,
        3,
        VK_FORMAT_R32G32B32_SFLOAT,
        sizeof(float) * 8);
    -- Location 4 : Bone weights
    vertices.attributeDescriptions[4] =
      vkTools::initializers::vertexInputAttributeDescription(
        VERTEX_BUFFER_BIND_ID,
        4,
        VK_FORMAT_R32G32B32A32_SFLOAT,
        sizeof(float) * 11);
    -- Location 5 : Bone IDs
    vertices.attributeDescriptions[5] =
      vkTools::initializers::vertexInputAttributeDescription(
        VERTEX_BUFFER_BIND_ID,
        5,
        VK_FORMAT_R32G32B32A32_SINT,
        sizeof(float) * 15);

    vertices.inputState = vkTools::initializers::pipelineVertexInputStateCreateInfo();
    vertices.inputState.vertexBindingDescriptionCount = vertices.bindingDescriptions.size();
    vertices.inputState.pVertexBindingDescriptions = vertices.bindingDescriptions.data();
    vertices.inputState.vertexAttributeDescriptionCount = vertices.attributeDescriptions.size();
    vertices.inputState.pVertexAttributeDescriptions = vertices.attributeDescriptions.data();


    -- prepareUniformBuffers();    -- Vertex shader uniform buffer block
    createBuffer(
      VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
      VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
      sizeof(uboVS),
      nullptr,
      &uniformData.vsScene.buffer,
      &uniformData.vsScene.memory,
      &uniformData.vsScene.descriptor);

    -- Map for host access
    VK_CHECK_RESULT(vkMapMemory(device, uniformData.vsScene.memory, 0, sizeof(uboVS), 0, (void **)&uniformData.vsScene.mapped));

    -- Floor
    createBuffer(
      VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
      VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
      sizeof(uboFloor),
      nullptr,
      &uniformData.floor.buffer,
      &uniformData.floor.memory,
      &uniformData.floor.descriptor);

    -- Map for host access
    VK_CHECK_RESULT(vkMapMemory(device, uniformData.floor.memory, 0, sizeof(uboFloor), 0, (void **)&uniformData.floor.mapped));

    updateUniformBuffers(true);

    -- setupDescriptorSetLayout();
    std::vector<VkDescriptorSetLayoutBinding> setLayoutBindings =
    {
      -- Binding 0 : Vertex shader uniform buffer
      vkTools::initializers::descriptorSetLayoutBinding(
        VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
        VK_SHADER_STAGE_VERTEX_BIT,
        0),
      -- Binding 1 : Fragment shader combined sampler
      vkTools::initializers::descriptorSetLayoutBinding(
        VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
        VK_SHADER_STAGE_FRAGMENT_BIT,
        1),
    };

    VkDescriptorSetLayoutCreateInfo descriptorLayout =
      vkTools::initializers::descriptorSetLayoutCreateInfo(
        setLayoutBindings.data(),
        setLayoutBindings.size());

    VK_CHECK_RESULT(vkCreateDescriptorSetLayout(device, &descriptorLayout, nullptr, &descriptorSetLayout));

    VkPipelineLayoutCreateInfo pPipelineLayoutCreateInfo =
      vkTools::initializers::pipelineLayoutCreateInfo(
        &descriptorSetLayout,
        1);

    VK_CHECK_RESULT(vkCreatePipelineLayout(device, &pPipelineLayoutCreateInfo, nullptr, &pipelineLayout));

    --preparePipelines();

    VkPipelineInputAssemblyStateCreateInfo inputAssemblyState =
      vkTools::initializers::pipelineInputAssemblyStateCreateInfo(
        VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
        0,
        VK_FALSE);

    VkPipelineRasterizationStateCreateInfo rasterizationState =
      vkTools::initializers::pipelineRasterizationStateCreateInfo(
        VK_POLYGON_MODE_FILL,
        VK_CULL_MODE_BACK_BIT,
        VK_FRONT_FACE_CLOCKWISE,
        0);

    VkPipelineColorBlendAttachmentState blendAttachmentState =
      vkTools::initializers::pipelineColorBlendAttachmentState(
        0xf,
        VK_FALSE);

    VkPipelineColorBlendStateCreateInfo colorBlendState =
      vkTools::initializers::pipelineColorBlendStateCreateInfo(
        1,
        &blendAttachmentState);

    VkPipelineDepthStencilStateCreateInfo depthStencilState =
      vkTools::initializers::pipelineDepthStencilStateCreateInfo(
        VK_TRUE,
        VK_TRUE,
        VK_COMPARE_OP_LESS_OR_EQUAL);

    VkPipelineViewportStateCreateInfo viewportState =
      vkTools::initializers::pipelineViewportStateCreateInfo(1, 1, 0);

    VkPipelineMultisampleStateCreateInfo multisampleState =
      vkTools::initializers::pipelineMultisampleStateCreateInfo(
        VK_SAMPLE_COUNT_1_BIT,
        0);

    std::vector<VkDynamicState> dynamicStateEnables = {
      VK_DYNAMIC_STATE_VIEWPORT,
      VK_DYNAMIC_STATE_SCISSOR
    };
    VkPipelineDynamicStateCreateInfo dynamicState =
      vkTools::initializers::pipelineDynamicStateCreateInfo(
        dynamicStateEnables.data(),
        dynamicStateEnables.size(),
        0);

    -- Skinned rendering pipeline
    std::array<VkPipelineShaderStageCreateInfo, 2> shaderStages;

    shaderStages[0] = loadShader(getAssetPath() + "shaders/skeletalanimation/mesh.vert.spv", VK_SHADER_STAGE_VERTEX_BIT);
    shaderStages[1] = loadShader(getAssetPath() + "shaders/skeletalanimation/mesh.frag.spv", VK_SHADER_STAGE_FRAGMENT_BIT);

    VkGraphicsPipelineCreateInfo pipelineCreateInfo =
      vkTools::initializers::pipelineCreateInfo(
        pipelineLayout,
        renderPass,
        0);
package body Neo.Engine.Renderer is
  procedure Render is
    begin
      Prepare_Frame;
      Submit_Info := .commandBufferCount = 1 .pCommandBuffers = &drawCmdBuffers[currentBuffer];
      Queue_Submit (Queue, 1, Submit_Info, VK_NULL_HANDLE);
      if Menu.Get then
        runningTime += frameTimer * skinnedMesh->animationSpeed;
        vkDeviceWaitIdle(device);
        updateUniformBuffers(false);
      end if;
    end;
  procedure Prepare is
    begin
      Load ("textures/goblin_bc3.ktx", VK_FORMAT_BC3_UNORM_BLOCK, &textures.colorMap);

end;
    pipelineCreateInfo.pVertexInputState = &vertices.inputState;
    pipelineCreateInfo.pInputAssemblyState = &inputAssemblyState;
    pipelineCreateInfo.pRasterizationState = &rasterizationState;
    pipelineCreateInfo.pColorBlendState = &colorBlendState;
    pipelineCreateInfo.pMultisampleState = &multisampleState;
    pipelineCreateInfo.pViewportState = &viewportState;
    pipelineCreateInfo.pDepthStencilState = &depthStencilState;
    pipelineCreateInfo.pDynamicState = &dynamicState;
    pipelineCreateInfo.stageCount = shaderStages.size();
    pipelineCreateInfo.pStages = shaderStages.data();

    VK_CHECK_RESULT(vkCreateGraphicsPipelines(device, pipelineCache, 1, &pipelineCreateInfo, nullptr, &pipelines.skinning));

    shaderStages[0] = loadShader(getAssetPath() + "shaders/skeletalanimation/texture.vert.spv", VK_SHADER_STAGE_VERTEX_BIT);
    shaderStages[1] = loadShader(getAssetPath() + "shaders/skeletalanimation/texture.frag.spv", VK_SHADER_STAGE_FRAGMENT_BIT);
    VK_CHECK_RESULT(vkCreateGraphicsPipelines(device, pipelineCache, 1, &pipelineCreateInfo, nullptr, &pipelines.texture));

    --setupDescriptorPool();
    -- Example uses one ubo and one combined image sampler
    std::vector<VkDescriptorPoolSize> poolSizes =
    {
      vkTools::initializers::descriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 2),
      vkTools::initializers::descriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 2),
    };

    VkDescriptorPoolCreateInfo descriptorPoolInfo =
      vkTools::initializers::descriptorPoolCreateInfo(
        poolSizes.size(),
        poolSizes.data(),
        2);

    VK_CHECK_RESULT(vkCreateDescriptorPool(device, &descriptorPoolInfo, nullptr, &descriptorPool));

    std::vector<VkDescriptorSetLayoutBinding> setLayoutBindings =
    {
      -- Binding 0 : Vertex shader uniform buffer
      vkTools::initializers::descriptorSetLayoutBinding(
        VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
        VK_SHADER_STAGE_VERTEX_BIT,
        0),
      -- Binding 1 : Fragment shader combined sampler
      vkTools::initializers::descriptorSetLayoutBinding(
        VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
        VK_SHADER_STAGE_FRAGMENT_BIT,
        1),
    };

    VkDescriptorSetLayoutCreateInfo descriptorLayout =
      vkTools::initializers::descriptorSetLayoutCreateInfo(
        setLayoutBindings.data(),
        setLayoutBindings.size());

    VK_CHECK_RESULT(vkCreateDescriptorSetLayout(device, &descriptorLayout, nullptr, &descriptorSetLayout));

    VkPipelineLayoutCreateInfo pPipelineLayoutCreateInfo =
      vkTools::initializers::pipelineLayoutCreateInfo(
        &descriptorSetLayout,
        1);

    VK_CHECK_RESULT(vkCreatePipelineLayout(device, &pPipelineLayoutCreateInfo, nullptr, &pipelineLayout));

    -- buildCommandBuffers();
    VkCommandBufferBeginInfo cmdBufInfo = vkTools::initializers::commandBufferBeginInfo();

    VkClearValue clearValues[2];
    clearValues[0].color = { { 0.0f, 0.0f, 0.0f, 0.0f} };
    clearValues[1].depthStencil = { 1.0f, 0 };

    VkRenderPassBeginInfo renderPassBeginInfo = vkTools::initializers::renderPassBeginInfo();
    renderPassBeginInfo.renderPass = renderPass;
    renderPassBeginInfo.renderArea.offset.x = 0;
    renderPassBeginInfo.renderArea.offset.y = 0;
    renderPassBeginInfo.renderArea.extent.width = width;
    renderPassBeginInfo.renderArea.extent.height = height;
    renderPassBeginInfo.clearValueCount = 2;
    renderPassBeginInfo.pClearValues = clearValues;

    for (int32_t i = 0; i < drawCmdBuffers.size(); ++i)
    {
      renderPassBeginInfo.framebuffer = frameBuffers[i];

      VK_CHECK_RESULT(vkBeginCommandBuffer(drawCmdBuffers[i], &cmdBufInfo));

      vkCmdBeginRenderPass(drawCmdBuffers[i], &renderPassBeginInfo, VK_SUBPASS_CONTENTS_INLINE);

      VkViewport viewport = vkTools::initializers::viewport((float)width, (float)height, 0.0f, 1.0f);
      vkCmdSetViewport(drawCmdBuffers[i], 0, 1, &viewport);

      VkRect2D scissor = vkTools::initializers::rect2D(width, height, 0, 0);
      vkCmdSetScissor(drawCmdBuffers[i], 0, 1, &scissor);

      VkDeviceSize offsets[1] = { 0 };

      -- Skinned mesh
      vkCmdBindDescriptorSets(drawCmdBuffers[i], VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout, 0, 1, &descriptorSet, 0, NULL);
      vkCmdBindPipeline(drawCmdBuffers[i], VK_PIPELINE_BIND_POINT_GRAPHICS, pipelines.skinning);

      vkCmdBindVertexBuffers(drawCmdBuffers[i], VERTEX_BUFFER_BIND_ID, 1, &skinnedMesh->meshBuffer.vertices.buf, offsets);
      vkCmdBindIndexBuffer(drawCmdBuffers[i], skinnedMesh->meshBuffer.indices.buf, 0, VK_INDEX_TYPE_UINT32);
      vkCmdDrawIndexed(drawCmdBuffers[i], skinnedMesh->meshBuffer.indexCount, 1, 0, 0, 0);

      -- Floor
      vkCmdBindDescriptorSets(drawCmdBuffers[i], VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout, 0, 1, &descriptorSets.floor, 0, NULL);
      vkCmdBindPipeline(drawCmdBuffers[i], VK_PIPELINE_BIND_POINT_GRAPHICS, pipelines.texture);

      vkCmdBindVertexBuffers(drawCmdBuffers[i], VERTEX_BUFFER_BIND_ID, 1, &meshes.floor.vertices.buf, offsets);
      vkCmdBindIndexBuffer(drawCmdBuffers[i], meshes.floor.indices.buf, 0, VK_INDEX_TYPE_UINT32);
      vkCmdDrawIndexed(drawCmdBuffers[i], meshes.floor.indexCount, 1, 0, 0, 0);

      vkCmdEndRenderPass(drawCmdBuffers[i]);

      VK_CHECK_RESULT(vkEndCommandBuffer(drawCmdBuffers[i]));
    }
    prepared = true;

  --------------
  -- Finalize --
  --------------

    -- Clean up used Vulkan resources 
    -- Note : Inherited destructor cleans up resources stored in base class
    vkDestroyPipeline(device, pipelines.skinning, nullptr);

    vkDestroyPipelineLayout(device, pipelineLayout, nullptr);
    vkDestroyDescriptorSetLayout(device, descriptorSetLayout, nullptr);


    textureLoader->destroyTexture(textures.colorMap);

    vkTools::destroyUniformData(device, &uniformData.vsScene);

    -- Destroy and free mesh resources 
    vkMeshLoader::freeMeshBufferResources(device, &skinnedMesh->meshBuffer);
    delete(skinnedMesh->meshLoader);
    delete(skinnedMesh);
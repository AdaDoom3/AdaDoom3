#version 450
#extension GL_ARB_separate_shader_objects : enable

// MVP
layout(binding = 22) uniform ubo22 {mat4 model;};
layout(binding = 23) uniform ubo23 {mat4 view;};
layout(binding = 24) uniform ubo24 {mat4 projection;};
  
// Skinning flag
layout(binding = 11) uniform ubo11 {bool enableSkinning;};

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inColor;
layout(location = 2) in vec2 inTexCoord;

layout(location = 0) out vec3 fragColor;
layout(location = 1) out vec2 fragTexCoord;

void main() {
    gl_Position = projection * view * model * vec4(inPosition, 1.0);
    fragColor = inColor;
    fragTexCoord = inTexCoord;
}


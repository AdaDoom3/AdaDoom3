#version 450
#extension GL_ARB_separate_shader_objects : enable

// Samplers corresponding to each of the components of the material type
layout(binding = 0) uniform sampler2D baseColor;
//layout(binding = 1) uniform sampler2D irradiance;
//layout(binding = 2) uniform sampler2D filterMap;
layout(binding = 3) uniform sampler2D specular;
layout(binding = 4) uniform sampler2D normal;
//layout(binding = 5) uniform sampler2D displace;
//layout(binding = 6) uniform sampler2D metallic;
//layout(binding = 7) uniform sampler2D roughness;
//layout(binding = 8) uniform sampler2D projection;
//layout(binding = 9) uniform sampler2D falloff;
//layout(binding = 10) uniform sampler2D menu;

// Texture map flags
layout(binding = 12) uniform ubo12 {bool enableBaseColor;};
layout(binding = 13) uniform ubo13 {bool enableIrradiance;};
//layout(binding = 14) uniform ubo14 {bool enableFilter;};
layout(binding = 15) uniform ubo15 {bool enableSpecular;};
layout(binding = 16) uniform ubo16 {bool enableNormal;};
//layout(binding = 17) uniform ubo17 {bool enableDisplace;};
layout(binding = 18) uniform ubo18 {bool enableMetallic;};
layout(binding = 19) uniform ubo19 {bool enableRoughness;};
//layout(binding = 20) uniform ubo20 {bool enableProjection;};
//layout(binding = 21) uniform ubo21 {bool enableFalloff;};
  
// Skinning flag
layout(binding = 11) uniform ubo11 {bool enableSkinning;};

layout(location = 0) in vec3 fragColor;
layout(location = 1) in vec2 fragTexCoord;

layout(location = 0) out vec4 outColor;

void main() {
    //outColor = vec4(fragColor, 1.0);
    outColor = texture(baseColor, fragTexCoord);
}
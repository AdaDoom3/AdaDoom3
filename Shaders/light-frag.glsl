
//                                                                                                                                      //
//                                                         N E O  E N G I N E                                                           //
//                                                                                                                                      //
//                                                 Copyright (C) 2016 Justin Squirek                                                    //
//                                                                                                                                      //
// Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the //
// Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                    //
//                                                                                                                                      //
// Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of                //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                            //
//                                                                                                                                      //
// You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                       //
//                                                                                                                                      //

#version 450
#pragma shader_stage (fragment)
#extension GL_ARB_separate_shader_objects : enable

// Material parameters
uniform sampler2D albedoMap;
uniform sampler2D normalMap;
uniform sampler2D metallicMap;
uniform sampler2D roughnessMap;
uniform sampler2D aoMap;

// layout (binding  = 2) uniform samplerCube samp0;
// layout (location = 0) in  vec3 inTexCoord0;
// layout (location = 1) in  vec3 inTexCoord1;
// layout (location = 2) in  vec4 inColor;
// layout (location = 0) out vec4 fragmentColor;

out vec4 FragColor;
in  vec2 TexCoords;
in  vec3 WorldPos;
in  vec3 Normal;

const float PI = 3.14159265359;

float distributionGGX (vec3 N, vec3 H, float roughness){
  float a2    = roughness * roughness * roughness * roughness;
  float NdotH = max (dot (N, H), 0.0);
  float denom = (NdotH * NdotH * (a2 - 1.0) + 1.0);
  return a2 / (PI * denom * denom);
}

float geometrySchlickGGX (float NdotV, float roughness){
  float r = (roughness + 1.0);
  float k = (r * r) / 8.0;
  return NdotV / (NdotV * (1.0 - k) + k);
}

float geometrySmith (vec3 N, vec3 V, vec3 L, float roughness){
  return GeometrySchlickGGX (max (dot (N, L), 0.0), roughness) * 
         GeometrySchlickGGX (max (dot (N, V), 0.0), roughness);
}

vec3 fresnelSchlick (float cosTheta, vec3 F0){
  return F0 + (1.0 - F0) * pow (1.0 - cosTheta, 5.0);
}

vec3 fresnelSchlickRoughness (float cosTheta, vec3 F0, float roughness){
  return F0 + (max (vec3 (1.0 - roughness), F0) - F0) * pow (1.0 - cosTheta, 5.0);
}   

void main (){

  // Materials
  vec3  albedo    = pow (texture (albedoMap, TexCoords).rgb, vec3 (2.2));
  float metallic  = texture (metallicMap, TexCoords).r;
  float roughness = texture (roughnessMap, TexCoords).r;
  float ao        = texture (aoMap, TexCoords).r;
     
  // Lighting
  vec3 N = normalize (Normal);
  vec3 T = normalize (dFdx (WorldPos) * dFdy (TexCoords).t - dFdy (WorldPos) * dFdx (TexCoords).t);
  vec3 V = normalize (camPos - WorldPos);
       N = normalize (mat3 (T, -normalize (cross (N, T)), N) * texture (normalMap, TexCoords).xyz * 2.0 - 1.0);

  // Calculate reflectance at normal incidence  
  vec3 F0 = mix (vec3 (0.04), albedo, metallic); 
  vec3 Lo = vec3 (0.0);

  // Calculate per-light radiance
  vec3  L        = normalize (lightPositions [i] - WorldPos);
  vec3  H        = normalize (V + L);
  float distance = length (lightPositions [i] - WorldPos);

  // Cook-Torrance BRDF
  vec3 F = fresnelSchlick (max (dot (H, V), 0.0), F0);        
  
  // Sample the pre-filter map and the BRDF lut then combine them as per the Split-Sum approximation to get the IBL specular part
  vec2 brdf  = texture (brdfLUT, vec2 (max (dot (N, V), 0.0), roughness)).rg;  
  vec3 color = (((1.0 - fresnelSchlickRoughness (max (dot (N, V), 0.0), F0, roughness)) *
                 (1.0 - metallic) * texture (irradianceMap, N).rgb * albedo + 
                  textureLod (prefilterMap, reflect (-V, N),  roughness * 4.0).rgb * (F * brdf.x + brdf.y)) // Specular
                * ao) +

                // Add outgoing radiance Lo
                ((vec3 (1.0) - F) * (1.0 - metallic) * albedo / PI +
                 (DistributionGGX (N, H, roughness) * GeometrySmith (N, V, L, roughness) * F) / 
                 (4 * max (dot (N, V), 0.0) * max (dot (N, L), 0.0) + 0.001)) *
                lightColors [i] * (1.0 / (distance * distance)) * max (dot (N, L), 0.0);   

  // HDR tonemapping gamma correct
  fragmentColor = vec4 (pow (color / (color + vec3 (1.0)), vec3 (1.0/2.2)), 1.0);
}

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

vertexShader {

  in vertex {
    vec4 position;
    vec2 texCoord;
    vec4 normal;
    vec4 tangent;
    vec4 color;
  };

  out fragment {
    vec4 position;
    vec2 texCoord0;
    vec3 texCoord1;
    vec3 texCoord2;
    vec3 texCoord3;
    vec3 texCoord4;
    vec4 color;
  };

  void main () {

    vec4 tangent  = vertex.tangent * 2.0 - 1.0;
    vec4 normal   = vertex.normal  * 2.0 - 1.0;
    vec3 binormal = cross (normal.xyz, tangent.xyz) * tangent.w;

    fragment.color     = color;
    fragment.position  = vertex.position * MVP;
    fragment.texCoord0 = vertex.texCoord.xy;
    fragment.texCoord1 = (localViewOrigin - vertex.position) * model;
    fragment.texCoord2 = vec3 (dot3 (tangent,  model.x);
                               dot3 (binormal, model.x);
                               dot3 (normal,   model.x));
    fragment.texCoord3 = vec3 (dot3 (tangent,  model.y);
                               dot3 (binormal, model.y);
                               dot3 (normal,   model.y));
    fragment.texCoord4 = vec3 (dot3 (tangent,  model.z);
                               dot3 (binormal, model.z);
                               dot3 (normal,   model.z));
  }
}

fragmentShader {

  uniform samplerCUBE cubeMap;
  uniform sampler2D normalMap;

  in fragment {
    vec4 position;
    vec2 texCoord0;
    vec3 texCoord1;
    vec3 texCoord2;
    vec3 texCoord3;
    vec3 texCoord4;
    vec4 color;
  };

  out result {
    vec4 color;
  };

  void main () {

    vec3 localNormal = vec3 ((tex2D (normalMap, fragment.texCoord0) * 2.0f - 1.0f).rg, 0.0f);
    localNormal.z    = sqrt (1.0f - dot3 (localNormal, localNormal));

    vec3 globalNormal = vec3 (dot3 (localNormal, fragment.texCoord2);
                              dot3 (localNormal, fragment.texCoord3);
                              dot3 (localNormal, fragment.texCoord4));

    vec3 globalEye  = normalize (fragment.texCoord1);
    vec3 reflection = (globalNormal * dot3 (globalEye, globalNormal) * 2.0f) - globalEye;
    result.color = vec4 (texCUBE (cubeMap, reflectionVector).xyz, 1.0f) * fragment.color;
  }
}
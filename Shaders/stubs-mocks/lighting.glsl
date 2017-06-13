
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
    vec4 normal;
    vec4 color;
  };

  out fragment {
    vec4 position;
    vec3 texCoord0;
    vec3 texCoord1;
    vec4 color;
  };

  void main () {
    fragment.position  = vertex.position * MVP;
    fragment.texcoord0 = (localViewOrigin - vertex.position).xyz;
    fragment.texcoord1 = (vertex.normal * 2.0 - 1.0).xyz;
    fragment.color     = color;
  }
}

fragmentShader {

  uniform samplerCUBE cubeMap;

  in fragment {
    vec4 position;
    vec3 texCoord0;
    vec3 texCoord1;
    vec4 color;
  };

  out result {
    vec4 color;
  };

  void main () {
    vec3 globalEye    = normalize (fragment.texCoord0);
    vec3 globalNormal = normalize (fragment.texCoord1);
    vec3 reflection   = (vec3 (dot3 (globalEye, globalNormal)) * globalNormal * 2.0f) - globalEye;
    result.color      = vec4 (texCUBE (cubeMap, reflection).xyz, 1.0f) * fragment.color;
  }
}
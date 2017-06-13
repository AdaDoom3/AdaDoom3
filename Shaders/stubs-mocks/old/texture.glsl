
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
    vec2 texCoord;
  };

  void main () {
    fragment.position   = vertex.position * MVP;
    fragment.texCoord.x = dot (vertex.texCoord.xy, textureS);
    fragment.texCoord.y = dot (vertex.texCoord.xy, textureT);
  }
}

fragmentShader {

  uniform sampler2D sample;

  in fragment {
    vec4 position;
    vec2 texCoord;
  };

  out result {
    vec4 color;
  };

  void main () {
    result.color = tex2D (sample, fragment.texCoord) * color;
  }
}
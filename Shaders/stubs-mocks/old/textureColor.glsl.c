
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
struct VS_IN {
  float4 position : POSITION;
  float2 texcoord : TEXCOORD0;
  float4 normal : NORMAL;
  float4 tangent : TANGENT;
  float4 color : COLOR0;
};

struct VS_OUT {
  float4 position : POSITION;
  float2 texcoord0 : TEXCOORD0;
};

void main( VS_IN vertex, out VS_OUT result ) {
  result.position.x = dot4( vertex.position, rpMVPmatrixX );
  result.position.y = dot4( vertex.position, rpMVPmatrixY );
  result.position.z = dot4( vertex.position, rpMVPmatrixZ );
  result.position.w = dot4( vertex.position, rpMVPmatrixW );

  result.texcoord0.x = dot4( vertex.texcoord.xy, rpTextureMatrixS );
  result.texcoord0.y = dot4( vertex.texcoord.xy, rpTextureMatrixT );
  float4 vertexColor = ( swizzleColor( vertex.color ) * rpVertexColorModulate ) + rpVertexColorAdd;
  result.color =  vertexColor * rpColor;

  

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
    float4 color : COLOR0;
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
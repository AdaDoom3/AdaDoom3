
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

// Physcially-based rentderning


  //////////////
  // Fragment //
  //////////////

  fragment {
    precision mediump float;

    // INPUT
    in vec2 v_vTexCoord;

    // OUTPUT
    out vec4 o_vFragColor;

    // UNIFORMS
    uniform sampler2D u_tInputTex; // In BaseFullscreenShader

    void main(){
        float z = texture(u_tInputTex, v_vTexCoord).r;
        o_vFragCo{lor = vec4(z, z, z, 1.0);
    }
    uniform sampler2D samp0 : register(s0);
    struct PS_OUT {
      float4 color : COLOR;
    };

    void main( out PS_OUT result ) {
      result.color = float4( 0.0, 0.0, 0.0, 1.0 );
    }



  ////////////
  // vertex //
  ////////////

  vertex {
    struct VS_IN {
      float4 position : POSITION;
    };

    struct VS_OUT {
      float4 position : POSITION;
    };

    void main( VS_IN vertex, out VS_OUT result ) {
      result.position.x = dot4( vertex.position, rpMVPmatrixX );
      result.position.y = dot4( vertex.position, rpMVPmatrixY );
      result.position.z = dot4( vertex.position, rpMVPmatrixZ );
      result.position.w = dot4( vertex.position, rpMVPmatrixW );
    }
  }
}

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

// Physcially-based rendering
shader light{

  //////////////
  // Fragment //
  //////////////

  fragment{
    uniform sampler2D samp0 : register(s0);
    struct PS_OUT {
      float4 color : COLOR;
    };

    void main( out PS_OUT result ) {
      result.color = rpColor;
    }
  }

  ///////////////////
  // vertexSkinned //
  ///////////////////

  vertexSkinned {
    float4 vertexPosition = vertex.position;
    vertexPosition.w = 1.0;

    float4 modelPosition;
    modelPosition.x = dot4( matX, vertexPosition );
    modelPosition.y = dot4( matY, vertexPosition );
    modelPosition.z = dot4( matZ, vertexPosition );
    modelPosition.w = vertex.position.w;

    float4 vPos = modelPosition - rpLocalLightOrigin;
    vPos = ( vPos.wwww * rpLocalLightOrigin ) + vPos;

    result.position.x = dot4( vPos, rpMVPmatrixX );
    result.position.y = dot4( vPos, rpMVPmatrixY );
    result.position.z = dot4( vPos, rpMVPmatrixZ );
    result.position.w = dot4( vPos, rpMVPmatrixW );
  }

  ////////////
  // vertex //
  ////////////

  vertex {
    struct VS_OUT {
      float4 position : POSITION;
    };

    void main( VS_IN vertex, out VS_OUT result ) {
      float4 vPos = vertex.position - rpLocalLightOrigin;
      vPos = ( vPos.wwww * rpLocalLightOrigin ) + vPos;

      result.position.x = dot4( vPos, rpMVPmatrixX );
      result.position.y = dot4( vPos, rpMVPmatrixY );
      result.position.z = dot4( vPos, rpMVPmatrixZ );
      result.position.w = dot4( vPos, rpMVPmatrixW );
    }
  }
}
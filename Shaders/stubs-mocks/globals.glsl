
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

global {

  layout (binding = 0) uniform data {

    // 
    vec4 screenCorrectionFactor;
    vec4 windowCoordinate;
    vec4 diffuseModifier;
    vec4 specularModifier;

    // 
    vec4 color;
    vec4 viewOrigin;
    vec4 globalEyePosition;

    // 
    mat4 MVP;
    mat4 model;
    mat4 projection;
    mat4 modelView;

    // 
    vec4 vertexColorModulate;
    vec4 vertexColorAdd;

    // 
    vec4 localLightOrigin;
    vec4 localViewOrigin;

    //
    vec4 lightProjectionS;
    vec4 lightProjectionT;
    vec4 lightProjectionQ;
    vec4 lightFalloffS;

    // 
    vec4 bumpS;
    vec4 bumpT;

    // 
    vec4 diffuseS;
    vec4 diffuseT;

    // 
    vec4 specularS;
    vec4 specularT;

    //
    vec4 textureS;
    vec4 textureT;

    //
    vec4 texture0S;
    vec4 texture0T;
    vec4 texture0Q;
    vec4 texture0Enabled;

    //
    vec4 texture1S;
    vec4 texture1T;
    vec4 texture1Q;
    vec4 texture1Enabled;

    //
    vec4 wobbleSkyX;
    vec4 wobbleSkyY;
    vec4 wobbleSkyZ;

    //
    vec4 overbright;
    vec4 enableSkinning;
    vec4 alphaTest;
  }
}
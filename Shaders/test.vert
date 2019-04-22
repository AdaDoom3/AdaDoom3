#version 450

layout (location = 0) in vec3 inPos;
layout (location = 1) in vec3 inColor;
layout (location = 2) in vec2 tex;

layout (location = 0) smooth out vec3 outColor;

void main(){
	outColor = inColor;
	gl_Position = vec4( inPos.x, inPos.y, inPos.z, 1.0 );
}
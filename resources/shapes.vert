#version 410
#extension GL_ARB_shading_language_include : require
#include <noise.h>

uniform mat4 uTransform;
uniform float uTime;

in vec2 aPosition;
in vec2 aUV;

out vec2 vUV;
void main( void ) {
    float offset = fbm(vec3(aPosition+aUV, uTime/5)) * 0.02;

    gl_Position = uTransform * vec4(aPosition+offset, 0.0, 1.0);
    vUV = aUV;
}

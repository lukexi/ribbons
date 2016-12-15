#version 410

#extension GL_ARB_shading_language_include : require

#include <primitives.h>
#include <operations.h>
#include <color.h>
#include <noise.h>

uniform vec2 uResolution;
uniform vec2 uMouse;
uniform float uTime;

in vec2 vUV;

out vec4 fragColor;


void main() {


    fragColor = vec4(1,1,1,1);
}


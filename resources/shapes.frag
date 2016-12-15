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

    vec3 color = hsv2rgb_smooth(vec3(
        vUV.y/10 + uTime/10 + vUV.x * 0.05,
        0.8,
        0.6 + vUV.x * 0.1
        ));
    // vec3 color = vec3(1,1,1);
    fragColor = vec4(color,1.0);
}


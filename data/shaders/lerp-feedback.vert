#version 330 core

layout (location = 0) in float a;

layout (location = 1) in float b;

uniform float w = 0.0;

out float mixed;

void main() {
  mixed = mix(a, b, w);
}

#version 330 core

layout (location = 0) in vec4 position;

uniform float time;

uniform float dt;

out float new_position;

void main() {
  new_position = time + dt;
}

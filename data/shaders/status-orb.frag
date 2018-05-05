#version 330 core

in vec2 frag_text_coord;

uniform sampler2D texture_object;

out vec4 color;

void main () {
  vec4  texel = texture2D(texture_object, frag_text_coord);
  color       = texel;
}

#version 330 core

in vec2 frag_text_coord;

uniform sampler2D texture_object;

uniform float time;

out vec4 color;

void main () {
  vec4  texel = texture2D(texture_object, frag_text_coord);
  color       = vec4(texel.rgb, texel.a);
}

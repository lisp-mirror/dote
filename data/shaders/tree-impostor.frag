#version 330 core

in vec2 frag_text_coord;

uniform sampler2D texture_object;

uniform vec4 mult_color = vec4(1.0, 0.0, 0.0, 1.0);

out vec4 color;

void main () {
  vec4  texel = texture2D(texture_object,frag_text_coord);
  if (texel.a <= 0.0) {
    discard;
  }
  color = texel;
}

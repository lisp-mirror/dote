#version 330 core

in vec2 frag_text_coord;

uniform sampler2D texture_object;

uniform float duration;

uniform float dx = 0.0;

uniform float dy = 0.0;

out vec4 color;

void main () {
  vec2  shifted_text_coord = vec2(frag_text_coord.s + dx, frag_text_coord.t + dy);
  vec4  texel              = texture2D(texture_object, shifted_text_coord);
  if (texel.a <= 0.0) {
      discard;
  }
  color                    = texel;
}

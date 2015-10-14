#version 330 core

in vec2 frag_text_coord;

uniform sampler2D texture_object;

uniform float progress;

out vec4 color;

void main () {
  vec4 texel = texture2D(texture_object,frag_text_coord);

  if (texel.a <= 0.0) {
    discard;
  }

  color      = texel * smoothstep(-0.02, 0.02, progress - frag_text_coord.s);
}

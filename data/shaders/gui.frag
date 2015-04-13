#version 330 core

in vec2 frag_text_coord;

uniform sampler2D texture_object;

uniform vec3   ia   = vec3(1.0,1.0,1.0);

out vec4 color;

void main () {
  vec4 texel = texture2D(texture_object,frag_text_coord);
  if (texel.a <= 0.0) {
    discard;
  }
  color      = texel * vec4(ia,1.0);
}


#version 330 core

in vec2 frag_text_coord;

uniform sampler2D texture_object;

uniform sampler2D texture_overlay;

uniform vec3   ia   = vec3(1.0,1.0,1.0);

out vec4 color;

void main () {
  vec4 texel_base    = texture2D(texture_object,frag_text_coord);
  vec4 texel_overlay = texture2D(texture_overlay,frag_text_coord * 2.0 - 0.5);
  vec4 texel         = mix(texel_base,texel_overlay,texel_overlay.a);
  
  if (texel.a <= 0.0) {
    discard;
  }
  color              = texel * vec4(ia,1.0);
}


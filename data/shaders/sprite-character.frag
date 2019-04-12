#version 330 core

in vec2 frag_text_coord;

uniform sampler2D texture_object;

uniform sampler2D texture_weapon;

uniform sampler2D texture_shield;

out vec4 color;

void main () {
  vec4  texel_body   = texture2D(texture_object, frag_text_coord);
  vec4  texel_weapon = texture2D(texture_weapon, frag_text_coord);
  vec4  texel_shield = texture2D(texture_shield, frag_text_coord);
  vec4  texel        = mix(texel_body, texel_weapon, texel_weapon.a);

  texel              = mix(texel, texel_shield, texel_shield.a);

  if (texel.a <= 0.1) {
    discard;
  }

  color       = texel;
}

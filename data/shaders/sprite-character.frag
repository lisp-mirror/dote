#version 330 core

in vec2 frag_text_coord;

uniform sampler2D texture_object;

uniform sampler2D texture_weapon;

uniform sampler2D texture_shield;

uniform sampler2D texture_helm;

uniform sampler2D texture_armor;

out vec4 color;

void main () {
  vec4  texel_body   = texture2D(texture_object, frag_text_coord);
  vec4  texel_armor  = texture2D(texture_armor, frag_text_coord);
  vec4  texel_weapon = texture2D(texture_weapon, frag_text_coord);
  vec4  texel_shield = texture2D(texture_shield, frag_text_coord);
  vec4  texel_helm   = texture2D(texture_helm, frag_text_coord);

  vec4  texel        = mix(texel_body, texel_armor, texel_armor.a);
  texel              = mix(texel, texel_helm, texel_helm.a);
  texel              = mix(texel, texel_weapon, texel_weapon.a);
  texel              = mix(texel, texel_shield, texel_shield.a);

  if (texel.a <= 0.1) {
    discard;
  }

  color       = texel;
}

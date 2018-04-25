#version 330 core

in vec2 frag_text_coord;

uniform sampler2D texture_object;

uniform float time;

uniform float alpha; // ignored

uniform vec4  fade_color;

out vec4 color;

void main () {
  vec4 texel      = texture2D(texture_object, frag_text_coord);
  vec4 lava_color = sin(time + frag_text_coord.s * frag_text_coord.t * -2.5) * fade_color;
  color           = vec4(texel.rgb * lava_color.rgb, texel.a);
}

#version 330 core

in vec2 frag_text_coord;

uniform sampler2D texture_object;

uniform float duration;

uniform float time;

uniform float alpha;

uniform vec4  fade_color;

out vec4 color;

void main () {
  vec4 texel = texture2D(texture_object, frag_text_coord);
  color = vec4(mix(texel, texel, alpha).rgb,
               alpha * texel.a);
}

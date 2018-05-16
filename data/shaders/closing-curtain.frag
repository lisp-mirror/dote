#version 330 core

in vec2 frag_text_coord;

uniform float alpha;

uniform sampler2D texture_object;

uniform float time;

out vec4 color;

void main () {
  vec4  c = texture2D(texture_object, frag_text_coord);
  color   = vec4(c.rgb, c.a * alpha);
}

#version 330 core

in vec2 frag_text_coord;

uniform sampler2D texture_object;

uniform float time;

uniform float alpha;

uniform vec4  fade_color;

out vec4 color;

void main () {
  color = vec4(fade_color.rgb, alpha);
}

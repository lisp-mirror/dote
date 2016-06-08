#version 330 core

in vec2 frag_text_coord;

in float alpha_val;

vec4 additional_color;

uniform sampler2D texture_object;

uniform float time;

out vec4 color;

void main () {
  vec4  c = texture2D(texture_object,frag_text_coord);
  color   = vec4(c.r * alpha_val,
		 c.g * alpha_val,
		 c.b * alpha_val,
		 1.0);
}

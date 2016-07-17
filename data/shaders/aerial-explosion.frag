#version 330 core

in vec2 frag_text_coord;

in float alpha_val;

in vec4 additional_color;

uniform sampler2D texture_object;

uniform float time;

out vec4 color;

void main () {
  vec4  c = texture2D(texture_object,frag_text_coord);
  color   = vec4(c.r * additional_color.r,
		 c.g * additional_color.g,
		 c.b * additional_color.b,
		 c.a * alpha_val * additional_color.a);
}

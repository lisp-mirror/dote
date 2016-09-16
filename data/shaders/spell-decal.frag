#version 330 core

in vec2 frag_text_coord;

in float alpha_val;

in vec4 additional_color;

uniform sampler2D texture_object;

uniform float time;

out vec4 color;

void main () {
  vec4  c = texture2D(texture_object,frag_text_coord);
  color   = vec4(c.rgb * additional_color.rgb, c.a * alpha_val);
  //  color   = vec4(additional_color.rgb, c.a * alpha_val);
}
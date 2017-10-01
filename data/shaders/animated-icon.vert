#version 330 core

layout (location = 0) in vec4 position;

layout (location = 12) in vec2 texture_coord;

uniform mat4 modelview_matrix;
uniform mat4 proj_matrix;

out vec2 frag_text_coord;

uniform float texture_horizontal_offset = 0.0;

void main () {
  frag_text_coord = texture_coord;
  frag_text_coord.s = frag_text_coord.s + texture_horizontal_offset;
  gl_Position = proj_matrix *  modelview_matrix * position;
}

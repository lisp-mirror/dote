#version 330 core

layout (location = 0) in vec4 position;

layout (location = 12) in vec2 texture_coord;

uniform mat4 modelview_matrix;
uniform mat4 proj_matrix;

out vec2 frag_text_coord;

void main () {
  frag_text_coord = texture_coord;
  gl_Position = proj_matrix *  modelview_matrix * position;
}

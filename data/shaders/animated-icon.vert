#version 330 core

layout (location = 0) in vec4 position;

layout (location = 12) in vec2 texture_coord;

uniform mat4 modelview_matrix;
uniform mat4 proj_matrix;

uniform float texture_window_width = 0.0;

uniform int   frame_idx             = 0;

out vec2 frag_text_coord;


void main () {
  frag_text_coord   = texture_coord;
  frag_text_coord.s = frag_text_coord.s + (frame_idx * texture_window_width);
  gl_Position       = proj_matrix *  modelview_matrix * position;
}

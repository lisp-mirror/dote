#version 330 core

layout (location = 0) in vec4 position;

layout (location = 12) in vec2 texture_coord;

uniform mat4 modelview_matrix;
uniform mat4 proj_matrix;

uniform float time;

out vec2 frag_text_coord;

%include billboard-cyl.vert.inc.glsl

void main () {

  mat4 modelview = mat4(modelview_matrix);
  remove_rotation(modelview);

  frag_text_coord = texture_coord;

  vec4 actual_position  = vec4(position);
  actual_position.x    += 0.0035 * pow(actual_position.y , 2) * sin(2*time);

  gl_Position = proj_matrix * modelview * actual_position;
}

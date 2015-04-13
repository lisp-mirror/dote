#version 330 core

layout (location = 0) in vec4 position;

layout (location = 4) in vec3 normal;

layout (location = 12) in vec2 texture_coord;

uniform mat4 modelview_matrix;
uniform mat4 view_matrix;
uniform mat4 proj_matrix;
uniform mat4 proj_texture_matrix;

out vec2 frag_text_coord;

out vec4 frag_smoke_text_coord;

out vec3 frag_pos;

//mat3 normal_matrix = mat3(view_matrix * model_matrix);

void main () {
  frag_text_coord       = texture_coord;
  gl_Position           = proj_matrix * modelview_matrix * position;
  frag_pos              = position.xyz;
  frag_smoke_text_coord = proj_texture_matrix * position;
}

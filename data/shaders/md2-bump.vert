#version 330 core

layout (location =  0) in vec4 position;

layout (location =  1) in vec4 position_next;

layout (location =  4) in vec3 normal;

layout (location =  8) in vec3 tangent;

layout (location = 12) in vec2 texture_coord;

%include fog.vert.inc

uniform mat4 modelview_matrix;
uniform mat4 model_matrix;
uniform mat4 proj_matrix;

uniform float keyframe_interpolation;

uniform vec3 light_pos;

out vec2 frag_text_coord;
out vec3 eye_dir;
out vec3 light_dir;
out vec3 N_stat;

//mat4 modelview_matrix = view_matrix * model_matrix;
mat3 normal_matrix = mat3(modelview_matrix);

mat3 gen_TBN_matrix(in vec4 pos, in vec3 normal, in vec3 tangent, in mat3 normal_matrix);

void main () {
  vec4 actual_position = mix(position, position_next, keyframe_interpolation);
  vec4 P               = modelview_matrix * actual_position;
  mat3 TBN             = gen_TBN_matrix(actual_position, normal, tangent, normal_matrix);
  N_stat               = normal_matrix * normal;
  frag_text_coord      = texture_coord;
  eye_dir              = normalize(TBN * -P.xyz);
  light_dir            = normalize(TBN * (light_pos - P.xyz));
  world_position       = model_matrix * actual_position;
  gl_Position          = proj_matrix * P;
}

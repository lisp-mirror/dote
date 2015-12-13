#version 330 core

layout (location = 0) in vec4 position;

layout (location = 1)  in float pick_weights;

layout (location = 4) in vec3 normal;

layout (location = 8) in vec3 tangent;

layout (location = 12) in vec2 texture_coord;

%include fog.vert.inc

uniform mat4 modelview_matrix;
//uniform mat4 view_matrix;
uniform mat4 proj_matrix;

uniform vec3 light_pos;

out vec2 frag_text_coord;
out vec3 eye_dir;
out vec3 light_dir;
out vec3 N_stat;

out float pick_weight;

//mat4 modelview_matrix = view_matrix * model_matrix;
mat3 normal_matrix = mat3(modelview_matrix);

mat3 gen_TBN_matrix(in vec4 pos, in vec3 normal, in vec3 tangent, in mat3 normal_matrix);

void main () {
  vec4 P          = modelview_matrix * position;
  mat3 TBN        = gen_TBN_matrix(position, normal, tangent, normal_matrix);
  pick_weight     = pick_weights;
  N_stat          = normal_matrix * normal;
  frag_text_coord = texture_coord;
  eye_dir         = normalize(TBN * -P.xyz);
  light_dir       = normalize(TBN * (light_pos - P.xyz));
  eye_position           = modelview_matrix * position;
  world_position  = position;
  gl_Position     = proj_matrix * P;
}

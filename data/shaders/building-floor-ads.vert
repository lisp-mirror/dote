#version 330 core

%include fog.vert.inc

layout (location = 0) in vec4 position;

layout (location = 1)  in float pick_weights;

layout (location = 4) in vec3 normal;

layout (location = 12) in vec2 texture_coord;

uniform mat4 modelview_matrix;
uniform mat4 proj_matrix;

uniform vec3 light_pos;

out vec2 frag_text_coord;

out vec3 N;
out vec3 V;
out vec3 L;

out float pick_weight;

mat3 normal_matrix = mat3(modelview_matrix);

void ADS(in vec4 pos, in vec3 normal, in vec4 light, in mat3 normal_matrix,
	 in mat4 modelview_matrix, out vec3 N, out vec3 V, out vec3 L);

void main () {
  ADS(position, normal, vec4(light_pos,1.0), normal_matrix, modelview_matrix, N, V, L);
  frag_text_coord = texture_coord;
  pick_weight     = pick_weights;
  eye_position    = modelview_matrix * position;
  world_position  = position;
  gl_Position = proj_matrix *  modelview_matrix * position;
}

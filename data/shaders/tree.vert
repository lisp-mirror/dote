#version 330 core

layout (location = 0) in vec4 position;

layout (location = 4) in vec3 normal;

layout (location = 12) in vec2 texture_coord;

uniform mat4 modelview_matrix;
uniform mat4 proj_matrix;

uniform vec3 light_pos;

uniform float time = 0.0;

out vec2 frag_text_coord;

out vec3 N;
out vec3 V;
out vec3 L;

mat3 normal_matrix = mat3(modelview_matrix);

void ADS(in vec4 pos, in vec3 normal, in vec4 light, in mat3 normal_matrix, 
	 in mat4 modelview_matrix, out vec3 N, out vec3 V, out vec3 L);

void main () {
  vec4 actual_position  = vec4(position);
  actual_position.x    += 0.003 * pow(actual_position.y , 2) *
    (0.5 * sin(3 * time) + 0.1* sin (9 * time) + 0.05  * sin(27 * time));
  ADS(actual_position, normal, vec4(light_pos,1.0), normal_matrix, modelview_matrix, N, V, L);
  frag_text_coord = texture_coord;

  gl_Position = proj_matrix *  modelview_matrix * actual_position;
}

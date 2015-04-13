#version 330 core

layout (location = 0) in vec4 position;

layout (location = 4) in vec3 normal;

layout (location = 12) in vec2 texture_coord;

uniform mat4 modelview_matrix;
uniform mat4 view_matrix;
uniform mat4 proj_matrix;

uniform mat4 proj_texture_matrix;

uniform vec3 light_pos;

uniform float time;

uniform float wave_ampl = 0.5;

uniform float wave_freq = 30.0;

out vec4 frag_text_coord;


out vec3 N;
out vec3 V;
out vec3 L;

mat3 normal_matrix = mat3(modelview_matrix);

void ADS(in vec4 pos, in vec3 normal, in vec4 light, in mat3 normal_matrix, 
	 in mat4 modelview_matrix, out vec3 N, out vec3 V, out vec3 L);

void main () {
  vec4 actual_pos       = vec4(position.x,
			       position.y + 
			       wave_ampl *
			       sin(wave_freq * position.x + time * 10.0 + position.z),
			       position.z,
			       position.w);

  ADS(actual_pos, normal, vec4(light_pos,1.0), normal_matrix, 
      modelview_matrix, N, V, L);

  frag_text_coord       = proj_texture_matrix * position;
  gl_Position           = proj_matrix *  modelview_matrix * actual_pos;
}

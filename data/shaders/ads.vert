#version 330 core

void ADS(in vec4 pos, in vec3 normal, in vec4 light, in mat3 normal_matrix, 
	 in mat4 modelview_matrix, out vec3 N, out vec3 V, out vec3 L){
  vec4 P             = modelview_matrix * pos;
  N = normal_matrix * normal;
  V = - P.xyz;
  L = vec3(light - P);
}

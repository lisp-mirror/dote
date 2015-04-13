#version 330 core

mat3 gen_TBN_matrix(in vec4 pos, in vec3 normal, in vec3 tangent, in mat3 normal_matrix){
  vec3 N = normalize(normal_matrix * normal);
  vec3 T = normalize(normal_matrix * tangent);
  vec3 B = normalize(cross(N, T));
  mat3 results= mat3(T,B,N);
  return transpose(results);
}


#version 410 core

// Per-vertex inputs
layout (location = 0) in vec4 position;

layout (location = 1) in vec3 normal;

uniform vec3 light_pos;

uniform mat4 mv_matrix;

uniform mat4 view_matrix;

uniform mat4 proj_matrix;

mat3  normal_matrix = mat3(mv_matrix);

// Inputs from vertex shader

out vec3 N;
out vec3 L;
out vec3 V;


// Position of light


void main(void)
{
    // Calculate view-space coordinate
    vec4 P = mv_matrix * position;

    // Calculate normal in view-space
    N = normal_matrix * normal;

    // Calculate light vector
    L = light_pos - P.xyz;

    // Calculate view vector
    V = -P.xyz;

    // Calculate the clip-space position of each vertex
    gl_Position = proj_matrix * P;
}

#version 130

// Output
layout (location = 0) out vec4 color;

// Input from vertex shader

in vec3 N;
in vec3 L;
in vec3 V;

// Material properties
uniform vec3 diffuse_albedo = vec3(0.5, 0.2, 0.7);
uniform vec3 specular_albedo = vec3(0.7);
uniform float specular_power = 200.0;

void main(void)
{
    // Normalize the incoming N, L and V vectors
    vec3 N = normalize(N);
    vec3 L = normalize(L);
    vec3 V = normalize(V);
    vec3 H = normalize(L + V);

    // Compute the diffuse and specular components for each fragment
    vec3 diffuse = max(dot(N, L), 0.0) * diffuse_albedo;
    vec3 specular = pow(max(dot(N, H), 0.0), specular_power) * specular_albedo;

    // Write final color to the framebuffer
    color = vec4(diffuse + specular, 1.0);
}

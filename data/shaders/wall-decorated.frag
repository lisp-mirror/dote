#version 330 core

in vec2 frag_text_coord;

in vec4 proj_text_coord;

in vec3 N;
in vec3 V;
in vec3 L;

uniform sampler2D texture_object;

uniform sampler2D texture_projector;

uniform vec3   ia   = vec3(.2,.2,.2);
uniform vec3   id   = vec3(1.0,1.0,1.0);
uniform vec3   is   = vec3(1.0,1.0,1.0);

uniform float  ka   = 1.0;
uniform float  kd   = 1.0;
uniform float  ks   = 1.0;

uniform float shine = 12.0;

out vec4 color;

void main () {
  vec4 texel          = texture2D(texture_object,frag_text_coord);
  vec4 decal          = proj_text_coord.q < 0 ? 
                        vec4(0.0) :
                        texture2DProj(texture_projector, proj_text_coord);
  vec3 N              = normalize(N);
  vec3 V              = normalize(V);
  vec3 L              = normalize(L);
  vec3 R              = reflect(-L, N);
  vec3 amb_diff_color = ka * ia + kd * (max(dot(N , L),0.0) * id);
  vec3 spec_color     = ks * (pow(max(dot(R, V),0.0),shine) * is);
  vec4 color_raw      = vec4(amb_diff_color,1.0) * texel + vec4(spec_color,1.0);
  color               = mix(color_raw, decal, decal.a);
}


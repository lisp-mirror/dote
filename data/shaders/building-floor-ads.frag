#version 330 core

%include fog.frag.inc

uniform float time = 0.0;

in vec2 frag_text_coord;

in vec3 N;
in vec3 V;
in vec3 L;

in float pick_weight;

uniform sampler2D texture_object;

uniform vec3   ia   = vec3(.2,.2,.2);
uniform vec3   id   = vec3(1.0,1.0,1.0);
uniform vec3   is   = vec3(1.0,1.0,1.0);

uniform float  ka   = 1.0;
uniform float  kd   = 1.0;
uniform float  ks   = 1.0;

uniform float shine = 12.0;

uniform vec4 pick_color         = vec4(0.0, 0.0, 1.0, 1.0);

uniform vec4 fow_color         = vec4(0.0, 0.0, 0.0, 1.0);

uniform float thrown_in_fow     = 1.0;

uniform float scale_text_coord  = 1.0;

out vec4 color;

void main () {
  vec4 texel          = texture2D(texture_object,frag_text_coord * scale_text_coord);
  if (texel.a <= 0.0) {
    discard;
  }
  vec3 N              = normalize(N);
  vec3 V              = normalize(V);
  vec3 L              = normalize(L);
  vec3 R              = reflect(-L, N);
  vec3 amb_diff_color = ka * ia + kd * (max(dot(N , L),0.0) * id);
  vec3 spec_color     = ks * (pow(max(dot(R, V),0.0),shine) * is);
  color               = vec4(amb_diff_color,1.0) * texel + vec4(spec_color,1.0);
  float fog_factor    = calc_fog_factor(eye_position, world_position, time);
  color               = mix(mix(color, fog_color, fog_factor), fow_color, thrown_in_fow);
  color               = mix(color, pick_color, pick_weight);
}

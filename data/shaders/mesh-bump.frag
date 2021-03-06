#version 330 core

%include fog.frag.inc

in vec2 frag_text_coord;
in vec3 eye_dir;
in vec3 light_dir;
in vec3 N_stat;

uniform sampler2D texture_object;
uniform sampler2D normal_map;

uniform vec3  ia    = vec3(1.0,1.0,1.0);
uniform vec3  id    = vec3(1.0,1.0,1.0);
uniform vec3  is    = vec3(1.0,1.0,1.0);

uniform float  ka   = 0.0;
uniform float  kd   = 1.0;
uniform float  ks   = 1.0;

uniform float time;

uniform float shine = 10.0;

out vec4 color;

void main () {
  vec4 texel          = texture2D(texture_object,frag_text_coord);
  vec3 N              = normalize(texture2D(normal_map,frag_text_coord).rgb * 2.0 - vec3(1.0));
  vec3 V              = normalize(eye_dir);
  vec3 L              = normalize(light_dir);
  vec3 R              = reflect(-L, N);
  vec3 amb_diff_color = ka * ia + kd * (max(dot(N , L),0.0) * texel.rgb);
  vec3 spec_color     = ks * (pow(max(dot(R, V),0.0),shine) * is);
  color               = vec4(amb_diff_color,1.0) + vec4(spec_color,1.0);
  float fog_factor    = calc_fog_factor(eye_position, world_position, time);
  color               = mix(color, fog_color, fog_factor);

}

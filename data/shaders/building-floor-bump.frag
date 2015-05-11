#version 330 core

in vec2 frag_text_coord;
in vec3 eye_dir;
in vec3 light_dir;
in vec3 N_stat;

in float pick_weight;

uniform sampler2D texture_object;
uniform sampler2D normal_map;

uniform vec3  ia    = vec3(1.0,1.0,1.0);
uniform vec3  id    = vec3(1.0,1.0,1.0);
uniform vec3  is    = vec3(1.0,1.0,1.0);

uniform float  ka   = 0.0;
uniform float  kd   = 1.0;
uniform float  ks   = 1.0;

uniform float shine = 10.0;

uniform vec4 pick_color          = vec4(0.0, 0.0, 1.0, 1.0);

uniform float scale_text_coord  = 10.0;

out vec4 color;

void main () {
  vec2 text_coord     = frag_text_coord * scale_text_coord;
  vec4 texel          = texture2D(texture_object,text_coord);
  vec3 N              = normalize(texture2D(normal_map,text_coord).rgb * 2.0 - vec3(1.0));
  vec3 V              = normalize(eye_dir);
  vec3 L              = normalize(light_dir);
  vec3 R              = reflect(-L, N);
  vec3 amb_diff_color = ka * ia + kd * (max(dot(N , L),0.0) * texel.rgb);
  vec3 spec_color     = ks * (pow(max(dot(R, V),0.0),shine) * is);

  color = mix(vec4(amb_diff_color,1.0) + vec4(spec_color,1.0), pick_color, pick_weight);
}


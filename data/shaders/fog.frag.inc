uniform float fog_density        = 0.004;
uniform float fog_treshold       = 16.0;
uniform vec4 fog_color = vec4(.5, .5 ,.5, 1.0);
in vec4 eye_position;
in vec4 world_position;

float calc_fog_factor(in vec4 eye_pos , in vec4 pos, in float time){
  float eye_distance = length(eye_pos);
  float res = 0.0;

  res = mix(1- clamp(exp( - fog_density  * eye_distance), 0.0, 1.0),
	    0.0,
	    smoothstep(0.0,
		       1.0 * (2 + sin (time * 0.5) + sin(pos.x * 0.5)),
		       pos.y  - fog_treshold));

  return res;

}

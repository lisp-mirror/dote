#version 330 core

#define PI 3.141592653589793238462643383279

in vec2 frag_text_coord;

in vec4 frag_smoke_text_coord;

in vec3 frag_pos;

uniform sampler2D texture_object;

uniform sampler2D texture_clouds_1;

uniform sampler2D texture_clouds_2;

uniform sampler2D texture_clouds_3;

uniform sampler2D texture_smoke;

uniform vec4  sky_color = vec4(0.43137255, 0.7529412, 1.0, 1.0);
//uniform float ka=1.0;

uniform float traslation_clouds_speed = 0.0;

uniform int weather_type = 0;

out vec4 color;

void main () {
  float interp              = (1.0 + sin (traslation_clouds_speed)) / 2.0;
  vec3 norm_p               = normalize(frag_pos);
  vec2 act_frag_text_coord  = frag_text_coord.st;

  act_frag_text_coord.s     = 0.5 + (atan(norm_p.z, norm_p.x) / (2.0 * PI));
  act_frag_text_coord.t     = norm_p.y;

  vec2 clouds_text_coords_1 = vec2(act_frag_text_coord.s + traslation_clouds_speed * .15,
				   act_frag_text_coord.t );
  vec2 clouds_text_coords_2 = vec2(act_frag_text_coord.s  + traslation_clouds_speed * .2,
				   act_frag_text_coord.t);
  vec2 clouds_text_coords_3 = vec2(act_frag_text_coord.s + traslation_clouds_speed  * .25,
				   act_frag_text_coord.t);
  vec4 smoke_text_coords    = vec4(frag_smoke_text_coord.s               +
				   0.002 * pow(frag_smoke_text_coord.t, 2) *
				    sin (traslation_clouds_speed * 0.0 -
					.1 * frag_smoke_text_coord.t +
					- 25.0 * traslation_clouds_speed),
				   frag_smoke_text_coord.t,
				   frag_smoke_text_coord.r,
				   frag_smoke_text_coord.q);

  vec4 bg                   = texture2D(texture_object,act_frag_text_coord);
  vec4 clouds_1             = texture2D(texture_clouds_1,clouds_text_coords_1);
  vec4 clouds_2             = texture2D(texture_clouds_2,clouds_text_coords_2);
  vec4 clouds_3             = texture2D(texture_clouds_3,clouds_text_coords_3);

  vec4 smoke                = smoke_text_coords.q < 0 ?
                              vec4(0.0) :
                              texture2DProj(texture_smoke,smoke_text_coords);

  float smoke_fading_speed  = sin(((1- act_frag_text_coord.t) * PI * 0.7) +
				  traslation_clouds_speed * 20.0);

  smoke.a                   = clamp(smoke_fading_speed, 0.0, 1.0) * smoke.a;

  vec4 layer_1              = mix(clouds_2, clouds_3, interp);
  vec4 layer_2;

  if (weather_type == 1) { // clear night stormy day
    layer_2                 = clamp(layer_1-clouds_1, 0.0, 1.0);
  }else{
    layer_2                 = clamp(layer_1+clouds_1, 0.0, 1.0); //foggy night clear day
  }

  layer_2.a                 = mix(layer_1.a, clouds_1.a, smoothstep(0.0, 0.5, layer_1.a));
  vec4 res                  = mix(mix(bg,smoke,smoke.a), layer_2,vec4(layer_2[3]));

  //vec3 amb_diff_color       = ka * ia;
  float stopper             = 1.0 * pow(act_frag_text_coord.t,2);
  float sky_top_weight      = clamp(stopper,0.0, 1.0);
  vec4  color_raw           = mix(res, sky_color, smoothstep(0.9,1.0,act_frag_text_coord.t-0.01));
  color                     = vec4(color_raw.rgb,1.0);

}

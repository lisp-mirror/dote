#version 330 core

%include fog.frag.inc

in vec2 frag_text_coord;

in vec3 N;
in vec3 V;
in vec3 L;

in float height;

in float slope;

//used for decals (roads, swamp, etc) and for fog of war too.
in vec2 frag_text_coord_decals;

in float pick_weight;

const int thrs_sand  = 0;
const int thrs_grass = 1;
const int thrs_empty = 2;
const int thrs_snow  = 3;

const int idx_decal_road       = 0;
const int idx_decal_building   = 1;
const int idx_decal_muddy_soil = 2;

uniform vec4 height_texture_thrs = vec4(0.02, 0.03, 0.2, 0.65);

uniform vec4 pick_color          = vec4(0.0, 0.0, 1.0, 1.0);

uniform float time = 0.0;

// sand
uniform sampler2D texture_terrain_level_1;
// grass
uniform sampler2D texture_terrain_level_2;
// snow
uniform sampler2D texture_terrain_level_3;
// soil
uniform sampler2D texture_terrain_rock_level_1;
// stratigraphic rock
uniform sampler2D texture_terrain_rock_level_2;
// muddy soil
uniform sampler2D texture_muddy_soil_decal;
// roads
uniform sampler2D texture_roads_decal;
// buildings
uniform sampler2D texture_building_decal;
// fow
uniform sampler2D texture_fow;

// channel 0 road, channel 1 building, channel 2 muddy muddy_soil, channel 3 border
uniform sampler2D decals_weights;

uniform vec3  ia = vec3(.2,.2,.2);
uniform vec3  id = vec3(1.0,1.0,1.0);
uniform vec3  is = vec3(1.0,1.0,1.0);

uniform float  ka = 1.0;
uniform float  kd = 1.0;
uniform float  ks = 1.0;

uniform float shine = 12.0;

uniform float scale_building_text_coord   = 10.0;

uniform float scale_road_text_coord       = 10.0;

uniform float scale_muddy_soil_text_coord = 2.0;

uniform vec4  color_border = vec4(0.0, 0.0, 0.0, 1.0);

out vec4 color;

float lerp (in float from, in float to, in float pos) {
  float w = (pos - from) / (to - from);
  return w;
}

vec4 col_empty  = vec4(0.0);

vec4 terrain_color_by_height() {

  if (height <= height_texture_thrs[thrs_sand]){
    return texture2D(texture_terrain_level_1, frag_text_coord);
  }else if (height >  height_texture_thrs[thrs_sand] &&
	    height <= height_texture_thrs[thrs_grass]){
    vec4 col_sand  = texture2D(texture_terrain_level_1, frag_text_coord);
    vec4 col_grass = texture2D(texture_terrain_level_2, frag_text_coord);
    return mix(col_sand,col_grass,
	       lerp(height_texture_thrs[thrs_sand], height_texture_thrs[thrs_grass],height));
  }else if (height >  height_texture_thrs[thrs_grass] &&
	    height <= height_texture_thrs[thrs_empty]){
    vec4 col_grass = texture2D(texture_terrain_level_2, frag_text_coord);

    return mix(col_grass,col_empty,
	       lerp(height_texture_thrs[thrs_grass], height_texture_thrs[thrs_empty],height));
  }else if (height >  height_texture_thrs[thrs_empty] &&
	    height <= height_texture_thrs[thrs_snow]){
    vec4 col_snow  = texture2D(texture_terrain_level_3, frag_text_coord);
    return mix(col_empty,col_snow,
	       smoothstep(0.0, 1.0,
			  lerp(height_texture_thrs[thrs_empty], height_texture_thrs[thrs_snow],
			       height)));
  }else {
    return texture2D(texture_terrain_level_3, frag_text_coord);
  }

}


vec4 rock_color_by_height() {
  vec4 col_muddy_soil  = texture2D(texture_terrain_rock_level_1, frag_text_coord);
  vec4 col_rock  = texture2D(texture_terrain_rock_level_2, frag_text_coord);
  //col_rock       = mix(col_muddy_soil, col_rock, smoothstep(0.0, 1.0, 2.0*facing_z*facing_z));
  col_rock       = mix(col_muddy_soil, col_rock, smoothstep(0.0, 1.0, 10.0*height*height));
  return col_rock;
}

vec4 color_by_slope (vec4 terr , vec4 rock){
  return mix(terr, rock, smoothstep(0.0, 1.0, 0.65 * slope));
}

vec4 sample_decal_weigths (){
  return texture2D(decals_weights, frag_text_coord_decals);
}

vec4 h_border (in vec4 c1, in vec4 c2, float w) {
  return mix(c1, c2, smoothstep(0.999, 1.0, w));
}

vec4 v_border (in vec4 c1, in vec4 c2, float w) {
  return mix(c1, c2, smoothstep(0.0, 0.001, w));
}


vec4 terrain_color (){
  vec4 terrain     = terrain_color_by_height();
  vec4 rocks       = rock_color_by_height();
  vec4 raw_terrain = color_by_slope(terrain, rocks);

  vec4 weights = sample_decal_weigths();

  vec4 color_muddy_soil = texture2D(texture_muddy_soil_decal,
			      frag_text_coord * scale_muddy_soil_text_coord);
  raw_terrain = mix(raw_terrain, color_muddy_soil, weights[idx_decal_muddy_soil]);

  vec4 color_building = texture2D(texture_building_decal,
				  frag_text_coord  * scale_building_text_coord);
  raw_terrain = mix(raw_terrain, color_building, weights[idx_decal_building]);

  vec4 color_roads = texture2D(texture_roads_decal,
			       frag_text_coord * scale_road_text_coord);
  raw_terrain = mix(raw_terrain, color_roads, weights[idx_decal_road]);

  raw_terrain = h_border(raw_terrain, color_border, frag_text_coord_decals.s);

  raw_terrain = v_border(color_border, raw_terrain, frag_text_coord_decals.s);

  raw_terrain = h_border(raw_terrain, color_border, frag_text_coord_decals.t);

  raw_terrain = v_border(color_border, raw_terrain, frag_text_coord_decals.t);

  //raw_terrain = weights;
  return raw_terrain;
}

void main () {
  vec3 N              = normalize(N);
  vec3 V              = normalize(V);
  vec3 L              = normalize(L);
  vec3 R              = reflect(-L, N);
  vec4 texel          = terrain_color();
  vec3 amb_diff_color = ka * ia + kd * (max(dot(N , L),0.0) * id);

  vec3 spec_color;
  vec4 decals = sample_decal_weigths();

  spec_color       = ks * (pow(max(dot(R, V),0.0),shine) * is);
  spec_color      *=  smoothstep(0.0, 1.0, decals[idx_decal_road]);

  color            = vec4(amb_diff_color,1.0) * texel + vec4(spec_color,1.0);

  float fog_factor = calc_fog_factor(eye_position, world_position, time);
  vec4  fow_color  = texture2D(texture_fow, frag_text_coord_decals);

  color            = mix(mix(color, fog_color, fog_factor), fow_color, fow_color.a);
  color            = mix(color, pick_color, pick_weight);
}

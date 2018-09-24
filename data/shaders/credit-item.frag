#version 330 core

in vec2 frag_text_coord;

uniform sampler2D texture_object;

uniform vec3  ia           = vec3(1.0,1.0,1.0);

uniform float pixel_size   = 1.0;

out vec4 color;

float scale_pixel (float pixel_size, float text_coord){
  return floor(text_coord * pixel_size) / pixel_size;
}

void main () {
  vec2  new_texture_coord = vec2(scale_pixel(pixel_size, frag_text_coord.s),
                                 scale_pixel(pixel_size, frag_text_coord.t));
  vec4 texel = texture2D(texture_object, new_texture_coord);
  color      = texel * vec4(ia,1.0);
}

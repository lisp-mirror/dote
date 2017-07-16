#version 330 core

in vec2 frag_text_coord;

uniform sampler2D texture_object;

uniform float duration;

uniform float time;

out vec4 color;

void main () {
  vec4  texel = texture2D(texture_object, frag_text_coord);
  //texel.s     = texel.s * texture_horizontal_offset;

  float alpha = smoothstep(0.0, 1.0, time) * (1 - step(duration, time))  +
                (1 - smoothstep(0.0, 1.0, time - duration)) * step(duration, time);
  color       = vec4(texel.rgb, min(alpha,texel.a));
}

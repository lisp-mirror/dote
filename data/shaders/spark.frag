#version 330 core

in vec2 frag_text_coord;

uniform sampler2D texture_object;

uniform float time;

uniform float alpha;

uniform vec4  fade_color;

out vec4 color;

void main () {
  vec4 texel         = texture2D(texture_object, frag_text_coord);
  if(alpha > 0.5) {
    float tr         = frag_text_coord.s * frag_text_coord.t;
    float w          = 1e-1;
    float x          = time - trunc(time);
    float spark      = smoothstep(tr-w, tr, x) - smoothstep(tr, tr+w, x);
    vec4 spark_color = spark * fade_color;
    color            = vec4(texel.rgb + spark_color.rgb, texel.a);
  } else {
    color            = texel;
  }
}

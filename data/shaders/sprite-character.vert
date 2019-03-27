#version 330 core

layout (location = 0) in vec4 position;

layout (location = 12) in vec2 texture_coord;

uniform mat4 modelview_matrix;
uniform mat4 proj_matrix;

uniform float texel_s_offset = 0.0;
uniform float texel_t_offset = 0.0;


out vec2 frag_text_coord;

%include billboard.vert.inc.glsl

void main () {

  mat4 modelview = mat4(modelview_matrix);
  remove_rotation(modelview);

  frag_text_coord = texture_coord + vec2(texel_s_offset, texel_t_offset);
  gl_Position = proj_matrix   *
                modelview     *
                post_scaling  *
                vec4(position.x,
		     position.y,
		     position.z,
                     1.0);
}

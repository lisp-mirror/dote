#version 330 core

layout (location = 0) in vec4 position;

layout (location = 12) in vec2 texture_coord;

uniform mat4 modelview_matrix;
uniform mat4 proj_matrix;

uniform float time;

uniform float vert_displacement_speed;

out vec2 frag_text_coord;

%include billboard.vert.inc.glsl

void main () {

  mat4 modelview = mat4(modelview_matrix);
  remove_rotation(modelview);

  frag_text_coord = texture_coord;
  gl_Position = proj_matrix   *
                modelview     *
                post_scaling  *
                vec4(position.x,
		     position.y + time * vert_displacement_speed,
		     position.z, 1.0);
}

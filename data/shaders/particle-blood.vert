#version 330 core

layout (location = 0) in vec4 position;

layout (location = 1) in vec4 center_position;

layout (location = 3) in float delay;

layout (location = 4) in float life;

layout (location = 5) in float scaling;

uniform mat4 modelview_matrix;

uniform mat4 proj_matrix;

%include billboard.vert.inc.glsl

%include make-translation-matrix.vert.inc.glsl

%include make-scale-matrix.vert.inc.glsl

void main () {
  mat4 scale     = make_scale(scaling, scaling, scaling);
  mat4 modelview = mat4(modelview_matrix *
			make_translation(center_position.x,
					 center_position.y,
					 center_position.z));
  remove_rotation(modelview);

  if (delay < 0.0 && life > 0.0){
    gl_Position = proj_matrix * modelview * scale * position;
  } else {
    gl_Position = vec4(2.0, 2.0, 2.0, 1.0);
  }

}

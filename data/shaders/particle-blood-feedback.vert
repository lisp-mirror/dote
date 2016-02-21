#version 330 core

layout (location = 0) in vec3 position;

layout (location = 1) in float mass;

layout (location = 2) in vec3 v0;

layout (location = 3) in float delay;

uniform float dt = 1.0;

uniform vec3 gravity = vec3(0.0, -9.0, 0.0);

uniform float min_y =   10.0;

out vec3 new_position;

out vec3 new_velocity;

void integrate (in vec3 force, in vec3 velo, in float mass, in float dt,
		out vec3 dv, out vec3 displ){
  vec3 da = force / mass;
  dv      = velo + da * dt;
  displ   = dv * dt;

}

void main() {
  vec3 dv    = vec3(0.0);
  vec3 displ = vec3(0.0);
  if (position.y > min_y){
    if (delay < 0.0){
      integrate(gravity, v0, mass, dt, // in
		dv, displ);           // out
      new_position = position + displ;
      new_velocity = dv;
    }else{
      new_position = position;
      new_velocity = v0;
    }
  }else{
    new_position = position;
    new_velocity = vec3(0.0);
  }

}

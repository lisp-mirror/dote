void integrate (in vec3 force, in vec3 velo, in float mass, in float dt,
		out vec3 dv, out vec3 displ){
  vec3 da = force / mass;
  dv      = velo + da * dt;
  displ   = dv * dt;

}

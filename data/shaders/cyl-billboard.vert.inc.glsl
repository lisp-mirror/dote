
void remove_rotation (inout mat4 modelview) {
  modelview[0].xyz = vec3(1, 0 ,0);
  modelview[2].xyz = vec3(0, 0, 1);
}

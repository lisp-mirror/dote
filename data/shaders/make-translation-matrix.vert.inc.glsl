mat4 make_translation (float x, float y, float z) {
  mat4 res = mat4(1.0);
  res[3].xyzw = vec4(x,   y,   z,   1.0);
  return res;
}

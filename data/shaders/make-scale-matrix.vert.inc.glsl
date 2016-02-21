mat4 make_scale (float x, float y, float z) {
  mat4 res  = mat4(1.0);
  res[0][0] = x;
  res[1][1] = y;
  res[2][2] = z;
  return res;
}

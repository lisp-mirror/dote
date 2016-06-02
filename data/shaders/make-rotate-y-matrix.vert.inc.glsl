mat4 make_rotate_y (float angle) {
  mat4 res  = mat4(1.0);

  res[0][0] = cos(angle);
  res[0][2] = sin(angle);
  res[2][0] = -sin(angle);
  res[2][2] = cos(angle);

  return res;
}

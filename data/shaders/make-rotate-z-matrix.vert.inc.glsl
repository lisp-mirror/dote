mat4 make_rotate_z (float angle) {
  mat4 res  = mat4(1.0);

  res[0][0] = cos(angle);
  res[0][1] = - sin(angle);
  res[1][0] = sin(angle);
  res[1][1] = cos(angle);

  return res;
}

// Copyright (c) 2012 Jakob Progsch

// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.

// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:

//    1. The origin of this software must not be misrepresented; you must not
//    claim that you wrote the original software. If you use this software
//    in a product, an acknowledgment in the product documentation would be
//    appreciated but is not required.

//    2. Altered source versions must be plainly marked as such, and must not be
//    misrepresented as being the original software.

//    3. This notice may not be removed or altered from any source
//    distribution.

uniform int noise_seed;

float noise_hash (int x) {
  x = x * 1235167 + gl_VertexID * 948737 + seed * 9284365;
  x = (x >> 13) ^ x;
  return ((x * (x * x * 60493 + 19990303) + 1376312589) & 0x7fffffff) / float(0x7fffffff-1);
}

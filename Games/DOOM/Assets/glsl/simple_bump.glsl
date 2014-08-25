attribute vec3 tangent;

varying vec3 lightVec;
varying vec3 halfVec;
varying vec3 eyeVec;


// -------------------------------------------------------
// Vertex shader's main entry point.
//
// Bump mapping with parallax offset.
// -------------------------------------------------------

void main (void)
{
  // output vertex position
  gl_Position = ftransform ();

  vec3 n = normalize (gl_NormalMatrix * gl_Normal);
  vec3 t = normalize (gl_NormalMatrix * tangent);
  vec3 b = cross (n, t);

  // output texture coordinates for decal and normal maps
  gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;

  // transform light and half angle vectors by tangent basis
  vec3 v;
  v.x = dot (vec3 (gl_LightSource[0].position), t);
  v.y = dot (vec3 (gl_LightSource[0].position), b);
  v.z = dot (vec3 (gl_LightSource[0].position), n);
  lightVec = normalize (v);

  v.x = dot (vec3 (gl_LightSource[0].halfVector), t);
  v.y = dot (vec3 (gl_LightSource[0].halfVector), b);
  v.z = dot (vec3 (gl_LightSource[0].halfVector), n);
  halfVec = normalize (v);

  eyeVec = vec3 (gl_ModelViewMatrix * gl_Vertex);
  v.x = dot (eyeVec, t);
  v.y = dot (eyeVec, b);
  v.z = dot (eyeVec, n);
  eyeVec = normalize (v);
}

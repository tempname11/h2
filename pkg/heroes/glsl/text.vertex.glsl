attribute vec2 interp;
attribute vec2 offset;
attribute vec2 texPlace;
attribute vec2 box;

uniform vec2 scrPlace;
uniform vec2 texDimensions;
uniform vec2 scrDimensions;

varying vec2 texPos;

void main()
{
         texPos = (texPlace + interp * box) / texDimensions;
    vec2 scrPos = (scrPlace + offset + interp * box) / scrDimensions;
    vec2 ndc = (vec2(scrPos.x, 1.0 - scrPos.y) - 0.5) * 2.0;
    gl_Position = vec4(ndc, 0.0, 1.0);
}

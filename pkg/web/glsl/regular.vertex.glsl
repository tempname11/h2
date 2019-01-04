precision mediump float;

attribute vec2 interp;

uniform vec2 texDimensions;
uniform vec2 scrDimensions;
uniform vec2 texPlace;
uniform vec2 scrPlace;
uniform vec2 texBox;
uniform vec2 scrBox;

varying vec2 texPos;

void main()
{
         texPos = (texPlace + interp * texBox) / texDimensions;
    vec2 scrPos = (scrPlace + interp * scrBox) / scrDimensions;
    vec2 ndc = (vec2(scrPos.x, 1.0 - scrPos.y) - 0.5) * 2.0;
    gl_Position = vec4(ndc, 0.0, 1.0);
}

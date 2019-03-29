uniform sampler2D texImage;
varying vec2 texPos;

void main()
{
    gl_FragColor = texture2D(texImage, texPos).rgba;
}

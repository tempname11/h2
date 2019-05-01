uniform sampler2D texImage;
varying vec2 texPos;

void main()
{
    float alpha = texture2D(texImage, texPos).r;
    gl_FragColor = vec4(1, 1, 1, alpha);
}

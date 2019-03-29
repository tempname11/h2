uniform sampler2D texAtlas;
uniform sampler2D texPalette;
varying vec2 texPos;

void main()
{
    float r = texture2D(texAtlas, texPos).r;
    vec4 color = texture2D(texPalette, vec2(r, 0.5));
    gl_FragColor = color;//vec4(0,0,0,1);
}

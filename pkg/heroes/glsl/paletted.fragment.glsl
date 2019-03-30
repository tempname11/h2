uniform vec4 outlineColor;
uniform vec4 shadowColor;
uniform sampler2D texAtlas;
uniform sampler2D texPalette;
varying vec2 texPos;

/* Magic Indices:
    0: background
    1: weird shadow outline?
    2: unused?
    3: unused?
    4: shadow bits?
    5: major bits of outline?
    6: minor bits of outline?
    7: minor bits of outline?
*/

void main()
{
    float r = texture2D(texAtlas, texPos).r;
    vec4 color = texture2D(texPalette, vec2(r, 0.5));
    int index = int(r * 255.0);
    if (index == 1 || index == 4) {
      gl_FragColor = shadowColor;
    } else if (index >= 5 && index <= 7) {
      gl_FragColor = outlineColor;
    } else {
      gl_FragColor = color;
    }
}

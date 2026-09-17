#version 120

// Grayscale pass: luminance-weighted desaturation.

uniform sampler2D uTex;
uniform vec2 uTexSize;

varying vec2 vUV;

void main() {
    vec4 c = texture2D(uTex, vUV / uTexSize);
    float lum = dot(c.rgb, vec3(0.299, 0.587, 0.114));
    gl_FragColor = vec4(vec3(lum), c.a);
}

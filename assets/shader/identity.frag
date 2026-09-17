#version 120

// Identity pass: sample the source texture unchanged.
// Uniforms: uTex (sampler2D), uTexSize (vec2, in pixels).

uniform sampler2D uTex;
uniform vec2 uTexSize;

varying vec2 vUV;

void main() {
    gl_FragColor = texture2D(uTex, vUV / uTexSize);
}

#version 120

// Box blur pass: 3x3 kernel. Cheap separable-ish approximation done in
// one pass; good enough for glow/vignette-softening test shaders.

uniform sampler2D uTex;
uniform vec2 uTexSize;

varying vec2 vUV;

void main() {
    vec2 px = vec2(1.0, 1.0) / uTexSize;
    vec2 uv = vUV / uTexSize;
    vec4 acc = vec4(0.0);
    acc += texture2D(uTex, uv + vec2(-px.x, -px.y));
    acc += texture2D(uTex, uv + vec2( 0.0,  -px.y));
    acc += texture2D(uTex, uv + vec2( px.x, -px.y));
    acc += texture2D(uTex, uv + vec2(-px.x,  0.0));
    acc += texture2D(uTex, uv);
    acc += texture2D(uTex, uv + vec2( px.x,  0.0));
    acc += texture2D(uTex, uv + vec2(-px.x,  px.y));
    acc += texture2D(uTex, uv + vec2( 0.0,   px.y));
    acc += texture2D(uTex, uv + vec2( px.x,  px.y));
    gl_FragColor = acc / 9.0;
}

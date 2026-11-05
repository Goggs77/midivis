#version 120

// Invert pass: negate each channel.

uniform sampler2D uTex;
uniform vec2 uTexSize;

varying vec2 vUV;

void main() {
    //vec2 uv = vUV / uTexSize;
    //vec2 center = vec2(0.5, 0.5);
    //vec2 delta = uv - center;
    //float r = length(delta);
    //float factor = 1.0 + 0.1 * r + 0.01 * r * r;
    //vec2 distortedUV = center + delta * factor;
    //vec4 c = texture2D(uTex, distortedUV);
    vec4 c = texture2D(uTex, vUV / uTexSize);
    // The FX result is composited as opaque output.  Setting alpha here
    // avoids a full-frame CPU pass over the resolved pixels.
    gl_FragColor = vec4(1.0 - c.rgb, 1.0);
}

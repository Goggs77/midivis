#version 120

// TODO 2 skeleton: interval-gap visualization.
// Per-frame CPU data (set via fxWith + uniform1fv in Example.hs):
//   uAngles[0..127] = held-note angles (0.0 = empty slot), in radians
//   uAngles[128]    = baseAngle (global rotation)
// The pass is a plain passthrough until the algorithm below is filled in.
//
// Planned algorithm (see TODO.md item 2):
//   - count the held notes (non-zero uAngles[0..127]) -> n
//   - sort their angles, compute the n angular gaps between neighbours
//     (and the wrap-around gap back to the first)
//   - each gap encodes the frequency ratio of its adjacent pair, mapped to a
//     hue (hsv2rgb), drawn as an arc / wedge near the disc edge
//   - overlays on top of the inverted scene, kept additive-ish

uniform sampler2D uTex;
uniform vec2 uTexSize;
uniform float uAngles[129];

varying vec2 vUV;

void main() {
    vec2 uv = vUV / uTexSize;

    // TODO 2: gap visualization from uAngles goes here.
    //   vec2 p = uv - 0.5;                 // disc centre
    //   float a = atan(p.y, p.x);          // fragment angle
    //   ... find which gap this angle falls into, colour by ratio hue ...
    //   gl_FragColor = mix(texel, gapColor, mask);

    gl_FragColor = texture2D(uTex, uv);   // passthrough for now
}

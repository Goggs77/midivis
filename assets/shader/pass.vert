#version 120

// Full-screen textured quad vertex shader, shared by all FX passes.
// The quad covers the whole window in clip space; texture coords in
// pixels (0..width, 0..height) for direct texel addressing.

attribute vec2 aPos;
attribute vec2 aUV;

varying vec2 vUV;

void main() {
    gl_Position = vec4(aPos, 0.0, 1.0);
    vUV = aUV;
}

#!/usr/bin/env python3
"""Convert FLAC impulse responses in assets/audio/IR to mono PCM16 WAV.

The Haskell reverb loads .wav files (WavLoader.hs).  Drop your .flac IRs into
assets/audio/IR/ and run this script once from the project root:

    python tools/flac2wav.py

It writes a .wav next to each .flac (mono, 16-bit PCM).
"""
import glob
import os
import subprocess
import sys

SRC = os.path.join("assets", "audio", "IR")


def main() -> int:
    os.makedirs(SRC, exist_ok=True)
    flacs = sorted(glob.glob(os.path.join(SRC, "*.flac")))
    if not flacs:
        print(f"no .flac files in {SRC} — nothing to convert")
        return 0
    for f in flacs:
        out = f[:-5] + ".wav"
        # -ac 1: mono mix, -c:a pcm_s16le: 16-bit PCM
        subprocess.run(
            ["ffmpeg", "-y", "-i", f, "-ac", "1", "-c:a", "pcm_s16le", out],
            check=True,
            capture_output=True,
        )
        print(f"converted {os.path.basename(f)} -> {os.path.basename(out)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())

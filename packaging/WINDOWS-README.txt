midivis - Windows x64
=====================

Requirements
------------

- 64-bit Windows 10 or Windows 11.
- OpenGL support from the installed graphics driver.
- An audio output device that supports 48 kHz stereo output.
- A MIDI input device is optional.

Running
-------

Run midivis.exe. Keep midivis.exe, freeglut.dll, and vcruntime140.dll in the
same directory. The application contains its fonts, impulse response, tuning
files, and shaders, so there is no separate assets directory.

The executable is compressed with UPX to reduce the download size. Some
antivirus software may report packed executables conservatively.

No installation is required. To remove the application, close it and delete
this directory.

Licensing
---------

See LICENSE for the midivis terms, font licenses, and runtime-library notices.

Diagnostics
-----------

midivis runs as a Windows GUI application without a console window. To capture
diagnostics, launch it with standard error redirected:

    $env:MIDIVIS_DEBUG = "1"
    Start-Process .\midivis.exe -RedirectStandardError .\midivis.log -Wait

This enables detailed startup, callback, and signal-level diagnostics. If no
audio device can be opened, the visualizer still starts.

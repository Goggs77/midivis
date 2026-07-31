# 修复报告

## 问题汇总

GHC 9.10.3（LLVM/Clang + libc++/UCRT 工具链）下，midivis 项目使用了两个依赖包存在 Windows 平台兼容问题。

---

## 修复 1：RtMidi — `addDLL: stdc++` 错误

### 根因

`RtMidi-0.8.0.0` 的 `.cabal` 全局声明了 `extra-libraries: stdc++`。GHC 9.10.3 的内置工具链已从 GCC/libstdc++ 切换为 Clang/libc++，`libstdc++-6.dll` 不存在于 GHC 的 mingw 目录中（只有 `libc++.dll`）。GHC 运行时链接器调用 `addDLL("stdc++")` 时失败，且此错误只在 Template Haskell 编译期触发（非 TH 文件延迟到最终链接）。

### 解决

修改 `rtmidifix/RtMidi-0.8.0.0/RtMidi.cabal`：

- 删除全局 `extra-libraries: stdc++`
- Linux/macOS 各自条件分支保留 `stdc++`
- Windows（`os(mingw32)`）改用 `extra-libraries: c++`（对应 libc++ DLL）

**强制覆盖本地版**：`cabal.project` 中加入 `./rtmidifix/RtMidi-0.8.0.0`。

---

## 修复 2：portaudio — `_snprintf` + `SetupDi*` 链接错误

### 问题

```
ld.lld: error: undefined symbol: __declspec(dllimport) _snprintf
>>> referenced by libportaudio.a(pa_win_wasapi.c.obj)
ld.lld: error: undefined symbol: __declspec(dllimport) SetupDiGetClassDevsW
>>> referenced by libportaudio.a(pa_win_wdmks.c.obj)
...（多个 SetupDi* 符号）
```

### 根因

`libportaudio.a` 是 **MSVC 编译**（非 MinGW/Clang），其 .obj 文件：
1. 引用 `__declspec(dllimport) _snprintf` — MSVC CRT 符号（COFF 名 `__imp__snprintf`），GHC 的 Clang/UCRT 工具链不提供此 DLL import 符号，只提供 POSIX 名的 `snprintf`。
2. 引用 `SetupDi*` 系列函数 — 来自 Windows `setupapi.dll`，未在链接时声明。

### 解决

修改 `rtmidifix/portaudio-0.2.4/portaudio.cabal`：

```cabal
if os(mingw32)
    extra-libraries: setupapi                        ← 补充 Windows Setup API
    c-sources:       cbits/imp_snprintf.c            ← 提供 _snprintf 的 DLL import 桩
```

**`imp_snprintf.c`**：

```c
int snprintf(char *, unsigned long long, const char *, ...);

/* COFF __declspec(dllimport) 机制期望 __imp__snprintf 是一个 DATA 符号
 * 指向实际函数地址。我们将它指向 C 运行时的 snprintf。 */
__attribute__((used))
int (*__imp__snprintf)(char *, unsigned long long, const char *, ...) = snprintf;
```

**强制覆盖本地版**：`cabal.project` 中加入 `./rtmidifix/portaudio-0.2.4`。

---

## cabal.project（当前状态）

位于 `I:\Goggs\Works\haskell\midivis\cabal.project`：

```cabal
packages: .
          ./rtmidifix/RtMidi-0.8.0.0
          ./rtmidifix/portaudio-0.2.4
          ./newerGlossRelative/gloss-relative
```

---

## 目录结构

```
midivis/
├── cabal.project                        ← 多包覆盖配置
├── Setup.hs                             ← postBuild 钩子（拷贝 freeglut.dll）
├── rtmidifix/
│   ├── RtMidi-0.8.0.0/
│   │   └── RtMidi.cabal                 ← Windows 用 c++ 替代 stdc++
│   └── portaudio-0.2.4/
│       ├── portaudio.cabal              ← Windows 加 setupapi + imp_snprintf.c
│       └── cbits/
│           └── imp_snprintf.c           ← __imp__snprintf 桩（MSVC CRT → UCRT 桥接）
└── freeglut/                            ← 解压后的 freeglut DLL
    └── bin/x64/freeglut.dll
```

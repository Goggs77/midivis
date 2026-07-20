# RtMidi 修复报告：解决 `addDLL: stdc++` 错误

## 问题

GHC 9.10.3 编译启用 `TemplateHaskell` 的模块时失败：
```
<no location info>: error:
    addDLL: stdc++ or dependencies not loaded. (Win32 error 126)
```

## 根因

**RtMidi-0.8.0.0** 的 `.cabal` 文件第 43 行声明了全局的：
```cabal
extra-libraries:     stdc++
```

该声明**无条件**作用于所有平台（包括 Windows）。

GHC 9.10.3 的内置工具链已从 **GCC/libstdc++** 切换为 **LLVM/Clang + libc++**。RtMidi 链接时仍然要求 `libstdc++-6.dll`，但 GHC 的 mingw 目录下只有 `libc++.dll`，因此 GHC 的运行时链接器调用 `addDLL("stdc++")` 时失败。

### 最小复现

```bash
cabal exec -- ghc -package RtMidi -e 'putStrLn "test"'
# → addDLL: stdc++ or dependencies not loaded. (Win32 error 126)
```

该错误只需加载 RtMidi 包就会触发，与 Template Haskell 本身无关。但非 TH 文件编译时 GHC 不加载原生库符号（延迟到最终可执行文件链接），因此不受影响。

## 涉及的包

| 包 | `extra-libraries` | 状态 |
|---|---|---|
| **RtMidi** | `stdc++` (旧) → `c++` (新) | ⚠️ 已修复 |
| proteaaudio | `ole32 dsound winmm`（无 stdc++） | ✅ 无需修复 |
| gloss/GLUT/OpenGL/apecs | 无 C++ 依赖 | ✅ 无需修复 |

## 修改内容

**文件**：`RtMidi-0.8.0.0/RtMidi.cabal`

| 改动 | 说明 |
|------|------|
| 删除全局 `extra-libraries: stdc++` | 不再无条件要求 stdc++ |
| Linux 各条件分支加入 `extra-libraries: stdc++` | 保持 Linux 上 GCC 工具链兼容 |
| macOS 各条件分支加入 `extra-libraries: stdc++` | 保持 macOS 上 Clang 兼容 |
| Windows (`os(mingw32)`) 改用 `extra-libraries: c++` | `c++` 对应 Clang/LLVM 的 `libc++.dll`（GHC 9.10.3 内置） |

### 关键修改对照

**旧版（RtMidi.cabal 第 42-74 行）：**
```cabal
  extra-libraries:     stdc++          ← 全局，所有平台
  ...

  if os(mingw32)
    cxx-options:       -std=c++11 -D__WINDOWS_MM__
    extra-libraries:   winmm
```

**新版：**
```cabal
  -- 全局 extra-libraries 已移除
  ...

  if os(mingw32)
    cxx-options:       -std=c++11 -D__WINDOWS_MM__
    extra-libraries:   c++ winmm       ← Windows 改用 libc++
  ...
  -- Linux/macOS 各自条件内已加入 stdc++（保持不变）
```

## 强制使用本地修改版 RtMidi（关键）

### 问题

仅执行 `cabal install --lib` 将修改版 RtMidi 安装到 store 是不够的。当在 `midivis` 工程目录执行 `cabal build` 时，cabal 仍然会从 **Hackage 索引**拉取原生 RtMidi 源码重建，覆盖本地修复版。

这是因为 cabal 默认将远程索引中的源码视为最新来源，而本地安装的 store 包仅在 hash 匹配时复用。

### 解决方案：cabal.project

在 `midivis` 工程根目录创建一个 `cabal.project` 文件（**已创建完毕**），将修改版 RtMidi 作为本地包覆盖：

```
文件：I:\Goggs\Works\haskell\midivis\cabal.project
内容：
```

```cabal
packages: .
          ./rtmidifix/RtMidi-0.8.0.0
```

该配置告诉 cabal：
- `.` — 构建当前目录（midivis）
- `./rtmidifix/RtMidi-0.8.0.0` — 同时从本地路径构建 RtMidi，**不下载 Hackage 版本**

当 cabal 解析依赖时，看到 `RtMidi` 已被本地包覆盖，就会使用本地源码编译，从而应用 `extra-libraries: c++ winmm` 的修改。

### 如果其他工程也需要

`cabal.project` 文件支持多个包路径，语法为：
```cabal
packages: .
          ./rtmidifix/RtMidi-0.8.0.0
          ../../other-project/RtMidi-0.8.0.0   # 其他工程的覆盖路径
```

## 编译步骤

### 第一步：安装本地修改版 RtMidi（仅首次）

```bash
cd I:\Goggs\Works\haskell\midivis\rtmidifix\RtMidi-0.8.0.0
cabal install --lib --disable-tests --disable-benchmarks --overwrite-policy=always
```

### 第二步：构建 midivis（自动使用本地 RtMidi）

```bash
cd I:\Goggs\Works\haskell\midivis
cabal clean
cabal build
```

`cabal.project` 文件已位于 `I:\Goggs\Works\haskell\midivis\cabal.project`，无需手动创建。

### 如果内存受限（页面文件过小）

```bash
cd I:\Goggs\Works\haskell\midivis
cabal clean
cabal configure --disable-tests --disable-benchmarks --disable-optimization
cabal build
```

## 验证方法

安装后运行以下命令应不再报错（`libc++.dll` 的警告为非致命，可忽略）：

```bash
cd I:\Goggs\Works\VibeCoding\hdebug\test-repro
cabal exec -- ghc -package RtMidi -e 'putStrLn "ok"'
```

输出应为 `ok`。

## `cabal.project` 完整参考

```cabal
packages: .
          ./rtmidifix/RtMidi-0.8.0.0
```

可选的附加配置：
```cabal
packages: .
          ./rtmidifix/RtMidi-0.8.0.0

-- 以下为优化选项（非必须）
package RtMidi
  ghc-options: -O0   -- 禁用 RtMidi 的优化以加速构建

package midivis
  ghc-options: -O2   -- midivis 仍可使用正常优化

write-ghc-environment-files: always
```

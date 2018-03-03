    Title: Startup benchmark for various languages
    Tags: language:en, category:kisaragi, Programming, Benchmark

# Why benchmark hello-world?

Sometimes I want to write interactive scripts for my graphical environment that simply wires some logic together. In such workloads where little calculation is involved, the biggest contributing factor to speed is the startup time of the script itself. For example, a language that takes 2 seconds to launch is certainly not the right choice for a input method switcher script like %lozenge%github{kisaragi-hiu/scripts/blob/master/fcitx-switch-im}.

This article provides the results from a script made to benchmark many languages.

# The script

`language-startup-benchmark` is a script that `time`s a bunch of hello world scripts and reports them in parsable JSON. I'm also timing a few compiled languages by first compiling them in /tmp, then timing that final executable.

The script itself is very specific to this benchmark, but the functions I'm using aren't. Some functions that may be useful to other benchmarks include:

- `report_time <language title> <command> <args> ...`: time `<command> <args> ...` and report it in JSON.

```
[flyin1501@MF-PC language-startup-benchmark]$ report_time test sleep 1
{"status":"success","report":{"title":"test","output":"","time":1.001,"unit":"second"}}
```

- `compile_and_time <title> <compile function> <file>`: Compile `<file>` with `<compile function>`, then time the output with `report_time`.

The compile function should take one argument as a file, compile it, and return the final executable path through stdout. This means any output during compilation has to be silenced or redirected in the function.

```
[flyin1501@MF-PC language-startup-benchmark]$ compile_and_time haskell compile:haskell hello.hs
{"status":"success","report":{"title":"haskell","output":"hello","time":0.002,"unit":"second"}}
```

# The benchmark

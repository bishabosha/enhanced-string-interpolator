# Enhanced String Interpolator

## Basic usage

Currently all functionality is accessed with one import:
```scala
import regexglob.RegexGlobbing.r
```

> the exact namespace for the final library will likely change.

A simple example can be illustrated here, where we parse some basic text representing a sequence of integers, delimited by `"["` and `"]"`, and separated by `", "`.
In one pattern, you can extract a typed value `xs: IndexedSeq[Int]` as follows:

```scala sc:nocompile
"[23, 56, 71]" match
  case r"[${r"$xs%d"}...(, )]" => xs.sum // 150
```

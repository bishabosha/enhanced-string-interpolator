# enhanced-string-interpolator
![CI status](https://github.com/bishabosha/enhanced-string-interpolator/actions/workflows/ci.yml/badge.svg)
[![javadoc](https://javadoc.io/badge2/io.github.bishabosha/enhanced-string-interpolator_3/latest_documentation.svg)](https://javadoc.io/doc/io.github.bishabosha/enhanced-string-interpolator_3)

An enhanced glob interpolator for Scala with format strings

read full usage [in the documentation](https://javadoc.io/doc/io.github.bishabosha/enhanced-string-interpolator_3), or the original inspiration in my [blog post](https://bishabosha.github.io/articles/simple-parsing-with-strings.html)

## Basic usage

```scala
import stringmatching.regex.Interpolators.r

"[23, 56, 71]" match
  case r"[${r"$xs%d"}...(, )]" => xs.sum // 150
```

Below is a table of all the useful format strings:

| Format                     | Binding            | Note                  |
|----------------------------|--------------------|-----------------------|
| `$foo`                     | `foo: String`      |                       |
| `$foo%d`                   | `foo: Int`         |                       |
| `$foo%L`                   | `foo: Long`        |                       |
| `$foo%f`                   | `foo: Float`       |                       |
| `$foo%g`                   | `foo: Double`      |                       |
| `$foo...(<regex>)`         | `foo: Seq[String]` | split by regex        |
| `$foo..!(<regex>)`         | `foo: Seq[String]` | drops first if empty  |
| `${r"$foo%d"}...(<regex>)` | `foo: Seq[Int]`    | match on each element |
| `${r"$foo%L"}...(<regex>)` | `foo: Seq[Long]`   | match on each element |
| `${r"$foo%f"}...(<regex>)` | `foo: Seq[Float]`  | match on each element |
| `${r"$foo%g"}...(<regex>)` | `foo: Seq[Double]` | match on each element |

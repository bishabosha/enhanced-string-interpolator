# enhanced-string-interpolator
An enhanced glob interpolator for Scala with format strings

read more [in the blog](https://bishabosha.github.io/articles/simple-parsing-with-strings.html)

## Basic usage

```scala
import regexglob.RegexGlobbing.r

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

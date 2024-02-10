# Enhanced String Interpolator

_You can read some background motivation for this project in my [blog post](https://bishabosha.github.io/articles/simple-parsing-with-strings.html)._

## Basic usage

Currently all functionality is accessed with one import:
```scala
import stringmatching.regex.Interpolators.r
```

### Example 1

A simple example can be illustrated here, where we parse some basic text representing a sequence of integers, delimited by `"["` and `"]"`, and separated by `", "`.
In one pattern, you can extract a typed value `xs: IndexedSeq[Int]` as follows:

```scala sc:nocompile
"[23, 56, 71]" match
  case r"[${r"$xs%d"}...(, )]" => xs.sum // 150
```

The get an intuition for how `r` works, it works like the [simple string matcher](https://www.scala-lang.org/api/3.3.1/scala/StringContext$s$.html#unapplySeq-fffffd22) from the standard library (i.e. `case s"$foo $bar"`), except that after each splice you can append an optional format string, such as `%d`.

### Example 2

The format pattern is stripped before matching against the string, meaning that it only provides a directive of how to interpret the splice.

To illustrate, take the following example:

```scala sc:nocompile
"age: 23, year: 2019" match
  case r"age: $n%d, year: $y%d" => n + y
```

The two `%d` format strings will be removed before matching, and tell the interpolator to treat `n` and `d` as integer patterns. This means that the behavior of above snippet is equivalent to the following:

```scala sc:nocompile
"age: 23, year: 2019" match
  case str @ s"age: $n0, year: $y0" =>
    (n0.toIntOption, y0.toIntOption) match
      case (Some(n), Some(y)) => n + y
      _                       => throw MatchError(str)
```

## Possible Formats

### String Pattern

e.g. `$foo`, which extracts `val foo: String`.

### Int Pattern

e.g. `$foo%d`, which extracts `val foo: Int`.

### Long Pattern

e.g. `$foo%L`, which extracts `val foo: Long`.

### Float Pattern

e.g. `$foo%f`, which extracts `val foo: Float`.

### Double Pattern

e.g. `$foo%g`, which extracts `val foo: Double`.

### Split Pattern

e.g. `$foo...(<regex>)`, which extracts `val foo: IndexedSeq[String]`.

This is equivalent to extracting with `$foo` and then performing`foo.split(raw"<regex>").toIndexedSeq`.

This means that inside the `<regex>` you may put any valid regex accepted by `scala.util.matching.Regex`.
String escape characters are also not processed within the regex.

There is also a special case where if the first element of the sequence is expected to be empty you can drop it with the `$foo..!(<regex>)` pattern.


Putting this all together, you could split Windows style paths with the following pattern:

```scala sc:nocompile
raw"C:\foo\bar\baz.pdf" match
  case r"C:$elems..!(\\)" => elems.mkString("/")
// yields "foo/bar/baz.pdf"
```

### Nested Patterns

The `r` interpolator can also match on `Seq` of strings, arbitrarily nested.

For example

```scala sc:nocompile
val strings: Seq[String] = ???

val foo: Seq[Int] = strings match
  case r"$foo%d" => foo
```

or even

```scala sc:nocompile
val stringss: Seq[Seq[String]] = ???

val foo: Seq[Seq[Int]] = stringss match
  case r"$foo%d" => foo
```

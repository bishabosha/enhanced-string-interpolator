package stringmatching.regex

import scala.annotation.compileTimeOnly

object Interpolators:

  /** enum with tokens representing format strings for `PatternElement.Format`. */
  enum FormatPattern:
    case AsInt
    case AsLong
    case AsDouble
    case AsFloat
  end FormatPattern

  /** enum representing all possible patterns described by the `r` interpolator. */
  enum PatternElement:
    case Glob(pattern: String)
    case Split(splitOn: String, pattern: String)
    case SplitEmpty(splitOn: String, pattern: String)
    case Format(format: FormatPattern, pattern: String)
  end PatternElement

  /** Holder for the pattern elements described by a string interpolated with `r`. */
  enum Pattern:
    case Literal(glob: String)
    case Single(glob: String, pattern: PatternElement)
    case Multiple(glob: String, patterns: Seq[PatternElement])

  extension (inline sc: StringContext)
    /** use in patterns like `case r"$foo...(, )" => println(foo)` */
    transparent inline def r: RSStringContext[Any] = ${ Macros.rsApplyExpr('sc) }

  extension [R](inline rsSC: RSStringContext[R])
    /** enables compile time splitting of string globs with the syntax `r"$foo...(, )"`. can be
      * arbitrarily nested, e.g. `r"${r"$foos...(, )"}...(; )"`.
      * ```
      * case r"Foo $id: $bars...(, )" => (id, bars)
      * ```
      * is equivalent to
      * ```
      * case s"Foo $id: $bars0" => (id, bars0.split(", ").toIndexedSeq)
      * ```
      */
    transparent inline def unapply[Base](scrutinee: Base): Any =
      ${ Macros.rsUnapplyExpr('rsSC, 'scrutinee) }

  /** An intermediate representation that stores the format described by an interpolated string. */
  @annotation.nowarn("msg=unused explicit parameter")
  @compileTimeOnly("should be eliminated by `r` pattern interpolator")
  class RSStringContext[+R](pattern: Pattern) // used in macro extraction

end Interpolators

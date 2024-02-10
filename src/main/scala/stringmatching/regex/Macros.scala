package stringmatching.regex

import quoted.*

import scala.annotation.tailrec

import Interpolators.{RSStringContext, Pattern, PatternElement, FormatPattern}

object Macros:

  import Reify.given

  /** Parse the `StringContext` into a `RSStringContext` */
  def rsApplyExpr(rsSCExpr: Expr[StringContext])(using Quotes): Expr[RSStringContext[Any]] =
    val pattern         = parsed(rsSCExpr)
    val patternExpr     = Expr(pattern)
    val patternTypeRepr = refineResult(pattern)
    patternTypeRepr.asType match
      case '[t] =>
        '{ new RSStringContext[t]($patternExpr) }
  end rsApplyExpr

  /** Process a `RSStringContext` into a well-typed call to
    * [[stringmatching.regex.Runtime.unsafeExtract]]
    */
  def rsUnapplyExpr[R: Type, Base: Type](
      rsSCExpr: Expr[RSStringContext[R]],
      scrutinee: Expr[Base]
    )(using Quotes
    ): Expr[Any] =
    import quotes.reflect.*
    val '{ new RSStringContext[R]($patternExpr: Pattern) } = rsSCExpr: @unchecked
    val levels                                             = wrapping[Base]
    val returnType                                         = wrapResult[R](levels)
    val levelsExpr                                         = Expr(levels)
    returnType match
      case '[t] =>
        '{
          Runtime.unsafeExtract[Base, t]($patternExpr, levels = $levelsExpr)($scrutinee)
        }
    end match
  end rsUnapplyExpr

  private object Reify:

    given PatternToExpr: ToExpr[Pattern] with
      import Pattern.*

      def apply(pattern: Pattern)(using Quotes): Expr[Pattern] = pattern match
        case Literal(glob) =>
          '{ Literal(${ Expr(glob) }) }
        case Single(glob, pattern) =>
          '{ Single(${ Expr(glob) }, ${ Expr(pattern) }) }
        case Multiple(glob, patterns) =>
          '{ Multiple(${ Expr(glob) }, ${ Expr.ofSeq(patterns.map(Expr(_))) }) }
    end PatternToExpr

    given FormatPatternToExpr: ToExpr[FormatPattern] with
      import FormatPattern.*

      def apply(formatPattern: FormatPattern)(using Quotes): Expr[FormatPattern] =
        formatPattern match
          case AsInt    => '{ AsInt }
          case AsLong   => '{ AsLong }
          case AsDouble => '{ AsDouble }
          case AsFloat  => '{ AsFloat }
    end FormatPatternToExpr

    given PatternElementToExpr: ToExpr[PatternElement] with
      import PatternElement.*

      def apply(patternElement: PatternElement)(using Quotes): Expr[PatternElement] =
        patternElement match
          case Glob(pattern) =>
            '{ Glob(${ Expr(pattern) }) }
          case Split(splitOn, pattern) =>
            '{ Split(${ Expr(splitOn) }, ${ Expr(pattern) }) }
          case SplitEmpty(splitOn, pattern) =>
            '{ SplitEmpty(${ Expr(splitOn) }, ${ Expr(pattern) }) }
          case Format(format, pattern) =>
            '{ Format(${ Expr(format) }, ${ Expr(pattern) }) }
    end PatternElementToExpr
  end Reify

  private def parsed(scExpr: Expr[StringContext])(using Quotes): Pattern =
    import quotes.reflect.*
    val sc: StringContext = scExpr.valueOrAbort
    val parts             = sc.parts
    val g +: rest         = parts: @unchecked
    g match
      case s"...($rest0" =>
        val _ = parseRegex(g, rest0) // check for unbalanced parentheses
        report.errorAndAbort(s"split is not allowed without preceding splice: $g")
      case s"..!($rest0" =>
        val _ = parseRegex(g, rest0) // check for unbalanced parentheses
        report.errorAndAbort(s"split is not allowed without preceding splice: $g")
      case s"%$format" =>
        report.errorAndAbort(
          s"format `%$format` is not allowed without preceding splice: $g"
        )
      case _ =>
    end match

    def globPattern(rest: String): String =
      StringContext.processEscapes(rest)

    def parseRegex(elem: String, s: String): (String, String) =
      @tailrec
      def loop(i: Int, levels: Int): (String, String) =
        if i == s.length then
          report.errorAndAbort(
            s"unbalanced parentheses when parsing regex from format string: $"$elem$""
          )
        else
          val c = s(i)
          if c == '(' then loop(i + 1, levels + 1)
          else if c == ')' then
            if levels == 1 then (s.take(i), s.drop(i + 1))
            else loop(i + 1, levels - 1)
          else loop(i + 1, levels)
          end if
      loop(0, levels = 1)
    end parseRegex

    val rest0 = rest.map:
      case elem @ s"...($rest1" =>
        val (regex, rest) = parseRegex(elem, rest1)
        if rest.indexOf("...") > 0 || rest.indexOf("..!") > 0 then
          report.errorAndAbort(
            s"split is not allowed without preceding splice: $rest"
          )
        PatternElement.Split(regex, globPattern(rest))
      case elem @ s"..!($rest1" =>
        val (regex, rest) = parseRegex(elem, rest1)
        if rest.indexOf("...") > 0 || rest.indexOf("..!") > 0 then
          report.errorAndAbort(
            s"split is not allowed without preceding splice: $rest"
          )
        PatternElement.SplitEmpty(regex, globPattern(rest))
      case s"...$rest" =>
        report.errorAndAbort(
          s"split `$$foo...` is not allowed without qualifying regex e.g. `$$foo...(,)`: $rest"
        )
      case s"..!$rest" =>
        report.errorAndAbort(
          s"split `$$foo..!` is not allowed without qualifying regex e.g. `$$foo..!(,)`: $rest"
        )
      case s"%$format" =>
        format.headOption match
          case Some('d') => PatternElement.Format(FormatPattern.AsInt, format.tail)
          case Some('L') => PatternElement.Format(FormatPattern.AsLong, format.tail)
          case Some('f') => PatternElement.Format(FormatPattern.AsFloat, format.tail)
          case Some('g') => PatternElement.Format(FormatPattern.AsDouble, format.tail)
          case _         => report.errorAndAbort(s"unsupported format: `%$format`")
      case rest =>
        PatternElement.Glob(globPattern(rest))

    val g0 = globPattern(g)

    if rest0.isEmpty then Pattern.Literal(g0)
    else if rest0.sizeIs == 1 then Pattern.Single(g0, rest0.head)
    else Pattern.Multiple(g0, rest0)
  end parsed

  private def refineResult(pattern: Pattern)(using Quotes): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    def typeOfPattern(element: PatternElement) = element match
      case PatternElement.Glob(_)          => TypeRepr.of[String]
      case PatternElement.Split(_, _)      => TypeRepr.of[IndexedSeq[String]]
      case PatternElement.SplitEmpty(_, _) => TypeRepr.of[IndexedSeq[String]]
      case PatternElement.Format(format, _) =>
        format match
          case FormatPattern.AsInt    => TypeRepr.of[Int]
          case FormatPattern.AsLong   => TypeRepr.of[Long]
          case FormatPattern.AsDouble => TypeRepr.of[Double]
          case FormatPattern.AsFloat  => TypeRepr.of[Float]

    pattern match
      case Pattern.Literal(_)         => TypeRepr.of[EmptyTuple]
      case Pattern.Single(_, pattern) => typeOfPattern(pattern)
      case Pattern.Multiple(_, elements) =>
        val args = elements.map(typeOfPattern)
        if args.size <= 22 then AppliedType(defn.TupleClass(args.size).typeRef, args.toList)
        else
          report.errorAndAbort(
            s"too many captures: ${args.size} (implementation restriction: max 22)"
          )
        end if
    end match
  end refineResult

  private def wrapping[Base: Type](using Quotes): Int =
    import quotes.reflect.*
    Type.of[Base] match
      case '[String]        => 0
      case '[IndexedSeq[t]] => wrapping[t] + 1
      case _                => report.errorAndAbort(s"unsupported type: ${TypeRepr.of[Base]}")
  end wrapping

  private def wrapResult[R: Type](times: Int)(using Quotes): Type[?] =
    import quotes.reflect.*
    val args = wrapElems[R](times)
    if args.size == 0 then Type.of[Boolean]
    else
      val res0 =
        if args.size == 1 then args.head
        else if args.size <= 22 then
          AppliedType(
            defn.TupleClass(args.size).typeRef,
            args.toList.map({ case '[t] => TypeRepr.of[t] })
          ).asType
        else
          report.errorAndAbort(
            s"too many captures: ${args.size} (implementation restriction: max 22)"
          )
      res0 match
        case '[t] => Type.of[Option[t]]
    end if
  end wrapResult

  private def wrapElem[Elem: Type](times: Int)(using Quotes): Type[?] =
    times match
      case 0 => Type.of[Elem]
      case n =>
        wrapElem[Elem](n - 1) match
          case '[t] => Type.of[IndexedSeq[t]]

  private def wrapElems[R: Type](times: Int)(using Quotes): List[Type[?]] =
    import quotes.reflect.*
    Type.of[R] match
      case '[t *: ts] =>
        wrapElem[t](times) :: wrapElems[ts](times)
      case '[EmptyTuple] => Nil
      case '[singleton]  => wrapElem[singleton](times) :: Nil
      case _             => report.errorAndAbort(s"unsupported type: ${TypeRepr.of[R]}")
    end match
  end wrapElems

end Macros

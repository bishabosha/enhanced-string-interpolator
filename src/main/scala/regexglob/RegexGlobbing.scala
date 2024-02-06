package regexglob

import quoted.*
import scala.annotation.compileTimeOnly
import collection.immutable
import collection.mutable
import scala.util.boundary, boundary.break
import scala.collection.immutable.ArraySeq
import scala.annotation.tailrec

object RegexGlobbing:
  extension (inline sc: StringContext)
    /** use in patterns like `case r"$foo...(, )" =>` */
    @compileTimeOnly("should be used with `r` pattern interpolator")
    transparent inline def r: RSStringContext[Any] = ${ rsApplyExpr('sc) }

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
      ${ rsUnapplyExpr('rsSC, 'scrutinee) }

  @annotation.nowarn("msg=unused explicit parameter")
  class RSStringContext[+Base](pattern: Pattern) // used in macro extraction

  enum FormatPattern:
    case AsInt
    case AsLong
    case AsDouble
    case AsFloat
  end FormatPattern

  object FormatPattern:
    given ToExpr[FormatPattern] with
      def apply(formatPattern: FormatPattern)(using Quotes): Expr[FormatPattern] =
        formatPattern match
          case AsInt    => '{ AsInt }
          case AsLong   => '{ AsLong }
          case AsDouble => '{ AsDouble }
          case AsFloat  => '{ AsFloat }
    end given
  end FormatPattern

  enum PatternElement:
    case Glob(pattern: String)
    case Split(splitOn: String, pattern: String)
    case SplitEmpty(splitOn: String, pattern: String)
    case Format(format: FormatPattern, pattern: String)
  end PatternElement

  object PatternElement:
    given ToExpr[PatternElement] with
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
    end given
  end PatternElement

  case class PatternLive(elements: Seq[PatternElement], levels: Int):
    private class ArraySeqBuilderProduct(
        arr: Array[mutable.Builder[AnyRef, immutable.ArraySeq[AnyRef]]])
        extends Product:
      def canEqual(that: Any): Boolean = true
      def productArity: Int            = arr.length
      def productElement(n: Int): Any  = arr(n).result()
      override def toString: String    = arr.mkString("ArraySeqBuilderProduct(...)")
    end ArraySeqBuilderProduct

    private def unapply0(scrutinee: String, slots: Int): Option[Any] | Boolean =
      def foldGlobs(acc: Seq[String], self: PatternElement): Seq[String] =
        self match
          case PatternElement.Glob(pattern)                => acc :+ pattern
          case PatternElement.Split(_, pattern)            => acc :+ pattern
          case PatternElement.SplitEmpty(splitOn, pattern) => acc :+ pattern
          case PatternElement.Format(_, pattern)           => acc :+ pattern
      val globs = elements.foldLeft(Vector.empty[String]: Seq[String])(foldGlobs)
      StringContext.glob(globs, scrutinee) match
        case None =>
          if slots == 0 then false else None
        case Some(stage1) =>
          if slots == 0 then true
          else
            def process(globbed: String, self: PatternElement): String | Seq[String] | Option[Any] =
              self match
                case PatternElement.Glob(_)           => globbed
                case PatternElement.Split(splitOn, _) => globbed.split(splitOn).toIndexedSeq
                case PatternElement.SplitEmpty(splitOn, _) =>
                  val res0 = globbed.split(splitOn)
                  if res0.isEmpty then ArraySeq.empty[String]
                  else if res0(0).isEmpty then res0.toIndexedSeq.tail
                  else res0.toIndexedSeq
                case PatternElement.Format(format, _) =>
                  format match
                    case FormatPattern.AsInt    => globbed.toIntOption
                    case FormatPattern.AsLong   => globbed.toLongOption
                    case FormatPattern.AsDouble => globbed.toDoubleOption
                    case FormatPattern.AsFloat  => globbed.toFloatOption
            if slots == 1 then
              process(stage1.head, elements.last) match
                case opt: Option[Any] => opt // from format
                case other            => Some(other)
            else
              boundary:
                val state = new Array[AnyRef](slots)
                stage1
                  .lazyZip(elements.drop(1))
                  .lazyZip(0 until slots)
                  .foreach: (globbed, element, index) =>
                    process(globbed, element) match
                      case None => break(None) // format failed
                      case Some(value) =>
                        state(index) = value.asInstanceOf[AnyRef] // format succeeded
                      case value => state(index) = value.asInstanceOf[AnyRef] // no format
                Some(Tuple.fromArray(state))
            end if
      end match
    end unapply0

    private def unapplyN(scrutinee: Any, slots: Int, level: Int): Option[Any] | Boolean =
      level match
        case 0 => unapply0(scrutinee.asInstanceOf[String], slots)
        case i =>
          val stageN1 = scrutinee.asInstanceOf[Seq[Any]].map(unapplyN(_, slots, i - 1))
          if slots == 0 then stageN1.asInstanceOf[Seq[Boolean]].forall(identity)
          else
            val stageN1Refined = stageN1.asInstanceOf[Seq[Option[Any]]]
            if stageN1Refined.forall(_.isDefined) then
              if slots == 1 then Some(stageN1Refined.map(_.get))
              else
                val state = Array.fill(slots)(immutable.ArraySeq.newBuilder[AnyRef])
                stageN1Refined.foreach: res =>
                  val tup = res.get.asInstanceOf[Tuple]
                  var idx = 0
                  while idx < slots do
                    state(idx) += tup.productElement(idx).asInstanceOf[AnyRef]
                    idx += 1
                Some(Tuple.fromProduct(new ArraySeqBuilderProduct(state)))
            else None
            end if
          end if

    def unapply[Base](scrutinee: Base): Option[Any] | Boolean =
      unapplyN(scrutinee, slots = elements.size - 1, levels)
  end PatternLive

  case class Pattern(elements: Seq[PatternElement])

  object Pattern:
    given ToExpr[Pattern] with
      def apply(pattern: Pattern)(using Quotes): Expr[Pattern] =
        '{ Pattern(${ Expr.ofSeq(pattern.elements.map(Expr(_))) }) }
  end Pattern

  def parsed(scExpr: Expr[StringContext])(using Quotes): Pattern =
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
    Pattern(PatternElement.Glob(globPattern(g)) +: rest0)
  end parsed

  def refineResult(pattern: Pattern)(using Quotes): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    val args = pattern.elements
      .drop(1)
      .map:
        case PatternElement.Glob(_)          => TypeRepr.of[String]
        case PatternElement.Split(_, _)      => TypeRepr.of[IndexedSeq[String]]
        case PatternElement.SplitEmpty(_, _) => TypeRepr.of[IndexedSeq[String]]
        case PatternElement.Format(format, _) =>
          format match
            case FormatPattern.AsInt    => TypeRepr.of[Int]
            case FormatPattern.AsLong   => TypeRepr.of[Long]
            case FormatPattern.AsDouble => TypeRepr.of[Double]
            case FormatPattern.AsFloat  => TypeRepr.of[Float]
    if args.size == 0 then TypeRepr.of[EmptyTuple]
    else if args.size == 1 then args.head
    else if args.size <= 22 then AppliedType(defn.TupleClass(args.size).typeRef, args.toList)
    else
      report.errorAndAbort(s"too many captures: ${args.size} (implementation restriction: max 22)")
    end if
  end refineResult

  def rsApplyExpr(rsSCExpr: Expr[StringContext])(using Quotes): Expr[RSStringContext[Any]] =
    val pattern         = parsed(rsSCExpr)
    val patternExpr     = Expr(pattern)
    val patternTypeRepr = refineResult(pattern)
    patternTypeRepr.asType match
      case '[t] =>
        '{ new RSStringContext[t]($patternExpr) }
  end rsApplyExpr

  def wrapping[Base: Type](using Quotes): Int =
    import quotes.reflect.*
    Type.of[Base] match
      case '[String]        => 0
      case '[IndexedSeq[t]] => wrapping[t] + 1
      case _                => report.errorAndAbort(s"unsupported type: ${TypeRepr.of[Base]}")
  end wrapping

  def wrapResult[R: Type](times: Int)(using Quotes): Type[?] =
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

  def wrapElem[Elem: Type](times: Int)(using Quotes): Type[?] =
    times match
      case 0 => Type.of[Elem]
      case n =>
        wrapElem[Elem](n - 1) match
          case '[t] => Type.of[IndexedSeq[t]]

  def wrapElems[R: Type](times: Int)(using Quotes): List[Type[?]] =
    import quotes.reflect.*
    Type.of[R] match
      case '[t *: ts] =>
        wrapElem[t](times) :: wrapElems[ts](times)
      case '[EmptyTuple] => Nil
      case '[singleton]  => wrapElem[singleton](times) :: Nil
      case _             => report.errorAndAbort(s"unsupported type: ${TypeRepr.of[R]}")
    end match
  end wrapElems

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
          PatternLive($patternExpr.elements, levels = $levelsExpr)
            .unapply($scrutinee)
            .asInstanceOf[t]
        }
    end match
  end rsUnapplyExpr
end RegexGlobbing

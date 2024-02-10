package stringmatching.regex

import collection.immutable.ArraySeq
import collection.mutable
import scala.util.boundary, boundary.break

import Interpolators.{Pattern, PatternElement, FormatPattern}

object Runtime:

  /** Type-erased extractor method, returning a value corresponding to the pattern and levels. The
    * caller is responsible for ensuring that `value` corresponds to the number of levels, i.e. A
    * `String` for 0 levels, a `Seq[String]` for 1 level, a `Seq[Seq[String]]` for 2 levels, etc.
    * The caller is also responsible for ensuring that `Out` corresponds to the pattern, wrapped in
    * `Seq` as many times as `levels`.
    */
  def unsafeExtract[In, Out](pattern: Pattern, levels: Int)(value: In): Out =
    pattern match
      case Pattern.Literal(glob) =>
        PatternLive0.extract(glob, value, levels).asInstanceOf[Out]
      case Pattern.Single(glob, pattern) =>
        PatternLive1.extract(glob, pattern, value, levels).asInstanceOf[Out]
      case Pattern.Multiple(glob, elements) =>
        PatternLiveN.extract(glob, elements, value, levels).asInstanceOf[Out]

  private object PatternLive0:

    private def unapply0(glob: String, scrutinee: String): Boolean =
      StringContext.glob(Seq(glob), scrutinee).isDefined

    private def unapplyN(glob: String, scrutinee: Any, level: Int): Boolean =
      level match
        case 0 => unapply0(glob, scrutinee.asInstanceOf[String])
        case i =>
          val stageN1 = scrutinee.asInstanceOf[Seq[Any]].map(unapplyN(glob, _, i - 1))
          stageN1.asInstanceOf[Seq[Boolean]].forall(identity)

    def extract[Base](glob: String, scrutinee: Base, levels: Int): Boolean =
      unapplyN(glob, scrutinee, levels)
  end PatternLive0

  private def foldGlobs(acc: Seq[String], self: PatternElement): Seq[String] =
    self match
      case PatternElement.Glob(pattern)                => acc :+ pattern
      case PatternElement.Split(_, pattern)            => acc :+ pattern
      case PatternElement.SplitEmpty(splitOn, pattern) => acc :+ pattern
      case PatternElement.Format(_, pattern)           => acc :+ pattern

  private def process(globbed: String, self: PatternElement): String | Seq[String] | Option[Any] =
    import scala.language.unsafeNulls
    self match
      case PatternElement.Glob(_) => globbed
      case PatternElement.Split(splitOn, _) =>
        globbed.split(splitOn).toIndexedSeq
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
    end match
  end process

  private object PatternLive1:
    private def unapply0(glob: String, pattern: PatternElement, scrutinee: String): Option[Any] =
      val globs = foldGlobs(Vector(glob), pattern)
      StringContext.glob(globs, scrutinee) match
        case None => None
        case Some(stage1) =>
          process(stage1.head, pattern) match
            case opt: Option[Any] => opt // from format
            case other            => Some(other)
      end match
    end unapply0

    private def unapplyN(glob: String, pattern: PatternElement, scrutinee: Any, level: Int)
        : Option[Any] =
      level match
        case 0 => unapply0(glob, pattern, scrutinee.asInstanceOf[String])
        case i =>
          val stageN1 = scrutinee.asInstanceOf[Seq[Any]].map(unapplyN(glob, pattern, _, i - 1))
          val stageN1Refined = stageN1.asInstanceOf[Seq[Option[Any]]]
          if stageN1Refined.forall(_.isDefined) then Some(stageN1Refined.map(_.get))
          else None

    def extract[Base](glob: String, pattern: PatternElement, scrutinee: Base, levels: Int)
        : Option[Any] =
      unapplyN(glob, pattern, scrutinee, levels)
  end PatternLive1

  private object PatternLiveN:

    private class ArraySeqBuilderProduct(
        arr: Array[mutable.Builder[AnyRef, ArraySeq[AnyRef]]])
        extends Product:
      def canEqual(that: Any): Boolean = true
      def productArity: Int            = arr.length
      def productElement(n: Int): Any  = arr(n).result()
      override def toString: String    = arr.mkString("ArraySeqBuilderProduct(...)")
    end ArraySeqBuilderProduct

    private def unapply0(glob: String, elements: Seq[PatternElement], scrutinee: String)
        : Option[Any] =
      val globs = elements.foldLeft(Vector(glob))(foldGlobs)
      StringContext.glob(globs, scrutinee) match
        case None => None
        case Some(stage1) =>
          boundary:
            var _state: Array[AnyRef] | Null = null
            def state: Array[AnyRef] =
              val read = _state
              if read == null then
                val init = new Array[AnyRef](elements.size)
                try init
                finally _state = init
              else read
              end if
            end state
            stage1
              .lazyZip(elements)
              .lazyZip(0 until elements.size)
              .foreach: (globbed, element, index) =>
                process(globbed, element) match
                  case None => break(None) // format failed
                  case Some(value) =>
                    state(index) = value.asInstanceOf[AnyRef] // format succeeded
                  case value => state(index) = value.asInstanceOf[AnyRef] // no format
            Some(Tuple.fromArray(state))
      end match
    end unapply0

    private def unapplyN(glob: String, elements: Seq[PatternElement], scrutinee: Any, level: Int)
        : Option[Any] =
      level match
        case 0 => unapply0(glob, elements, scrutinee.asInstanceOf[String])
        case i =>
          val stageN1 = scrutinee.asInstanceOf[Seq[Any]].map(unapplyN(glob, elements, _, i - 1))
          val stageN1Refined = stageN1.asInstanceOf[Seq[Option[Any]]]
          if stageN1Refined.forall(_.isDefined) then
            val state = Array.fill(elements.size)(ArraySeq.newBuilder[AnyRef])
            stageN1Refined.foreach: res =>
              val tup = res.get.asInstanceOf[Tuple]
              var idx = 0
              while idx < elements.size do
                state(idx) += tup.productElement(idx).asInstanceOf[AnyRef]
                idx += 1
            Some(Tuple.fromProduct(new ArraySeqBuilderProduct(state)))
          else None
          end if

    def extract[Base](glob: String, elements: Seq[PatternElement], scrutinee: Base, levels: Int)
        : Option[Any] =
      unapplyN(glob, elements, scrutinee, levels)
  end PatternLiveN

end Runtime

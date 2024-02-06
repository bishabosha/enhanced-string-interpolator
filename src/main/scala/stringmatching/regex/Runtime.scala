package stringmatching.regex

import collection.immutable.ArraySeq
import collection.mutable
import scala.util.boundary, boundary.break

import Interpolators.{PatternElement, FormatPattern}

object Runtime:

  import PatternLive.*

  /** Type-erased extractor method, returning a value corresponding to the elements and levels. The
    * caller is responsible for ensuring that `value` corresponds to the number of levels, i.e. A
    * `String` for 0 levels, a `Seq[String]` for 1 level, a `Seq[Seq[String]]` for 2 levels, etc.
    * The caller is also responsible for ensuring that `Out` corresponds to the pattern elements,
    * wrapped in Seq as many times as `levels`.
    */
  def extract[In, Out](elements: Seq[PatternElement], levels: Int)(value: In): Out =
    PatternLive(elements, levels).unapply(value).asInstanceOf[Out]

  private object PatternLive:
    private class ArraySeqBuilderProduct(
        arr: Array[mutable.Builder[AnyRef, ArraySeq[AnyRef]]])
        extends Product:
      def canEqual(that: Any): Boolean = true
      def productArity: Int            = arr.length
      def productElement(n: Int): Any  = arr(n).result()
      override def toString: String    = arr.mkString("ArraySeqBuilderProduct(...)")
    end ArraySeqBuilderProduct
  end PatternLive

  private case class PatternLive(elements: Seq[PatternElement], levels: Int):

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
                val state = Array.fill(slots)(ArraySeq.newBuilder[AnyRef])
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

    /** */
    def unapply[Base](scrutinee: Base): Option[Any] | Boolean =
      unapplyN(scrutinee, slots = elements.size - 1, levels)
  end PatternLive

end Runtime

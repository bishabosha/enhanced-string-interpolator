package regexglob

import RegexGlobbing.r

class ResultTypesSuite extends munit.FunSuite:

  test("no splices"):
    "foo" match
      case r"foo" => ()
      case _      => fail("no match")

  test("globbing match"):
    "foo bar" match
      case r"$foo $bar" =>
        val foo1: String = foo
        val bar1: String = bar
        assert(foo1 == "foo")
        assert(bar1 == "bar")
      case _ => fail("no match")

  test("basic string format"):
    "foo" match
      case r"$foo" =>
        val foo1: String = foo
        assert(foo1 == "foo")
      case _ => fail("no match")

  test("basic int format"):
    "12345" match
      case r"$foo%d" =>
        val foo1: Int = foo
        assert(foo1 == 12345)
      case _ => fail("no match")

  test("basic long format"):
    "12345" match
      case r"$foo%L" =>
        val foo1: Long = foo
        assert(foo1 == 12345L)
      case _ => fail("no match")

  test("basic float format"):
    "0.03125" match
      case r"$foo%f" =>
        val foo1: Float = foo
        assert(foo1 == 0.03125f)
      case _ => fail("no match")

  test("basic double format"):
    "0.03125" match
      case r"$foo%g" =>
        val foo1: Double = foo
        assert(foo1 == 0.03125)
      case _ => fail("no match")

  test("basic seq format"):
    "a,b,c,d" match
      case r"$cs...(,)" =>
        val cs1: Seq[String] = cs
        val Seq(a, b, c, d)  = cs1: @unchecked
        assert(a == "a")
        assert(b == "b")
        assert(c == "c")
        assert(d == "d")
      case _ => fail("no match")

  test("basic seq-tail format"):
    ",b,c,d" match
      case r"$cs..!(,)" =>
        val cs1: Seq[String] = cs
        val Seq(b, c, d)     = cs1: @unchecked
        assert(b == "b")
        assert(c == "c")
        assert(d == "d")
      case _ => fail("no match")

  test("basic nested seq format"):
    "a:w,b:x,c:y,d:z" match
      case r"${r"$css...(:)"}...(,)" =>
        val css1: Seq[Seq[String]] = css
        val Seq(as, bs, cs, ds)    = css1: @unchecked
        assert(as == Seq("a", "w"))
        assert(bs == Seq("b", "x"))
        assert(cs == Seq("c", "y"))
        assert(ds == Seq("d", "z"))
      case _ => fail("no match")

  test("basic double nested seq format"):
    "a&1:w&2,b&3:x&4,c&5:y&6,d&7:z&8" match
      case r"${r"${r"$csss...(&)"}...(:)"}...(,)" =>
        val csss1: Seq[Seq[Seq[String]]] = csss
        val Seq(as, bs, cs, ds)          = csss1: @unchecked
        assert(as == Seq(Seq("a", "1"), Seq("w", "2")))
        assert(bs == Seq(Seq("b", "3"), Seq("x", "4")))
        assert(cs == Seq(Seq("c", "5"), Seq("y", "6")))
        assert(ds == Seq(Seq("d", "7"), Seq("z", "8")))
      case _ => fail("no match")

  test("int long float double format"):
    "12345 12345 0.03125 0.03125" match
      case r"$a%d $b%L $c%f $d%g" =>
        val a1: Int    = a
        val b1: Long   = b
        val c1: Float  = c
        val d1: Double = d
        assert(a1 == 12345)
        assert(b1 == 12345L)
        assert(c1 == 0.03125f)
        assert(d1 == 0.03125)
      case _ => fail("no match")
end ResultTypesSuite

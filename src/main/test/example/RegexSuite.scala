package example

import stringmatching.regex.Interpolators.r

import scala.compiletime.testing.typeCheckErrors

class RegexSuite extends munit.FunSuite:

  test("regex or"):
    "-O-X-O-" match
      case r"$os...(O|X)" =>
        assertEquals(os, Seq("-", "-", "-", "-"))
      case _ => fail("no match")

  test("regex or with capture"):
    "-O-X-O-" match
      case r"$os...((O|X))" =>
        assertEquals(os, Seq("-", "-", "-", "-"))
      case _ => fail("no match")

  test("regex capture imbalanced"):
    val errors = typeCheckErrors("""
      "-O-X-O-" match
        case r"$os...(((O|X))" => // unbalanced parentheses in regex
          assertEquals(os, Seq("-", "-", "-", "-"))
        case _ =>
          ()
    """)
    val queryMsg =
      """unbalanced parentheses when parsing regex from format string: "...(((O|X))""""
    assert(errors.exists(_.message.contains(queryMsg)))

  test("regex newline"):
    val in = """foo
      |bar
      |baz""".stripMargin
    in match
      case r"$lines...(\R)" => // no escapes in regex part
        assertEquals(lines, Seq("foo", "bar", "baz"))
      case _ => fail("no match")
end RegexSuite

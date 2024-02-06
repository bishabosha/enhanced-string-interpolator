package example

import stringmatching.regex.Interpolators.r

class ExampleSuite extends munit.FunSuite:

  test("sum json-like array"):
    val res = "[23, 56, 71]" match
      case r"[${r"$xs%d"}...(, )]" => xs.sum

    assertEquals(res, 150)
end ExampleSuite

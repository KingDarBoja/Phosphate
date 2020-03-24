import strutils
import unittest

import language/block_string

suite "Describe dedentBlockStringValue":

  test "Removes uniform indentation from a string":
    let rawValue = join([
      "",
      "    Hello,",
      "      World!",
      "",
      "    Yours,",
      "      GraphQL."
    ], "\n")
    let expectedValue = join([
      "Hello,", "  World!", "", "Yours,", "  GraphQL."
    ], "\n")
    check(dedentBlockStringValue(rawValue) == expectedValue)

  test "Removes empty leading and trailing lines":
    let rawValue = join([
      "",
      "",
      "    Hello,",
      "      World!",
      "",
      "    Yours,",
      "      GraphQL.",
      "",
      "",
    ], "\n")
    let expectedValue = join([
      "Hello,", "  World!", "", "Yours,", "  GraphQL."
    ], "\n")
    check(dedentBlockStringValue(rawValue) == expectedValue)

  test "Removes blank leading and trailing lines":
    let rawValue = join([
      "  ",
      "        ",
      "    Hello,",
      "      World!",
      "",
      "    Yours,",
      "      GraphQL.",
      "        ",
      "  ",
    ], "\n")
    let expectedValue = join([
      "Hello,", "  World!", "", "Yours,", "  GraphQL."
    ], "\n")
    check(dedentBlockStringValue(rawValue) == expectedValue)
  
  test "Retains indentation from first line":
    let rawValue = join([
      "    Hello,", "      World!", "", "    Yours,", "      GraphQL."
    ], "\n")
    let expectedValue = join([
      "    Hello,", "  World!", "", "Yours,", "  GraphQL."
    ], "\n")
    check(dedentBlockStringValue(rawValue) == expectedValue)
  
  test "Does not alter trailing spaces":
    let rawValue = join([
      "               ",
      "    Hello,     ",
      "      World!   ",
      "               ",
      "    Yours,     ",
      "      GraphQL. ",
      "               ",
    ], "\n")
    let expectedValue = join([
      "Hello,     ", "  World!   ", "           ", "Yours,     ", "  GraphQL. "
    ], "\n")
    check(dedentBlockStringValue(rawValue) == expectedValue)

suite "Describe getBlockStringIndentation":

  test "Returns zero for an empty list":
    check(getBlockStringIndentation(@[]) == 0)

  test "Do not take first line into account":
    check(getBlockStringIndentation(@["  a"]) == 0)
    check(getBlockStringIndentation(@[" a", "  b"]) == 2)

  test "Returns minimal indentation length":
    check(getBlockStringIndentation(@["", " a", "  b"]) == 1)
    check(getBlockStringIndentation(@["", "  a", " b"]) == 1)
    check(getBlockStringIndentation(@["", "  a", " b", "c"]) == 0)

  test "Count both tab and space as single character":
    check(getBlockStringIndentation(@["", "\ta", "          b"]) == 1)
    check(getBlockStringIndentation(@["", "\t a", "          b"]) == 2)
    check(getBlockStringIndentation(@["", " \t a", "          b"]) == 3)

  test "Do not take empty lines into account":
    check(getBlockStringIndentation(@["a", "\t"]) == 0)
    check(getBlockStringIndentation(@["a", " "]) == 0)
    check(getBlockStringIndentation(@["a", " ", "  b"]) == 2)
    check(getBlockStringIndentation(@["a", "", " b"]) == 1)

suite "Describe print block string":

  test "By default print block strings as single line":
    let ss = "one liner"
    check(printBlockString(ss) == "\"\"\"one liner\"\"\"")
    check(printBlockString(ss, "", true) == "\"\"\"\none liner\n\"\"\"")

  test "Correctly prints single line with leading space":
    let ss = "    space-led string"
    check(printBlockString(ss) == "\"\"\"    space-led string\"\"\"")
    check(printBlockString(ss, "", true) == "\"\"\"    space-led string\n\"\"\"")

  test "Correctly prints single line with leading space and quotation":
    let ss = "    space-led value \"quoted string\""
    check(printBlockString(ss) == "\"\"\"    space-led value \"quoted string\"\n\"\"\"")
    check(printBlockString(ss, "", true) == "\"\"\"    space-led value \"quoted string\"\n\"\"\"")

  test "Correctly prints string with a first line indentation":
    let ss = join(["    first  ", "  line     ", "indentation", "     string"], "\n")
    check(printBlockString(ss) == join([
      "\"\"\"", "    first  ", "  line     ", "indentation", "     string", "\"\"\""
    ], "\n"))
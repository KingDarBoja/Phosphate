import unittest
import strutils

import language/source_location
import language/print_location

import nimutils/dedent

suite "Describe Print Location":

  test "Prints minified documents":
    let
      minifiedSource = newSource(
        "query SomeMinifiedQueryWithErrorInside(" &
        "$foo:String!=FIRST_ERROR_HERE$bar:String)" &
        "{someField(foo:$foo bar:$bar baz:SECOND_ERROR_HERE)" &
        "{fieldA fieldB{fieldC fieldD...on THIRD_ERROR_HERE}}}"
      )
      firstLocation = printSourceLocation(
        minifiedSource,
        newSourceLocation(1, minifiedSource.body.find("FIRST_ERROR_HERE") + 1)
      )
      secondLocation = printSourceLocation(
        minifiedSource,
        newSourceLocation(1, minifiedSource.body.find("SECOND_ERROR_HERE") + 1)
      )
      thirdLocation = printSourceLocation(
        minifiedSource,
        newSourceLocation(1, minifiedSource.body.find("THIRD_ERROR_HERE") + 1)
      )
    
    check(firstLocation & "\n" == dedent(
      """
      GraphQL request:1:53
      1 | query SomeMinifiedQueryWithErrorInside($foo:String!=FIRST_ERROR_HERE$bar:String)
        |                                                     ^
        | {someField(foo:$foo bar:$bar baz:SECOND_ERROR_HERE){fieldA fieldB{fieldC fieldD.
      """
    ))

    check(secondLocation & "\n" == dedent(
      """
      GraphQL request:1:114
      1 | query SomeMinifiedQueryWithErrorInside($foo:String!=FIRST_ERROR_HERE$bar:String)
        | {someField(foo:$foo bar:$bar baz:SECOND_ERROR_HERE){fieldA fieldB{fieldC fieldD.
        |                                  ^
        | ..on THIRD_ERROR_HERE}}}
      """
    ))

    # TODO: this one is failing
    check(thirdLocation & "\n" == dedent(
      """
      GraphQL request:1:166
      1 | query SomeMinifiedQueryWithErrorInside($foo:String!=FIRST_ERROR_HERE$bar:String)
        | {someField(foo:$foo bar:$bar baz:SECOND_ERROR_HERE){fieldA fieldB{fieldC fieldD.
        | ..on THIRD_ERROR_HERE}}}
        |      ^
      """
    ))

  test "Prints single digit line number with no padding":
    let res = printSourceLocation(
      newSource("*", "Test", newSourceLocation(9, 1)),
      newSourceLocation(1, 1)
    )

    check(res & "\n" == dedent(
      """
      Test:9:1
      9 | *
        | ^
      """
    ))

  # TODO: this one is failing
  test "Prints line numbers with correct padding":
    let res = printSourceLocation(
      newSource("*\n", "Test", newSourceLocation(9, 1)),
      newSourceLocation(1, 1)
    )

    check(res & "\n" == dedent(
      """
      Test:9:1
       9 | *
         | ^
      10 |
      """
    ))
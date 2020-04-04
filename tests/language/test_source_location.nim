import unittest

import language/source_location

import nimutils/dedent

suite "Source Test Suite":

  test "Accepts body and name":
    let source = newSource("foo", "bar")
    check(source.body == "foo")
    check(source.name == "bar")

  test "Accepts locationOffset":
    let locationOffset = newSourceLocation(2, 3)
    let source = newSource("", "", locationOffset)
    check(source.locationOffset.line == 2)
    check(source.locationOffset.column == 3)

  test "Uses default arguments":
    let source = newSource("")
    check(source.name == "GraphQL request")
    check(source.locationOffset.line == 1)
    check(source.locationOffset.column == 1)

  test "Can get location":
    let body = dedent(
      """
      line 1
      line 2
      line 3
      """
    )
    let source = newSource(body)
    check(source.body == body)
    let location = source.getLocation(body.find('2'))
    check(location is SourceLocation)
    check(location.line == 2)
    check(location.column == 6)

  test "Rejects invalid location offset":
    proc createSource(line: int, column: int): Source =
      return newSource("", "", newSourceLocation(line, column))

    try:
      discard createSource(0, 1)
    except ValueError as e:
      check(e.msg == "line in locationOffset is 1-indexed and must be positive.")

    try:
      discard createSource(-1, 1)
    except ValueError as e:
      check(e.msg == "line in locationOffset is 1-indexed and must be positive.")

    try:
      discard createSource(1, 0)
    except ValueError as e:
      check(e.msg == "column in locationOffset is 1-indexed and must be positive.")

    try:
      discard createSource(1, -1)
    except ValueError as e:
      check(e.msg == "column in locationOffset is 1-indexed and must be positive.")

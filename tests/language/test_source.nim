import unittest

import language/source

suite "Source Test Suite":

  test "Accepts body and name":
    let source = initSource("foo", "bar")
    check(source.body == "foo")
    check(source.name == "bar")

  test "Accepts locationOffset":
    let locationOffset = initLocation(2, 3)
    let source = initSource("", "", locationOffset)
    check(source.locationOffset.line == 2)
    check(source.locationOffset.column == 3)

  test "Uses default arguments":
    let source = initSource("")
    check(source.name == "GraphQL request")
    check(source.locationOffset.line == 1)
    check(source.locationOffset.column == 1)

  test "Rejects invalid locattion offset":
    proc createSource(line: int, column: int): Source =
      return initSource("", "", initLocation(line, column))

    expect ValueError:
      discard createSource(-5, 6)
    expect ValueError:
      discard createSource(2, -1)
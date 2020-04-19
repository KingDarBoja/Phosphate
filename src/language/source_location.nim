## This file contains the code of two modules instead of one as opposed to the source code due to Nim's inability 
## (so far) to perform cyclic imports. In Python it's not a problem due to its dynamic typing nature 
## so to save me headaches in Nim, I preferred to join those modules.
import strutils, strformat, tables

## Location Module Section
type SourceLocation* = ref object
  #[
    Represents a location in a Source.
  ]#
  line*: int
  column*: int


proc newSourceLocation*(line: int, column: int): SourceLocation = 
  result = SourceLocation(line: line, column: column)


proc formatted*(self: SourceLocation): Table[string, int] =
  result = { "line": self.line, "column": self.column }.toTable


proc `==`*(a, b: SourceLocation): bool =
  result = a.line == b.line
  result = a.column == b.column


proc `==`*(a, b: seq[SourceLocation]): bool =
  result = a.len == b.len
  if result:
    for idx in 0 .. a.high:
      result = a[idx][] == b[idx][]
      if not result: break


## Source Module Section
type Source* = ref object ## A representation of source input to GraphQL.
  body*: string
  name*: string
  locationOffset*: SourceLocation


proc newSource*(body: string, name: string = "GraphQL request", locationOffset: SourceLocation = newSourceLocation(1, 1)): Source =
  #[
    `name` and `locationOffset` are optional. They are useful for clients who
    store GraphQL documents in source files; for example, if the GraphQL input
    starts at line 40 in a file named Foo.graphql, it might be useful for name to
    be "Foo.graphql" and location to be `{ line: 40, column: 0 }`.

    line and column in locationOffset are 1-indexed
  ]#
  new(result)
  result.body = body
  result.name = name
  if locationOffset.line <= 0:
    raise newException(ValueError, "line in locationOffset is 1-indexed and must be positive.")
  if locationOffset.column <= 0:
    raise newException(ValueError, "column in locationOffset is 1-indexed and must be positive.")
  result.locationOffset = locationOffset


proc getLocation*(self: Source, position: int): SourceLocation =
  #[
    Get the line and column for a character position in the source.

    Takes a Source and a UTF-8 character offset, and returns the corresponding line 
    and column as a SourceLocation.
  ]#
  var
    lines: seq[string] = self.body[0 ..< position].splitLines
    line: int
    column: int

  if lines.len > 0:
    line = lines.len
    column = lines[^1].len + 1
  else:
    line = 1
    column = 1
  return newSourceLocation(line, column)


proc `$`*(self: SourceLocation): string =
  return fmt"{$(type(self))}(line={self.line}, column={self.column})"

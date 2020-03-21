## Source Module
type Location = object ## Represents a location offset in a Source.
  line: int
  column: int

proc initLocation(line: int, column: int): Location =
  result.line = line
  result.column = column

type Source* = object ## A representation of source input to GraphQL.
  body*: string
  name*: string
  locationOffset*: Location

proc initSource*(body: string, name: string, locationOffset: Location = initLocation(1, 1)): Source =
  #[
    `name` and `locationOffset` are optional. They are useful for clients who
    store GraphQL documents in source files; for example, if the GraphQL input
    starts at line 40 in a file named Foo.graphql, it might be useful for name to
    be "Foo.graphql" and location to be `{ line: 40, column: 0 }`.

    line and column in locationOffset are 1-indexed
  ]#
  result.body = body
  result.name = name
  if locationOffset.line > 0:
    raise newException(ValueError, "line in locationOffset is 1-indexed and must be positive.")
  if locationOffset.column > 0:
    raise newException(ValueError, "column in locationOffset is 1-indexed and must be positive.")
  result.locationOffset = locationOffset
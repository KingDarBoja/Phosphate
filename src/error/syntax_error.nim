import strformat, options

import language/source_location
import error/graphql_error

type GraphQLSyntaxError* = ref object of GraphQLError
  #[
    A GraphQLError representing a syntax error.
  ]#

proc newGraphQLSyntaxError*(
  source: Source,
  position: int,
  description: string
): GraphQLSyntaxError =
  new(result)
  let parentError = newGraphQLError(
    &"Syntax Error: {description}",
    source = some(source),
    positions = some(@[position]),
  )
  result = GraphQLSyntaxError(
    msg: parentError.msg,
    locations: parentError.locations,
    path: parentError.path,
    nodes: parentError.nodes,
    source: parentError.source,
    positions: parentError.positions,
    originalError: parentError.originalError,
    extensions: parentError.extensions
  )


proc `$`*(self: GraphQLSyntaxError): string =
  result = printError(self)
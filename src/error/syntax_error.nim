import strformat

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
  result.msg = fmt"Syntax Error: {description}"
  result.source = source
  result.positions = @[position]
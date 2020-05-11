## GraphQL Error Module
import options, strutils, strformat, sugar, strtabs, tables

import language/ast
import language/source_location
import language/print_location

type GraphQLError* = ref object of CatchableError
  #[
    A GraphQLError describes an Error found during the parse, validate, or execute
    phases of performing a GraphQL operation. In addition to a message, it also includes
    information about the locations in a GraphQL document and/or execution result that
    correspond to the Error.
  ]#

  locations*: seq[SourceLocation]
  #[
    A list of (line, column) locations within the source GraphQL document which
    correspond to this error.

    Errors during validation often contain multiple locations, for example to point out
    two things with the same name. Errors during execution include a single location,
    the field which produced the error.
  ]#

  path*: seq[string]
  #[
    A list of field names and array indexes describing the JSON-path into the execution
    response which corresponds to this error.

    Only included for errors during execution.
  ]#

  nodes*: seq[GraphNode]
  #[
    A list of GraphQL AST Nodes corresponding to this error
  ]#

  source*: Source
  #[
    The source GraphQL document for the first location of this error

    Note that if this Error represents more than one node, the source may not represent
    nodes after the first node.
  ]#

  positions*: seq[int]
  #[
    Error positions

    A list of character offsets within the source GraphQL document which correspond
    to this error.
  ]#

  originalError*: ref Exception
  #[
    The original error thrown from a field resolver during execution
  ]#

  extensions*: StringTableRef
  #[
    Extension fields to add to the formatted error

    Noe that there can't be mixed types on Nim, hence
    the values of the table are strings too.
  ]#


proc newGraphQLError*(
  message: string,
  nodes: Option[seq[GraphNode]] or Option[GraphNode] = none(GraphNode),
  source: Option[Source] = none(Source),
  positions: Option[seq[int]] = none(seq[int]),
  path: seq[string] = @[],
  originalError: Option[ref Exception] = none(ref Exception),
  extensions: StringTableRef = newStringTable()
): GraphQLError =
  new(result)
  result.msg = message

  var nodesCopy: seq[GraphNode]
  if nodes.isSome:
    let nodesVal = nodes.get()
    when nodesVal is GraphNode:
      nodesCopy = @[nodesVal]
    else:
      nodesCopy = nodesVal
  result.nodes = nodesCopy

  if source.isSome:
    result.source = source.get()
  if source.isNone and nodes.isSome:
    let node: GraphNode = nodesCopy[0]
    if not node.isNil and not node.loc.isNil and not node.loc.source.isNil:
      result.source = node.loc.source
  
  var positionsCopy: seq[int]
  if positions.isSome:
    positionsCopy = positions.get()
  elif positions.isNone and nodes.isSome:
    positionsCopy = collect(newSeq):
      for node in nodesCopy:
        if not node.loc.isNil: node.loc.start
  else:
    discard
  result.positions = positionsCopy

  var locations: seq[SourceLocation]
  if positions.isSome and source.isSome:
    locations = collect(newSeq):
      for pos in positionsCopy: getLocation(source.get(), pos)
  elif nodes.isSome:
    locations = collect(newSeq):
      for node in nodesCopy:
        if not node.loc.isNil: getLocation(node.loc.source, node.loc.start)
  else:
    discard
  result.locations = locations

  result.path = path

  if originalError.isSome:
    let originalErrorVal = originalError.get()
    result.originalError = originalErrorVal
  # TODO: Need to check the correct type for originalError
  # in order to handle missing extensions
  result.extensions = extensions


proc printError*(error: GraphQLError): string


proc `$`*(self: GraphQLError): string =
  result = printError(self)


proc `repr`*(self: GraphQLError): string =
  var args = @[self.msg]

  if self.locations.len > 0:
    let parsedLocations = collect(newSeq):
      for location in self.locations: $location
    let joinedLocations = join(parsedLocations, ", ")
    args.add(&"locations=[{joinedLocations}]")
  
  if self.path.len > 0:
    let joinedPaths = join(self.path, ", ")
    args.add(&"path=[{joinedPaths}]")

  if self.extensions.len > 0:
    args.add(&"extensions={$(self.extensions)}")
  
  let joinedArgs = join(args, ", ")
  return &"{$(type(self))}({joinedArgs})"


proc printError*(error: GraphQLError): string =
  #[
    Print a GraphQLError to a string.

    Represents useful location information about the error's position in the source.
  ]#
  var output = @[error.msg]

  if error.nodes.len > 0:
    for node in error.nodes:
      if not node.loc.isNil:
        output.add(printLocation(node.loc))
  elif not error.source.isNil and error.locations.len > 0:
    let source = error.source
    for location in error.locations:
      output.add(printSourceLocation(source, location))

  return join(output, "\n\n")


type
  GraphQLFormattedError = object
    message: string
    locations: seq[Table[string, int]]
    path: seq[string]
    extensions: StringTableRef


proc formatError(error: GraphQLError): GraphQLFormattedError = 
  ##[
    Format a GraphQL error.

    Given a GraphQLError, format it according to the rules described by the "Response
    Format, Errors" section of the GraphQL Specification.
  ]##
  let formattedLocations = collect(newSeq):
    for location in error.locations:
      if error.locations.len > 0: location.formatted

  let formatted = GraphQLFormattedError(
    message: if error.msg.isEmptyOrWhitespace: error.msg else: "An unknown error ocurred.",
    locations: formattedLocations,
    path: error.path,
    extensions: if not error.extensions.isNil: error.extensions else: nil
  )
  result = formatted


proc formatted*(self: GraphQLError): GraphQLFormattedError =
  ##[
    Get error formatted according to the specification.
  ]##
  result = formatError(self)


proc equalFormatted*(a, b: GraphQLFormattedError): bool =
  result = a.message == b.message
  result = a.locations == b.locations
  result = a.path == b.path
  result = a.extensions[] == b.extensions[]

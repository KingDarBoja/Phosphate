## GraphQL Error Module
import tables
import options
import sugar
import strutils
import sequtils

from language/ast import Node
from language/source import Source
from language/location import SourceLocation, getLocation

type GraphQLError* = object of Exception
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

  nodes*: seq[Node]
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

  extensions: Table[string, string]
  #[
    Extension fields to add to the formatted error

    Noe that there can't be mixed types on Nim, hence
    the values of the table are strings too.
  ]#

proc initGraphQLError*(
  message: string,
  nodes: Option[seq[Node]] or Option[Node] = none(Node),
  source: Option[Source] = none(Source),
  positions: Option[seq[int]] = none(seq[int]),
  path: Option[seq[string]] or Option[seq[int]] = none(seq[string]),
  originalError: Option[ref Exception] = none(ref Exception),
  extensions: Table[string, string] = initTable[string, string]()
): GraphQLError =
  result.msg = message

  var nodesCopy: seq[Node]
  if nodes.isSome:
    let nodesVal = nodes.get()
    when nodesVal is Node:
      nodesCopy = @[nodesVal]
    else:
      nodesCopy = nodesVal
  result.nodes = nodesCopy

  if source.isSome:
    result.source = source.get()
  if source.isNone and nodes.isSome:
    let node: Node = nodesCopy[0]
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
      for pos in positions.get(): getLocation(source.get(), pos)
  elif nodes.isSome:
    locations = collect(newSeq):
      for node in nodesCopy:
        if not node.loc.isNil: getLocation(node.loc.source, node.loc.start)
  else:
    discard
  result.locations = locations

  if path.isSome:
    let pathVal = path.get()
    when pathVal is seq[int]:
      result.path = mapIt(pathVal, it.intToStr)
    else:
      result.path = pathVal

  if originalError.isSome:
    let originalErrorVal = originalError.get()
    result.originalError = originalErrorVal
  # TODO: Need to check the correct type for originalError
  # in order to handle missing extensions
  result.extensions = extensions
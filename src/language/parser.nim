import options, strformat, strutils

import language/ast
import language/directive_location
import language/lexer
import language/source_location
import language/token_kind
import error/syntax_error


proc isDirectiveLocation(s: string): bool =
  try:
    discard parseEnum[DirectiveLocation](s)
    return true
  except ValueError:
    return false


proc getTokenKindDesc(kind: TokenKind): string =
  #[
    Describe a token kind as a string for debugging.
  ]#
  if isPunctuatorTokenKind(kind):
    return fmt"'{$kind}'"
  else:
    return $kind


proc getTokenDesc(token: Token): string =
  #[
    Describe a token as a string for debugging.
  ]#
  let
    value = token.value
    tokenDesc = getTokenKindDesc(token.kind)
  return tokenDesc & (
    if value.len > 0 or token.kind == TokenKind.STRING: fmt" '{value}'" else: ""
  )


type Parser* = ref object
  lexer: Lexer
  noLocation: bool
  experimentalFragmentVariables: bool


proc newParser*(
  source: Source | string,
  noLocation = false,
  experimentalFragmentVariables = false
): Parser =
  new(result)
  var sourceCopy: Source
  when source is string:
    let sourceStr = cast[string](source)
    sourceCopy = newSource(sourceStr)
  else:
    sourceCopy = source
  result.lexer = newLexer(sourceCopy)
  result.noLocation = noLocation
  result.experimentalFragmentVariables = experimentalFragmentVariables


# Forward declarations
proc expectToken(self: Parser, kind: TokenKind): Token

# Base Section
proc parseName(self: Parser): GraphNode
proc parseDocument(self: Parser): GraphNode

# Document Section
proc parseDefinition(self: Parser): GraphNode
proc parseOperationDefinition(self: Parser): GraphNode
proc parseOperationType(self: Parser): GraphNode
proc parseVariableDefinitions(self: Parser): GraphNode
proc parseVariableDefinition(self: Parser): GraphNode
proc parseVariable(self: Parser): GraphNode
proc parseSelectionSet(self: Parser): GraphNode
proc parseSelection(self: Parser): GraphNode
proc parseField(self: Parser): GraphNode
proc parseArguments(self: Parser, isConst: bool): GraphNode
proc parseArgument(self: Parser): GraphNode
proc parseConstArgument(self: Parser): GraphNode

# Fragments Section
proc parseFragment(self: Parser): GraphNode
proc parseFragmentDefinition(self: Parser): GraphNode
proc parseFragmentName(self: Parser): GraphNode
proc parseTypeCondition(self: Parser): GraphNode

# Values Section
proc parseValueLiteral(self: Parser, isConst: bool): GraphNode
proc parseListValue(self: Parser, isConst: bool): GraphNode
proc parseObjectField(self: Parser, isConst: bool): GraphNode
proc parseObjectValue(self: Parser, isConst: bool): GraphNode
proc parseIntValue(self: Parser, isConst: bool = false): GraphNode
proc parseFloatValue(self: Parser, isConst: bool = false): GraphNode
proc parseStringValue(self: Parser, isConst: bool = false): GraphNode
proc parseNamedValues(self: Parser, isConst: bool = false): GraphNode
proc parseVariableValue(self: Parser, isConst: bool = false): GraphNode

# Directives Section
proc parseDirectives(self: Parser, isConst: bool): GraphNode
proc parseDirective(self: Parser, isConst: bool): GraphNode

# Types Section
proc parseTypeReference(self: Parser): GraphNode
proc parseNamedType(self: Parser): GraphNode

# Type Definition Section
proc parseTypeSystemDefinition(self: Parser): GraphNode
proc parseDescription(self: Parser): GraphNode
proc parseSchemaDefinition(self: Parser): GraphNode
proc parseOperationTypeDefinition(self: Parser): GraphNode
proc parseScalarTypeDefinition(self: Parser): GraphNode
proc parseObjectTypeDefinition(self: Parser): GraphNode
proc parseImplementsInterfaces(self: Parser): GraphNode
proc parseFieldsDefinition(self: Parser): GraphNode
proc parseFieldDefinition(self: Parser): GraphNode
proc parseArgumentDefs(self: Parser): GraphNode
proc parseInputValueDef(self: Parser): GraphNode
proc parseInterfaceTypeDefinition(self: Parser): GraphNode
proc parseUnionTypeDefinition(self: Parser): GraphNode
proc parseUnionMemberTypes(self: Parser): GraphNode
proc parseEnumTypeDefinition(self: Parser): GraphNode
proc parseEnumValuesDefinition(self: Parser): GraphNode
proc parseEnumValueDefinition(self: Parser): GraphNode
proc parseInputObjectTypeDefinition(self: Parser): GraphNode
proc parseInputFieldsDefinition(self: Parser): GraphNode
proc parseTypeSystemExtension(self: Parser): GraphNode
proc parseSchemaExtension(self: Parser): GraphNode
proc parseScalarTypeExtension(self: Parser): GraphNode
proc parseObjectTypeExtension(self: Parser): GraphNode
proc parseInterfaceTypeExtension(self: Parser): GraphNode
proc parseUnionTypeExtension(self: Parser): GraphNode
proc parseEnumTypeExtension(self: Parser): GraphNode
proc parseInputObjectTypeExtension(self: Parser): GraphNode
proc parseDirectiveDefinition(self: Parser): GraphNode
proc parseDirectiveLocations(self: Parser): GraphNode
proc parseDirectiveLocation(self: Parser): GraphNode

## Emulate Python "Partial" by using closures described by Varriount 
## on the devnotes doc.
proc partialValueLiteral(self: Parser, isConst: bool): auto =
  result = proc(): auto = return parseValueLiteral(self, isConst)

proc partialObjectField(self: Parser, isConst: bool): auto =
  result = proc(): auto = return parseObjectField(self, isConst)


proc parse*(
  source: Source or string,
  noLocation = false,
  experimentalFragmentVariables = false,
): GraphNode =
  #[
    Given a GraphQL source, parse it into a Document.

    Throws GraphQLError if a syntax error is encountered.

    By default, the parser creates AST nodes that know the location in the source that
    they correspond to. The `no_location` option disables that behavior for performance
    or testing.

    Experimental features:

    If `experimental_fragment_variables` is set to True, the parser will understand
    and parse variable definitions contained in a fragment definition. They'll be
    represented in the `variable_definitions` field of the `FragmentDefinitionNode`.

    The syntax is identical to normal, query-defined variables. For example::

    fragment A($var: Boolean = false) on T  {
      ...
    }
  ]#
  let parser = newParser(
    source,
    noLocation,
    experimentalFragmentVariables
  )
  return parser.parseDocument()


proc parseValue*(
  source: Source or string,
  noLocation = false,
  experimentalFragmentVariables = false,
): GraphNode =
  ##[
    Parse the AST for a given string containing a GraphQL value.

    Throws GraphQLError if a syntax error is encountered.

    This is useful within tools that operate upon GraphQL Values directly and in
    isolation of complete GraphQL documents.

    Consider providing the results to the utility function:
    :func:`~graphql.value_from_ast`.
  ]##
  let parser = newParser(
    source,
    noLocation,
    experimentalFragmentVariables
  )
  discard parser.expectToken(TokenKind.SOF)
  let value = parser.parseValueLiteral(false)
  discard parser.expectToken(TokenKind.EOF)
  return value


proc parseType*(
  source: Source or string,
  noLocation = false,
  experimentalFragmentVariables = false,
): GraphNode =
  ##[
    Parse the AST for a given string containing a GraphQL Type.

    Throws GraphQLError if a syntax error is encountered.

    This is useful within tools that operate upon GraphQL Types directly and
    in isolation of complete GraphQL documents.

    Consider providing the results to the utility function:
    :func:`~graphql.value_from_ast`.
  ]##
  let parser = newParser(
    source,
    noLocation,
    experimentalFragmentVariables
  )
  discard parser.expectToken(TokenKind.SOF)
  let `type` = parser.parseTypeReference()
  discard parser.expectToken(TokenKind.EOF)
  return `type`


# Core parsing utility functions


proc loc(self: Parser, startToken: Token): Location =
  ##[
    Return a location object.

    Used to identify the place in the source that created a given parsed object.
  ]##
  if not self.noLocation:
    let
      endToken = self.lexer.lastToken
      source = self.lexer.source
    return newLocation(startToken, endToken, source)
  return nil


proc peek(self: Parser, kind: TokenKind): bool =
  ##[
    Determine if the next token is of a given kind
  ]##
  return self.lexer.token.kind == kind


proc peekDescription(self: Parser): bool =
  return self.peek(TokenKind.STRING) or self.peek(TokenKind.BLOCK_STRING)


proc expectToken(self: Parser, kind: TokenKind): Token =
  ##[
    Expect the next token to be of the given kind.

    If the next token is of the given kind, return that token after advancing
    the lexer. Otherwise, do not change the parser state and throw an error.
  ]##
  let token = self.lexer.token
  if token.kind == kind:
    discard self.lexer.advance()
    return token

  raise newGraphQLSyntaxError(
    self.lexer.source,
    token.start,
    fmt"Expected {getTokenKindDesc(kind)}, found {getTokenDesc(token)}."
  )


proc expectOptionalToken(self: Parser, kind: TokenKind): Token =
  ##[
    Expect the next token optionally to be of the given kind.

    If the next token is of the given kind, return that token after advancing
    the lexer. Otherwise, do not change the parser state and return None.
  ]##
  let token = self.lexer.token
  if token.kind == kind:
    discard self.lexer.advance()
    return token

  return nil


proc expectKeyword(self: Parser, value: string): GraphQLSyntaxError =
  ##[
    Expect the next token to be a given keyword.

    If the next token is a given keyword, advance the lexer.
    Otherwise, do not change the parser state and throw an error.
  ]##
  let token = self.lexer.token
  if token.kind == TokenKind.NAME and token.value == value:
    discard self.lexer.advance()
  else:
    raise newGraphQLSyntaxError(
      self.lexer.source,
      token.start,
      fmt"Expected '{value}', found {getTokenDesc(token)}."
    )


proc expectOptionalKeyword(self: Parser, value: string): bool =
  ##[
    Expect the next token optionally to be a given keyword.

    If the next token is a given keyword, return True after advancing the lexer.
    Otherwise, do not change the parser state and return False.
  ]##
  let token = self.lexer.token
  if token.kind == TokenKind.NAME and token.value == value:
    discard self.lexer.advance()
    return true

  return false


proc unexpected(self: Parser, atToken: Option[Token] = none(Token)): GraphQLSyntaxError =
  ##[
    Create an error when an unexpected lexed token is encountered.
  ]##
  let token: Token = if atToken.isSome: atToken.get() else: self.lexer.token
  return newGraphQLSyntaxError(
    self.lexer.source,
    token.start,
    fmt"Unexpected {getTokenDesc(token)}."
  )


proc anyNode(
  self: Parser,
  openKind: TokenKind,
  parseProc: proc (): GraphNode,
  closeKind: TokenKind
): seq[GraphNode] =
  ##[
    Fetch any matching nodes, possibly none.

    Returns a possibly empty list of parse nodes, determined by the `parse_fn`.
    This list begins with a lex token of `open_kind` and ends with a lex token of
    `close_kind`. Advances the parser to the next lex token after the closing token.
  ]##
  discard self.expectToken(openKind)
  var nodes: seq[GraphNode]
  while self.expectOptionalToken(closeKind).isNil:
    nodes.add(parseProc())
  return nodes


proc optionalManyNode(
  self: Parser,
  openKind: TokenKind,
  parseProc: proc(self: Parser): GraphNode,
  closeKind: TokenKind
): seq[GraphNode] =
  ##[
    Fetch matching nodes, maybe none.

    Returns a list of parse nodes, determined by the `parse_fn`. It can be empty
    only if the open token is missing, otherwise it will always return a non-empty
    list that begins with a lex token of `open_kind` and ends with a lex token of
    `close_kind`. Advances the parser to the next lex token after the closing token.
  ]##
  if not self.expectOptionalToken(openKind).isNil:
    var nodes: seq[GraphNode] = @[self.parseProc()]
    while self.expectOptionalToken(closeKind).isNil:
      nodes.add(self.parseProc())
    return nodes
  return @[]


proc manyNode(
  self: Parser,
  openKind: TokenKind,
  parseProc: proc(self: Parser): GraphNode,
  closeKind: TokenKind
): seq[GraphNode] =
  ##[
    Fetch matching nodes, at least one.

    Returns a non-empty list of parse nodes, determined by the `parse_fn`.
    This list begins with a lex token of `open_kind` and ends with a lex token of
    `close_kind`. Advances the parser to the next lex token after the closing token.
  ]##
  discard self.expectToken(openKind)
  var nodes: seq[GraphNode] = @[self.parseProc()]
  while self.expectOptionalToken(closeKind).isNil:
    nodes.add(self.parseProc())
  return nodes


# Implement the parsing rules in the base section


proc parseEmpty(self: Parser, kind: GraphNodeKind): GraphNode =
  let start = self.lexer.token
  return GraphNode(kind: kind, loc: self.loc(start))


proc parseName(self: Parser): GraphNode =
  ##[
    Convert a name lex token into a name parse node.
  ]##
  let token = self.expectToken(TokenKind.NAME)
  return GraphNode(kind: gnkName, value: token.value, loc: self.loc(token))


# Implement the parsing rules in the Document section.


proc parseDocument(self: Parser): GraphNode =
  ##[
    Document: Definition
  ]##
  let start = self.lexer.token
  return GraphNode(
    kind: gnkDocument,
    children: self.manyNode(
      TokenKind.SOF, parseDefinition, TokenKind.EOF
    ),
    loc: self.loc(start),
  )


proc parseDefinition(self: Parser): GraphNode =
  ##[
    Definition: ExecutableDefinition or TypeSystemDefinition/Extension

    ExecutableDefinition: OperationDefinition or FragmentDefinition
  ]##
  if self.peek(TokenKind.NAME):
    let methodName = self.lexer.token.value
    case methodName
    of "query", "mutation", "subscription":
      return self.parseOperationDefinition()
    of "fragment":
      return self.parseFragmentDefinition()
    of "schema", "scalar", "type", "interface", "union", "enum", "input", "directive":
      return self.parseTypeSystemDefinition()
    of "extend":
      return self.parseTypeSystemExtension()
    else:
      discard
  elif self.peek(TokenKind.BRACE_L):
    return self.parseOperationDefinition()
  elif self.peekDescription():
    return self.parseTypeSystemDefinition()
  else:
    discard

  raise self.unexpected()


# Implement the parsing rules in the Operations section.


proc parseOperationDefinition(self: Parser): GraphNode =
  ##[
    Operation Definition
  ]##
  let start = self.lexer.token
  if self.peek(TokenKind.BRACE_L):
    let op = GraphNode(
      kind: gnkOperationType,
      operation: GraphOperationTypeKind.gnkOperationQuery
    )
    return GraphNode(
      kind: gnkOperationDefinition,
      children: @[
        op,
        self.parseEmpty(gnkName),
        self.parseEmpty(gnkVariableDefinitionList),
        self.parseEmpty(gnkDirectiveList),
        self.parseSelectionSet()
      ],
      loc: self.loc(start)
    )
  let
    operation = self.parseOperationType()
    name = if self.peek(TokenKind.NAME): self.parseName() else: self.parseEmpty(gnkName)
  return GraphNode(
    kind: gnkOperationDefinition,
    children: @[
      operation,
      name,
      self.parseVariableDefinitions(),
      self.parseDirectives(false),
      self.parseSelectionSet()
    ],
    loc: self.loc(start)
  )


proc parseOperationType(self: Parser): GraphNode =
  ##[
    OperationType: one of query mutation subscription
  ]##
  let operationToken = self.expectToken(TokenKind.NAME)
  try:
    let operationVal = parseEnum[GraphOperationTypeKind](operationToken.value)
    return GraphNode(
      kind: gnkOperationType,
      operation: operationVal
    )
  except ValueError:
    raise self.unexpected(some(operationToken))


proc parseVariableDefinitions(self: Parser): GraphNode =
  #[
    VariableDefinitions:
      ( VariableDefinition[list] )
  ]#
  let start = self.lexer.token
  return GraphNode(
    kind: gnkVariableDefinitionList,
    children: self.optionalManyNode(
      TokenKind.PAREN_L,
      parseVariableDefinition,
      TokenKind.PAREN_R
    ),
    loc: self.loc(start),
  )


proc parseVariableDefinition(self: Parser): GraphNode =
  ##[
    VariableDefinition:
      Variable : Type DefaultValue? Directives[Const]?
  ]##
  let start = self.lexer.token
  return GraphNode(
    kind: gnkVariableDefinition,
    children: @[
      self.parseVariable(),
      if not self.expectToken(TokenKind.COLON).isNil: self.parseTypeReference() else: self.parseEmpty(gnkEmpty),
      if not self.expectOptionalToken(TokenKind.EQUALS).isNil: self.parseValueLiteral(true) else: self.parseEmpty(gnkEmpty),
      self.parseDirectives(true)
    ],
    loc: self.loc(start),
  )


proc parseVariable(self: Parser): GraphNode =
  ##[
    Variable: $Name
  ]##
  let start = self.lexer.token
  discard self.expectToken(TokenKind.DOLLAR)
  return GraphNode(
    kind: gnkVariable,
    children: @[self.parseName()],
    loc: self.loc(start)
  )


proc parseSelectionSet(self: Parser): GraphNode =
  ##[
    SelectionSet:
      **{** Selection[list] **}**
  ]##
  let start = self.lexer.token
  return GraphNode(
    kind: gnkSelectionSet,
    children: self.manyNode(
      TokenKind.BRACE_L,
      parseSelection,
      TokenKind.BRACE_R
    ),
    loc: self.loc(start)
  )


proc parseSelection(self: Parser): GraphNode =
  ##[
    Selection: Field or FragmentSpread or InlineFragment
  ]##
  return if self.peek(TokenKind.SPREAD): self.parseFragment() else: self.parseField()


proc parseField(self: Parser): GraphNode =
  ##[
    Field:
      Alias? Name Arguments? Directives? SelectionSet?
  ]##
  let
    start = self.lexer.token
    nameOrAlias = self.parseName()
  var
    alias, name: GraphNode
  if not self.expectOptionalToken(TokenKind.COLON).isNil:
    alias = nameOrAlias
    name = self.parseName()
  else:
    alias = self.parseEmpty(gnkEmpty)
    name = nameOrAlias
  return GraphNode(
    kind: gnkField,
    children: @[
      alias,
      name,
      self.parseArguments(false),
      self.parseDirectives(false),
      if self.peek(TokenKind.BRACE_L): self.parseSelectionSet() else: self.parseEmpty(gnkSelectionSet)
    ],
    loc: self.loc(start)
  )


proc parseArguments(self: Parser, isConst: bool): GraphNode =
  ##[
    Arguments[Const]:
      ( Argument[?Const]+ )
  ]##
  let
    start = self.lexer.token
    item = if isConst: parseConstArgument else: parseArgument
  return GraphNode(
    kind: gnkArgumentList,
    children: self.optionalManyNode(
      TokenKind.PAREN_L,
      item,
      TokenKind.PAREN_R
    ),
    loc: self.loc(start)
  )

proc parseArgument(self: Parser): GraphNode =
  ##[
    Argument:
      Name : Value
  ]##
  let
    start = self.lexer.token
    name = self.parseName()

  discard self.expectToken(TokenKind.COLON)
  return GraphNode(
    kind: gnkArgument,
    children: @[name, self.parseValueLiteral(false)],
    loc: self.loc(start)
  )


proc parseConstArgument(self: Parser): GraphNode =
  ##[
    Argument[Const]:
      Name : Value[?Const]
  ]##
  let
    start = self.lexer.token
    value = if not self.expectToken(TokenKind.COLON).isNil: self.parseValueLiteral(true) else: self.parseEmpty(gnkEmpty) 
  return GraphNode(
    kind: gnkArgument,
    children: @[self.parseName(), value],
    loc: self.loc(start)
  )


# Implement the parsing rules in the Fragments section.


proc parseFragment(self: Parser): GraphNode =
  ##[
    Corresponds to both FragmentSpread and InlineFragment in the spec.

    FragmentSpread: ... FragmentName Directives?
    InlineFragment: ... TypeCondition? Directives? SelectionSet
  ]##
  let start = self.lexer.token
  discard self.expectToken(TokenKind.SPREAD)

  let hasTypeCondition = self.expectOptionalKeyword("on")
  if not hasTypeCondition and self.peek(TokenKind.NAME):
    return GraphNode(
      kind: gnkFragmentSpread,
      children: @[
        self.parseFragmentName(),
        self.parseDirectives(false)
      ],
      loc: self.loc(start)
    )

  return GraphNode(
    kind: gnkInlineFragment,
    children: @[
      if hasTypeCondition: self.parseNamedType() else: self.parseEmpty(gnkNamedType),
      self.parseDirectives(false),
      self.parseSelectionSet()
    ],
    loc: self.loc(start)
  )


proc parseFragmentDefinition(self: Parser): GraphNode =
  ##[
    FragmentDefinition:
      *fragment* FragmentName TypeCondition Directives[opt] SelectionSet

    FragmentName:
      Name but not *on*
  ]##
  let start = self.lexer.token
  discard self.expectKeyword("fragment")
  ## Experimental support for defining variables within fragments changes
  ## the grammar of FragmentDefinition
  return GraphNode(
    kind: gnkFragmentDefinition,
    children: @[
      self.parseFragmentName(),
      if self.experimentalFragmentVariables: self.parseVariableDefinitions() else: self.parseEmpty(gnkVariableDefinitionList),
      self.parseTypeCondition(),
      self.parseDirectives(false),
      self.parseSelectionSet()
    ],
    loc: self.loc(start)
  )


proc parseFragmentName(self: Parser): GraphNode =
  ##[
    FragmentName: Name but not `on`
  ]##
  if self.lexer.token.value == "on":
    raise self.unexpected()
  return self.parseName()


proc parseTypeCondition(self: Parser): GraphNode =
  ##[
    TypeCondition: NamedType
  ]##
  discard self.expectKeyword("on")
  return self.parseNamedType()


# Implement the parsing rules in the Values section.


proc parseValueLiteral(self: Parser, isConst: bool): GraphNode =
  let kind = self.lexer.token.kind
  case kind
  of TokenKind.BRACKET_L:
    return self.parseListValue(isConst)
  of TokenKind.BRACE_L:
    return self.parseObjectValue(isConst)
  of TokenKind.INT:
    return self.parseIntValue(isConst)
  of TokenKind.FLOAT:
    return self.parseFloatValue(isConst)
  of TokenKind.STRING, TokenKind.BLOCK_STRING:
    return self.parseStringValue()
  of TokenKind.NAME:
    return self.parseNamedValues(isConst)
  of TokenKind.DOLLAR:
    return self.parseVariableValue(isConst)
  else: discard

  raise self.unexpected()


proc parseStringValue(self: Parser, isConst: bool = false): GraphNode =
  let token = self.lexer.token
  discard self.lexer.advance()
  return GraphNode(
    kind: gnkStringValue,
    strValue: token.value,
    isBlockString: token.kind == TokenKind.BLOCK_STRING,
    loc: self.loc(token)
  )


proc parseListValue(self: Parser, isConst: bool): GraphNode =
  ##[
    ListValue[Const]
  ]##
  let start = self.lexer.token
  return GraphNode(
    kind: gnkListValue,
    children: self.anyNode(
      TokenKind.BRACKET_L,
      partialValueLiteral(self, isConst),
      TokenKind.BRACKET_R
    ),
    loc: self.loc(start)
  )


proc parseObjectField(self: Parser, isConst: bool): GraphNode =
  let
    start = self.lexer.token
    name = self.parseName()
  discard self.expectToken(TokenKind.COLON)
  let value = self.parseValueLiteral(isConst)

  return GraphNode(
    kind: gnkObjectField,
    children: @[name, value],
    loc: self.loc(start)
  )


proc parseObjectValue(self: Parser, isConst: bool): GraphNode =
  ##[
    ObjectValue[Const]
  ]##
  let start = self.lexer.token
  return GraphNode(
    kind: gnkObjectValue,
    children: self.anyNode(
      TokenKind.BRACE_L,
      partialObjectField(self, isConst),
      TokenKind.BRACE_R
    ),
    loc: self.loc(start)
  )


proc parseIntValue(self: Parser, isConst: bool = false): GraphNode =
  let token = self.lexer.token
  discard self.lexer.advance()
  return GraphNode(
    kind: gnkIntValue,
    intValue: token.value,
    loc: self.loc(token)
  )


proc parseFloatValue(self: Parser, isConst: bool = false): GraphNode =
  let token = self.lexer.token
  discard self.lexer.advance()
  return GraphNode(
    kind: gnkFloatValue,
    floatValue: token.value,
    loc: self.loc(token)
  )


proc parseNamedValues(self: Parser, isConst: bool = false): GraphNode =
  let
    token = self.lexer.token
    value = token.value
  discard self.lexer.advance()
  case value
  of "true":
    return GraphNode(kind: gnkBooleanValue, loc: self.loc(token), boolValue: true)
  of "false":
    return GraphNode(kind: gnkBooleanValue, loc: self.loc(token), boolValue: false)
  of "null":
    return GraphNode(kind: gnkNullValue, loc: self.loc(token))
  else:
    return GraphNode(kind: gnkEnumValue, loc: self.loc(token), enumValue: value)


proc parseVariableValue(self: Parser, isConst: bool = false): GraphNode =
  if not isConst:
    return self.parseVariable()
  raise self.unexpected()


# Implement the parsing rules in the Directives section.


proc parseDirectives(self: Parser, isConst: bool): GraphNode =
  ##[
    Directives[Const]:
      Directive[?Const]+
  ]##
  let start = self.lexer.token
  var directives: seq[GraphNode]
  while self.peek(TokenKind.AT):
    directives.add(self.parseDirective(isConst))
  return GraphNode(kind: gnkDirectiveList, children: directives, loc: self.loc(start))


proc parseDirective(self: Parser, isConst: bool): GraphNode =
  ##[
    Directive[Const]: @ Name Arguments[?Const]?
  ]##
  let start = self.lexer.token
  discard self.expectToken(TokenKind.AT)
  return GraphNode(
    kind: gnkDirective,
    children: @[
      self.parseName(),
      self.parseArguments(isConst)
    ],
    loc: self.loc(start)
  )


# Implement the parsing rules in the Types section.


proc parseTypeReference(self: Parser): GraphNode =
  ##[
    Type: NamedType or ListType or NonNullType
  ]##
  let start = self.lexer.token
  var typeRef: GraphNode
  if not self.expectOptionalToken(TokenKind.BRACKET_L).isNil:
    typeRef = self.parseTypeReference()
    discard self.expectToken(TokenKind.BRACKET_R)
    typeRef = GraphNode(
      kind: gnkListType,
      children: @[typeRef],
      loc: self.loc(start)
    )
  else:
    typeRef = self.parseNamedType()
  if not self.expectOptionalToken(TokenKind.BANG).isNil:
    return GraphNode(
      kind: gnkNonNullType,
      children: @[typeRef],
      loc: self.loc(start)
    )
  return typeRef
    

proc parseNamedType(self: Parser): GraphNode =
  ##[
    NamedType: Name
  ]##
  let start = self.lexer.token
  return GraphNode(
    kind: gnkNamedType,
    children: @[self.parseName()],
    loc: self.loc(start)
  )


# Implement the parsing rules in the Type Definition section.


## TypeSystemDefinition :
## - SchemaDefinition
## - TypeDefinition
## - DirectiveDefinition
## TypeDefinition :
## - ScalarTypeDefinition
## - ObjectTypeDefinition
## - InterfaceTypeDefinition
## - UnionTypeDefinition
## - EnumTypeDefinition
## - InputObjectTypeDefinition

proc parseTypeSystemDefinition(self: Parser): GraphNode =
  ##[
    Many definitions begin with a description and require a lookahead.
  ]##
  let keywordToken = if self.peekDescription(): self.lexer.lookahead() else: self.lexer.token
  if keywordToken.kind == TokenKind.NAME:
    case keywordToken.value:
    of "schema":
      return self.parseSchemaDefinition()
    of "scalar":
      return self.parseScalarTypeDefinition()
    of "type":
      return self.parseObjectTypeDefinition()
    of "interface":
      return self.parseInterfaceTypeDefinition()
    of "union":
      return self.parseUnionTypeDefinition()
    of "enum":
      return self.parseEnumTypeDefinition()
    of "input":
      return self.parseInputObjectTypeDefinition()
    of "directive":
      return self.parseDirectiveDefinition()
    else: discard
  
  raise self.unexpected(some(keywordToken))


## TypeSystemExtension :
## - SchemaExtension
## - TypeExtension
## 
## TypeExtension :
## - ScalarTypeExtension
## - ObjectTypeExtension
## - InterfaceTypeExtension
## - UnionTypeExtension
## - EnumTypeExtension
## - InputObjectTypeDefinition

proc parseTypeSystemExtension(self: Parser): GraphNode =
  ##[
    TypeSystemExtension
  ]##
  let keywordToken = self.lexer.lookahead()
  if keywordToken.kind == TokenKind.NAME:
    case keywordToken.value
    of "schema":
      return self.parseSchemaExtension()
    of "scalar":
      return self.parseScalarTypeExtension()
    of "type":
      return self.parseObjectTypeExtension()
    of "interface":
      return self.parseInterfaceTypeExtension()
    of "union":
      return self.parseUnionTypeExtension()
    of "enum":
      return self.parseEnumTypeExtension()
    of "input":
      return self.parseInputObjectTypeExtension()
    else: discard

  raise self.unexpected(some(keywordToken))



proc parseDescription(self: Parser): GraphNode =
  ##[
    Description: StringValue
  ]##
  if self.peekDescription():
    return self.parseStringValue()
  return self.parseEmpty(gnkEmpty)


proc parseSchemaDefinition(self: Parser): GraphNode =
  ##[
    SchemaDefinition:
      Description[opt] **schema** Directives[Const, opt] **{** RootOperationTypeDefinition[list] **}**

    RootOperationTypeDefinition:
      OperationType **:** NamedType
  ]##
  let
    start = self.lexer.token
    description = self.parseDescription()
  discard self.expectKeyword("schema")
  let
    directives = self.parseDirectives(true)
    otStart = self.lexer.token
    operationTypes = GraphNode(
      kind: gnkOperationTypeDefinitionList,
      children: self.manyNode(
        TokenKind.BRACE_L,
        parseOperationTypeDefinition,
        TokenKind.BRACE_R
      ),
      loc: self.loc(otStart)
    )
  return GraphNode(
    kind: gnkSchemaDefinition,
    children: @[
      description,
      directives,
      operationTypes
    ],
    loc: self.loc(start)
  )

proc parseOperationTypeDefinition(self: Parser): GraphNode =
  ##[
    OperationTypeDefinition: OperationType : NamedType
  ]##
  let
    start = self.lexer.token
    operation = self.parseOperationType()
  discard self.expectToken(TokenKind.COLON)
  let typeRef = self.parseNamedType()
  return GraphNode(
    kind: gnkOperationTypeDefinition,
    children: @[operation, typeRef],
    loc: self.loc(start)
  )


proc parseScalarTypeDefinition(self: Parser): GraphNode =
  ##[
    ScalarTypeDefinition: Description? scalar Name Directives[Const]?
  ]##
  let
    start = self.lexer.token
    description = self.parseDescription()
  discard self.expectKeyword("scalar")
  let
    name = self.parseName()
    directives = self.parseDirectives(true)
  return GraphNode(
    kind: gnkScalarTypeDefinition,
    children: @[
      description,
      name,
      directives
    ],
    loc: self.loc(start)
  )


proc parseObjectTypeDefinition(self: Parser): GraphNode =
  ##[
    ObjectTypeDefinition
  ]##
  let
    start = self.lexer.token
    description = self.parseDescription()
  discard self.expectKeyword("type")
  let
    name = self.parseName()
    interfaces = self.parseImplementsInterfaces()
    directives = self.parseDirectives(true)
    fields = self.parseFieldsDefinition()
  return GraphNode(
    kind: gnkObjectTypeDefinition,
    children: @[
      description,
      name,
      interfaces,
      directives,
      fields
    ],
    loc: self.loc(start)
  )


proc parseImplementsInterfaces(self: Parser): GraphNode =
  ##[
    ImplementsInterfaces
  ]##
  var types: seq[GraphNode]
  let start = self.lexer.token
  if self.expectOptionalKeyword("implements"):
    # optional leading ampersand
    discard self.expectOptionalToken(TokenKind.AMP)
    while true:
      types.add(self.parseNamedType())
      if not self.expectOptionalToken(TokenKind.AMP).isNil:
        break
  return GraphNode(kind: gnkImplementsInterfaces, children: types, loc: self.loc(start))


proc parseFieldsDefinition(self: Parser): GraphNode =
  ##[
    FieldsDefinition:
      **{** FieldDefinition[list] **}**
  ]##
  let start = self.lexer.token
  return GraphNode(
    kind: gnkFieldDefinitionList,
    children: self.optionalManyNode(
      TokenKind.BRACE_L,
      parseFieldDefinition,
      TokenKind.BRACE_R
    ),
    loc: self.loc(start)
  )


proc parseFieldDefinition(self: Parser): GraphNode =
  ##[
    FieldDefinition
  ]##
  let
    start = self.lexer.token
    description = self.parseDescription()
    name = self.parseName()
    arguments = self.parseArgumentDefs()
  discard self.expectToken(TokenKind.COLON)
  let
    typeRef = self.parseTypeReference()
    directives = self.parseDirectives(true)
  return GraphNode(
    kind: gnkFieldDefinition,
    children: @[
      description,
      name,
      arguments,
      typeRef,
      directives
    ],
    loc: self.loc(start)
  )


proc parseArgumentDefs(self: Parser): GraphNode =
  ##[
    ArgumentsDefinition:
      **(** InputValueDefinition[list] **)**
  ]##
  let start = self.lexer.token
  return GraphNode(
    kind: gnkArgumentsDefinition,
    children: self.optionalManyNode(
      TokenKind.PAREN_L,
      parseInputValueDef,
      TokenKind.PAREN_R
    ),
    loc: self.loc(start)
  )


proc parseInputValueDef(self: Parser): GraphNode =
  ##[
    InputValueDefinition
  ]##
  let 
    start = self.lexer.token
    description = self.parseDescription()
    name = self.parseName()
  discard self.expectToken(TokenKind.COLON)
  let
    typeRef = self.parseTypeReference()
    defaultValue = if not self.expectOptionalToken(TokenKind.EQUALS).isNil: self.parseValueLiteral(true) else: self.parseEmpty(gnkEmpty)
    directives = self.parseDirectives(true)
  return GraphNode(
    kind: gnkInputValueDefinition,
    children: @[
      name,
      description,
      typeRef,
      defaultValue,
      directives
    ],
    loc: self.loc(start)
  )


proc parseInterfaceTypeDefinition(self: Parser): GraphNode =
  ##[
    InterfaceTypeDefinition
  ]##
  let
    start = self.lexer.token
    description = self.parseDescription()
  discard self.expectKeyword("interface")
  let
    name = self.parseName()
    interfaces = self.parseImplementsInterfaces()
    directives = self.parseDirectives(true)
    fields = self.parseFieldsDefinition()
  return GraphNode(
    kind: gnkInterfaceTypeDefinition,
    children: @[
      description,
      name,
      interfaces,
      directives,
      fields
    ],
    loc: self.loc(start)
  )


proc parseUnionTypeDefinition(self: Parser): GraphNode =
  ##[
    UnionTypeDefinition
  ]##
  let
    start = self.lexer.token
    description = self.parseDescription()
  discard self.expectKeyword("union")
  let
    name = self.parseName()
    directives = self.parseDirectives(true)
    types = self.parseUnionMemberTypes()
  return GraphNode(
    kind: gnkUnionTypeDefinition,
    children: @[
      description,
      name,
      directives,
      types
    ],
    loc: self.loc(start)
  )


proc parseUnionMemberTypes(self: Parser): GraphNode =
  ##[
    UnionMemberTypes
  ]##
  let start = self.lexer.token
  var types: seq[GraphNode]
  if not self.expectOptionalToken(TokenKind.EQUALS).isNil:
    # optional leading pipe
    discard self.expectOptionalToken(TokenKind.PIPE)
  while true:
    types.add(self.parseNamedType)
    if not self.expectOptionalToken(TokenKind.PIPE).isNil:
      break
  return GraphNode(kind: gnkUnionMemberTypes, children: types, loc: self.loc(start))


proc parseEnumTypeDefinition(self: Parser): GraphNode =
  ##[
    UnionTypeDefinition
  ]##
  let
    start = self.lexer.token
    description = self.parseDescription()
  discard self.expectKeyword("enum")
  let
    name = self.parseName()
    directives = self.parseDirectives(true)
    values = self.parseEnumValuesDefinition()
  return GraphNode(
    kind: gnkEnumTypeDefinition,
    children: @[
      description,
      name,
      directives,
      values
    ],
    loc: self.loc(start)
  )


proc parseEnumValuesDefinition(self: Parser): GraphNode =
  ##[
    EnumValuesDefinition:
      **{** EnumValueDefinition[list] **}**
  ]##
  let start = self.lexer.token
  return GraphNode(
    kind: gnkEnumValueDefinitionList,
    children: self.optionalManyNode(
      TokenKind.BRACE_L,
      parseEnumValueDefinition,
      TokenKind.BRACE_R
    ),
    loc: self.loc(start)
  )


proc parseEnumValueDefinition(self: Parser): GraphNode =
  ##[
    EnumValueDefinition:
      Description? EnumValue Directives[Const]?
  ]##
  let
    start = self.lexer.token
    description = self.parseDescription()
    name = self.parseName()
    directives = self.parseDirectives(true)
  return GraphNode(
    kind: gnkEnumValueDefinition,
    children: @[
      description,
      name,
      directives
    ],
    loc: self.loc(start)
  )


proc parseInputObjectTypeDefinition(self: Parser): GraphNode =
  ##[
    InputObjectTypeDefinition
  ]##
  let
    start = self.lexer.token
    description = self.parseDescription()
  discard self.expectKeyword("input")
  let
    name = self.parseName()
    directives = self.parseDirectives(true)
    fields = self.parseInputFieldsDefinition()
  return GraphNode(
    kind: gnkInputObjectTypeDefinition,
    children: @[
      description,
      name,
      directives,
      fields
    ],
    loc: self.loc(start)
  )


proc parseInputFieldsDefinition(self: Parser): GraphNode =
  ##[
    InputFieldsDefinition:
      **{** InputValueDefinition[list] **}**
  ]##
  let start = self.lexer.token
  return GraphNode(
    kind: gnkInputFieldsDefinition,
    children: self.optionalManyNode(
      TokenKind.BRACE_L,
      parseInputValueDef,
      TokenKind.BRACE_R
    ),
    loc: self.loc(start)
  )


proc parseSchemaExtension(self: Parser): GraphNode =
  ##[
    SchemaExtension
  ]##
  let start = self.lexer.token
  discard self.expectKeyword("extend")
  discard self.expectKeyword("schema")
  let
    directives = self.parseDirectives(true)
    otStart = self.lexer.token
    operationTypes = GraphNode(
      kind: gnkOperationTypeDefinitionList,
      children: self.optionalManyNode(
        TokenKind.BRACE_L,
        parseOperationTypeDefinition,
        TokenKind.BRACE_R
      ),
      loc: self.loc(otStart)
    )
  if directives.children.len == 0 and operationTypes.children.len == 0:
    raise self.unexpected()
  return GraphNode(
    kind: gnkSchemaExtension,
    children: @[
      directives,
      operationTypes
    ],
    loc: self.loc(start)
  )


proc parseScalarTypeExtension(self: Parser): GraphNode =
  ##[
    ScalarTypeExtension
  ]##
  let start = self.lexer.token
  discard self.expectKeyword("extends")
  discard self.expectKeyword("scalar")
  let
    name = self.parseName()
    directives = self.parseDirectives(true)
  if directives.children.len == 0:
    raise self.unexpected()
  return GraphNode(
    kind: gnkScalarTypeExtension,
    children: @[
      name,
      directives
    ],
    loc: self.loc(start)
  )


proc parseObjectTypeExtension(self: Parser): GraphNode =
  ##[
    ObjectTypeExtension
  ]##
  let start = self.lexer.token
  discard self.expectKeyword("extend")
  discard self.expectKeyword("type")
  let
    name = self.parseName()
    interfaces = self.parseImplementsInterfaces()
    directives = self.parseDirectives(true)
    fields = self.parseFieldsDefinition()
  if (interfaces.children.len == 0 or directives.children.len == 0 or fields.children.len == 0):
    raise self.unexpected()
  return GraphNode(
    kind: gnkObjectTypeExtension,
    children: @[
      name,
      interfaces,
      directives,
      fields
    ],
    loc: self.loc(start)
  )


proc parseInterfaceTypeExtension(self: Parser): GraphNode =
  ##[
    InterfaceTypeExtension
  ]##
  let start = self.lexer.token
  discard self.expectKeyword("extend")
  discard self.expectKeyword("interface")
  let
    name = self.parseName()
    interfaces = self.parseImplementsInterfaces()
    directives = self.parseDirectives(true)
    fields = self.parseFieldsDefinition()
  if (interfaces.children.len == 0 or directives.children.len == 0 or fields.children.len == 0):
    raise self.unexpected()
  return GraphNode(
    kind: gnkInterfaceTypeExtension,
    children: @[
      name,
      interfaces,
      directives,
      fields
    ],
    loc: self.loc(start)
  )


proc parseUnionTypeExtension(self: Parser): GraphNode =
  ##[
    UnionTypeExtension
  ]##
  let start = self.lexer.token
  discard self.expectKeyword("extend")
  discard self.expectKeyword("union")
  let
    name = self.parseName()
    directives = self.parseDirectives(true)
    types = self.parseUnionMemberTypes()
  if (directives.children.len == 0 or types.children.len == 0):
    raise self.unexpected()
  return GraphNode(
    kind: gnkUnionTypeExtension,
    children: @[
      name,
      directives,
      types
    ],
    loc: self.loc(start)
  )


proc parseEnumTypeExtension(self: Parser): GraphNode =
  ##[
    EnumTypeExtension
  ]##
  let start = self.lexer.token
  discard self.expectKeyword("extend")
  discard self.expectKeyword("enum")
  let
    name = self.parseName()
    directives = self.parseDirectives(true)
    values = self.parseEnumValuesDefinition()
  if (directives.children.len == 0 or values.children.len == 0):
    raise self.unexpected()
  return GraphNode(
    kind: gnkEnumTypeExtension,
    children: @[
      name,
      directives,
      values
    ],
    loc: self.loc(start)
  )


proc parseInputObjectTypeExtension(self: Parser): GraphNode =
  ##[
    InputObjectTypeExtension
  ]##
  let start = self.lexer.token
  discard self.expectKeyword("extend")
  discard self.expectKeyword("input")
  let
    name = self.parseName()
    directives = self.parseDirectives(true)
    fields = self.parseInputFieldsDefinition()
  if (directives.children.len == 0 or fields.children.len == 0):
    raise self.unexpected()
  return GraphNode(
    kind: gnkInputObjectTypeExtension,
    children: @[
      name,
      directives,
      fields
    ],
    loc: self.loc(start)
  )


proc parseDirectiveDefinition(self: Parser): GraphNode =
  ##[
    DirectiveDefinition
  ]##
  let
    start = self.lexer.token
    description = self.parseDescription()
  discard self.expectKeyword("directive")
  discard self.expectToken(TokenKind.AT)
  let 
    name = self.parseName()
    args = self.parseArgumentDefs()
    repeatable = self.expectOptionalKeyword("repeatable")
  discard self.expectKeyword("on")
  let locations = self.parseDirectiveLocations()
  return GraphNode(
    kind: gnkDirectiveDefinition,
    children: @[
      description,
      name,
      args,
      GraphNode(kind: gnkBooleanValue, boolValue: repeatable),
      locations
    ],
    loc: self.loc(start)
  )


proc parseDirectiveLocations(self: Parser): GraphNode =
  ##[
    DirectiveLocations:
      DirectiveLocations **|** DirectiveLocation **|**[opt] DirectiveLocation
  ]##
  # optional leading pipe
  discard self.expectOptionalToken(TokenKind.PIPE)
  let start = self.lexer.token
  var locations: seq[GraphNode]
  while true:
    locations.add(self.parseDirectiveLocation())
    if self.expectOptionalToken(TokenKind.PIPE).isNil:
      break
  return GraphNode(kind: gnkDirectiveLocations, children: locations, loc: self.loc(start))


proc parseDirectiveLocation(self: Parser): GraphNode =
  ##[
    DirectiveLocation:
      ExecutableDirectiveLocation
      TypeSystemDirectiveLocation

    ExecutableDirectiveLocation: **one of**
      QUERY
      MUTATION
      SUBSCRIPTION
      FIELD
      FRAGMENT_DEFINITION
      FRAGMENT_SPREAD
      INLINE_FRAGMENT
      VARIABLE_DEFINITION
    
    TypeSystemDirectiveLocation: **one of**
      SCHEMA
      SCALAR
      OBJECT
      FIELD_DEFINITION
      ARGUMENT_DEFINITION
      INTERFACE
      UNION
      ENUM
      ENUM_VALUE
      INPUT_OBJECT
      INPUT_FIELD_DEFINITION
  ]##
  let
    start = self.lexer.token
    name = self.parseName()
  if isDirectiveLocation(name.value):
    return name
  
  raise self.unexpected(some(start))

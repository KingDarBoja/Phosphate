import strformat, strutils, options

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
proc parseName(self: Parser): NameNode
proc parseDocument(self: Parser): DocumentNode

# Document Section
proc parseDefinition(self: Parser): DefinitionNode
proc parseOperationDefinition(self: Parser): OperationDefinitionNode
proc parseOperationType(self: Parser): OperationTypeNode
proc parseVariableDefinitions(self: Parser): seq[VariableDefinitionNode]
proc parseVariableDefinition(self: Parser): VariableDefinitionNode
proc parseVariable(self: Parser): ValueNode
proc parseSelectionSet(self: Parser): SelectionSetNode
proc parseSelection(self: Parser): SelectionNode
proc parseField(self: Parser): SelectionNode
proc parseArguments(self: Parser, isConst: bool): seq[ArgumentNode]
proc parseArgument(self: Parser): ArgumentNode
proc parseConstArgument(self: Parser): ArgumentNode

# Fragments Section
proc parseFragment(self: Parser): SelectionNode
proc parseFragmentDefinition(self: Parser): FragmentDefinitionNode
proc parseFragmentName(self: Parser): NameNode
proc parseTypeCondition(self: Parser): TypeNode

# Values Section
proc parseValueLiteral(self: Parser, isConst: bool): ValueNode
proc parseList(self: Parser, isConst: bool): ValueNode
proc parseObjectField(self: Parser, isConst: bool): ObjectFieldNode
proc parseObject(self: Parser, isConst: bool): ValueNode
proc parseInt(self: Parser, isConst: bool = false): ValueNode
proc parseFloat(self: Parser, isConst: bool = false): ValueNode
proc parseStringLiteral(self: Parser, isConst: bool = false): ValueNode
proc parseNamedValues(self: Parser, isConst: bool = false): ValueNode
proc parseVariableValue(self: Parser, isConst: bool = false): ValueNode

# Directives Section
proc parseDirectives(self: Parser, isConst: bool): seq[DirectiveNode]
proc parseDirective(self: Parser, isConst: bool): DirectiveNode

# Types Section
proc parseTypeReference(self: Parser): TypeNode
proc parseNamedType(self: Parser): TypeNode

# Type Definition Section
proc parseTypeSystemDefinition(self: Parser): TypeSystemDefinitionNode
proc parseDescription(self: Parser): ValueNode
proc parseSchemaDefinition(self: Parser): TypeSystemDefinitionNode
proc parseOperationTypeDefinition(self: Parser): OperationTypeDefinitionNode
proc parseScalarTypeDefinition(self: Parser): TypeSystemDefinitionNode
proc parseObjectTypeDefinition(self: Parser): TypeSystemDefinitionNode
proc parseImplementsInterfaces(self: Parser): seq[TypeNode]
proc parseFieldsDefinition(self: Parser): seq[FieldDefinitionNode]
proc parseFieldDefinition(self: Parser): FieldDefinitionNode
proc parseArgumentDefs(self: Parser): seq[InputValueDefinitionNode]
proc parseInputValueDef(self: Parser): InputValueDefinitionNode
proc parseInterfaceTypeDefinition(self: Parser): TypeSystemDefinitionNode
proc parseUnionTypeDefinition(self: Parser): TypeSystemDefinitionNode
proc parseUnionMemberTypes(self: Parser): seq[TypeNode]
proc parseEnumTypeDefinition(self: Parser): TypeSystemDefinitionNode
proc parseEnumValuesDefinition(self: Parser): seq[EnumValueDefinitionNode]
proc parseEnumValueDefinition(self: Parser): EnumValueDefinitionNode
proc parseInputObjectTypeDefinition(self: Parser): TypeSystemDefinitionNode
proc parseInputFieldsDefinition(self: Parser): seq[InputValueDefinitionNode]
proc parseTypeSystemExtension(self: Parser): TypeSystemExtensionNode
proc parseSchemaExtension(self: Parser): SchemaExtensionNode
proc parseScalarTypeExtension(self: Parser): ScalarTypeExtensionNode
proc parseObjectTypeExtension(self: Parser): ObjectTypeExtensionNode
proc parseInterfaceTypeExtension(self: Parser): InterfaceTypeExtensionNode
proc parseUnionTypeExtension(self: Parser): UnionTypeExtensionNode
proc parseEnumTypeExtension(self: Parser): EnumTypeExtensionNode
proc parseInputObjectTypeExtension(self: Parser): InputObjectTypeExtensionNode
proc parseDirectiveDefinition(self: Parser): TypeSystemDefinitionNode
proc parseDirectiveLocations(self: Parser): seq[NameNode]
proc parseDirectiveLocation(self: Parser): NameNode

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
): DocumentNode =
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
): ValueNode =
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
): TypeNode =
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
  #[
    Return a location object.

    Used to identify the place in the source that created a given parsed object.
  ]#
  if not self.noLocation:
    let 
      endToken = self.lexer.lastToken
      source = self.lexer.source
    return newLocation(startToken, endToken, source)
  return nil


proc peek(self: Parser, kind: TokenKind): bool =
  #[
    Determine if the next token is of a given kind
  ]#
  return self.lexer.token.kind == kind


proc peekDescription(self: Parser): bool =
  return self.peek(TokenKind.STRING) or self.peek(TokenKind.BLOCK_STRING)


proc expectToken(self: Parser, kind: TokenKind): Token =
  #[
    Expect the next token to be of the given kind.

    If the next token is of the given kind, return that token after advancing
    the lexer. Otherwise, do not change the parser state and throw an error.
  ]#
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
  #[
    Expect the next token optionally to be of the given kind.

    If the next token is of the given kind, return that token after advancing
    the lexer. Otherwise, do not change the parser state and return None.
  ]#
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
  #[
    Expect the next token optionally to be a given keyword.

    If the next token is a given keyword, return True after advancing the lexer.
    Otherwise, do not change the parser state and return False.
  ]#
  let token = self.lexer.token
  if token.kind == TokenKind.NAME and token.value == value:
    discard self.lexer.advance()
    return true

  return false


proc unexpected(self: Parser, atToken: Option[Token] = none(Token)): GraphQLSyntaxError =
  #[
    Create an error when an unexpected lexed token is encountered.
  ]#
  let token: Token = if atToken.isSome: atToken.get() else: self.lexer.token
  return newGraphQLSyntaxError(
    self.lexer.source,
    token.start,
    fmt"Unexpected {getTokenDesc(token)}."
  )


proc anyNode[T](
  self: Parser,
  openKind: TokenKind,
  parseProc: proc (): T,
  closeKind: TokenKind
): seq[T] =
  #[
    Fetch any matching nodes, possibly none.

    Returns a possibly empty list of parse nodes, determined by the `parse_fn`.
    This list begins with a lex token of `open_kind` and ends with a lex token of
    `close_kind`. Advances the parser to the next lex token after the closing token.
  ]#
  discard self.expectToken(openKind)
  var nodes: seq[T]
  while self.expectOptionalToken(closeKind).isNil:
    nodes.add(parseProc())
  return nodes


proc optionalManyNode[T](
  self: Parser,
  openKind: TokenKind,
  parseProc: proc(self: Parser): T,
  closeKind: TokenKind
): seq[T] =
  ##[
    Fetch matching nodes, maybe none.

    Returns a list of parse nodes, determined by the `parse_fn`. It can be empty
    only if the open token is missing, otherwise it will always return a non-empty
    list that begins with a lex token of `open_kind` and ends with a lex token of
    `close_kind`. Advances the parser to the next lex token after the closing token.
  ]##
  if not self.expectOptionalToken(openKind).isNil:
    var nodes: seq[T] = @[self.parseProc()]
    while self.expectOptionalToken(closeKind).isNil:
      nodes.add(self.parseProc())
    return nodes
  return @[]


proc manyNode[T](
  self: Parser,
  openKind: TokenKind,
  parseProc: proc(self: Parser): T,
  closeKind: TokenKind
): seq[T] =
  ##[
    Fetch matching nodes, at least one.

    Returns a non-empty list of parse nodes, determined by the `parse_fn`.
    This list begins with a lex token of `open_kind` and ends with a lex token of
    `close_kind`. Advances the parser to the next lex token after the closing token.
  ]##
  discard self.expectToken(openKind)
  var nodes: seq[T] = @[self.parseProc()]
  while self.expectOptionalToken(closeKind).isNil:
    nodes.add(self.parseProc())
  return nodes


# Implement the parsing rules in the base section


proc parseName(self: Parser): NameNode =
  ##[
    Convert a name lex token into a name parse node.
  ]##
  let token = self.expectToken(TokenKind.NAME)
  return NameNode(value: token.value, loc: self.loc(token))


# Implement the parsing rules in the Document section.


proc parseDocument(self: Parser): DocumentNode =
  ##[
    Document: Definition
  ]##
  let start = self.lexer.token
  return DocumentNode(
    definitions: self.manyNode[:DefinitionNode](
      TokenKind.SOF, parseDefinition, TokenKind.EOF
    ),
    loc: self.loc(start)
  )


proc parseDefinition(self: Parser): DefinitionNode =
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


proc parseOperationDefinition(self: Parser): OperationDefinitionNode =
  ##[
    Operation Definition
  ]##
  let start = self.lexer.token
  if self.peek(TokenKind.BRACE_L):
    return OperationDefinitionNode(
      operation: OperationTypeNode.QUERY,
      name: nil,
      variableDefinitions: @[],
      directives: @[],
      selectionSet: self.parseSelectionSet(),
      loc: self.loc(start)
    )
  let operation = self.parseOperationType()
  let name = if self.peek(TokenKind.NAME): self.parseName() else: nil
  return OperationDefinitionNode(
    operation: operation,
    name: name,
    variableDefinitions: self.parseVariableDefinitions(),
    directives: self.parseDirectives(false),
    selectionSet: self.parseSelectionSet(),
    loc: self.loc(start)
  )


proc parseOperationType(self: Parser): OperationTypeNode =
  ##[
    OperationType: one of query mutation subscription
  ]##
  let operationToken = self.expectToken(TokenKind.NAME)
  try:
    return parseEnum[OperationTypeNode](operationToken.value)
  except ValueError:
    raise self.unexpected(some(operationToken))


proc parseVariableDefinitions(self: Parser): seq[VariableDefinitionNode] =
  #[
    VariableDefinitions: (VariableDefinition+)
  ]#
  return self.optionalManyNode[:VariableDefinitionNode](
    TokenKind.PAREN_L,
    parseVariableDefinition,
    TokenKind.PAREN_R
  )


proc parseVariableDefinition(self: Parser): VariableDefinitionNode =
  ##[
    VariableDefinition: Variable: Type DefaultValue? Directives[Const]?
  ]##
  let start = self.lexer.token
  return VariableDefinitionNode(
    variable: self.parseVariable(),
    `type`: if not self.expectToken(TokenKind.COLON).isNil: self.parseTypeReference() else: nil,
    defaultValue: if not self.expectOptionalToken(TokenKind.EQUALS).isNil: self.parseValueLiteral(true) else: nil,
    directives: self.parseDirectives(true),
    loc: self.loc(start)
  )


proc parseVariable(self: Parser): ValueNode =
  ##[
    Variable: $Name
  ]##
  let start = self.lexer.token
  discard self.expectToken(TokenKind.DOLLAR)
  return ValueNode(
    kind: VariableNode,
    name: self.parseName(),
    loc: self.loc(start)
  )


proc parseSelectionSet(self: Parser): SelectionSetNode =
  ##[
    SelectionSet: {Selection+}
  ]##
  let start = self.lexer.token
  return SelectionSetNode(
    selections: self.manyNode[:SelectionNode](
      TokenKind.BRACE_L,
      parseSelection,
      TokenKind.BRACE_R
    ),
    loc: self.loc(start)
  )


proc parseSelection(self: Parser): SelectionNode =
  ##[
    Selection: Field or FragmentSpread or InlineFragment
  ]##
  return if self.peek(TokenKind.SPREAD): self.parseFragment() else: self.parseField()


proc parseField(self: Parser): SelectionNode =
  ##[
    Field: Alias? Name Arguments? Directives? SelectionSet?
  ]##
  let start = self.lexer.token
  let nameOrAlias = self.parseName()
  var alias: NameNode
  var name: NameNode
  if not self.expectOptionalToken(TokenKind.COLON).isNil:
    alias = nameOrAlias
    name = self.parseName()
  else:
    alias = nil
    name = nameOrAlias
  return SelectionNode(
    kind: FieldNode,
    alias: alias,
    name: name,
    arguments: self.parseArguments(false),
    directives: self.parseDirectives(false),
    selectionSet: if self.peek(TokenKind.BRACE_L): self.parseSelectionSet() else: nil,
    loc: self.loc(start)
  )


proc parseArguments(self: Parser, isConst: bool): seq[ArgumentNode] =
  ##[
    Arguments[Const]: (Argument[?Const]+)
  ]##
  let item = if isConst: parseConstArgument else: parseArgument
  return self.optionalManyNode[:ArgumentNode](
    TokenKind.PAREN_L,
    item,
    TokenKind.PAREN_R
  )

proc parseArgument(self: Parser): ArgumentNode =
  ##[
    Argument: Name : Value
  ]##
  let start = self.lexer.token
  let name = self.parseName()

  discard self.expectToken(TokenKind.COLON)
  return ArgumentNode(
    name: name,
    value: self.parseValueLiteral(false),
    loc: self.loc(start)
  )


proc parseConstArgument(self: Parser): ArgumentNode =
  ##[
    Argument[Const]: Name : Value[?Const]
  ]##
  let start = self.lexer.token
  let value = if not self.expectToken(TokenKind.COLON).isNil: self.parseValueLiteral(true) else: nil 
  return ArgumentNode(
    name: self.parseName(),
    value: value,
    loc: self.loc(start)
  )


# Implement the parsing rules in the Fragments section.


proc parseFragment(self: Parser): SelectionNode =
  ##[
    Corresponds to both FragmentSpread and InlineFragment in the spec.

    FragmentSpread: ... FragmentName Directives?
    InlineFragment: ... TypeCondition? Directives? SelectionSet
  ]##
  let start = self.lexer.token
  discard self.expectToken(TokenKind.SPREAD)

  let hasTypeCondition = self.expectOptionalKeyword("on")
  if not hasTypeCondition and self.peek(TokenKind.NAME):
    return SelectionNode(
      kind: FragmentSpreadNode,
      name: self.parseFragmentName(),
      directives: self.parseDirectives(false),
      loc: self.loc(start)
    )
  else: discard

  return SelectionNode(
    kind: InlineFragmentNode,
    typeCondition: if hasTypeCondition: self.parseNamedType() else: nil,
    directives: self.parseDirectives(false),
    selectionSet: self.parseSelectionSet(),
    loc: self.loc(start)
  )


proc parseFragmentDefinition(self: Parser): FragmentDefinitionNode =
  ##[
    FragmentDefinition
  ]##
  let start = self.lexer.token
  discard self.expectKeyword("fragment")
  ## Experimental support for defining variables within fragments changes
  ## the grammar of FragmentDefinition
  return FragmentDefinitionNode(
    name: self.parseFragmentName(),
    variableDefinitions: if self.experimentalFragmentVariables: self.parseVariableDefinitions() else: @[],
    typeCondition: self.parseTypeCondition(),
    directives: self.parseDirectives(false),
    selectionSet: self.parseSelectionSet(),
    loc: self.loc(start),
  )


proc parseFragmentName(self: Parser): NameNode =
  ##[
    FragmentName: Name but not `on`
  ]##
  if self.lexer.token.value == "on":
    raise self.unexpected()
  return self.parseName()


proc parseTypeCondition(self: Parser): TypeNode =
  ##[
    TypeCondition: NamedType
  ]##
  discard self.expectKeyword("on")
  return self.parseNamedType()


# Implement the parsing rules in the Values section.


proc parseValueLiteral(self: Parser, isConst: bool): ValueNode =
  let kind = self.lexer.token.kind
  case kind
  of TokenKind.BRACKET_L:
    return self.parseList(isConst)
  of TokenKind.BRACE_L:
    return self.parseObject(isConst)
  of TokenKind.INT:
    return self.parseInt(isConst)
  of TokenKind.FLOAT:
    return self.parseFloat(isConst)
  of TokenKind.STRING, TokenKind.BLOCK_STRING:
    return self.parseStringLiteral()
  of TokenKind.NAME:
    return self.parseNamedValues(isConst)
  of TokenKind.DOLLAR:
    return self.parseVariableValue(isConst)
  else: discard

  raise self.unexpected()


proc parseStringLiteral(self: Parser, isConst: bool = false): ValueNode =
  let token = self.lexer.token
  discard self.lexer.advance()
  return ValueNode(
    kind: StringValueNode,
    value: token.value,
    `block`: token.kind == TokenKind.BLOCK_STRING,
    loc: self.loc(token)
  )


proc parseList(self: Parser, isConst: bool): ValueNode =
  ##[
    ListValue[Const]
  ]##
  let start = self.lexer.token
  return ValueNode(
    kind: ListValueNode,
    values: self.anyNode[:ValueNode](
      TokenKind.BRACKET_L,
      partialValueLiteral(self, isConst),
      TokenKind.BRACKET_R
    ),
    loc: self.loc(start)
  )


proc parseObjectField(self: Parser, isConst: bool): ObjectFieldNode =
  let start = self.lexer.token
  let name = self.parseName()
  discard self.expectToken(TokenKind.COLON)

  return ObjectFieldNode(
    name: name,
    value: self.parseValueLiteral(isConst),
    loc: self.loc(start)
  )


proc parseObject(self: Parser, isConst: bool): ValueNode =
  ##[
    ObjectValue[Const]
  ]##
  let start = self.lexer.token
  return ValueNode(
    kind: ObjectValueNode,
    fields: self.anyNode[:ObjectFieldNode](
      TokenKind.BRACE_L,
      partialObjectField(self, isConst),
      TokenKind.BRACE_R
    ),
    loc: self.loc(start)
  )


proc parseInt(self: Parser, isConst: bool = false): ValueNode =
  let token = self.lexer.token
  discard self.lexer.advance()
  return ValueNode(
    kind: IntValueNode,
    strValue: token.value,
    loc: self.loc(token)
  )


proc parseFloat(self: Parser, isConst: bool = false): ValueNode =
  let token = self.lexer.token
  discard self.lexer.advance()
  return ValueNode(
    kind: FloatValueNode,
    strValue: token.value,
    loc: self.loc(token)
  )


proc parseNamedValues(self: Parser, isConst: bool = false): ValueNode =
  let
    token = self.lexer.token
    value = token.value
  discard self.lexer.advance()
  case value
  of "true":
    return ValueNode(kind: BooleanValueNode, boolValue: true, loc: self.loc(token))
  of "false":
    return ValueNode(kind: BooleanValueNode, boolValue: false, loc: self.loc(token))
  of "null":
    return ValueNode(kind: BooleanValueNode, loc: self.loc(token))
  else:
    return ValueNode(kind: EnumValueNode, strValue: value, loc: self.loc(token))


proc parseVariableValue(self: Parser, isConst: bool = false): ValueNode =
  if not isConst:
    return self.parseVariable()
  raise self.unexpected()


# Implement the parsing rules in the Directives section.


proc parseDirectives(self: Parser, isConst: bool): seq[DirectiveNode] =
  ##[
    Directives[Const]: Directive[?Const]+
  ]##
  var directives: seq[DirectiveNode]
  while self.peek(TokenKind.AT):
    directives.add(self.parseDirective(isConst))
  return directives


proc parseDirective(self: Parser, isConst: bool): DirectiveNode =
  ##[
    Directive[Const]: @ Name Arguments[?Const]?
  ]##
  let start = self.lexer.token
  discard self.expectToken(TokenKind.AT)
  return DirectiveNode(
    name: self.parseName(),
    arguments: self.parseArguments(isConst),
    loc: self.loc(start)
  )


# Implement the parsing rules in the Types section.


proc parseTypeReference(self: Parser): TypeNode =
  ##[
    Type: NamedType or ListType or NonNullType
  ]##
  let start = self.lexer.token
  var typeRef: TypeNode
  if not self.expectOptionalToken(TokenKind.BRACKET_L).isNil:
    typeRef = self.parseTypeReference()
    discard self.expectToken(TokenKind.BRACKET_R)
    typeRef = TypeNode(
      kind: ListTypeNode,
      `type`: typeRef,
      loc: self.loc(start)
    )
  else:
    typeRef = self.parseNamedType()
  if not self.expectOptionalToken(TokenKind.BANG).isNil:
    return TypeNode(
      kind: NonNullTypeNode,
      `type`: typeRef,
      loc: self.loc(start)
    )
  return typeRef
    

proc parseNamedType(self: Parser): TypeNode =
  ##[
    NamedType: Name
  ]##
  let start = self.lexer.token
  return TypeNode(
    kind: NamedTypeNode,
    name: self.parseName(),
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

proc parseTypeSystemDefinition(self: Parser): TypeSystemDefinitionNode =
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

proc parseTypeSystemExtension(self: Parser): TypeSystemExtensionNode =
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



proc parseDescription(self: Parser): ValueNode =
  ##[
    Description: StringValue
  ]##
  if self.peekDescription():
    return self.parseStringLiteral()
  return nil


proc parseSchemaDefinition(self: Parser): TypeSystemDefinitionNode =
  ##[
    SchemaDefinition
  ]##
  let
    start = self.lexer.token
    description = self.parseDescription()
  discard self.expectKeyword("schema")
  let
    directives = self.parseDirectives(true)
    operationTypes = self.manyNode[:OperationTypeDefinitionNode](
      TokenKind.BRACE_L,
      parseOperationTypeDefinition,
      TokenKind.BRACE_R
    )
  return TypeSystemDefinitionNode(
    kind: SchemaDefinitionNode,
    description: description,
    directives: directives,
    operationTypes: operationTypes,
    loc: self.loc(start)
  )


proc parseOperationTypeDefinition(self: Parser): OperationTypeDefinitionNode =
  ##[
    OperationTypeDefinition: OperationType : NamedType
  ]##
  let
    start = self.lexer.token
    operation = self.parseOperationType()
  discard self.expectToken(TokenKind.COLON)
  let typeRef = self.parseNamedType()
  return OperationTypeDefinitionNode(
    operation: operation,
    `type`: typeRef,
    loc: self.loc(start)
  )


proc parseScalarTypeDefinition(self: Parser): TypeSystemDefinitionNode =
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
  return TypeSystemDefinitionNode(
    kind: TypeDefinitionNode,
    tdKind: ScalarTypeDefinitionNode,
    description: description,
    name: name,
    directives: directives,
    loc: self.loc(start)
  )


proc parseObjectTypeDefinition(self: Parser): TypeSystemDefinitionNode =
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
  return TypeSystemDefinitionNode(
    kind: TypeDefinitionNode,
    tdKind: ObjectTypeDefinitionNode,
    description: description,
    name: name,
    interfaces: interfaces,
    directives: directives,
    fields: fields,
    loc: self.loc(start),
  )



proc parseImplementsInterfaces(self: Parser): seq[TypeNode] =
  ##[
    ImplementsInterfaces
  ]##
  var types: seq[TypeNode]
  if self.expectOptionalKeyword("implements"):
    # optional leading ampersand
    discard self.expectOptionalToken(TokenKind.AMP)
    while true:
      types.add(self.parseNamedType())
      if not self.expectOptionalToken(TokenKind.AMP).isNil:
        break
  return types


proc parseFieldsDefinition(self: Parser): seq[FieldDefinitionNode] =
  ##[
    FieldsDefinition: {FieldDefinition+}
  ]##
  return self.optionalManyNode[:FieldDefinitionNode](
    TokenKind.BRACE_L,
    parseFieldDefinition,
    TokenKind.BRACE_R
  )


proc parseFieldDefinition(self: Parser): FieldDefinitionNode =
  ##[
    FieldDefinition
  ]##
  let
    start = self.lexer.token
    description = self.parseDescription()
    name = self.parseName()
    args = self.parseArgumentDefs()
  discard self.expectToken(TokenKind.COLON)
  let
    typeRef = self.parseTypeReference()
    directives = self.parseDirectives(true)
  return FieldDefinitionNode(
    description: description,
    name: name,
    arguments: args,
    `type`: typeRef,
    directives: directives,
    loc: self.loc(start)
  )


proc parseArgumentDefs(self: Parser): seq[InputValueDefinitionNode] =
  ##[
    ArgumentsDefinition: (InputValueDefinition+)
  ]##
  return self.optionalManyNode[:InputValueDefinitionNode](
    TokenKind.PAREN_L,
    parseInputValueDef,
    TokenKind.PAREN_R
  )


proc parseInputValueDef(self: Parser): InputValueDefinitionNode =
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
    defaultValue = if not self.expectOptionalToken(TokenKind.EQUALS).isNil: self.parseValueLiteral(true) else: nil
    directives = self.parseDirectives(true)
  return InputValueDefinitionNode(
    description: description,
    name: name,
    `type`: typeRef,
    defaultValue: defaultValue,
    directives: directives,
    loc: self.loc(start)
  )


proc parseInterfaceTypeDefinition(self: Parser): TypeSystemDefinitionNode =
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
  return TypeSystemDefinitionNode(
    kind: TypeDefinitionNode,
    tdKind: InterfaceTypeDefinitionNode,
    description: description,
    name: name,
    interfaces: interfaces,
    directives: directives,
    fields: fields,
    loc: self.loc(start)
  )


proc parseUnionTypeDefinition(self: Parser): TypeSystemDefinitionNode =
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
  return TypeSystemDefinitionNode(
    kind: TypeDefinitionNode,
    tdKind: UnionTypeDefinitionNode,
    description: description,
    name: name,
    directives: directives,
    types: types,
    loc: self.loc(start)
  )


proc parseUnionMemberTypes(self: Parser): seq[TypeNode] =
  ##[
    UnionMemberTypes
  ]##
  var types: seq[TypeNode]
  if not self.expectOptionalToken(TokenKind.EQUALS).isNil:
    # optional leading pipe
    discard self.expectOptionalToken(TokenKind.PIPE)
  while true:
    types.add(self.parseNamedType)
    if not self.expectOptionalToken(TokenKind.PIPE).isNil:
      break
  return types


proc parseEnumTypeDefinition(self: Parser): TypeSystemDefinitionNode =
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
  return TypeSystemDefinitionNode(
    kind: TypeDefinitionNode,
    tdKind: EnumTypeDefinitionNode,
    description: description,
    name: name,
    directives: directives,
    values: values,
    loc: self.loc(start)
  )


proc parseEnumValuesDefinition(self: Parser): seq[EnumValueDefinitionNode] =
  ##[
    EnumValuesDefinition: {EnumValueDefinition+}
  ]##
  return self.optionalManyNode[:EnumValueDefinitionNode](
    TokenKind.BRACE_L,
    parseEnumValueDefinition,
    TokenKind.BRACE_R
  )


proc parseEnumValueDefinition(self: Parser): EnumValueDefinitionNode =
  ##[
    EnumValueDefinition: Description? EnumValue Directives[Const]?
  ]##
  let
    start = self.lexer.token
    description = self.parseDescription()
    name = self.parseName()
    directives = self.parseDirectives(true)
  return EnumValueDefinitionNode(
    description: description,
    name: name,
    directives: directives,
    loc: self.loc(start)
  )


proc parseInputObjectTypeDefinition(self: Parser): TypeSystemDefinitionNode =
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
  return TypeSystemDefinitionNode(
    kind: TypeDefinitionNode,
    tdKind: InputObjectTypeDefinitionNode,
    description: description,
    name: name,
    directives: directives,
    fieldsDef: fields,
    loc: self.loc(start)
  )


proc parseInputFieldsDefinition(self: Parser): seq[InputValueDefinitionNode] =
  ##[
    InputFieldsDefinition: {InputValueDefinition+}
  ]##
  return self.optionalManyNode[:InputValueDefinitionNode](
    TokenKind.BRACE_L,
    parseInputValueDef,
    TokenKind.BRACE_R
  )


proc parseSchemaExtension(self: Parser): SchemaExtensionNode =
  ##[
    SchemaExtension
  ]##
  let start = self.lexer.token
  discard self.expectKeyword("extend")
  discard self.expectKeyword("schema")
  let directives = self.parseDirectives(true)
  let operationTypes = self.optionalManyNode[:OperationTypeDefinitionNode](
    TokenKind.BRACE_L,
    parseOperationTypeDefinition,
    TokenKind.BRACE_R
  )
  if directives.len == 0 and operationTypes.len == 0:
    raise self.unexpected()
  return SchemaExtensionNode(
    directives: directives,
    operationTypes: operationTypes,
    loc: self.loc(start)
  )


proc parseScalarTypeExtension(self: Parser): ScalarTypeExtensionNode =
  ##[
    ScalarTypeExtension
  ]##
  let start = self.lexer.token
  discard self.expectKeyword("extends")
  discard self.expectKeyword("scalar")
  let
    name = self.parseName()
    directives = self.parseDirectives(true)
  if directives.len == 0:
    raise self.unexpected()
  return ScalarTypeExtensionNode(
    name: name,
    directives: directives,
    loc: self.loc(start)
  )


proc parseObjectTypeExtension(self: Parser): ObjectTypeExtensionNode =
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
  if (interfaces.len == 0 or directives.len == 0 or fields.len == 0):
    raise self.unexpected()
  return ObjectTypeExtensionNode(
    name: name,
    interfaces: interfaces,
    directives: directives,
    fields: fields,
    loc: self.loc(start)
  )


proc parseInterfaceTypeExtension(self: Parser): InterfaceTypeExtensionNode =
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
  if (interfaces.len == 0 or directives.len == 0 or fields.len == 0):
    raise self.unexpected()
  return InterfaceTypeExtensionNode(
    name: name,
    interfaces: interfaces,
    directives: directives,
    fields: fields,
    loc: self.loc(start)
  )


proc parseUnionTypeExtension(self: Parser): UnionTypeExtensionNode =
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
  if (directives.len == 0 or types.len == 0):
    raise self.unexpected()
  return UnionTypeExtensionNode(
    name: name,
    directives: directives,
    types: types,
    loc: self.loc(start)
  )


proc parseEnumTypeExtension(self: Parser): EnumTypeExtensionNode =
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
  if (directives.len == 0 or values.len == 0):
    raise self.unexpected()
  return EnumTypeExtensionNode(
    name: name,
    directives: directives,
    values: values,
    loc: self.loc(start)
  )


proc parseInputObjectTypeExtension(self: Parser): InputObjectTypeExtensionNode =
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
  if (directives.len == 0 or fields.len == 0):
    raise self.unexpected()
  return InputObjectTypeExtensionNode(
    name: name,
    directives: directives,
    fields: fields,
    loc: self.loc(start)
  )


proc parseDirectiveDefinition(self: Parser): TypeSystemDefinitionNode =
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
  return TypeSystemDefinitionNode(
    kind: DirectiveDefinitionNode,
    description: description,
    name: name,
    arguments: args,
    repeatable: repeatable,
    locations: locations,
    loc: self.loc(start)
  )


proc parseDirectiveLocations(self: Parser): seq[NameNode] =
  ##[
    DirectiveLocations
  ]##
  # optional leading pipe
  discard self.expectOptionalToken(TokenKind.PIPE)
  var locations: seq[NameNode]
  while true:
    locations.add(self.parseDirectiveLocation())
    if self.expectOptionalToken(TokenKind.PIPE).isNil:
      break
  return locations


proc parseDirectiveLocation(self: Parser): NameNode =
  ##[
    DirectiveLocation
  ]##
  let
    start = self.lexer.token
    name = self.parseName()
  if isDirectiveLocation(name.value):
    return name
  
  raise self.unexpected(some(start))

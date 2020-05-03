## AST Module
import strformat

import source_location
import token_kind

type Token* = ref object
  ##[
    AST Token

    Represents a range of characters 
    represented by a lexical token within a Source.
  ]##
  kind*: TokenKind ## the kind of token
  start*: int ## the character offset at which this Node begins
  `end`*: int ## the character offset at which this Node ends
  line*: int ## the 1-indexed line number on which this Token appears
  column*: int ## the 1-indexed column number at which this Token begins
  value*: string ## For non-punctuation tokens, represents the interpreted value of the token.
  ## Tokens exist as nodes in a double-linked-list amongst all tokens including
  ## ignored tokens. <SOF> is always the first node and <EOF> the last.
  prev*: Token
  next*: Token


proc newToken*(
  kind: TokenKind,
  start: int,
  `end`: int,
  line: int,
  column: int,
  prev: Token = nil,
  value: string = ""
): Token =
  result = Token(
    kind: kind,
    start: start,
    `end`: `end`,
    line: line,
    column: column,
    value: value,
    prev: prev,
    next: nil,
  )


proc desc*(self: Token): string =
  ##[
    A helper property to describe a token as a string for debugging"""
  ]##
  let
    kind = $(self.kind)
    value = self.value
  result = if value.len > 0: fmt"{kind} '{value}'" else: kind


proc `$`*(self: Token): string =
  result = fmt"<Token {self.desc} {self.line}:{self.column}>"


type Location* = ref object
  ##[
    AST Location

    Contains a range of UTF-8 character offsets and token references that identify the
    region of the source from which the AST derived.
  ]##
  start*: int ## character offset at which this Node begins
  `end`*: int ## character offset at which this Node ends
  startToken*: Token ## Token at which this Node begins
  endToken*: Token ## Token at which this Node ends.
  source*: Source ## Source document the AST represents


proc `==`*(a, b: Location): bool =
  result = a.start == b.start
  result = a.`end` == b.`end`


proc `==`*(a: Location, b: tuple[start: int, `end`: int]): bool =
  result = a.start == b[0]
  result = a.`end` == b[1]


proc `$`*(self: Location): string =
  result = "{" & fmt" start: {self.start}, end: {self.`end`} " & "}"


proc desc*(self: Location): string =
  result = fmt"<Location {self.start}:{self.`end`}>"


proc newLocation*(
  startToken: Token,
  endToken: Token,
  source: Source
): Location =
  result = Location(
    start: startToken.start,
    `end`: endToken.`end`,
    startToken: startToken,
    endToken: endToken,
    source: source,
  )

# type
#   NodeKind = enum
#     ## GraphQL AST node kinds according to the 
#     ## GraphQL spec query language:
#     ## https://spec.graphql.org/draft/#sec-Language
#     NameNode
#     DocumentNode
#     DefinitionNode
#   OperationTypeNode = enum
#     ## There are three types of operations that GraphQL models
#     ## - query: a read‐only fetch.
#     ## - mutation: a write followed by a fetch.
#     ## - subscription: a long‐lived request that fetches 
#     ##   data in response to source events.
#     ## https://spec.graphql.org/draft/#sec-Language.Operations
#     QUERY = "query"
#     MUTATION = "mutation"
#     SUBSCRIPTION = "subscription"
  #   DefinitionNode
  #   VariableDefinitionNode
  # DefinitionKind = enum
  #   ## GraphQL Document definition kinds.
  #   ## https://spec.graphql.org/draft/#sec-Document
  #   ExecutableDefinitionNode
  #   TypeSystemDefinitionNode
  #   TypeSystemExtensionNode
  # ExecutableDefinitionKind = enum
  #   OperationDefinitionNode
  #   FragmentDefinitionNode
  # TypeSystemDefinitionKind = enum
  #   SchemaDefinitionNode
  #   TypeDefinitionNode
  #   DirectiveDefinitionNode
  # TypeDefinitionKind = enum
  #   ScalarTypeDefinitionNode
  #   ObjectTypeDefinitionNode
  #   InterfaceTypeDefinitionNode
  #   UnionTypeDefinitionNode
  #   EnumTypeDefinitionNode
  #   InputObjectTypeDefinitionNode
  # TypeSystemExtensionKind = enum
  #   SchemaExtensionNode
  #   TypeExtensionNode
  # TypeExtensionKind = enum
  #   ScalarTypeExtensionNode
  #   ObjectTypeExtensionNode
  #   InterfaceTypeExtensionNode
  #   UnionTypeExtensionNode
  #   EnumTypeExtensionNode
  #   InputObjectTypeExtensionNode
    # OperationDefinitionNode
    # VariableDefinitionNode
    # VariableNode
    # SelectionSetNode
    # FieldNode
    # ArgumentNode
    # FragmentSpreadNode
    # InlineFragmentNode
    # FragmentDefinitionNode
    # IntValueNode
    # FloatValueNode
    # StringValueNode
    # BooleanValueNode
    # NullValueNode
    # EnumValueNode
    # ListValueNode
    # ObjectValueNode
    # ObjectFieldNode
    # DirectiveNode
    # NamedTypeNode
    # ListTypeNode
    # NonNullTypeNode
    # SchemaDefinitionNode
    # OperationTypeDefinitionNode
    # ScalarTypeDefinitionNode
    # ObjectTypeDefinitionNode
    # FieldDefinitionNode
    # InputValueDefinitionNode
    # InterfaceTypeDefinitionNode
    # UnionTypeDefinitionNode
    # EnumTypeDefinitionNode
    # EnumValueDefinitionNode
    # InputObjectTypeDefinitionNode
    # DirectiveDefinitionNode
    # SchemaExtensionNode
    # ScalarTypeExtensionNode
    # ObjectTypeExtensionNode
    # InterfaceTypeExtensionNode
    # UnionTypeExtensionNode
    # EnumTypeExtensionNode
    # InputObjectTypeExtensionNode
  # Node = ref object
  #   loc*: Location
  #   case kind: NodeKind
  #   of NameNode:
  #     value: string
  #   of DefinitionNode:
  #     discard
  #   of DocumentNode:
  #     definitions: seq[Node]


type
  ## Base AST Node
  Node* = ref object of RootObj
    loc*: Location

  ## Name Section
  ## https://spec.graphql.org/draft/#sec-Names
  NameNode* = ref object of Node
    value*: string

  ## Input Values Section
  ## https://spec.graphql.org/draft/#sec-Input-Values
  ObjectFieldNode* = ref object of Node
    name*: NameNode
    value*: ValueNode

  ValueNodeKind* = enum
    VariableNode
    IntValueNode
    FloatValueNode
    StringValueNode
    BooleanValueNode
    NullValueNode
    EnumValueNode
    ListValueNode
    ObjectValueNode
  
  ValueNode* = ref object of Node
    case kind*: ValueNodeKind
    of VariableNode:
      name*: NameNode
    of IntValueNode, FloatValueNode, EnumValueNode:
      strValue*: string
    of StringValueNode:
      value*: string
      `block`*: bool
    of BooleanValueNode:
      boolValue*: bool
    of NullValueNode:
      discard
    of ListValueNode:
      values*: seq[ValueNode]
    of ObjectValueNode:
      fields*: seq[ObjectFieldNode]

  ## Document Section
  ## https://spec.graphql.org/draft/#sec-Document
  DefinitionNode* = ref object of Node
  
  DocumentNode* = ref object of Node ## Document AST Node
    definitions*: seq[DefinitionNode]

  ArgumentNode* = ref object of Node
    name*: NameNode
    value*: ValueNode

  OperationTypeNode* = enum
    ## There are three types of operations that GraphQL models
    ## - query: a read‐only fetch.
    ## - mutation: a write followed by a fetch.
    ## - subscription: a long‐lived request that fetches 
    ##   data in response to source events.
    ## https://spec.graphql.org/draft/#sec-Language.Operations
    QUERY = "query"
    MUTATION = "mutation"
    SUBSCRIPTION = "subscription"

  ## Selection Sets Section
  ## https://spec.graphql.org/draft/#sec-Selection-Sets
  SelectionSetNode* = ref object of Node
    selections*: seq[SelectionNode]

  SelectionNodeKind* = enum
    FieldNode
    FragmentSpreadNode
    InlineFragmentNode

  SelectionNode* = ref object of Node
    name*: NameNode ## Does not exist on InlineFragmentNode
    directives*: seq[DirectiveNode]
    selectionSet*: SelectionSetNode ## Does not exist on FragmentSpreadNode
    case kind*: SelectionNodeKind
    of FieldNode:
      ## https://spec.graphql.org/draft/#sec-Language.Fields
      alias*: NameNode
      arguments*: seq[ArgumentNode]
    of FragmentSpreadNode:
      ## https://spec.graphql.org/draft/#sec-Language.Fragments
      discard
    of InlineFragmentNode:
      ## https://spec.graphql.org/draft/#sec-Inline-Fragments
      typeCondition*: TypeNode


  ## Directives Section
  ## https://spec.graphql.org/draft/#sec-Language.Directives
  DirectiveNode* = ref object of Node
    name*: NameNode
    arguments*: seq[ArgumentNode]

  ## Type References Section
  ## https://spec.graphql.org/draft/#sec-Type-References
  TypeNodeKind* = enum
    NamedTypeNode
    ListTypeNode
    NonNullTypeNode
  TypeNode* = ref object of Node
    case kind*: TypeNodeKind
    of NamedTypeNode:
      name*: NameNode
    of ListTypeNode, NonNullTypeNode:
      `type`*: TypeNode

  ## Objects Section
  ## https://spec.graphql.org/draft/#sec-Objects
  FieldDefinitionNode* = ref object of Node
    description*: ValueNode
    name*: NameNode
    directives*: seq[DirectiveNode]
    arguments*: seq[InputValueDefinitionNode]
    `type`*: TypeNode
  
  ## Field Arguments Section
  ## https://spec.graphql.org/draft/#sec-Field-Arguments
  InputValueDefinitionNode* = ref object of Node
    description*: ValueNode
    name*: NameNode
    directives*: seq[DirectiveNode]
    `type`*: TypeNode
    defaultValue*: ValueNode

  ## Type Definition Section
  ## https://spec.graphql.org/draft/#sec-Types
  TypeDefinitionNodeKind* = enum
    ScalarTypeDefinitionNode
    ObjectTypeDefinitionNode
    InterfaceTypeDefinitionNode
    UnionTypeDefinitionNode
    EnumTypeDefinitionNode
    InputObjectTypeDefinitionNode

  ## Enum Section
  ## https://spec.graphql.org/draft/#sec-Enums
  EnumValueDefinitionNode* = ref object of Node
    name*: NameNode
    description*: ValueNode
    directives*: seq[DirectiveNode]

  ## Type System Definition Section
  ## https://spec.graphql.org/draft/#sec-Type-System
  OperationTypeDefinitionNode* = ref object of Node
    operation*: OperationTypeNode
    `type`*: TypeNode

  TypeSystemDefinitionNodeKind* = enum
    SchemaDefinitionNode
    TypeDefinitionNode
    DirectiveDefinitionNode

  TypeSystemDefinitionNode* = ref object of DefinitionNode
    description*: ValueNode
    name*: NameNode ## Does not exist on SchemaDefinitionNode
    directives*: seq[DirectiveNode] ## Does not exist on DirectiveDefinitionNode
    case kind*: TypeSystemDefinitionNodeKind
    of SchemaDefinitionNode:
      ## https://spec.graphql.org/draft/#sec-Schema
      operationTypes*: seq[OperationTypeDefinitionNode]
    of TypeDefinitionNode:
      ## https://spec.graphql.org/draft/#sec-Types
      case tdKind*: TypeDefinitionNodeKind
      of ScalarTypeDefinitionNode:
        discard
      of ObjectTypeDefinitionNode, InterfaceTypeDefinitionNode:
        interfaces*: seq[TypeNode]
        fields*: seq[FieldDefinitionNode]
      of UnionTypeDefinitionNode:
        types*: seq[TypeNode]
      of EnumTypeDefinitionNode:
        values*: seq[EnumValueDefinitionNode]
      of InputObjectTypeDefinitionNode:
        fieldsDef*: seq[InputValueDefinitionNode]

    of DirectiveDefinitionNode:
      ## https://spec.graphql.org/draft/#sec-Type-System.Directives
      arguments*: seq[InputValueDefinitionNode]
      repeatable*: bool
      locations*: seq[NameNode]


type
  # ----- Forward declarations -----
  # Document
  # DefinitionNode* = ref object of Node ## Base Definition Node
  # Values
  # ValueNode* = ref object of Node 
  # VariableNode* = ref object of ValueNode
  #   name*: NameNode
  # # Document
  # ArgumentNode* = ref object of Node ## Argument AST Node
  #   name*: NameNode
  #   value*: ValueNode
  # Directives
  # DirectiveNode* = ref object of Node ## Directive AST Node
  #   name*: NameNode
  #   arguments*: seq[ArgumentNode]
  ##[
    Additional type to simulate union type like in Python
    or NamedTypeNode | ListTypeNode in TypeScript
    Check the link below for the reasoning behind this:
    https://forum.nim-lang.org/t/4406#27516
  ]##
  # NonNullablesTypes* = ref object of TypeNode
  # NamedTypeNode* = ref object of NonNullablesTypes
  #   name*: NameNode
  # ----- Normal Declarations -----
  # Document
  # DocumentNode* = ref object of Node ## Document AST Node
  #   definitions*: seq[DefinitionNode]
  VariableDefinitionNode* = ref object of Node
    variable*: ValueNode
    `type`*: TypeNode
    defaultValue*: ValueNode
    directives*: seq[DirectiveNode]
  # SelectionNode* = ref object of Node
  #   directives*: seq[DirectiveNode]
  # SelectionSetNode* = ref object of Node
  #   selections*: seq[SelectionNode]
  ExecutableDefinitionNode* = ref object of DefinitionNode
    name*: NameNode
    directives*: seq[DirectiveNode]
    variableDefinitions*: seq[VariableDefinitionNode]
    selectionSet*: SelectionSetNode
  # OperationTypeNode* = enum
  #   ##[
  #     Operation Type Enum for most operation definition
  #     node types.
  #   ]##
  #   QUERY = "query"
  #   MUTATION = "mutation"
  #   SUBSCRIPTION = "subscription"
  OperationDefinitionNode* = ref object of ExecutableDefinitionNode
    operation*: OperationTypeNode
  # FieldNode* = ref object of SelectionNode
  #   alias*: NameNode
  #   name*: NameNode
  #   arguments*: seq[ArgumentNode]
  #   selectionSet*: SelectionSetNode
  # # Fragments
  # FragmentSpreadNode* = ref object of SelectionNode
  #   name*: NameNode
  # InlineFragmentNode* = ref object of SelectionNode
  #   typeCondition*: TypeNode
  #   selectionSet*: SelectionSetNode
  FragmentDefinitionNode* = ref object of ExecutableDefinitionNode
    typeCondition*: TypeNode
  # Values
  # IntValueNode* = ref object of ValueNode
  #   value*: string
  # FloatValueNode* = ref object of ValueNode
  #   value*: string
  # StringValueNode* = ref object of ValueNode
  #   value*: string
  #   `block`*: bool
  # BooleanValueNode* = ref object of ValueNode
  #   value*: bool
  # NullValueNode* = ref object of ValueNode
  # EnumValueNode* = ref object of ValueNode
  #   value*: string
  # ListValueNode* = ref object of ValueNode
  #   values*: seq[ValueNode]
  # ObjectFieldNode* = ref object of ValueNode
  #   name*: NameNode
  #   value*: ValueNode
  # ObjectValueNode* = ref object of ValueNode
  #   fields*: seq[ObjectFieldNode]
  # Type Reference
  # ListTypeNode* = ref object of NonNullablesTypes
  #   `type`*: TypeNode
  # NonNullTypeNode* = ref object of TypeNode
  #   `type`*: NonNullablesTypes
  # Type System Definition
  # TypeSystemDefinitionNode* = ref object of DefinitionNode
  # OperationTypeDefinitionNode* = ref object of Node
  #   operation*: OperationTypeNode
  #   `type`*: TypeNode
  # SchemaDefinitionNode* = ref object of TypeSystemDefinitionNode
  #   description*: ValueNode
  #   directives*: seq[DirectiveNode]
  #   operationTypes*: seq[OperationTypeDefinitionNode]
  # Type Definition
  # TypeDefinitionNode* = ref object of TypeSystemDefinitionNode
  #   description*: ValueNode
  #   directives*: seq[DirectiveNode]
  #   name*: NameNode
  # ScalarTypeDefinitionNode* = ref object of TypeDefinitionNode
  # InputValueDefinitionNode* = ref object of DefinitionNode
  #   description*: ValueNode
  #   name*: NameNode
  #   directives*: seq[DirectiveNode]
  #   `type`*: TypeNode
  #   defaultValue*: ValueNode
  # FieldDefinitionNode* = ref object of DefinitionNode
  #   description*: ValueNode
  #   name*: NameNode
  #   directives*: seq[DirectiveNode]
  #   arguments*: seq[InputValueDefinitionNode]
  #   `type`*: TypeNode
  # ObjectTypeDefinitionNode* = ref object of TypeDefinitionNode
  #   interfaces*: seq[TypeNode]
  #   fields*: seq[FieldDefinitionNode]
  # InterfaceTypeDefinitionNode* = ref object of TypeDefinitionNode
  #   fields*: seq[FieldDefinitionNode]
  #   interfaces*: seq[TypeNode]
  # UnionTypeDefinitionNode* = ref object of TypeDefinitionNode
  #   types*: seq[TypeNode]
  # EnumValueDefinitionNode* = ref object of TypeDefinitionNode
  # EnumTypeDefinitionNode* = ref object of TypeDefinitionNode
  #   values*: seq[EnumValueDefinitionNode]
  # InputObjectTypeDefinitionNode* = ref object of TypeDefinitionNode
  #   fields*: seq[InputValueDefinitionNode]
  # Directive Definitions
  # DirectiveDefinitionNode* = ref object of TypeSystemDefinitionNode
  #   description*: ValueNode
  #   name*: NameNode
  #   arguments*: seq[InputValueDefinitionNode]
  #   repeatable*: bool
  #   locations*: seq[NameNode]
  ##[
    Additional type to simulate union type
    SchemaExtensionNode | TypeExtensionNode
  ]##
  TypeSystemExtensionNode* = ref object of DefinitionNode
    directives*: seq[DirectiveNode]
  # Type System Extensions
  SchemaExtensionNode* = ref object of TypeSystemExtensionNode
    operationTypes*: seq[OperationTypeDefinitionNode]
  # Type Extensions
  TypeExtensionNode* = ref object of TypeSystemExtensionNode
    name*: NameNode
  ScalarTypeExtensionNode* = ref object of TypeExtensionNode
  ObjectTypeExtensionNode* = ref object of TypeExtensionNode
    interfaces*: seq[TypeNode]
    fields*: seq[FieldDefinitionNode]
  InterfaceTypeExtensionNode* = ref object of TypeExtensionNode
    interfaces*: seq[TypeNode]
    fields*: seq[FieldDefinitionNode]
  UnionTypeExtensionNode* = ref object of TypeExtensionNode
    types*: seq[TypeNode]
  EnumTypeExtensionNode* = ref object of TypeExtensionNode
    values*: seq[EnumValueDefinitionNode]
  InputObjectTypeExtensionNode* = ref object of TypeExtensionNode
    fields*: seq[InputValueDefinitionNode]
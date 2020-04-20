## AST Module
import source_location
import token_kind

type Token* = ref object
  #[
    AST Token

    Represents a range of characters 
    represented by a lexical token within a Source.
  ]#
  kind*: TokenKind ## the kind of token
  start*: int ## the character offset at which this Node begins
  `end`*: int ## the character offset at which this Node ends
  line*: int ## the 1-indexed line number on which this Token appears
  column*: int ## the 1-indexed column number at which this Token begins
  value*: string ## For non-punctuation tokens, represents the interpreted value of the token.
  #[
    Tokens exist as nodes in a double-linked-list amongst all tokens including
    ignored tokens. <SOF> is always the first node and <EOF> the last.
  ]#
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


type Location* = ref object
  #[
    AST Location

    Contains a range of UTF-8 character offsets and token references that identify the
    region of the source from which the AST derived.
  ]#
  start*: int ## character offset at which this Node begins
  `end`*: int ## character offset at which this Node ends
  startToken*: Token ## Token at which this Node begins
  endToken*: Token ## Token at which this Node ends.
  source*: Source ## Source document the AST represents


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


## THIS IS INHERITANCE APPROACH

#[
  TODO NOTE:
  To ensure array / seq immutability

  1. make it private
  2. create a getter and setter
  3. (rename fields to some other name, perhaps pFields) then make procs "fields" and "`fields=`"
  signature: proc fields(self: MyChildClass): seq[OtherType]
  -> Dont actually make the 2nd proc to make it immutable
  5. now it's immutable for any module that imports it
  but you cant guarantee immutability within the same file

  The rename part is just to ensure the field name doesnt clash with the proc name
]#

type
  Node* = ref object of RootObj ## Base AST Node
    loc*: Location
  # Name
  NameNode* = ref object of Node ## Name AST Node
    value*: string
  # ----- Forward declarations -----
  # Document
  DefinitionNode* = ref object of Node ## Base Definition Node
  # Values
  ValueNode* = ref object of Node 
  VariableNode* = ref object of ValueNode
    name*: NameNode
  # Document
  ArgumentNode* = ref object of Node ## Argument AST Node
    name*: NameNode
    value*: ValueNode
  # Directives
  DirectiveNode* = ref object of Node ## Directive AST Node
    name*: NameNode
    arguments*: seq[ArgumentNode]
  # Type Reference
  TypeNode* = ref object of Node

  ##[
    Additional type to simulate union type like in Python
    or NamedTypeNode | ListTypeNode in TypeScript
    Check the link below for the reasoning behind this:
    https://forum.nim-lang.org/t/4406#27516
  ]##
  NonNullablesTypes* = ref object of TypeNode
  NamedTypeNode* = ref object of NonNullablesTypes
    name*: NameNode
  # ----- Normal Declarations -----
  # Document
  DocumentNode* = ref object of Node ## Document AST Node
    definitions*: seq[DefinitionNode]
  VariableDefinitionNode* = ref object of Node
    variable*: VariableNode
    `type`*: TypeNode
    defaultValue*: ValueNode
    directives*: seq[DirectiveNode]
  SelectionNode* = ref object of Node
    directives*: seq[DirectiveNode]
  SelectionSetNode* = ref object of Node
    selections*: seq[SelectionNode]
  ExecutableDefinitionNode* = ref object of DefinitionNode
    name*: NameNode
    directives*: seq[DirectiveNode]
    variableDefinitions*: seq[VariableDefinitionNode]
    selectionSet*: SelectionSetNode
  OperationTypeNode* = enum
    ##[
      Operation Type Enum for most operation definition
      node types.
    ]##
    QUERY = "query"
    MUTATION = "mutation"
    SUBSCRIPTION = "subscription"
  OperationDefinitionNode* = ref object of ExecutableDefinitionNode
    operation*: OperationTypeNode
  FieldNode* = ref object of SelectionNode
    alias*: NameNode
    name*: NameNode
    arguments*: seq[ArgumentNode]
    selectionSet*: SelectionSetNode
  # Fragments
  FragmentSpreadNode* = ref object of SelectionNode
    name*: NameNode
  InlineFragmentNode* = ref object of SelectionNode
    typeCondition*: NamedTypeNode
    selectionSet*: SelectionSetNode
  FragmentDefinitionNode* = ref object of ExecutableDefinitionNode
    typeCondition*: NamedTypeNode
  # Values
  IntValueNode* = ref object of ValueNode
    value*: string
  FloatValueNode* = ref object of ValueNode
    value*: string
  StringValueNode* = ref object of ValueNode
    value*: string
    `block`*: bool
  BooleanValueNode* = ref object of ValueNode
    value*: bool
  NullValueNode* = ref object of ValueNode
  EnumValueNode* = ref object of ValueNode
    value*: string
  ListValueNode* = ref object of ValueNode
    values*: seq[ValueNode]
  ObjectFieldNode* = ref object of ValueNode
    name*: NameNode
    value*: ValueNode
  ObjectValueNode* = ref object of ValueNode
    fields*: seq[ObjectFieldNode]
  # Type Reference
  ListTypeNode* = ref object of NonNullablesTypes
    `type`*: TypeNode
  NonNullTypeNode* = ref object of TypeNode
    `type`*: NonNullablesTypes
  # Type System Definition
  TypeSystemDefinitionNode* = ref object of DefinitionNode
  OperationTypeDefinitionNode* = ref object of Node
    operation*: OperationTypeNode
    `type`*: NamedTypeNode
  SchemaDefinitionNode* = ref object of TypeSystemDefinitionNode
    description*: StringValueNode
    directives*: seq[DirectiveNode]
    operationTypes*: seq[OperationTypeDefinitionNode]
  # Type Definition
  TypeDefinitionNode* = ref object of TypeSystemDefinitionNode
    description*: StringValueNode
    directives*: seq[DirectiveNode]
    name*: NameNode
  ScalarTypeDefinitionNode* = ref object of TypeDefinitionNode
  InputValueDefinitionNode* = ref object of DefinitionNode
    description*: StringValueNode
    name*: NameNode
    directives*: seq[DirectiveNode]
    `type`*: TypeNode
    defaultValue*: ValueNode
  FieldDefinitionNode* = ref object of DefinitionNode
    description*: StringValueNode
    name*: NameNode
    directives*: seq[DirectiveNode]
    arguments*: seq[InputValueDefinitionNode]
    `type`*: TypeNode
  ObjectTypeDefinitionNode* = ref object of TypeDefinitionNode
    interfaces*: seq[NamedTypeNode]
    fields*: seq[FieldDefinitionNode]
  InterfaceTypeDefinitionNode* = ref object of TypeDefinitionNode
    fields*: seq[FieldDefinitionNode]
    interfaces*: seq[NamedTypeNode]
  UnionTypeDefinitionNode* = ref object of TypeDefinitionNode
    types*: seq[NamedTypeNode]
  EnumValueDefinitionNode* = ref object of TypeDefinitionNode
  EnumTypeDefinitionNode* = ref object of TypeDefinitionNode
    values*: seq[EnumValueDefinitionNode]
  InputObjectTypeDefinitionNode* = ref object of TypeDefinitionNode
    fields*: seq[InputValueDefinitionNode]
  # Directive Definitions
  DirectiveDefinitionNode* = ref object of TypeSystemDefinitionNode
    description*: StringValueNode
    name*: NameNode
    arguments*: seq[InputValueDefinitionNode]
    repeatable*: bool
    locations*: seq[NameNode]
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
    interfaces*: seq[NamedTypeNode]
    fields*: seq[FieldDefinitionNode]
  InterfaceTypeExtensionNode* = ref object of TypeExtensionNode
    interfaces*: seq[NamedTypeNode]
    fields*: seq[FieldDefinitionNode]
  UnionTypeExtensionNode* = ref object of TypeExtensionNode
    types*: seq[NamedTypeNode]
  EnumTypeExtensionNode* = ref object of TypeExtensionNode
    values*: seq[EnumValueDefinitionNode]
  InputObjectTypeExtensionNode* = ref object of TypeExtensionNode
    fields*: seq[InputValueDefinitionNode]
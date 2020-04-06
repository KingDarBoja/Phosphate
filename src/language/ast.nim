## AST Module
from source_location import Source
from token_kind import TokenKind

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

proc initToken*(
  kind: TokenKind,
  start: int,
  `end`: int,
  line: int,
  column: int,
  prev: Token = nil,
  value: string = ""
): Token =
  new(result)
  result.kind = kind
  result.start = start
  result.`end` = `end`
  result.line = line
  result.column = column
  result.value = value
  result.prev = prev
  result.next = nil

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

proc initLocation*(
  startToken: Token,
  endToken: Token,
  source: Source
): Location =
  new(result)
  result.start = startToken.start
  result.`end` = endToken.`end`
  result.startToken = startToken
  result.endToken = endToken
  result.source = source

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

type Node* = ref object of RootObj ## Base AST Node
  loc*: Location

# Name

type NameNode* = ref object of Node ## Name AST Node
  value*: string

## Forward declarations

# Document
type DefinitionNode* = ref object of Node ## Base Definition Node

# Values
type ValueNode* = ref object of Node 

type VariableNode* = ref object of ValueNode
  name*: NameNode

# Document
type ArgumentNode* = ref object of Node ## Argument AST Node
  name*: NameNode
  value*: ValueNode

# Directives
type DirectiveNode* = ref object of Node ## Directive AST Node
  name*: NameNode
  arguments*: seq[ArgumentNode]

# Type Reference
type TypeNode* = ref object of Node

## Additional type to simulate union type like in Python
## or NamedTypeNode | ListTypeNode in TypeScript
## Check the link below for the reasoning behind this
## https://forum.nim-lang.org/t/4406#27516
type NonNullablesTypes* = ref object of TypeNode

type NamedTypeNode* = ref object of NonNullablesTypes
  name*: NameNode

## Normal Declarations

# Document

type DocumentNode* = ref object of Node ## Document AST Node
  definitions*: DefinitionNode

type VariableDefinitionNode* = ref object of Node
  variable*: VariableNode
  `type`*: TypeNode
  defaultValue*: ValueNode
  directives*: seq[DirectiveNode]

type SelectionNode* = ref object of Node
  directives*: seq[DirectiveNode]

type SelectionSetNode* = ref object of Node
  selections*: seq[SelectionNode]

type ExecutableDefinitionNode* = ref object of DefinitionNode
  name*: NameNode
  directives*: seq[DirectiveNode]
  variableDefinitions*: seq[VariableDefinitionNode]
  selectionSet*: SelectionSetNode

type OperationTypeNode* = enum
  #[
    Operation Type Enum for most operation definition
    node types.
  ]#
  QUERY = "query"
  MUTATION = "mutation"
  SUBSCRIPTION = "subscription"

type OperationDefinitionNode* = ref object of ExecutableDefinitionNode
  operation*: OperationTypeNode

type FieldNode* = ref object of SelectionNode
  alias*: NameNode
  name*: NameNode
  arguments*: seq[ArgumentNode]
  selectionSet*: SelectionSetNode

# Fragments

type FragmentSpreadNode* = ref object of SelectionNode
  name*: NameNode

type InlineFragmentNode* = ref object of SelectionNode
  typeCondition*: NamedTypeNode
  selectionSet*: SelectionSetNode

type FragmentDefinitionNode* = ref object of ExecutableDefinitionNode
  typeCondition*: NamedTypeNode

# Values

type IntValueNode* = ref object of ValueNode
  value*: string

type FloatValueNode* = ref object of ValueNode
  value*: string

type StringValueNode* = ref object of ValueNode
  value*: string
  `block`*: bool

type BooleanValueNode* = ref object of ValueNode
  value*: bool

type NullValueNode* = ref object of ValueNode

type EnumValueNode* = ref object of ValueNode
  value*: string

type ListValueNode* = ref object of ValueNode
  values*: seq[ValueNode]

type ObjectFieldNode* = ref object of ValueNode
  name*: NameNode
  value*: ValueNode

type ObjectValueNode* = ref object of ValueNode
  fields*: seq[ObjectFieldNode]

# Type Reference

type ListTypeNode* = ref object of NonNullablesTypes
  `type`*: TypeNode

type NonNullTypeNode* = ref object of TypeNode
  `type`*: NonNullablesTypes

# Type System Definition

type TypeSystemDefinitionNode* = ref object of DefinitionNode

type OperationTypeDefinitionNode* = ref object of Node
  operation*: OperationTypeNode
  `type`*: NamedTypeNode

type SchemaDefinitionNode* = ref object of TypeSystemDefinitionNode
  description*: StringValueNode
  directives*: seq[DirectiveNode]
  operationTypes*: OperationTypeDefinitionNode

# Type Definition

type TypeDefinitionNode* = ref object of TypeSystemDefinitionNode
  description*: StringValueNode
  directives*: seq[DirectiveNode]
  name*: NameNode

type ScalarTypeDefinitionNode* = ref object of TypeDefinitionNode

type InputValueDefinitionNode* = ref object of DefinitionNode
  description*: StringValueNode
  name*: NameNode
  directives*: seq[DirectiveNode]
  `type`*: TypeNode
  defaultValue*: ValueNode

type FieldDefinitionNode* = ref object of DefinitionNode
  description*: StringValueNode
  name*: NameNode
  directives*: seq[DirectiveNode]
  arguments*: seq[InputValueDefinitionNode]
  `type`*: TypeNode

type ObjectTypeDefinitionNode* = ref object of TypeDefinitionNode
  interfaces*: seq[NamedTypeNode]
  fields*: seq[FieldDefinitionNode]

type InterfaceTypeDefinitionNode* = ref object of TypeDefinitionNode
  fields*: seq[FieldDefinitionNode]
  interfaces*: seq[NamedTypeNode]

type UnionTypeDefinitionNode* = ref object of TypeDefinitionNode
  types*: seq[NamedTypeNode]

type EnumValueDefinitionNode* = ref object of TypeDefinitionNode

type EnumTypeDefinitionNode* = ref object of TypeDefinitionNode
  values*: seq[EnumValueDefinitionNode]

type InputObjectTypeDefinitionNode* = ref object of TypeDefinitionNode
  fields*: seq[InputValueDefinitionNode]

# Directive Definitions

type DirectiveDefinitionNode* = ref object of TypeSystemDefinitionNode
  description*: StringValueNode
  name*: NameNode
  arguments*: seq[InputValueDefinitionNode]
  repeatable*: bool
  locations*: seq[NameNode]

## Additional type to simulate union type
## SchemaExtensionNode | TypeExtensionNode

type TypeSystemExtensionNode* = ref object of DefinitionNode
  directives*: seq[DirectiveNode]

# Type System Extensions

type SchemaExtensionNode* = ref object of TypeSystemExtensionNode
  operationTypes*: seq[OperationTypeDefinitionNode]

# Type Extensions

type TypeExtensionNode* = ref object of TypeSystemExtensionNode
  name*: NameNode

type ScalarTypeExtensionNode* = ref object of TypeExtensionNode

type ObjectTypeExtensionNode* = ref object of TypeExtensionNode
  interfaces*: seq[NamedTypeNode]
  fields*: seq[FieldDefinitionNode]

type InterfaceTypeExtensionNode* = ref object of TypeExtensionNode
  interfaces*: seq[NamedTypeNode]
  fields*: seq[FieldDefinitionNode]

type UnionTypeExtensionNode* = ref object of TypeExtensionNode
  types*: seq[NamedTypeNode]

type EnumTypeExtensionNode* = ref object of TypeExtensionNode
  values*: seq[EnumValueDefinitionNode]

type InputObjectTypeExtensionNode* = ref object of TypeExtensionNode
  fields*: seq[InputValueDefinitionNode]
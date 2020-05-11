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


type
  GraphOperationTypeKind* = enum
    gnkOperationQuery = "query"
    gnkOperationMutation = "mutation"
    gnkOperationSubscription = "subscription"

type
  GraphNodeKind* = enum
    ## GraphQL AST node kinds based on the spec language section:
    ## https://spec.graphql.org/draft/#sec-Language
    ## 
    ## The entire document syntax resume can be found at:
    ## https://spec.graphql.org/draft/#sec-Document-Syntax
    gnkEmpty                        ## Helper for empty leafs
    gnkName                         ## https://spec.graphql.org/draft/#Name
    gnkDocument
    gnkIntValue                     ## https://spec.graphql.org/draft/#sec-Int-Value
    gnkFloatValue                   ## https://spec.graphql.org/draft/#sec-Float-Value
    gnkStringValue                  ## https://spec.graphql.org/draft/#sec-String-Value
    gnkBooleanValue                 ## https://spec.graphql.org/draft/#sec-Boolean-Value
    gnkNullValue                    ## https://spec.graphql.org/draft/#sec-Null-Value
    gnkEnumValue                    ## https://spec.graphql.org/draft/#sec-Enum-Value
    gnkListValue                    ## https://spec.graphql.org/draft/#ListValue
    gnkObjectValue                  ## https://spec.graphql.org/draft/#ObjectValue
    gnkOperationDefinition          ## https://spec.graphql.org/draft/#OperationDefinition
    gnkOperationType                ## https://spec.graphql.org/draft/#OperationType
    gnkVariableDefinition           ## https://spec.graphql.org/draft/#VariableDefinition
    gnkVariable                     ## https://spec.graphql.org/draft/#Variable
    gnkSelectionSet                 ## https://spec.graphql.org/draft/#SelectionSet
    gnkField                        ## https://spec.graphql.org/draft/#Field
    gnkArgument                     ## https://spec.graphql.org/draft/#Argument
    gnkFragmentSpread               ## https://spec.graphql.org/draft/#FragmentSpread
    gnkInlineFragment               ## https://spec.graphql.org/draft/#InlineFragment
    gnkFragmentDefinition           ## https://spec.graphql.org/draft/#FragmentDefinition
    gnkObjectField                  ## https://spec.graphql.org/draft/#ObjectField
    gnkDirective                    ## https://spec.graphql.org/draft/#Directive
    gnkNamedType                    ## https://spec.graphql.org/draft/#NamedType
    gnkListType                     ## https://spec.graphql.org/draft/#ListType
    gnkNonNullType                  ## https://spec.graphql.org/draft/#NonNullType
    gnkSchemaDefinition             ## https://spec.graphql.org/draft/#SchemaDefinition
    gnkOperationTypeDefinition      ## https://spec.graphql.org/draft/#RootOperationTypeDefinition
    gnkScalarTypeDefinition         ## https://spec.graphql.org/draft/#ScalarTypeDefinition
    gnkObjectTypeDefinition         ## https://spec.graphql.org/draft/#ObjectTypeDefinition
    gnkFieldDefinition              ## https://spec.graphql.org/draft/#FieldDefinition
    gnkInputValueDefinition         ## https://spec.graphql.org/draft/#InputValueDefinition
    gnkInterfaceTypeDefinition      ## https://spec.graphql.org/draft/#InterfaceTypeDefinition
    gnkUnionTypeDefinition          ## https://spec.graphql.org/draft/#UnionTypeDefinition
    gnkEnumTypeDefinition           ## https://spec.graphql.org/draft/#EnumTypeDefinition
    gnkEnumValueDefinition          ## https://spec.graphql.org/draft/#EnumValueDefinition
    gnkInputObjectTypeDefinition    ## https://spec.graphql.org/draft/#InputObjectTypeDefinition
    gnkDirectiveDefinition          ## https://spec.graphql.org/draft/#DirectiveDefinition
    gnkSchemaExtension              ## https://spec.graphql.org/draft/#SchemaExtension
    gnkScalarTypeExtension          ## https://spec.graphql.org/draft/#ScalarTypeExtension
    gnkObjectTypeExtension          ## https://spec.graphql.org/draft/#ObjectTypeExtension
    gnkInterfaceTypeExtension       ## https://spec.graphql.org/draft/#InterfaceTypeExtension
    gnkUnionTypeExtension           ## https://spec.graphql.org/draft/#UnionTypeExtension
    gnkEnumTypeExtension            ## https://spec.graphql.org/draft/#EnumTypeExtension
    gnkInputObjectTypeExtension     ## https://spec.graphql.org/draft/#InputObjectTypeExtension
    gnkVariableDefinitionList       ## https://spec.graphql.org/draft/#VariableDefinitions
    ## Extra Kind Definitions
    gnkArgumentList                 ## https://spec.graphql.org/draft/#Arguments
    gnkDirectiveList                ## https://spec.graphql.org/draft/#Directives
    gnkOperationTypeDefinitionList  ## Helper to group a list of operation type definition nodes
    gnkFieldDefinitionList          ## https://spec.graphql.org/draft/#FieldsDefinition
    gnkArgumentsDefinition          ## https://spec.graphql.org/draft/#ArgumentsDefinition
    gnkImplementsInterfaces         ## A helper based on https://spec.graphql.org/draft/#ImplementsInterfaces
    gnkUnionMemberTypes             ## https://spec.graphql.org/draft/#UnionMemberTypes
    gnkEnumValueDefinitionList      ## https://spec.graphql.org/draft/#EnumValuesDefinition
    gnkInputFieldsDefinition        ## https://spec.graphql.org/draft/#InputFieldsDefinition
    gnkDirectiveLocations           ## https://spec.graphql.org/draft/#DirectiveLocations


type
  GraphNode* = ref object
    loc*: Location
    case kind*: GraphNodeKind
    of gnkName:
      ##[
        GraphQL Documents are full of named things:
        operations, fields, arguments, types, directives, fragments, and variables.
        All names must follow the same grammatical form.
        See more at: https://spec.graphql.org/draft/#sec-Names
      ]##
      value*: string
    of gnkIntValue:
      ##[
        An IntValue is specified without a decimal point 
        or exponent but may be negative (ex. -123).
        It must not have any leading 0.
        See more at: https://spec.graphql.org/draft/#sec-Int-Value
      ]##
      intValue*: string
    of gnkFloatValue:
      ##[
        A FloatValue includes either a decimal point (ex. 1.0)
        or an exponent (ex. 1e50) or both (ex. 6.0221413e23)
        and may be negative. Like IntValue, it also must not 
        have any leading 0.
        See more at: https://spec.graphql.org/draft/#sec-Float-Value
      ]##
      floatValue*: string
    of gnkStringValue:
      ##[
        Strings are sequences of characters wrapped 
        in quotation marks (U+0022). (ex. "Hello World").
        White space and other otherwise‐ignored characters
        are significant within a string value.
        https://spec.graphql.org/draft/#sec-String-Value
      ]##
      strValue*: string
      isBlockString*: bool
    of gnkBooleanValue:
      ##[
        The two keywords true and false represent the two boolean values.
        https://spec.graphql.org/draft/#sec-Boolean-Value
      ]##
      boolValue*: bool
    of gnkNullValue, gnkEmpty:
      ##[
        Null values are represented as the keyword null.
        In Nim, discard is used as this node has no
        additional fields.
        https://spec.graphql.org/draft/#sec-Null-Value
      ]##
      discard
    of gnkEnumValue:
      ##[
        Enum values are represented as unquoted names (ex. MOBILE_WEB).
        https://spec.graphql.org/draft/#sec-Enum-Value
      ]##
      enumValue*: string
    of gnkOperationType:
      ##[
        There are three types of operations that GraphQL models
          - query: a read‐only fetch.
          - mutation: a write followed by a fetch.
          - subscription: a long‐lived request that fetches data in response to source events.
        https://spec.graphql.org/draft/#sec-Language.Operations
      ]##
      operation*: GraphOperationTypeKind
    else:
      children*: seq[GraphNode]

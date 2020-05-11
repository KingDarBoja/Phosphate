## Token Kind Module
type TokenKind* = enum
  ##[
    A GraphQL document is comprised of several kinds of indivisible lexical tokens.
    
    These token kinds are emitted by the lexer and therefore defined on 
    the spec: https://spec.graphql.org/draft/#sec-Language.Source-Text.Lexical-Tokens

    The SOF and EOF token kinds are delimiters of a GraphQL Document, hence only appears
    at the base JavaScript implementation and not at the spec.
  ]##
  SOF = "<SOF>"
  EOF = "<EOF>"
  BANG = "!"
  DOLLAR = "$"
  AMP = "&"
  PAREN_L = "("
  PAREN_R = ")"
  SPREAD = "..."
  COLON = ":"
  EQUALS = "="
  AT = "@"
  BRACKET_L = "["
  BRACKET_R = "]"
  BRACE_L = "{"
  PIPE = "|"
  BRACE_R = "}"
  NAME = "Name"
  INT = "Int"
  FLOAT = "Float"
  STRING = "String"
  BLOCK_STRING = "BlockString"
  COMMENT = "Comment"
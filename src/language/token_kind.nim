## Token Kind Module
type TokenKind* = enum
  #[
    The different kinds of tokens that the lexer emits
  ]#
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
  BRAKET_R = "]"
  BRACE_L = "{"
  PIPE = "|"
  BRACE_R = "}"
  NAME = "Name"
  INT = "Int"
  FLOAT = "Float"
  STRING = "String"
  BLOCK_STRING = "BlockString"
  COMMENT = "Comment"
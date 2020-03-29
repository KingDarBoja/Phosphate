import unittest
# import strformat

import language/ast
import language/token_kind
import language/source
import language/lexer

suite "Describe Lexer":

  setup:
    let TokenSOF = initToken(TokenKind.SOF, 0, 0, 0, 0)
    
    proc lexOne(s: string): Token =
      let source = initSource(s)
      var lexer = initLexer(source)
      return lexer.advance()

    proc lexSecond(s: string): Token =
      let source = initSource(s)
      var lexer = initLexer(source)
      discard lexer.advance()
      return lexer.advance()

    proc compareTokensOne(a: Token, b: Token) =
      check(a.kind == b.kind)
      check(a.start == b.start)
      check(a.`end` == b.`end`)
      check(a.value == b.value)

    proc compareTokensTwo(a: Token, b: Token) =
      check(a.kind == b.kind)
      check(a.start == b.start)
      check(a.`end` == b.`end`)
      check(a.line == b.line)
      check(a.column == b.column)
      check(a.value == b.value)

    # TODO: Add Location parameter
    proc assertSyntaxError(text: string, msg: string) =
      try:
        discard lexSecond(text)
      except ValueError as error:
        check(error.msg == msg)

  # TODO: Failing test, check for hex handling.
  test "Disallows uncommon control characters":
    assertSyntaxError("\x07", "Cannot contain the invalid character '\\x07'.")

  test "Accepts Bom header":
    compareTokensOne(lexOne("\uFEFF foo"), initToken(TokenKind.NAME, 2, 5, 1, 3, value = "foo"))

  test "Tracks Line Breaks":
    compareTokensTwo(lexOne("foo"), initToken(TokenKind.NAME, 0, 3, 1, 1, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\nfoo"), initToken(TokenKind.NAME, 1, 4, 2, 1, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\rfoo"), initToken(TokenKind.NAME, 1, 4, 2, 1, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\r\nfoo"), initToken(TokenKind.NAME, 2, 5, 2, 1, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\n\rfoo"), initToken(TokenKind.NAME, 2, 5, 3, 1, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\r\r\n\nfoo"), initToken(TokenKind.NAME, 4, 7, 4, 1, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\n\n\r\rfoo"), initToken(TokenKind.NAME, 4, 7, 5, 1, TokenSOF, "foo"))

  test "Records line and column":
    compareTokensTwo(lexOne("\n \r\n \r  foo\n"), initToken(TokenKind.NAME, 8, 11, 4, 3, TokenSOF, "foo"))

  # TODO: Provide nim utils to stringify the object.
  # Example: '{"kind":"Name","value":"foo","line":1,"column":1}',
  test "Can be stringified or nimutils inspected":
    discard

  test "Skips whitespace and comments":
    compareTokensTwo(lexOne("\n\n    foo\n\n\n"), initToken(TokenKind.NAME, 6, 9, 3, 5, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\r\n\r\n  foo\r\n\r\n"), initToken(TokenKind.NAME, 6, 9, 3, 3, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\r\r  foo\r\r"), initToken(TokenKind.NAME, 4, 7, 3, 3, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\t\t  foo\t\t"), initToken(TokenKind.NAME, 4, 7, 1, 5, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\n    #comment\n    foo#comment\n"), initToken(TokenKind.NAME, 18, 21, 3, 5, TokenSOF, "foo"))
    compareTokensTwo(lexOne(",,,foo,,,"), initToken(TokenKind.NAME, 3, 6, 1, 4, TokenSOF, "foo"))

  # TODO: Requires GraphQLSyntaxError type.
  test "Errors respect whitespace":
    discard

  # TODO: Requires GraphQLSyntaxError type.
  test "Updates line numbers in error for file context":
    discard

  # TODO: Requires GraphQLSyntaxError type.
  test "Updates column numbers in error for file context":
    discard

  test "Lexes strings":
    compareTokensOne(lexOne("\"\""), initToken(TokenKind.STRING, 0, 2, 1, 1, TokenSOF, ""))
    compareTokensOne(lexOne("\"simple\""), initToken(TokenKind.STRING, 0, 8, 1, 1, TokenSOF, "simple"))
    compareTokensOne(lexOne("\" white space \""), initToken(TokenKind.STRING, 0, 15, 1, 1, TokenSOF, " white space "))
    compareTokensOne(lexOne("\"quote \\\"\""), initToken(TokenKind.STRING, 0, 10, 1, 1, TokenSOF, "quote \""))
    compareTokensOne(lexOne("\"escaped \\n\\r\\b\\t\\f\""), initToken(TokenKind.STRING, 0, 20, 1, 1, TokenSOF, "escaped \n\r\b\t\f"))
    compareTokensOne(lexOne("\"slashes \\\\ \\/\""), initToken(TokenKind.STRING, 0, 15, 1, 1, TokenSOF, "slashes \\ /"))
    # TODO: value props are not equal, left is hex, right is unicode.
    compareTokensOne(lexOne("\"unicode \\u1234\\u5678\\u90AB\\uCDEF\""), initToken(TokenKind.STRING, 0, 34, 1, 1, TokenSOF, "unicode \u1234\u5678\u90AB\uCDEF"))

  test "Lex reports useful string errors":
    assertSyntaxError("\"", "Unterminated string.")
    assertSyntaxError("\"\"\"", "Unterminated string.")
    assertSyntaxError("\"\"\"\"", "Unterminated string.")
    assertSyntaxError("\"no end quote", "Unterminated string.")
    assertSyntaxError("'single quotes'", "Unexpected single quote character ('), did you mean to use a double quote (\")?")
    # TODO: Failing test, check for hex handling.
    assertSyntaxError("\"contains unescaped \x07 control char\"", "Invalid character within String: '\\x07'.")
    # TODO: Failing test, check for hex handling.
    assertSyntaxError("\"null-byte is not \x00 end of file\"", "Invalid character within String: '\\x00'.")
    assertSyntaxError("\"multi\nline\"", "Unterminated string.")
    assertSyntaxError("\"multi\rline\"", "Unterminated string.")
    assertSyntaxError("\"bad \\x esc\"", "Invalid character escape sequence: '\\x'.")
    # TODO: Failing test, check for hex handling.
    assertSyntaxError("\"bad \\u1 esc\"", "Invalid character escape sequence: '\\u1 es'.")
    # TODO: Failing test, check for hex handling.
    assertSyntaxError("\"bad \\u0XX1 esc\"", "Invalid character escape sequence: '\\u0XX1'.")
    # TODO: Failing test, check for hex handling.
    assertSyntaxError("\"bad \\uXXXX esc\"", "Invalid character escape sequence: '\\uXXXX'.")
    # TODO: Failing test, check for hex handling.
    assertSyntaxError("\"bad \\uFXXX esc\"", "Invalid character escape sequence: '\\uFXXX'.")
    # TODO: Failing test, check for hex handling.
    assertSyntaxError("\"bad \\uXXXF esc\"", "Invalid character escape sequence: '\\uXXXF'.")

  test "Lexes Block Strings":
    compareTokensOne(
      lexOne("\"\"\"\"\"\""),
      initToken(TokenKind.BLOCK_STRING, 0, 6, 1, 1, TokenSOF, "")
    )
    compareTokensOne(
      lexOne("\"\"\"simple\"\"\""),
      initToken(TokenKind.BLOCK_STRING, 0, 12, 1, 1, TokenSOF, "simple")
    )
    compareTokensOne(
      lexOne("\"\"\" white space \"\"\""),
      initToken(TokenKind.BLOCK_STRING, 0, 19, 1, 1, TokenSOF, " white space ")
    )
    compareTokensOne(
      lexOne("\"\"\"contains \" quote\"\"\""),
      initToken(TokenKind.BLOCK_STRING, 0, 22, 1, 1, TokenSOF, "contains \" quote")
    )
    compareTokensOne(
      lexOne("\"\"\"contains \" quote\"\"\""),
      initToken(TokenKind.BLOCK_STRING, 0, 22, 1, 1, TokenSOF, "contains \" quote")
    )
    compareTokensOne(
      lexOne("\"\"\"contains \\\"\"\" triple-quote\"\"\""),
      initToken(TokenKind.BLOCK_STRING, 0, 32, 1, 1, TokenSOF, "contains \"\"\" triple-quote")
    )
    compareTokensOne(
      lexOne("\"\"\"multi\nline\"\"\""),
      initToken(TokenKind.BLOCK_STRING, 0, 16, 1, 1, TokenSOF, "multi\nline")
    )
    compareTokensOne(
      lexOne("\"\"\"multi\rline\r\nnormalized\"\"\""),
      initToken(TokenKind.BLOCK_STRING, 0, 28, 1, 1, TokenSOF, "multi\nline\nnormalized")
    )
    compareTokensOne(
      lexOne("\"\"\"unescaped \\n\\r\\b\\t\\f\\u1234\"\"\""),
      initToken(TokenKind.BLOCK_STRING, 0, 32, 1, 1, TokenSOF, "unescaped \\n\\r\\b\\t\\f\\u1234")
    )
    compareTokensOne(
      lexOne("\"\"\"slashes \\\\ \\/\"\"\""),
      initToken(TokenKind.BLOCK_STRING, 0, 19, 1, 1, TokenSOF, "slashes \\\\ \\/")
    )
    compareTokensOne(
      lexOne(
        "\"\"\"\n\n        spans\n          multiple\n" &
        "            lines\n\n        \"\"\""
      ),
      initToken(TokenKind.BLOCK_STRING, 0, 68, 1, 1, TokenSOF, "spans\n  multiple\n    lines")
    )
    
suite "Describe Is Punctuator Token Kind":

  setup:
    proc lexOne(s: string): Token =
      let source = initSource(s)
      var lexer = initLexer(source)
      return lexer.advance()

    proc isPunctuatorToken(text: string): bool =
      return isPunctuatorTokenKind(lexOne(text).kind)

  test "Returns true for punctuator tokens":
    check(isPunctuatorToken("!") == true)
    check(isPunctuatorToken("$") == true)
    check(isPunctuatorToken("&") == true)
    check(isPunctuatorToken("(") == true)
    check(isPunctuatorToken(")") == true)
    check(isPunctuatorToken("...") == true)
    check(isPunctuatorToken(":") == true)
    check(isPunctuatorToken("=") == true)
    check(isPunctuatorToken("@") == true)
    check(isPunctuatorToken("[") == true)
    check(isPunctuatorToken("]") == true)
    check(isPunctuatorToken("{") == true)
    check(isPunctuatorToken("|") == true)
    check(isPunctuatorToken("}") == true)

  test "Returns false for non punctuator tokens":
    check(isPunctuatorToken("") == false)
    check(isPunctuatorToken("name") == false)
    check(isPunctuatorToken("1") == false)
    check(isPunctuatorToken("3.14") == false)
    check(isPunctuatorToken("\"str\"") == false)
    check(isPunctuatorToken("\"\"\"str\"\"\"") == false)
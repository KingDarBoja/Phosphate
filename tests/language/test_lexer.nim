import unittest
import strformat

import language/ast
import language/token_kind
import language/source_location
import language/lexer
import nimutils/dedent

suite "Describe Lexer":

  setup:
    let TokenSOF = newToken(TokenKind.SOF, 0, 0, 0, 0)
    
    proc lexOne(s: string): Token =
      let source = newSource(s)
      var lexer = newLexer(source)
      return lexer.advance()

    proc lexSecond(s: string): Token =
      let source = newSource(s)
      var lexer = newLexer(source)
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

  # TODO: Failing test due to start and end offset by 2 (unicode)
  test "Accepts Bom header":
    compareTokensOne(lexOne("\uFEFF foo"), newToken(TokenKind.NAME, 2, 5, 1, 3, value = "foo"))

  test "Tracks Line Breaks":
    compareTokensTwo(lexOne("foo"), newToken(TokenKind.NAME, 0, 3, 1, 1, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\nfoo"), newToken(TokenKind.NAME, 1, 4, 2, 1, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\rfoo"), newToken(TokenKind.NAME, 1, 4, 2, 1, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\r\nfoo"), newToken(TokenKind.NAME, 2, 5, 2, 1, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\n\rfoo"), newToken(TokenKind.NAME, 2, 5, 3, 1, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\r\r\n\nfoo"), newToken(TokenKind.NAME, 4, 7, 4, 1, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\n\n\r\rfoo"), newToken(TokenKind.NAME, 4, 7, 5, 1, TokenSOF, "foo"))

  test "Records line and column":
    compareTokensTwo(lexOne("\n \r\n \r  foo\n"), newToken(TokenKind.NAME, 8, 11, 4, 3, TokenSOF, "foo"))

  # TODO: Provide nim utils to stringify the object.
  # Example: '{"kind":"Name","value":"foo","line":1,"column":1}',
  test "Can be stringified or nimutils inspected":
    discard

  test "Skips whitespace and comments":
    compareTokensTwo(lexOne("\n\n    foo\n\n\n"), newToken(TokenKind.NAME, 6, 9, 3, 5, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\r\n\r\n  foo\r\n\r\n"), newToken(TokenKind.NAME, 6, 9, 3, 3, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\r\r  foo\r\r"), newToken(TokenKind.NAME, 4, 7, 3, 3, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\t\t  foo\t\t"), newToken(TokenKind.NAME, 4, 7, 1, 5, TokenSOF, "foo"))
    compareTokensTwo(lexOne("\n    #comment\n    foo#comment\n"), newToken(TokenKind.NAME, 18, 21, 3, 5, TokenSOF, "foo"))
    compareTokensTwo(lexOne(",,,foo,,,"), newToken(TokenKind.NAME, 3, 6, 1, 4, TokenSOF, "foo"))

  # TODO: Requires GraphQLSyntaxError type.
  test "Errors respect whitespace":
    try:
      discard lexOne("\n\n    ?\n")
    except ValueError as error:
      check(
        error.msg & '\n' == dedent(
        """
        Cannot parse the unexpected character '?'.
        """)
      )

  # TODO: Requires GraphQLSyntaxError type.
  test "Updates line numbers in error for file context":
    let s = "\n\n     ?\n\n"
    let source = newSource(s, "foo.js")
    try:
      var lex = newLexer(source)
      discard lex.advance()
    except ValueError as error:
      check(
        error.msg & '\n' == dedent(
        """
        Cannot parse the unexpected character '?'.
        """)
      )

  # TODO: Requires GraphQLSyntaxError type.
  test "Updates column numbers in error for file context":
    let source = newSource("?", "foo.js")
    try:
      var lex = newLexer(source)
      discard lex.advance()
    except ValueError as error:
      check(
        error.msg & '\n' == dedent(
        """
        Cannot parse the unexpected character '?'.
        """)
      )

  test "Lexes strings":
    compareTokensOne(lexOne("\"\""), newToken(TokenKind.STRING, 0, 2, 1, 1, TokenSOF, ""))
    compareTokensOne(lexOne("\"simple\""), newToken(TokenKind.STRING, 0, 8, 1, 1, TokenSOF, "simple"))
    compareTokensOne(lexOne("\" white space \""), newToken(TokenKind.STRING, 0, 15, 1, 1, TokenSOF, " white space "))
    compareTokensOne(lexOne("\"quote \\\"\""), newToken(TokenKind.STRING, 0, 10, 1, 1, TokenSOF, "quote \""))
    compareTokensOne(lexOne("\"escaped \\n\\r\\b\\t\\f\""), newToken(TokenKind.STRING, 0, 20, 1, 1, TokenSOF, "escaped \n\r\b\t\f"))
    compareTokensOne(lexOne("\"slashes \\\\ \\/\""), newToken(TokenKind.STRING, 0, 15, 1, 1, TokenSOF, "slashes \\ /"))
    # TODO: value props are not equal, left is hex, right is unicode.
    compareTokensOne(lexOne("\"unicode \\u1234\\u5678\\u90AB\\uCDEF\""), newToken(TokenKind.STRING, 0, 34, 1, 1, TokenSOF, "unicode \u1234\u5678\u90AB\uCDEF"))

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
      newToken(TokenKind.BLOCK_STRING, 0, 6, 1, 1, TokenSOF, "")
    )
    compareTokensOne(
      lexOne("\"\"\"simple\"\"\""),
      newToken(TokenKind.BLOCK_STRING, 0, 12, 1, 1, TokenSOF, "simple")
    )
    compareTokensOne(
      lexOne("\"\"\" white space \"\"\""),
      newToken(TokenKind.BLOCK_STRING, 0, 19, 1, 1, TokenSOF, " white space ")
    )
    compareTokensOne(
      lexOne("\"\"\"contains \" quote\"\"\""),
      newToken(TokenKind.BLOCK_STRING, 0, 22, 1, 1, TokenSOF, "contains \" quote")
    )
    compareTokensOne(
      lexOne("\"\"\"contains \" quote\"\"\""),
      newToken(TokenKind.BLOCK_STRING, 0, 22, 1, 1, TokenSOF, "contains \" quote")
    )
    compareTokensOne(
      lexOne("\"\"\"contains \\\"\"\" triple-quote\"\"\""),
      newToken(TokenKind.BLOCK_STRING, 0, 32, 1, 1, TokenSOF, "contains \"\"\" triple-quote")
    )
    compareTokensOne(
      lexOne("\"\"\"multi\nline\"\"\""),
      newToken(TokenKind.BLOCK_STRING, 0, 16, 1, 1, TokenSOF, "multi\nline")
    )
    compareTokensOne(
      lexOne("\"\"\"multi\rline\r\nnormalized\"\"\""),
      newToken(TokenKind.BLOCK_STRING, 0, 28, 1, 1, TokenSOF, "multi\nline\nnormalized")
    )
    compareTokensOne(
      lexOne("\"\"\"unescaped \\n\\r\\b\\t\\f\\u1234\"\"\""),
      newToken(TokenKind.BLOCK_STRING, 0, 32, 1, 1, TokenSOF, "unescaped \\n\\r\\b\\t\\f\\u1234")
    )
    compareTokensOne(
      lexOne("\"\"\"slashes \\\\ \\/\"\"\""),
      newToken(TokenKind.BLOCK_STRING, 0, 19, 1, 1, TokenSOF, "slashes \\\\ \\/")
    )
    compareTokensOne(
      lexOne(
        "\"\"\"\n\n        spans\n          multiple\n" &
        "            lines\n\n        \"\"\""
      ),
      newToken(TokenKind.BLOCK_STRING, 0, 68, 1, 1, TokenSOF, "spans\n  multiple\n    lines")
    )

  test "Advance line after lexing multiline block string":
    let trquote = "\"\"\""
    compareTokensTwo(
        lexSecond(
                fmt"""{trquote}

        spans
          multiple
            lines

        \n {trquote} second_token"""
        ),
      newToken(TokenKind.NAME, 72, 84, 8, 6, TokenSOF, "second_token"))

  test "Lex reports useful block string errors":
    assertSyntaxError("\"\"\"", "Unterminated string.")
    assertSyntaxError("\"\"\"no end quote", "Unterminated string.")
    # TODO: Failing test, check for hex handling.
    assertSyntaxError("\"\"\"contains unescaped \x07 control char\"\"\"", "Invalid character within String: '\\x07'.")
    # TODO: Failing test, check for hex handling.
    assertSyntaxError("\"\"\"null-byte is not \x00 end of file\"\"\"", "Invalid character within String: '\\x00'.")

  test "Lexes numbers":
    compareTokensOne(
      lexOne("0"), newToken(TokenKind.INT, 0, 1, 1, 1, TokenSOF, "0")
    )
    compareTokensOne(
      lexOne("1"), newToken(TokenKind.INT, 0, 1, 1, 1, TokenSOF, "1")
    )
    compareTokensOne(
      lexOne("4"), newToken(TokenKind.INT, 0, 1, 1, 1, TokenSOF, "4")
    )
    compareTokensOne(
      lexOne("9"), newToken(TokenKind.INT, 0, 1, 1, 1, TokenSOF, "9")
    )
    compareTokensOne(
      lexOne("42"), newToken(TokenKind.INT, 0, 2, 1, 1, TokenSOF, "42")
    )
    compareTokensOne(
      lexOne("4.123"), newToken(TokenKind.FLOAT, 0, 5, 1, 1, TokenSOF, "4.123")
    )
    compareTokensOne(
      lexOne("-4"), newToken(TokenKind.INT, 0, 2, 1, 1, TokenSOF, "-4")
    )
    compareTokensOne(
      lexOne("-42"), newToken(TokenKind.INT, 0, 3, 1, 1, TokenSOF, "-42")
    )
    compareTokensOne(
      lexOne("-4.123"), newToken(TokenKind.FLOAT, 0, 6, 1, 1, TokenSOF, "-4.123")
    )
    compareTokensOne(
      lexOne("0.123"), newToken(TokenKind.FLOAT, 0, 5, 1, 1, TokenSOF, "0.123")
    )
    compareTokensOne(
      lexOne("123e4"), newToken(TokenKind.FLOAT, 0, 5, 1, 1, TokenSOF, "123e4")
    )
    compareTokensOne(
      lexOne("123E4"), newToken(TokenKind.FLOAT, 0, 5, 1, 1, TokenSOF, "123E4")
    )
    compareTokensOne(
      lexOne("123e-4"), newToken(TokenKind.FLOAT, 0, 6, 1, 1, TokenSOF, "123e-4")
    )
    compareTokensOne(
      lexOne("123e+4"), newToken(TokenKind.FLOAT, 0, 6, 1, 1, TokenSOF, "123e+4")
    )
    compareTokensOne(
      lexOne("-1.123e4"), newToken(TokenKind.FLOAT, 0, 8, 1, 1, TokenSOF, "-1.123e4")
    )
    compareTokensOne(
      lexOne("-1.123E4"), newToken(TokenKind.FLOAT, 0, 8, 1, 1, TokenSOF, "-1.123E4")
    )
    compareTokensOne(
      lexOne("-1.123e-4"), newToken(TokenKind.FLOAT, 0, 9, 1, 1, TokenSOF, "-1.123e-4")
    )
    compareTokensOne(
      lexOne("-1.123e+4"), newToken(TokenKind.FLOAT, 0, 9, 1, 1, TokenSOF, "-1.123e+4")
    )
    compareTokensOne(
      lexOne("-1.123e4567"), newToken(TokenKind.FLOAT, 0, 11, 1, 1, TokenSOF, "-1.123e4567")
    )

  test "Lex reports useful number errors":
    assertSyntaxError("00", "Invalid number, unexpected digit after 0: '0'.")
    assertSyntaxError("01", "Invalid number, unexpected digit after 0: '1'.")
    assertSyntaxError("01.23", "Invalid number, unexpected digit after 0: '1'.")
    assertSyntaxError("+1", "Cannot parse the unexpected character '+'.")
    assertSyntaxError("1.", "Invalid number, expected digit but got: <EOF>.")
    assertSyntaxError("1e", "Invalid number, expected digit but got: <EOF>.")
    assertSyntaxError("1E", "Invalid number, expected digit but got: <EOF>.")
    assertSyntaxError("1.e1", "Invalid number, expected digit but got: 'e'.")
    assertSyntaxError(".123", "Cannot parse the unexpected character '.'.")
    assertSyntaxError("1.A", "Invalid number, expected digit but got: 'A'.")
    assertSyntaxError("-A", "Invalid number, expected digit but got: 'A'.")
    assertSyntaxError("1.0e", "Invalid number, expected digit but got: <EOF>.")
    assertSyntaxError("1.0eA", "Invalid number, expected digit but got: 'A'.")
    assertSyntaxError("1.2e3e", "Invalid number, expected digit but got: 'e'.")
    assertSyntaxError("1.2e3.4", "Invalid number, expected digit but got: '.'.")
    assertSyntaxError("1.23.4", "Invalid number, expected digit but got: '.'.")
    
  test "Lex does not allow name start after a number":
    assertSyntaxError("0xF1", "Invalid number, expected digit but got: 'x'.")
    assertSyntaxError("0b10", "Invalid number, expected digit but got: 'b'.")
    assertSyntaxError("123abc", "Invalid number, expected digit but got: 'a'.")
    assertSyntaxError("1_1234", "Invalid number, expected digit but got: '_'.")
    # TODO: Failing test, check for hex handling.
    assertSyntaxError("1ß", "Cannot parse the unexpected character 'ß'.")
    assertSyntaxError("1.23f", "Invalid number, expected digit but got: 'f'.")
    # TODO: Failing test, check for hex handling.
    assertSyntaxError("12ß", "Cannot parse the unexpected character 'ß'.")

  test "Lexes punctuation":
    compareTokensOne(lexOne("!"), newToken(TokenKind.BANG, 0, 1, 1, 1))
    compareTokensOne(lexOne("$"), newToken(TokenKind.DOLLAR, 0, 1, 1, 1))
    compareTokensOne(lexOne("("), newToken(TokenKind.PAREN_L, 0, 1, 1, 1))
    compareTokensOne(lexOne(")"), newToken(TokenKind.PAREN_R, 0, 1, 1, 1))
    compareTokensOne(lexOne("..."), newToken(TokenKind.SPREAD, 0, 3, 1, 1))
    compareTokensOne(lexOne(":"), newToken(TokenKind.COLON, 0, 1, 1, 1))
    compareTokensOne(lexOne("="), newToken(TokenKind.EQUALS, 0, 1, 1, 1))
    compareTokensOne(lexOne("@"), newToken(TokenKind.AT, 0, 1, 1, 1))
    compareTokensOne(lexOne("["), newToken(TokenKind.BRACKET_L, 0, 1, 1, 1))
    compareTokensOne(lexOne("]"), newToken(TokenKind.BRACKET_R, 0, 1, 1, 1))
    compareTokensOne(lexOne("{"), newToken(TokenKind.BRACE_L, 0, 1, 1, 1))
    compareTokensOne(lexOne("}"), newToken(TokenKind.BRACE_R, 0, 1, 1, 1))
    compareTokensOne(lexOne("|"), newToken(TokenKind.PIPE, 0, 1, 1, 1))

  test "Lex reports useful unknown character error":
    assertSyntaxError("..", "Cannot parse the unexpected character '.'.")
    assertSyntaxError("?", "Cannot parse the unexpected character '?'.")
    # TODO: Failing test, check for hex handling.
    assertSyntaxError("\u203B", "Cannot parse the unexpected character '\u203B'.")
    # TODO: Failing test, check for hex handling.
    assertSyntaxError("\u200b", "Cannot parse the unexpected character '\\u200b'.")

  test "Lex reports useful information for dashes in names":
    let source = newSource("a-b")
    var lex = newLexer(source)
    let firstToken = lex.advance()
    compareTokensOne(firstToken, newToken(TokenKind.NAME, 0, 1, 1, 1, TokenSOF, "a"))
    try:
      discard lex.advance()
    except ValueError as error:
      check(error.msg == "Invalid number, expected digit but got: 'b'.")

  test "Produces double linked list of tokens including comments":
    let source = newSource(
      """
      {
        #comment
        field
      }
      """
    )
    var lex = newLexer(source)
    let startToken = lex.token
    var endToken: Token
    while true:
      endToken = lex.advance()
      if endToken.kind == TokenKind.EOF:
        break
      check(endToken.kind != TokenKind.COMMENT)
    check(startToken.prev.isNil)
    check(endToken.next.isNil)

    var tokens: seq[Token]
    var tok = startToken
    while not isNil tok:
      check(tokens.len == 0 or tok.prev == tokens[^1])
      tokens.add(tok)
      tok = tok.next
    var tokKind: seq[string]
    for tok in tokens:
      tokKind.add($tok.kind)
    check(tokKind == @[
      $TokenKind.SOF,
      $TokenKind.BRACE_L,
      $TokenKind.COMMENT,
      $TokenKind.NAME,
      $TokenKind.BRACE_R,
      $TokenKind.EOF
    ])

suite "Describe Is Punctuator Token Kind":

  setup:
    proc lexOne(s: string): Token =
      let source = newSource(s)
      var lexer = newLexer(source)
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
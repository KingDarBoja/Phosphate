import unittest
# import strformat

import language/ast
import language/token_kind
import language/source
import language/lexer

suite "Describe Lexer":

  setup:
    proc lexOne(s: string): Token =
      let source = initSource(s)
      var lexer = initLexer(source)
      return lexer.advance()

    proc lexSecond(s: string): Token =
      let source = initSource(s)
      var lexer = initLexer(source)
      discard lexer.advance()
      return lexer.advance()

    # TODO: Add Location parameter
    proc assertSyntaxError(text: string, msg: string) =
      try:
        discard lexSecond(text)
      except ValueError as error:
        check(error.msg == msg)

  # TODO: Check for another alternative to handle raw strings.
  test "Disallows uncommon control characters":
    assertSyntaxError("\x07", "Cannot contain the invalid character '\\7'.")
    assertSyntaxError(r"\x07", "Cannot parse the unexpected character '\\\\'.")

  test "Accepts Bom header":
    let sampleToken = lexOne("\uFEFF foo")
    check(sampleToken.kind == TokenKind.NAME)
    check(sampleToken.start == 4)
    check(sampleToken.`end` == 7)
    check(sampleToken.value == "foo")

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
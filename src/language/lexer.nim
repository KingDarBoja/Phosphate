## Lexer Module
import strformat, strutils, options, tables, unicode, unicodeplus

import language/ast
import language/source_location
import language/block_string
import language/token_kind


# const EscapedChars = {
#     '"': '"',
#     '/': '/',
#     '\\': '\\',
#     'b': '\b',
#     'f': '\f',
#     'n': '\n',
#     'r': '\r',
#     't': '\t'
# }.toTable

const EscapedChars = {
    Rune(0x0022): Rune(0x0022),
    Rune(0x002F): Rune(0x002F),
    Rune(0x005C): Rune(0x005C),
    Rune(0x0062): Rune(0x0008),
    Rune(0x0066): Rune(0x000C),
    Rune(0x006E): Rune(0x000A),
    Rune(0x0072): Rune(0x000D),
    Rune(0x0074): Rune(0x0009),
}.toTable

const PunctuactionTokenKind = {TokenKind.BANG..TokenKind.BRACE_R}
const AlphaAscii = {'a'.ord .. 'z'.ord, 'A'.ord .. 'Z'.ord}
const DigitAscii = {'0'.ord .. '9'.ord}


proc getCharacter*(s: seq[Rune], pos: int): Option[Rune] =
  #[
    Return the character at the specified position or `None` 
    option which represents an empty value
  ]#
  if pos < s.len:
    return option(s[pos])
  return none(Rune)


proc isTokenKind(s: string): bool =
  try:
    discard parseEnum[TokenKind](s)
    return true
  except ValueError:
    return false


proc isEscapedChar(s: Rune): bool =
  return s in EscapedChars


proc printChar(c: Rune): string =
  ##[
    Return the character as string by using the `repr` operator
    instead of `$` (stringify) as it yields a different string representation 
    (with enclosing quotes).
  ]##
  if unicodeplus.isPrintable(c):
    return c.toUTF8()
  else:
    return fmt"\u{c.int.toHex(4)}"


proc isAlphaAscii(c: Rune): bool =
  ##[
    Check whether character is plain ASCII letter.
  ]##
  return c.int in AlphaAscii


proc isDigitAscii(c: Rune): bool =
  return c.int in DigitAscii


proc isNameStart(c: Rune): bool =
  ##[
    Check whether character is an underscore or a plain ASCII letter
  ]##
  return c.isAlphaAscii or c == Rune(0x005F)


proc unicodeCharCode(a: Rune, b: Rune, c: Rune, d: Rune): int =
  ##[
    Convert unicode string to integers.

    Converts four hexadecimal chars to the integer that the string represents.
    For example, uni_char_code('0f') will return 15,
    and uni_char_code('0','0','f','f') returns 255.

    Returns a negative number on error, if hex integer was invalid.

    This is implemented by noting that strutils fromHex proc raises
    a ValueError if `s` is not a valid hex integer. Capture the error
    and return -1 instead.
  ]##
  return (a.int shl 12) or (b.int shl 8) or (c.int shl 4) or d.int


proc unexpectedCharacterMessage(c: Rune): string =
  ##[
    Report a message that an unexpected character was encountered.
  ]##
  # if c < ' ' and c notin "\t\n\r":
  if c.int < 32 and c notin @[Rune(0x0009), Rune(0x000A), Rune(0x000D)]:
    return &"Cannot contain the invalid character '{printChar(c)}'."
  # Check if it a single quote / apostrophe character (') 
  if c == Rune(0x0027):
    return "Unexpected single quote character ('), did you mean to use a double quote (\")?"
  return &"Cannot parse the unexpected character '{printChar(c)}'."


proc isPunctuatorTokenKind*(kind: TokenKind): bool =
  ##[
    Check whether the given token kind corresponds to a punctuator.

    For internal use only.
  ]##
  return kind in PunctuactionTokenKind


type Lexer* = ref object
  ##[
    A Lexer is a stateful stream generator in that every time it is advanced, it returns
    the next token in the Source. Assuming the source lexes, the final Token emitted by
    the lexer will be of kind EOF, after which the lexer will repeatedly return the same
    EOF token whenever called.
  ]##
  source*: Source
  lastToken*: Token ## The previously focused non-ignored token.
  token*: Token ## The currently focused non-ignored token.
  line*: int ## The (1-indexed) line containing the current token.
  lineStart*: int ## The character offset at which the current line begins.


proc newLexer*(source: Source): Lexer =
  ##[
    Given a Source object, creates a Lexer for that source.
  ]##
  new(result)
  let startOfFileToken = newToken(TokenKind.SOF, 0, 0, 0, 0)
  result = Lexer(
    source: source,
    lastToken: startOfFileToken,
    token: startOfFileToken,
    line: 1,
    lineStart: 0,
  )


proc positionAfterWhitespace*(self: var Lexer, body: seq[Rune], startPosition: int): int =
  ##[
    Go to next position after a whitespace.

    Reads from body starting at start_position until it finds a non-whitespace
    character, then returns the position of that character for lexing.
  ]##
  let bodyLen = body.len
  var pos = startPosition
  while pos < bodyLen:
    let character = body[pos]
    # Check if space, horizontal tab, comma or byte order mark (BOM)
    if character in [Rune(0x0020), Rune(0x0009), Rune(0x002C), Rune(0xFEFF)]:
      inc(pos)
    # Check if line feed (LF)
    elif character == Rune(0x000A):
      inc(pos)
      inc(self.line)
      self.lineStart = pos
    # Check if CR+LF
    elif character == Rune(0x000D):
      if body[pos + 1] == Rune(0x000A):
        inc(pos, 2)
      else:
        inc(pos)
      inc(self.line)
      self.lineStart = pos
    else:
      break
  return pos


proc readComment(self: Lexer, start: int, line: int, col: int, prev: Token): Token =
  ##[
    Read a comment token from the source file.
  ]##
  let
    body = self.source.body.toRunes()
    bodyLen = body.len

  var pos = start
  while true:
    inc(pos)
    if pos >= bodyLen:
      break;
    let character = body[pos]
    if not unicodeplus.isPrintable(character):
      break

  return newToken(TokenKind.COMMENT, start, pos, line, col, prev, $(body[start + 1 ..< pos]))


proc readName(self: Lexer, start: int, line: int, col: int, prev: Token): Token =
  ##[
    Read an alphanumeric + underscore name from the source.
  ]##
  let
    body = self.source.body.toRunes()
    bodyLen = body.len
  var pos = start + 1
  
  while pos < bodyLen:
    let character = body[pos]
    if not (
      character.isAlphaAscii or 
      character.isDigitAscii or
      character == Rune(0x005F)
    ):
      break
    inc(pos)
  
  return newToken(TokenKind.NAME, start, pos, line, col, prev, $(body[start ..< pos]))


proc readDigits(self: Lexer, start: int, character: var Rune): int =
  #[
    Return the new position in the source after reading digits.
  ]#
  let
    body = self.source.body.toRunes()
    bodyLen = body.len
  var pos = start
  while character.isDigitAscii:
    inc(pos)
    if pos < bodyLen:
      character = body[pos]
    else:
      break
  if pos == start:
    # TODO: Should be GraphQLSyntaxError
    raise newException(ValueError, &"Invalid number, expected digit but got: '{printChar(character)}'.")
  return pos


proc readNumber(self: Lexer, start: int, character: Rune, line: int, col: int, prev: Token): Token =
  ##[
    Reads a number token from the source file.
    
    Either a float or an int depending on whether a decimal point appears.
  ]##
  let
    body = self.source.body.toRunes()
  var
    pos = start
    isFloat = false
    newChar = option(character)

  # Check if hyphen-minus character (-)
  if not newChar.isNone and newChar.get() == Rune(0x002D):
    inc(pos)
    newChar = getCharacter(body, pos)
  # Check if digit zero (0)
  if not newChar.isNone and newChar.get() == Rune(0x0030):
    inc(pos)
    newChar = getCharacter(body, pos)
    if not newChar.isNone and isDigitAscii(newChar.get()):
      # TODO: Should be GraphQLSyntaxError
      raise newException(ValueError, &"Invalid number, unexpected digit after 0: '{printChar(newChar.get())}'.")
  elif not newChar.isNone:
    pos = self.readDigits(pos, newChar.get())
    newChar = getCharacter(body, pos)
  # else:
  #   newChar = getCharacter(body, pos + 1)

  # Check if dot character (.)
  if not newChar.isNone and newChar.get() == Rune(0x002E):
    isFloat = true
    inc(pos)
    newChar = getCharacter(body, pos)
    if not newChar.isNone:
      pos = self.readDigits(pos, newChar.get())
      newChar = getCharacter(body, pos)

  # Check if capital or small latin letter e (E, e).
  if not newChar.isNone and newChar.get() in [Rune(0x0045), Rune(0x0065)]:
    isFloat = true
    inc(pos)
    newChar = getCharacter(body, pos)
    # Check if plus or hyphen-minus sign (+, -)
    if not newChar.isNone and newChar.get() in [Rune(0x002B), Rune(0x002D)]:
      inc(pos)
      newChar = getCharacter(body, pos)
    if not newChar.isNone:
      pos = self.readDigits(pos, newChar.get())
      newChar = getCharacter(body, pos)

  # Numbers cannot be followed by . or NameStart
  if not newChar.isNone and (
    newChar.get() == Rune(0x002E) or isNameStart(newChar.get())
  ):
    # TODO: Should be GraphQLSyntaxError
    raise newException(ValueError, &"Invalid number, expected digit but got: '{printChar(newChar.get())}'.")

  var finalTokenKind: TokenKind
  if isFloat:
    finalTokenKind = TokenKind.FLOAT
  else:
    finalTokenKind = TokenKind.INT
  return newToken(finalTokenKind, start, pos, line, col, prev, $(body[start ..< pos]))


proc readBlockString(self: var Lexer, start: int, line: int, col: int, prev: Token): Token =
  ##[
    Reads a block string token from the source file.
  ]##
  let
    body = self.source.body.toRunes()
    bodyLen = body.len
  var
    pos = start + 3
    chunkStart = pos
    rawValue = ""

  while pos < bodyLen:
    var character = body[pos]
    if character == Rune(0x0022) and pos + 2 <= bodyLen and body[pos + 1 ..< pos + 3] == @[Rune(0x0022), Rune(0x0022)]:
      rawValue = rawValue & $(body[chunkStart ..< pos])
      return newToken(TokenKind.BLOCK_STRING, start, pos + 3, line, col, prev, dedentBlockStringValue(rawValue))
    # if character < ' ' and character notin {'\t', '\n', '\r'}:
    if character.int < 32 and character notin @[Rune(0x0009), Rune(0x000A), Rune(0x000D)]:
      # TODO: Should be GraphQLSyntaxError
      raise newException(ValueError, fmt"Invalid character within String: '{printChar(character)}'.")
    if character == Rune(0x000A):
      inc(pos)
      inc(self.line)
      self.lineStart = pos
    elif character == Rune(0x000D):
      if pos + 1 < bodyLen and body[pos + 1] == Rune(0x000A):
        inc(pos, 2)
      else:
        inc(pos, 1)
      inc(self.line)
      self.lineStart = pos
    elif character == Rune(0x005C) and pos + 3 < bodyLen and (
      body[pos + 1 ..< pos + 4] == @[Rune(0x0022), Rune(0x0022), Rune(0x0022)]
    ):
      rawValue = rawValue & $(body[chunkStart ..< pos]) & '"'.repeat(3)
      inc(pos, 4)
      chunkStart = pos
    else:
      inc(pos)
  
  # TODO: Should be GraphQLSyntaxError
  raise newException(ValueError, "Unterminated string.")


proc readString(self: Lexer, start: int, line: int, col: int, prev: Token): Token =
  ##[
    Read a string token from the source file.
  ]##
  let
    body = self.source.body.toRunes()
    bodyLen = body.len
  var
    pos = start + 1
    chunkStart = pos
    value: seq[Rune]

  while pos < bodyLen:
    var character = body[pos]
    if character in @[Rune(0x000D), Rune(0x000A)]:
      break
    if character == Rune(0x0022):
      value.add(body[chunkStart ..< pos])
      return newToken(TokenKind.STRING, start, pos + 1, line, col, prev, $value)
    # if character < ' ' and character != '\t':
    if character.int < 32 and character notin @[Rune(0x0009)]:
      # TODO: Should be GraphQLSyntaxError
      raise newException(ValueError, fmt"Invalid character within String: '{printChar(character)}'.")

    inc(pos)
    if character == Rune(0x005C):
      value.add(body[chunkStart ..< pos - 1])
      character = body[pos]
      let isEscaped = isEscapedChar(character)
      if isEscaped:
        let escapedValue = EscapedChars.getOrDefault(character)
        value.add(escapedValue)
      # Check if latin small letter u and unicode len
      elif character == Rune(0x0075) and pos + 4 < bodyLen:
        let code = unicodeCharCode(body[pos + 1], body[pos + 2], body[pos + 3], body[pos + 4])
        if code < 0:
          # TODO: Should be GraphQLSyntaxError
          raise newException(ValueError, fmt"Invalid character escape sequence: '\u{$body[pos ..< pos + 5]}'.")
        value.add([Rune(0x005C), Rune(0x0075)])
        value.add(body[pos + 1 ..< pos + 5])
        inc(pos, 4)
      else:
        let escape = "\\" & character.toUTF8()
        # TODO: Should be GraphQLSyntaxError
        raise newException(ValueError, fmt"Invalid character escape sequence: '{escape}'.")
      inc(pos)
      chunkStart = pos
  # TODO: Should be GraphQLSyntaxError
  raise newException(ValueError, "Unterminated string.")


proc readToken*(self: var Lexer, prev: Token): Token =
  ##[
    Get the next token from the source starting at the given position.

    This skips over whitespace until it finds the next lexable token, then lexes
    punctuators immediately or calls the appropriate helper function for more
    complicated tokens.
  ]##
  let
    body = self.source.body.toRunes()
    bodyLen = body.len
    pos = self.positionAfterWhitespace(body, prev.`end`)
    line = self.line
    col = 1 + pos - self.lineStart

  if pos >= bodyLen:
    return newToken(TokenKind.EOF, bodyLen, bodyLen, line, col, prev)

  var
    character = body[pos]
    kind: bool = false
  let isToken = isTokenKind($character)

  if isToken:
    kind = isPunctuatorTokenKind(parseEnum[TokenKind]($character))

  if kind:
    let parsedTokenKind = parseEnum[TokenKind]($character)
    return newToken(parsedTokenKind, pos, pos + 1, line, col, prev)
  # Check if it is a hashtag character (#)
  elif character == Rune(0x0023):
    return self.readComment(pos, line, col, prev)
  # Check if it is a triple dot character sequence (...)
  elif character == Rune(0x002E):
    if pos + 2 < bodyLen and body[pos + 1 ..< pos + 3] == @[Rune(0x002E), Rune(0x002E)]:
      return newToken(TokenKind.SPREAD, pos, pos + 3, line, col, prev)
  # Check if is is a alphabetic or Low line (_) character
  elif character.isAlphaAscii or character == Rune(0x005F):
    return self.readName(pos, line, col, prev)
  # Check if it is a number taking into account hyphen-minus character (-) for negative numbers.
  elif character.isDigitAscii or character == Rune(0x002D):
    return self.readNumber(pos, character, line, col, prev)
  # Check if triple quotation mark character (")
  elif character == Rune(0x0022):
    if pos + 2 < bodyLen and body[pos + 1 ..< pos + 3] == @[Rune(0x0022), Rune(0x0022)]:
      return self.readBlockString(pos, line, col, prev)
    return self.readString(pos, line, col, prev)
  # TODO: Should be GraphQLSyntaxError
  raise newException(ValueError, unexpectedCharacterMessage(character))


proc lookahead*(self: var Lexer): Token =
  ##[
    Look ahead and return the next non-ignored token, 
    but do not change state.
  ]##
  var token = self.token
  if token.kind != TokenKind.EOF:
    while true:
      if isNil token.next:
        token.next = self.readToken(token)
      token = token.next
      if token.kind != TokenKind.COMMENT:
        break
  return token


proc advance*(self: var Lexer): Token =
  ##[
    Advance the token stream to the next non-ignored token.
  ]##
  self.lastToken = self.token
  self.token = self.lookahead()
  return self.token

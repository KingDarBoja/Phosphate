import re, strutils

proc dedent*(text: string): string =
  #[
    Fix indentation of given text by removing leading spaces and tabs.

    Also removes leading newlines and trailing spaces and tabs, 
    but keeps trailing newlines.

    Thanks to Leorize for optimizing the code and making it
    work properly with the string comparison.
  ]#
  # use {.global.} to avoid recompiling the regex multiple times
  let leadingWhitespace {.global.} = re(r"(^[ \t]*)(?:[ \t\n])", {reMultiline})

  result = text.strip(trailing = false, chars = {'\n'})
               .strip(leading = false, chars = {' ', '\t'})

  # Look for the longest leading string of spaces 
  # and tabs common to all lines.
  var margin: string

  for indent in result.findAll(leadingWhitespace):
    if margin.len == 0:
      margin = indent

    # Current line more deeply indented than previous winner:
    # no change (previous winner is still on top).
    elif indent.startsWith(margin):
      discard

    # Current line consistent with and no deeper than previous winner:
    # it's the new winner.
    elif margin.startsWith(indent):
      margin = indent

    # Find the largest common whitespace between current line and previous
    # winner.
    else:
      # compare each character in margin and indent and cut off margin if 
      # indent contain a character different from what's in margin
      for i in 0 .. min(margin.high, indent.high):
        if margin[i] != indent[i]:
          margin = margin[0 ..< i]
          break

  if margin.len > 0:
    result = result.replace(re("(?m)^" & margin), "")
## Block String Module
import strutils

proc leadingWhitespace(s: string): int =
  var i = 0
  var n = s.len
  while i < n and s[i] in " \t":
    inc(i)
  return i


proc getBlockStringIndentation*(lines: seq[string]): int =
  #[
    Get the amount of indentation for the given block string.

    For internal use only.
  ]#
  var hasCommonIndent = false
  var commonIndent = 0

  if lines.len > 0:
    for line in lines[1..^1]:
      var indent = leadingWhitespace(line)
      if indent == line.len:
        # skip empty lines
        continue

      if not hasCommonIndent or indent < commonIndent:
        hasCommonIndent = true
        commonIndent = indent
        if commonIndent == 0:
          break
  
  return commonIndent
    

proc dedentBlockStringValue*(rawString: string): string =
  #[
    Produce the value of a block string from its parsed raw value.

    Similar to CoffeeScript's block string, Python's docstring trim or Ruby's
    strip_heredoc.

    This implements the GraphQL spec's BlockStringValue() static algorithm.

    For internal use only.
  ]#
  # Expand a block string's raw value into independent lines.
  var lines = rawString.splitLines

  # Remove common indentation from all lines but first.
  let commonIndent = getBlockStringIndentation(lines)

  if commonIndent != 0:
    var baseline: seq[string]
    for line in lines[1..^1]:
      if common_indent < line.len:
        baseline.add(line[common_indent..^1])
      else:
        baseline.add(line)
    lines[1..^1] = baseline

  # Remove leading and trailing blank lines.
  while lines[0].strip.len == 0:
    lines = lines[1..^1]
    
  while lines[^1].strip.len == 0:
    lines = lines[0..^2]

  # Return a string of the lines joined with U+000A.
  return join(lines, "\n")


proc printBlockString*(value: string, indentation: string = "", preferMultipleLines: bool = false): string =
  #[
    Print a block string in the indented block form.

    Prints a block string in the indented block form by adding a leading and
    trailing blank line. However, if a block string starts with whitespace and
    is a single-line, adding a leading blank line would strip that whitespace.

    For internal use only.
  ]#
  var isSingleLine = "\n" notin value
  var hasLeadingSpace = value.startsWith(" ") or value.startsWith("\t")
  var hasTrailingQuote = value.endsWith("\"")
  var printAsMultipleLines = not isSingleLine or hasTrailingQuote or preferMultipleLines

  var res: string
  # Format a multi-line block quote to account for leading space.
  if printAsMultipleLines and not (isSingleLine and hasLeadingSpace):
    res = "\n" & indentation
  else:
    res = ""

  if indentation.len > 0:
    res = res & value.replace("\n", "\n" & indentation)
  else:
    res = res & value
  
  if printAsMultipleLines:
    res = res & "\n"

  return "\"\"\"" & res.replace("\"\"\"", "\\\"\"\"") & "\"\"\""
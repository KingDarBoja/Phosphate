## Location Module
import strutils

from source import Source

type SourceLocation* = object ## Represents a location in a Source.
  line: int
  column: int

proc initSourceLocation*(line: int, column: int): SourceLocation = 
  result.line = line
  result.column = column

proc getLocation*(source: Source, position: int): SourceLocation =
  #[
    Get the line and column for a character position in the source.

    Takes a Source and a UTF-8 character offset, and returns the corresponding line 
    and column as a SourceLocation.
  ]#
  var lines: seq[string] = source.body[0..^position].splitLines
  var line: int
  var column: int
  if lines.len != 0:
    line = len(lines)
    column = len(lines[-1]) + 1
  else:
    line = 1
    column = 1
  return initSourceLocation(line, column)
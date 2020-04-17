import re, sequtils, strformat, strutils, sugar

import ast
import source_location


proc printPrefixedLines*(lines: seq[(string, string)]): string =
  ##[
    Print lines specified like this: ("prefix", "string")
  ]##
  var existingLines: seq[(string, string)]
  for line in lines.items:
    if line[1].len > 0:
      existingLines.add(line)

  let lineLenSeq = collect(newSeq):
    for line in existingLines: line[0].len
  let padLen = max(lineLenSeq)

  return join(
    map(
      existingLines,
      (proc (line: (string, string)): string = line[0].align(padLen) & (if line[1].len > 0: " | " & line[1]  else: " |"))
    ), "\n")


proc printSourceLocation*(source: Source, sourceLocation: SourceLocation): string =
  ##[
    Render a helpful description of the location in the GraphQL Source document.
  ]##
  let newLine {.global.} = re(r"\r\n|[\n\r]")

  let
    firstLineColumnOffset = source.locationOffset.column - 1
    body = repeat(" ", firstLineColumnOffset) & source.body

    lineIndex = sourceLocation.line - 1
    lineOffset = source.locationOffset.line - 1
    lineNum = sourceLocation.line + lineOffset

    columnOffset = if sourceLocation.line == 1: firstLineColumnOffset else: 0
    columnNum = sourceLocation.column + columnOffset
    locationStr = source.name & ":" & $lineNum & ":" & $columnNum & "\n"

    lines = split(body, newLine)
    locationLine = lines[lineIndex]

  # Special case for minified documents
  if locationLine.len > 120:
    let
      subLineIndex = (columnNum div 80)
      subLineColumnNum = (columnNum mod 80)
    var subLines: seq[string]
    for i in countup(0, locationLine.len, 80):
      if i + 80 < locationLine.len:
        subLines.add(locationLine[i ..< i + 80])
      else:
        subLines.add(locationLine[i ..^ 1])

    # This is a rough equivalent as there is no list unpacking
    var prefixedLines: seq[(string, string)]
    prefixedLines.add(($lineNum, subLines[0]))
    if subLineIndex + 1 < subLines.len:
      for subLine in subLines[1..<subLineIndex + 1]:
        prefixedLines.add(("", subLine))
    prefixedLines.add((" ", repeat(" ", subLineColumnNum - 1) & "^"))
    if subLineIndex < subLines.len - 1:
      prefixedLines.add(("", subLines[subLineIndex + 1]))

    return locationStr & printPrefixedLines(prefixedLines)

  return locationStr & printPrefixedLines(@[
    (fmt"{lineNum - 1}", if lineIndex > 0: lines[lineIndex - 1] else: ""),
    (fmt"{lineNum}", locationLine),
    ("", repeat(" ", columnNum - 1) & "^"),
    (fmt"{lineNum + 1}", if lineIndex < lines.len - 1: lines[lineIndex + 1] else: "")
  ])


proc printLocation*(location: Location): string =
  ##[
    Render a helpful description of the location in the GraphQL Source document.
  ]##
  return printSourceLocation(
    location.source,
    getLocation(location.source, location.start)
  )
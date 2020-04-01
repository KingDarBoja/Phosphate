import strutils

proc dedent*(text: string): string =
  #[
    Fix indentation of given text by removing leading spaces and tabs.

    Also removes leading newlines and trailing spaces and tabs, 
    but keeps trailing newlines.
  ]#
  return unindent(text
    .strip(trailing = false, chars = {'\n'})
    .strip(leading = false, chars = {' ', '\t'})  
  )
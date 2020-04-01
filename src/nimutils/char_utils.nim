import options

proc getCharacter*(s: string, pos: int): Option[char] =
  #[
    Return the character at the specified position or `None` 
    option which represents an empty value
  ]#
  if pos < s.len:
    return option(s[pos])
  return none(char)
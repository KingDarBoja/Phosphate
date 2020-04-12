import unittest

import nimutils/dedent

suite "Describe dedent":
  
  test "Removes indendation in typical usage":
    check(
      dedent(
        """
        type Query {
          me: User
        }

        type User {
          id: ID
          name: String
        }
        """
      ) == "type Query {\n  me: User\n}\n\n" &
      "type User {\n  id: ID\n  name: String\n}\n"
    )

  test "Removes only the first level of indentation":
    check(
      dedent(
        """
        first
          second
            third
              fourth
        """
      ) == "first\n  second\n    third\n      fourth\n"
    )

  test "Does not escape special characters":
    check(
      dedent(
        """
        type Root {
          field(arg: String = "wi\th de\fault"): String
        }
        """
      ) == "type Root {\n" &
      "  field(arg: String = \"wi\\th de\\fault\"): String\n}\n"
    )

  test "Also removes indendation using tabs":
    check(
      dedent(
        """
                type Query {
                  me: User
                }
        """
      ) == "type Query {\n  me: User\n}\n"
    )

  test "Removes leading newlines":
    check(
      dedent(
        """


         type Query {
           me: User
         }"""
      ) == "type Query {\n  me: User\n}"
    )

  test "Removes all trailing spaces and tabs":
    check(
      dedent(
        """
        type Query {
          me: User
        }
                     """
      ) == "type Query {\n  me: User\n}\n"
    )

  test "Works on text without leading newline":
    let sampleBlock = 
                """                type Query {
                  me: User
                }"""
    check(
      dedent(sampleBlock) == "type Query {\n  me: User\n}"
    )
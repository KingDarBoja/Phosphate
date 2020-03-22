import unittest

import language/token_kind

suite "Token Kind Test":
  
  test "Access a Token Kind enum value":
    let token = AMP
    check($token == "&")
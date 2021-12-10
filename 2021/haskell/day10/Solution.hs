module Solution where

{-(c : cs) (p : ps) if ( c = '(' and p = ')')
  continue with cs and ps
-}

bracketPairs =
  [ ('(', ')'),
    ('[', ']'),
    ('{', '}'),
    ('<', '>')
  ]

--matching :: String -> Char -- First wrong bracket

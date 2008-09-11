-- |Standardizes the language specification of submitted tests.
module CS173.NormalizeSubmissions 
  ( standardizeLang
  ) where

-- |Removes leading whitespace and PLT Scheme comments from the string.
pltWs :: String -> String
pltWs [] = []
pltWs (';':rest) = pltWs $ pltWsLine rest
pltWs ('#':'|':rest) = pltWs $ pltWsBlock rest
-- TODO: s-exp whitespace pltWs ('#':';':rest) = pltWs $ pltWsSexp rest
pltWs (ch:rest) 
  | ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' = pltWs rest
  | otherwise = ch:rest

pltWsLine ('\r':'\n':rest) = rest
pltWsLine ('\n':rest) = rest
pltWsLine ('\r':rest) = rest
pltWsLine (_:rest) = pltWsLine rest
pltWsLine [] = []

pltWsBlock ('|':'#':rest) = rest
pltWsBlock ('#':'|':rest) = pltWsBlock $ pltWsBlock rest
pltWsBlock (_:rest) = pltWsBlock rest
pltWsBlock [] = [] -- this is a syntax error

  


standardizeLang :: String -- ^'#lang' name
                -> String -- ^submission text
                -> String -- ^submission in the given '#lang'
standardizeLang langName submission = 
  let sub = pltWs submission in
  case sub of
    '#':'l':'a':'n':'g':_ -> submission
    '#':'r':'e':'a':'d':'e':'r':_ -> submission
    otherwise -> "#lang " ++ langName ++ "\n" ++ submission

module Globber where

import Data.List (tails)

data GlobChar
  = Literal Char
  | AnyOne
  | AnyMany deriving (Eq, Show)

type GlobPattern = String
type ParsedPattern = [GlobChar]


parsePattern :: GlobPattern -> ParsedPattern
parsePattern [] = []
parsePattern (x:xs) = parse x xs
  where parse '\\' (y:ys) = (Literal y):(parsePattern ys)
        parse '?' _ = AnyOne:parseRest
        parse '*' _ = AnyMany:parseRest
        parse c _ = (Literal c):parseRest
        parseRest = parsePattern xs

matchParsed :: ParsedPattern -> String -> Bool
matchParsed [] [] = True
matchParsed [] _ = False
matchParsed (g:gs) cs'@(c:cs) = match g
  where match AnyOne = matchRest
        match (Literal l) = l == c && matchRest
        match AnyMany = any (matchParsed gs) $ tails cs'
        matchRest = matchParsed gs cs
matchParsed gs [] = all (== AnyMany) gs

matchGlob :: GlobPattern -> String -> Bool
matchGlob = matchParsed . parsePattern

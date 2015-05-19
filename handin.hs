------------------------------ Globber.hs ------------------------------
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


------------------------------ TestGlobber.hs --------------------------
module Main (main) where

import Test.Hspec
import Data.List (tails)
import Globber

main :: IO ()
main = hspec $ describe "Testing Globber" $ do
         describe "parser" $ do
           it "preserves literals" $ do
             parsePattern "abce" `shouldBe` map Literal "abce"
           it "recognizes special characters" $ do
             parsePattern "*?" `shouldBe` [AnyMany, AnyOne]
           it "escapes special chacters" $ do
             parsePattern "\\*\\?*?ab" `shouldBe` [Literal '*',
                                                   Literal '?',
                                                   AnyMany,
                                                   AnyOne,
                                                   Literal 'a',
                                                   Literal 'b']
           it "escapes backslashes" $ do
             parsePattern "\\\\" `shouldBe` [Literal '\\']

         describe "empty pattern" $ do
           it "matches empty string" $
             matchGlob "" ""
           it "shouldn't match non-empty string" $ do
             not $ matchGlob "" "string"
         describe "question mark" $ do
           it "matches any character" $
             all (matchGlob "?") $ map (:[]) ['A'..'z']
           it "does not match empty string" $ do
             not $ matchGlob "?" ""
         describe "star" $ do
           it "matches the empty string" $
             matchGlob "*" ""
           it "matches any string" $
             all (matchGlob "*") $ tails ['A'..'z']
           it "can be repeated indefinitely" $
             all (matchGlob $ replicate 30 '*') $ tails ['A'..'z']
         describe "mixtures" $ do
             it "can intersperse ? and literals" $ do
               matchGlob "?b?b?b" "ababab"
             it "rejects bad matches with ? and literals" $ do
               not $ matchGlob "?b?b?b" "bababa"
             it "can intersperse * and ?" $ do
               all (matchGlob "*???*") ["aaa", "aaaaaa", "aaaaaaaa"]
             it "rejects bad patterns with * and ?" $ do
               all (not . matchGlob "*?????*") $ tails "aaaa"
             it "can intersperse * and literals" $ do
               matchGlob "*a*b*c*d*e" "aebdcbdae"

------------------------------ Output ----------------------------------
{- From a clean build:
$  cmsc-223-wk8-hw1  cabal configure --enable-tests
Resolving dependencies...
Configuring globber-1.0.0...
cabal: At least the following dependencies are missing:
hspec -any
$  cmsc-223-wk8-hw1  cabal install
Resolving dependencies...
Notice: installing into a sandbox located at
/home/patrick/hacking/courses/cmsc223/cmsc-223-wk8-hw1/.cabal-sandbox
Configuring globber-1.0.0...
Building globber-1.0.0...
...
Installed globber-1.0.0
$  cmsc-223-wk8-hw1  dist/build/test-globber/test-globber

Testing Globber
  parser
    preserves literals
    recognizes special characters
    escapes special chacters
    escapes backslashes
  empty pattern
    matches empty string
    shouldn't match non-empty string
  question mark
    matches any character
    does not match empty string
  star
    matches the empty string
    matches any string
    can be repeated indefinitely
  mixtures
    can intersperse ? and literals
    rejects bad matches with ? and literals
    can intersperse * and ?
    rejects bad patterns with * and ?
    can intersperse * and literals

Finished in 0.0105 seconds
16 examples, 0 failures
-}

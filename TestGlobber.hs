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

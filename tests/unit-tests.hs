{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.FlatSet as FlatSet

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "ut"
  [ testCase "size no duplicates" $ length values @=? FlatSet.size fs
  , testCase "size with duplicates" $ FlatSet.size (FlatSet.fromList ["a","b","b","a"]) @?= 2
  , testGroup "member"
     [ testCase "first element" $ FlatSet.member "a" fs @?= True
     , testCase "last element"  $ FlatSet.member "d" fs @?= True
     , testCase "not found"     $ FlatSet.member "c" fs @?= False
     , testCase "empty map"     $ FlatSet.member "x" (FlatSet.fromList []) @?= False
     , testCase "singleton exists"     $ FlatSet.member "xy" (FlatSet.fromList ["xy"]) @?= True
     , testCase "singleton missing"    $ FlatSet.member "yx" (FlatSet.fromList ["xy"]) @?= False
     , testCase "pair exists"    $ FlatSet.member "yx" (FlatSet.fromList ["xy","yx"]) @?= True
     , testCase "pair missing"   $ FlatSet.member "yz" (FlatSet.fromList ["xy","yx"]) @?= False
     , testCase "middle even 1"   $ FlatSet.member "c"  (FlatSet.fromList ["a","b","c","d"]) @?= True
     , testCase "middle even 2"   $ FlatSet.member "b"  (FlatSet.fromList ["a","b","c","d"]) @?= True
     , testCase "even mising"     $ FlatSet.member "bc" (FlatSet.fromList ["a","b","c","d"]) @?= False
     ]
  ]
  where
    values = ["a","d","b"]
    fs = FlatSet.fromList values


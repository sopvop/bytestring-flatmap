{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.FlatSet as FlatSet

import qualified Data.ByteString.Char8 as B8
import qualified Data.List as List
import qualified Data.Set  as Set

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC

main :: IO ()
main = defaultMain $ testGroup "ut"
  [ testCase "size no duplicates" $ length values @=? FlatSet.size fs
  , testCase "size with duplicates" $ FlatSet.size (FlatSet.fromList ["a","b","b","a"]) @?= 2
  , SC.testProperty "toList . fromList = nub . sort" $ changeDepth (\x -> x - 2) $
     \slist -> let list = map B8.pack slist
              in FlatSet.toList (FlatSet.fromList list) == List.nub (List.sort list)
  , SC.testProperty "notMember = not . member" $
     \slist selm -> let list = map B8.pack slist
                        elm  = B8.pack selm
                    in FlatSet.notMember elm (FlatSet.fromList list) == not (FlatSet.member elm (FlatSet.fromList list))
  , SC.testProperty "lookupGT = Set.lookupGT" $
     \slist -> let list = map B8.pack slist
                   fs   = FlatSet.fromList list
                   ss   = Set.fromList list
        in all (\e -> FlatSet.lookupGT e fs == Set.lookupGT e ss) list
  , SC.testProperty "lookupGE = Set.lookupGE" $
     \slist -> let list = map B8.pack slist
                   fs   = FlatSet.fromList list
                   ss   = Set.fromList list
        in all (\e -> FlatSet.lookupGE e fs == Set.lookupGE e ss) list
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


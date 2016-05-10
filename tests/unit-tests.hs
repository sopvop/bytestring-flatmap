{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Maybe              (catMaybes)

import qualified Data.ByteString.FlatMap as FlatMap
import qualified Data.ByteString.FlatSet as FlatSet

import qualified Data.ByteString.Char8   as B8
import qualified Data.List               as List
import qualified Data.Set                as Set

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck   as SC

main :: IO ()
main = defaultMain $ testGroup "ut" [flatSetTests  , flatMapTests ]

flatSetTests = testGroup "FlatSet"
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
  , testGroup "intersect" intersectTests
  , testGroup "uniont" unionTests
  ]
  where
    values = ["a","d","b"]
    fs = FlatSet.fromList values


intersectTests =
  [ testCase "intersect" $ FlatSet.intersection left right @?= intersection
  , testCase "intersect flip" $ FlatSet.intersection right left @?= intersection
  , testCase "intersect empty left" $ FlatSet.intersection left empty @?= empty
  , testCase "intersect empty right" $ FlatSet.intersection empty left @?= empty
  ]
  where
    intersection = FlatSet.fromList ["a", "c", "xy"]
    empty = FlatSet.fromList []
    left = FlatSet.fromList ["a", "b", "c", "d","xy"]
    right = FlatSet.fromList ["a", "c", "f", "xy", "zx"]

unionTests =
  [ testCase "union"              $ FlatSet.union left right @?= union
  , testCase "union flip"         $ FlatSet.union right left @?= union
  , testCase "union empty left"   $ FlatSet.union empty right @?= right
  , testCase "union empty right"  $ FlatSet.union right empty @?= right
  ]
  where
    union = FlatSet.fromList ["a", "b", "c", "d", "f", "xy", "zx"]
    empty = FlatSet.fromList []
    left = FlatSet.fromList ["a", "b", "c", "d","xy"]
    right = FlatSet.fromList ["a", "c", "f", "xy", "zx"]


flatMapTests = testGroup "FlatMap"
  [ testGroup "query" testMapConstruction
  , testGroup "map"  testMapMapping
  ]

testMapConstruction =
  [ testCase "size"      $ FlatMap.size flatMap     @?= 3
  , testCase "size dups" $ FlatMap.size flatMapDups @?= 3

  , testCase "all keys ord" $ FlatMap.toKeys flatMap     @?= keys
  , testCase "all dup keys" $ FlatMap.toKeys flatMapDups @?= keys

  , testCase "all vals"     $ FlatMap.toValues flatMap      @?= values
  , testCase "all dup vals" $ FlatMap.toValues flatMapDups  @?= values

  , testCase "lookup present" $ catMaybes (map (flip FlatMap.lookup flatMap) keys)  @?= values
  , testCase "lookup absent"  $ FlatMap.lookup "xx" flatMap  @?= Nothing

  ]
  where
    flatMap = FlatMap.fromList mList
    flatMapDups = FlatMap.fromList mListDups
    mList = [("c", 3), ("a", 1), ("b", (2::Int))]
    mListDups = [("c", 3), ("a", 1), ("c", 4), ("a", 5), ("b", (2::Int))]
    keys = ["a","b","c"]
    values = [1,2,3]

testMapMapping =
  [ testCase "adjust" $ FlatMap.toValues (FlatMap.adjust (+1) "b" flatMap) @?= [1,3,3]
  , testCase "adjust absent" $ FlatMap.toValues (FlatMap.adjust (+1) "xx" flatMap) @?= [1,2,3]

  , testCase "map"  $ FlatMap.toValues (FlatMap.map  (+1) flatMap) @?= (map (+1) values)
  , testCase "imap"  $ let go bs v = if bs == "b" then v + 1 else v - 1
                       in FlatMap.toValues (FlatMap.imap go flatMap) @?= [0,3,2]
  , testCase "mapM"  $ do
      mv <- FlatMap.mapM  (\v -> return (v+1)) flatMap
      FlatMap.toValues mv @?= (map (+1) values)
  , testCase "imapM"  $ do
      let go bs v = return $ if bs == "b" then v + 1 else v -1
      mv <- FlatMap.imapM  go flatMap
      FlatMap.toValues mv @?= [0,3,2]
  ]
  where
    flatMap = FlatMap.fromList mList
    mList = [("a", 1), ("b", 2), ("c", (3::Int))]
    keys = ["a","b","c"]
    values = [1,2,3]

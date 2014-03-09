import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Function
import Test.Tasty.HUnit

import Data.Word
import Data.WordMap

import Prelude hiding (lookup, null, filter, foldr, foldl, map)

instance Function Word where
    function = functionMap (fromIntegral :: Word -> Int) fromIntegral

instance Arbitrary a => Arbitrary (WordMap a) where
    arbitrary = fmap fromList arbitrary

main :: IO ()
main = defaultMain $ testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [ testProperty "fromList/toList" $ \m -> (m :: WordMap Int) == fromList (toList m)
    , testProperty "lookup/insert" $ \k v m -> lookup k (insert k v (m :: WordMap Int)) == Just v
    , testProperty "lookup/delete" $ \k m -> lookup k (delete k (m :: WordMap Int)) == Nothing
    , testProperty "lookup/alter" $ \(Fun _ f) k m -> lookup k (alter f k (m :: WordMap Int)) == f (lookup k m)
    , testProperty "insertLookupWithKey spec" $ \(Fun _ f) k v m -> insertLookupWithKey (curry3 f) k v (m :: WordMap Int) == (lookup k m, insertWithKey (curry3 f) k v m)
    , testProperty "updateLookupWithKey spec" $ \(Fun _ f) k m -> updateLookupWithKey (curry f) k (m :: WordMap Int) == (lookup k m, updateWithKey (curry f) k m)
    , testGroup "Union"
        [ testProperty "Associativity" $ \m1 m2 m3 -> union (union m1 m2) (m3 :: WordMap Int) == union m1 (union m2 m3)
        , testProperty "Commutativity" $ \(Fun _ f) m1 m2 -> unionWithKey (curry3 f) (m1 :: WordMap Int) m2 == unionWithKey (\k v1 v2 -> curry3 f k v2 v1) m2 m1
        ]
    , testGroup "Intersection"
        [ testProperty "Associativity" $ \m1 m2 m3 -> intersection (intersection (m1 :: WordMap Int) (m2 :: WordMap Int)) (m3 :: WordMap Int) == intersection m1 (intersection m2 m3)
        , testProperty "Commutativity" $ \(Fun _ f) m1 m2 -> intersectionWithKey (curry3 f) (m1 :: WordMap Int) (m2 :: WordMap Int) == intersectionWithKey (\k v1 v2 -> curry3 f k v2 v1 :: Int) m2 m1
        , testProperty "Specification" $ \(Fun _ f) m1 m2 -> intersectionWithKey (curry3 f) (m1 :: WordMap Int) (m2 :: WordMap Int) == (mapMaybeWithKey (\k v -> fmap (curry3 f k v) (lookup k m2)) m1 :: WordMap Int)
        ]
    , testGroup "Split"
        [ testProperty "Specification" $ \k m -> splitLookup k (m :: WordMap Int) == (filterWithKey (\k' _ -> k' < k) m, lookup k m, filterWithKey (\k' _ -> k' > k) m)
        ]
    , testGroup "Partition"
        [ testProperty "Specification" $ \(Fun _ f) m -> partitionWithKey (curry f) (m :: WordMap Int) == (filterWithKey (curry f) m, filterWithKey (curry (not . f)) m)
        ]
    ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
    [ testGroup "Operators"
        [ testCase "(!)" $ fromList [(5,'a'), (3,'b')] ! 5 @?= 'a'
        ]
    , testGroup "Query"
        [ testGroup "null"
            [ testCase "empty" $ null empty @?= True
            , testCase "singleton" $ null (singleton 1 'a') @?= False
            ]
        , testGroup "size"
            [ testCase "empty" $ size empty @?= 0
            , testCase "singleton" $ size (singleton 1 'a') @?= 1
            , testCase "many" $ size (fromList [(1,'a'), (2,'c'), (3,'b')]) @?= 3
            ]
        , testGroup "member"
            [ testCase "present" $ member 5 (fromList [(5,'a'), (3,'b')]) @?= True
            , testCase "absent" $ member 1 (fromList [(5,'a'), (3,'b')]) @?= False
            ]
        , testGroup "findWithDefault"
            [ testCase "absent" $ findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) @?= 'x'
            , testCase "present" $ findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) @?= 'a'
            ]
        , testGroup "lookupLT"
            [ testCase "Nothing (equal)" $ lookupLT 3 (fromList [(3,'a'), (5,'b')]) @?= Nothing
            , testCase "Just" $ lookupLT 4 (fromList [(3,'a'), (5,'b')]) @?= Just (3, 'a')
            ]
        , testGroup "lookupGT"
            [ testCase "Just" $ lookupGT 4 (fromList [(3,'a'), (5,'b')]) @?= Just (5, 'b')
            , testCase "Nothing (equal)" $ lookupGT 5 (fromList [(3,'a'), (5,'b')]) @?= Nothing
            ]
        , testGroup "lookupLE"
            [ testCase "Nothing" $ lookupLE 2 (fromList [(3,'a'), (5,'b')]) @?= Nothing
            , testCase "Just (not equal)" $ lookupLE 4 (fromList [(3,'a'), (5,'b')]) @?= Just (3, 'a')
            , testCase "Just (equal)" $ lookupLE 5 (fromList [(3,'a'), (5,'b')]) @?= Just (5, 'b')
            ]
        , testGroup "lookupGE"
            [ testCase "Just (equal)" $ lookupGE 3 (fromList [(3,'a'), (5,'b')]) @?= Just (3, 'a')
            , testCase "Just (not equal)" $ lookupGE 4 (fromList [(3,'a'), (5,'b')]) @?= Just (5, 'b')
            , testCase "Nothing" $ lookupGE 6 (fromList [(3,'a'), (5,'b')]) @?= Nothing
            ]
        ]
    , testGroup "Construction"
        [ testGroup "empty"
            [ testCase "fromList" $ empty @?= fromList ([] :: [(Word, Char)])
            , testCase "size" $ size empty @?= 0
            ]
        , testGroup "singleton"
            [ testCase "fromList" $ singleton 1 'a' @?= fromList [(1, 'a')]
            , testCase "size" $ size (singleton 1 'a') @?= 1
            ]
        , testGroup "Insertion"
            [ testGroup "insert"
                [ testCase "override" $ insert 5 'x' (fromList [(5,'a'), (3,'b')]) @?= fromList [(3, 'b'), (5, 'x')]
                , testCase "insert (full)" $ insert 7 'x' (fromList [(5,'a'), (3,'b')]) @?= fromList [(3, 'b'), (5, 'a'), (7, 'x')]
                , testCase "insert (empty)" $ insert 5 'x' empty @?= singleton 5 'x'
                ]
            , testGroup "insertWith"
                [ testCase "override" $ insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "xxxa")]
                , testCase "insert (full)" $ insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a"), (7, "xxx")]
                , testCase "insert (empty)" $ insertWith (++) 5 "xxx" empty @?= singleton 5 "xxx"
                ]
            , let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
              in testGroup "insertWithKey"
                    [ testCase "override" $ insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "5:xxx|a")]
                    , testCase "insert (full)" $ insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a"), (7, "xxx")]
                    , testCase "insert (empty)" $ insertWithKey f 5 "xxx" empty @?= singleton 5 "xxx"
                    ]
            , let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
              in testGroup "insertLookupWithKey"
                    [ testCase "override" $ insertLookupWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) @?= (Just "a", fromList [(3, "b"), (5, "5:xxx|a")])
                    , testCase "insert (full)" $ insertLookupWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) @?= (Nothing, fromList [(3, "b"), (5, "a"), (7, "xxx")])
                    , testCase "insert (empty)" $ insertLookupWithKey f 5 "xxx" empty @?= (Nothing, singleton 5 "xxx")
                    ]
            ]
        , testGroup "Delete/Update"
            [ testGroup "delete"
                [ testCase "present" $ delete 5 (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "b"
                , testCase "absent (full)" $ delete 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a")]
                , testCase "absent (empty)" $ delete 5 empty @?= (empty :: WordMap Char)
                ]
            , testGroup "adjust"
                [ testCase "present" $ adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "new a")]
                , testCase "absent (full)" $ adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a")]
                , testCase "absent (empty)" $ adjust ("new " ++) 7 empty @?= empty
                ]
            , let f key x = (show key) ++ ":new " ++ x
              in testGroup "adjustWithKey"
                    [ testCase "present" $ adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "5:new a")]
                    , testCase "absent (full)" $ adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a")]
                    , testCase "absent (empty)" $ adjustWithKey f 7 empty @?= empty
                    ]
            , let f x = if x == "a" then Just "new a" else Nothing
              in testGroup "update"
                    [ testCase "present (adjust)" $ update f 5 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "new a")]
                    , testCase "absent" $ update f 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a")]
                    , testCase "present (delete)" $ update f 3 (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "a"
                    ]
            , let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
              in testGroup "updateWithKey"
                    [ testCase "present (adjust)" $ updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "5:new a")]
                    , testCase "absent" $ updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a")]
                    , testCase "present (delete)" $ updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "a"
                    ]
            , let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
              in testGroup "updateLookupWithKey"
                    [ testCase "present (adjust)" $ updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) @?= (Just "a", fromList [(3, "b"), (5, "5:new a")])
                    , testCase "absent" $ updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) @?= (Nothing,  fromList [(3, "b"), (5, "a")])
                    , testCase "present (delete)" $ updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) @?= (Just "b", singleton 5 "a")
                    ]
            ]
        ]
    , testGroup "Combine"
        [ testGroup "Union"
            [ testCase "union" $ union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= fromList [(3, "b"), (5, "a"), (7, "C")]
            , testCase "unionWith" $ unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= fromList [(3, "b"), (5, "aA"), (7, "C")]
            , let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
              in testCase "unionWithKey" $ unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= fromList [(3, "b"), (5, "5:a|A"), (7, "C")]
            , testGroup "unions"
                [ testCase "lower->upper" $ unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])] @?= fromList [(3, "b"), (5, "a"), (7, "C")]
                , testCase "upper->lower" $ unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])] @?= fromList [(3, "B3"), (5, "A3"), (7, "C")]
                ]
            , testCase "unionsWith" $ unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])] @?= fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]
            ]
        , testGroup "Difference"
            [ testCase "difference" $ difference (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= singleton 3 "b"
            , let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
              in testCase "differenceWith" $ differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")]) @?= singleton 3 "b:B"
            , let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
              in testCase "differenceWithKey" $ differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")]) @?= singleton 3 "3:b|B"
            ]
        , testGroup "Intersection"
            [ testCase "intersection" $ intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= singleton 5 "a"
            , testCase "intersectionWith" $ intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= singleton 5 "aA"
            , let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
              in testCase "intersectionWithKey" $ intersectionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= singleton 5 "5:a|A"
            ]
        ]
    , testGroup "Traversal"
        [ testGroup "Map"
            [ testCase "map" $ map (++ "x") (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "bx"), (5, "ax")]
            , let f key x = (show key) ++ ":" ++ x
              in testCase "mapWithKey" $ mapWithKey f (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "3:b"), (5, "5:a")]
            , testGroup "traverseWithKey"
                [ testCase "present" $ traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(1, 'a'), (5, 'e')]) @?= Just (fromList [(1, 'b'), (5, 'f')])
                , testCase "absent" $ traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(2, 'c')]) @?= Nothing
                ]
            , let f a b = (a ++ b, b ++ "X")
              in testCase "mapAccum" $ mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) @?= ("Everything: ba", fromList [(3, "bX"), (5, "aX")])
            , let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
              in testCase "mapAccumWithKey" $ mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) @?= ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])
            -- NOTE: This isn't in the docs
            , let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
              in testCase "mapAccumRWithKey" $ mapAccumRWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) @?= ("Everything: 5-a 3-b", fromList [(3, "bX"), (5, "aX")])
            , testGroup "mapKeys"
                [ testCase "simple" $ mapKeys (+ 1) (fromList [(5,"a"), (3,"b")]) @?= fromList [(4, "b"), (6, "a")]
                , testCase "collapse1" $ mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) @?= singleton 1 "c"
                , testCase "collapse3" $ mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) @?= singleton 3 "c"
                ]
            , testGroup "mapKeysWith"
                [ testCase "collapse1" $ mapKeysWith (++) (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) @?= singleton 1 "cdab"
                , testCase "collapse3" $ mapKeysWith (++) (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) @?= singleton 3 "cdab"
                ]
            , testCase "mapKeysMonotonic" $ mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")]) @?= fromList [(6, "b"), (10, "a")]
            ]
        ]
    , testGroup "Folds"
        [ let f a len = len + (length a)
          in testCase "foldr" $ foldr f 0 (fromList [(5,"a"), (3,"bbb")]) @?= 4
        , let f len a = len + (length a)
          in testCase "foldl" $ foldl f 0 (fromList [(5,"a"), (3,"bbb")]) @?= 4
        , let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
          in testCase "foldrWithKey" $ foldrWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) @?= "Map: (5:a)(3:b)"
        , let f result k a = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
          in testCase "foldlWithKey" $ foldlWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) @?= "Map: (3:b)(5:a)"
        -- FIXME: foldMapWithKey
        , testGroup "Strict folds" -- NOTE: These aren't in the docs
            [ let f a len = len + (length a)
              in testCase "foldr'" $ foldr' f 0 (fromList [(5,"a"), (3,"bbb")]) @?= 4
            , let f len a = len + (length a)
              in testCase "foldl'" $ foldl' f 0 (fromList [(5,"a"), (3,"bbb")]) @?= 4
            , let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
              in testCase "foldrWithKey'" $ foldrWithKey' f "Map: " (fromList [(5,"a"), (3,"b")]) @?= "Map: (5:a)(3:b)"
            , let f result k a = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
              in testCase "foldlWithKey'" $ foldlWithKey' f "Map: " (fromList [(5,"a"), (3,"b")]) @?= "Map: (3:b)(5:a)"
            ]
        ]
    , testGroup "Conversion"
        [ testGroup "elems"
            [ testCase "full" $ elems (fromList [(5,"a"), (3,"b")]) @?= ["b","a"]
            , testCase "empty" $ elems empty @?= ([] :: [Char])
            ]
        , testGroup "keys"
            [ testCase "full" $ keys (fromList [(5,"a"), (3,"b")]) @?= [3,5]
            , testCase "empty" $ keys empty @?= ([] :: [Word])
            ]
        , testGroup "assocs"
            [ testCase "full" $ assocs (fromList [(5,"a"), (3,"b")]) @?= [(3,"b"), (5,"a")]
            , testCase "empty" $ assocs empty @?= ([] :: [(Word, Char)])
            ]
        -- TODO: keysSet (unimplemented)
        -- TODO: fromSet (unimplemented)
        , testGroup "Lists"
            [ testGroup "toList"
                [ testCase "full" $ toList (fromList [(5,"a"), (3,"b")]) @?= [(3,"b"), (5,"a")]
                , testCase "empty" $ toList empty @?= ([] :: [(Word, Char)])
                ]
            , testGroup "fromList"
                [ testCase "empty" $ fromList ([] :: [(Word, Char)]) @?= empty
                , testCase "combine1" $ fromList [(5,"a"), (3,"b"), (5, "c")] @?= fromList [(5,"c"), (3,"b")]
                , testCase "combine2" $ fromList [(5,"c"), (3,"b"), (5, "a")] @?= fromList [(5,"a"), (3,"b")]
                ]
            , testGroup "fromListWith"
                [ testCase "full" $ fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"c")] @?= fromList [(3, "ab"), (5, "cba")]
                , testCase "empty" $ fromListWith (++) ([] :: [(Word, String)]) @?= empty
                ]
            , let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
              in testGroup "fromListWithKey"
                    [ testCase "full" $ fromListWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"c")] @?= fromList [(3, "3:a|b"), (5, "5:c|5:b|a")]
                    , testCase "empty" $ fromListWithKey f ([] :: [(Word, String)]) @?= empty
                    ]
            ]
        , testGroup "Ordered lists"
            [ testCase "toAscList" $ toAscList (fromList [(5,"a"), (3,"b")]) @?= [(3,"b"), (5,"a")]
            , testCase "toDescList" $ toDescList (fromList [(5,"a"), (3,"b")]) @?= [(5,"a"), (3,"b")]
            , testGroup "fromAscList"
                [ testCase "simple" $ fromAscList [(3,"b"), (5,"a")] @?= fromList [(3, "b"), (5, "a")]
                , testCase "combine" $ fromAscList [(3,"b"), (5,"a"), (5,"b")] @?= fromList [(3, "b"), (5, "b")]
                ]
            , testCase "fromAscListWith" $ fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] @?= fromList [(3, "b"), (5, "ba")]
            , let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
              in testCase "fromAscListWithKey" $ fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b")] @?= fromList [(3, "b"), (5, "5:b|a")]
            , testCase "fromDistinctAscList" $ fromDistinctAscList [(3,"b"), (5,"a")] @?= fromList [(3, "b"), (5, "a")]
            ]
        ]
    , testGroup "Filter"
        [ testGroup "filter"
            [ testCase "some" $ filter (> "a") (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "b"
            , testCase "none1" $ filter (> "x") (fromList [(5,"a"), (3,"b")]) @?= empty
            , testCase "none2" $ filter (< "a") (fromList [(5,"a"), (3,"b")]) @?= empty
            ]
        , testCase "filterWithKey" $ filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "a"
        , testGroup "partition"
            [ testCase "split" $ partition (> "a") (fromList [(5,"a"), (3,"b")]) @?= (singleton 3 "b", singleton 5 "a")
            , testCase "allL" $ partition (< "x") (fromList [(5,"a"), (3,"b")]) @?= (fromList [(3, "b"), (5, "a")], empty)
            , testCase "allR" $ partition (> "x") (fromList [(5,"a"), (3,"b")]) @?= (empty, fromList [(3, "b"), (5, "a")])
            ]
        , testGroup "partitionWithKey"
            [ testCase "split" $ partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) @?= (singleton 5 "a", singleton 3 "b")
            , testCase "allL" $ partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) @?= (fromList [(3, "b"), (5, "a")], empty)
            , testCase "allR" $ partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) @?= (empty, fromList [(3, "b"), (5, "a")])
            ]
        , let f x = if x == "a" then Just "new a" else Nothing
          in testCase "mapMaybe" $ mapMaybe f (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "new a"
        , let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
          in testCase "mapMaybeWithKey" $ mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "key : 3"
        , let f a = if a < "c" then Left a else Right a
          in testGroup "mapEither"
                [ testCase "split" $ mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")]) @?= (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
                , testCase "allR" $ mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")]) @?= (empty :: WordMap String, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
                ]
        , let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
          in testGroup "mapEitherWithKey"
                [ testCase "split" $ mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")]) @?= (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
                , testCase "allR" $ mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")]) @?= (empty :: WordMap String, fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])
                ]
        , testGroup "split"
            [ testCase "allR" $ split 2 (fromList [(5,"a"), (3,"b")]) @?= (empty, fromList [(3,"b"), (5,"a")])
            , testCase "allR (del)" $ split 3 (fromList [(5,"a"), (3,"b")]) @?= (empty, singleton 5 "a")
            , testCase "split" $ split 4 (fromList [(5,"a"), (3,"b")]) @?= (singleton 3 "b", singleton 5 "a")
            , testCase "allL (del)" $ split 5 (fromList [(5,"a"), (3,"b")]) @?= (singleton 3 "b", empty)
            , testCase "allL" $ split 6 (fromList [(5,"a"), (3,"b")]) @?= (fromList [(3,"b"), (5,"a")], empty)
            ]
        , testGroup "splitLookup"
            [ testCase "allR" $ splitLookup 2 (fromList [(5,"a"), (3,"b")]) @?= (empty, Nothing, fromList [(3,"b"), (5,"a")])
            , testCase "allR (del)" $ splitLookup 3 (fromList [(5,"a"), (3,"b")]) @?= (empty, Just "b", singleton 5 "a")
            , testCase "split" $ splitLookup 4 (fromList [(5,"a"), (3,"b")]) @?= (singleton 3 "b", Nothing, singleton 5 "a")
            , testCase "allL (del)" $ splitLookup 5 (fromList [(5,"a"), (3,"b")]) @?= (singleton 3 "b", Just "a", empty)
            , testCase "allL" $ splitLookup 6 (fromList [(5,"a"), (3,"b")]) @?= (fromList [(3,"b"), (5,"a")], Nothing, empty)
            ]
        ]
    , testGroup "Submap"
        [ testGroup "isSubmapOf" -- NOTE: These are not in the docs
            [ testCase "true1" $ isSubmapOf (fromList [(1,1)]) (fromList [(1,1),(2,2)]) @?= True
            , testCase "true3" $ isSubmapOf (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)]) @?= True
            , testCase "false1" $ isSubmapOf (fromList [(1,2)]) (fromList [(1,1),(2,2)]) @?= False
            , testCase "false3" $ isSubmapOf (fromList [(1,1),(2,2)]) (fromList [(1,1)]) @?= False
            ]
        , testGroup "isSubmapOfBy"
            [ testCase "true1" $ isSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)]) @?= True
            , testCase "true2" $ isSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)]) @?= True
            , testCase "true3" $ isSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)]) @?= True
            , testCase "false1" $ isSubmapOfBy (==) (fromList [(1,2)]) (fromList [(1,1),(2,2)]) @?= False
            , testCase "false2" $ isSubmapOfBy (<) (fromList [(1,1)]) (fromList [(1,1),(2,2)]) @?= False
            , testCase "false3" $ isSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)]) @?= False
            ]
        , testGroup "isProperSubmapOf" -- NOTE: These are not in the docs
            [ testCase "true1" $ isProperSubmapOf (fromList [(1,1)]) (fromList [(1,1),(2,2)]) @?= True
            , testCase "false1" $ isProperSubmapOf (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)]) @?= False
            , testCase "false2" $ isProperSubmapOf (fromList [(1,1),(2,2)]) (fromList [(1,1)]) @?= False
            ]
        , testGroup "isProperSubmapOfBy"
            [ testCase "true1" $ isProperSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)]) @?= True
            , testCase "true2" $ isProperSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)]) @?= True
            , testCase "false1" $ isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)]) @?= False
            , testCase "false2" $ isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)]) @?= False
            , testCase "false3" $ isProperSubmapOfBy (<)  (fromList [(1,1)])       (fromList [(1,1),(2,2)]) @?= False
            ]
        ]
    , testGroup "MinMax"
        -- FIXME: findMin
        -- FIXME: findMax
        -- FIXME: deleteMin
        -- FIXME: deleteMax
        -- FIXME: deleteFindMin
        -- FIXME: deleteFindMax
        [ testGroup "updateMin"
            [ testCase "adjust" $ updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "Xb"), (5, "a")]
            , testCase "delete" $ updateMin (\ _ -> Nothing) (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "a"
            ]
        , testGroup "updateMax"
            [ testCase "adjust" $ updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "Xa")]
            , testCase "delete" $ updateMax (\ _ -> Nothing) (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "b"
            ]
        , testGroup "updateMinWithKey"
            [ testCase "adjust" $ updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) @?= fromList [(3,"3:b"), (5,"a")]
            , testCase "delete" $ updateMinWithKey (\ _ _ -> Nothing) (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "a"
            ]
        , testGroup "updateMaxWithKey"
            [ testCase "adjust" $ updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) @?= fromList [(3,"b"), (5,"5:a")]
            , testCase "delete" $ updateMaxWithKey (\ _ _ -> Nothing) (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "b"
            ]
        -- FIXME: minView
        -- FIXME: maxView
        , testGroup "minViewWithKey"
            [ testCase "full" $ minViewWithKey (fromList [(5,"a"), (3,"b")]) @?= Just ((3,"b"), singleton 5 "a")
            , testCase "empty" $ minViewWithKey (empty :: WordMap String) @?= Nothing
            ]
        , testGroup "mapViewWithKey"
            [ testCase "full" $ maxViewWithKey (fromList [(5,"a"), (3,"b")]) @?= Just ((5,"a"), singleton 3 "b")
            , testCase "empty" $ maxViewWithKey (empty :: WordMap String) @?= Nothing
            ]
        ]
    ]

---------------------------

curry3 f a b c = f (a, b, c)

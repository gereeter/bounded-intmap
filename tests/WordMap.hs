import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Function
import Test.Tasty.HUnit

import Data.Word
import Data.WordMap

import Prelude hiding (lookup, null)

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
    ]

---------------------------

curry3 f a b c = f (a, b, c)

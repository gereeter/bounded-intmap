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
            ]
        ]
    ]

---------------------------

curry3 f a b c = f (a, b, c)

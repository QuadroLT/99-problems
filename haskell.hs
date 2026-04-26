import Test.HUnit
import Text.Printf (printf)

-- P01 get last element of the list

lastElement :: [a] -> Maybe a
lastElement []  = Nothing
lastElement [x] = Just x
lastElement (x: xs) = lastElement xs

-- P02
secondToLast :: [a] -> Maybe a
secondToLast []  = Nothing
secondToLast [x] = Nothing
secondToLast (x: [_]) = Just x
secondToLast (x: xs) = secondToLast xs

nthElement :: Int -> [a] -> Maybe a
nthElement 0 lst    = Nothing
nthElement n []     = Nothing
nthElement 1 (x:_) = Just x
nthElement n (_ :xs) = nthElement (n - 1) xs


-- Tests (simple)
allTests = TestList
  [
    "P01 01" ~: lastElement [1,2,3]    @?= Just 3,
    "P01 02" ~: lastElement []         @?= (Nothing:: Maybe Int),
    "P02 01" ~: secondToLast [1, 2, 3] @?= Just 2,
    "P02 02" ~: secondToLast [1]       @?= (Nothing::Maybe Int),
    "P02 03" ~: secondToLast []        @?= (Nothing::Maybe Int),
    "P03 01" ~: nthElement 2 [1, 2, 3] @?= Just 2,
    "P03 02" ~: nthElement 0 [1, 2, 3] @?= (Nothing::Maybe Int),
    "P03 02" ~: nthElement 5 []        @?= (Nothing::Maybe Int)
    ]


main = do
  putStrLn "Runnning tests"
  runTestTT allTests

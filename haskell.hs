import Test.HUnit
import Text.Printf (printf)
import GHC.Arr (accum)
import qualified Data.List as List

-- P01 get last element of the list

lastElement :: [a] -> Maybe a
lastElement []  = Nothing
lastElement [x] = Just x
lastElement (x: xs) = lastElement xs

-- P02 second to last element

secondToLast :: [a] -> Maybe a
secondToLast []  = Nothing
secondToLast [x] = Nothing
secondToLast (x: [_]) = Just x
secondToLast (x: xs) = secondToLast xs


-- P03 get n-th element of a list


nthElement :: Int -> [a] -> Maybe a
nthElement 0 lst    = Nothing
nthElement n []     = Nothing
nthElement 1 (x:_) = Just x
nthElement n (_ :xs) = nthElement (n - 1) xs


-- P04 get list length

listLength :: [a] -> Int
listLength lst = aux 0 lst
  where
    aux acc []     = acc
    aux acc (x:xs) = aux (acc + 1) xs

-- P05 reverse a list

reverseList :: [a] -> [a]
reverseList lst = aux [] lst
  where
    aux acc []     = acc
    aux acc (x:xs) = aux (x:acc) xs

-- P06 test if list is palindrome

isPalindrome :: Eq a =>  [a] -> Bool
isPalindrome lst = lst == List.reverse lst 


-- P07 flatten nested list

data NestedList a = Elem a | List [NestedList a]

flattenList :: NestedList a -> [a]
flattenList lst = aux [] lst
  where
    aux acc (List [])     = acc
    aux acc (Elem x)      = x:acc
    aux acc (List (x:xs)) = aux (aux acc (List xs)) x



-- Tests (simple)
allTests = TestList
  [
    "P01 01" ~: lastElement [1,2,3]          @?= Just 3,
    "P01 02" ~: lastElement []               @?= (Nothing:: Maybe Int),
    "P02 01" ~: secondToLast [1, 2, 3]       @?= Just 2,
    "P02 02" ~: secondToLast [1]             @?= (Nothing::Maybe Int),
    "P02 03" ~: secondToLast []              @?= (Nothing::Maybe Int),
    "P03 01" ~: nthElement 2 [1, 2, 3]       @?= Just 2,
    "P03 02" ~: nthElement 0 [1, 2, 3]       @?= (Nothing::Maybe Int),
    "P03 03" ~: nthElement 5 []              @?= (Nothing::Maybe Int),
    "P04 01" ~: listLength [1, 2, 3]         @?= 3,
    "P04 02" ~: listLength []                @?= 0,
    "P05 01" ~: reverseList [1, 2, 3]        @?= [3, 2, 1],
    "P06 01" ~: isPalindrome [1, 2, 3, 2, 1] @?= True,
    "P06 02" ~: isPalindrome "ab"            @?= False,
    "P07 01" ~: flattenList (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) @?= [1, 2, 3, 4, 5]
    ]

main = do
  putStrLn "Runnning tests"
  runTestTT allTests

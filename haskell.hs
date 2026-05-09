import Test.HUnit
import Text.Printf (printf)
import GHC.Arr (accum)
import qualified Data.List as List
import GHC.IO.Exception (IOErrorType(SystemError))

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


-- P08 remove duplicates from a list

compressList :: Eq a => [a] -> [a]
compressList lst = List.reverse $ aux [] lst
  where
    aux acc [] = acc
    aux acc (x:xs) = if x `elem` acc then aux acc xs else aux (x:acc) xs


-- P09 pack consecutive items to a list


mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan func lst = aux func lst []
  where
    aux func [] acc = (acc, [])
    aux func (x: xs) acc
      | func x = aux func xs (x:acc)
      | otherwise = (acc, (x:xs))


packList :: Eq a => [a] -> [[a]]
packList lst = aux [] lst
  where
    aux acc []     = reverse acc
    aux acc (x:xs) = aux (y:acc) ys
      where
        (y, ys) = span (==x) (x:xs)


-- P10 Run-length encoding of a list

lengthEncode :: Eq a => [a] -> [(Int, a)]
lengthEncode lst = aux [] lst
  where
    aux acc [] = reverse acc
    aux acc (x:xs) = aux ((length y, head y): acc) ys
      where
        (y, ys) = span (==x) (x:xs)

-- P11 modified run length encode

data Encoding a = Single a | Multiple Int a
  deriving (Show, Eq)

modLengthEncode :: Eq a => [a] -> [Encoding a]
modLengthEncode lst = map modEncode (lengthEncode lst)
  where
    modEncode (num, obj)
      | num == 1  = Single obj
      | otherwise = Multiple num obj


-- P12 decode a list

decode :: [Encoding a] -> [a]
decode []    = []
decode (x:xs) = aux x ++ decode xs
  where
    aux (Single x)     = [x]
    aux (Multiple z x) = replicate z x


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
    "P07 01" ~: flattenList (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) @?= [1, 2, 3, 4, 5],
    "P08 01" ~: compressList [1, 1, 1, 2, 2, 3] @?= [1, 2, 3],
    "P09 00" ~: mySpan (==3) [3, 3, 4, 4, 4]    @?= ([3, 3], [4, 4, 4]),
    "P09 01" ~: packList [1, 1, 1, 2, 1, 1, 1, 2, 2]   @?= [[1, 1, 1],
                                                            [2],
                                                            [1, 1, 1],
                                                            [2, 2]],
    "P10 01" ~: lengthEncode [1, 1, 1, 2, 1, 1, 1, 2, 2] @?= [(3, 1),
                                                              (1, 2),
                                                              (3, 1),
                                                              (2, 2)],
    "P11 01" ~: modLengthEncode [1, 1, 1, 2, 1, 1, 1, 2, 2] @?= [Multiple 3 1,
                                                                Single 2,
                                                                Multiple 3 1,
                                                                Multiple 2 2],
    
    "P12 01" ~: decode [Multiple 3 1, Single 2, Multiple 3 3] @?= [1, 1, 1, 2, 3, 3, 3]
  ]

main = do
  putStrLn "Runnning tests"
  runTestTT allTests
  putStrLn "Tests complete"

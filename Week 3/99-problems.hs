--Using Haskell 99 Problems to become comfortable with coding Functions

--Problem 1
--Find the last element of a list
myLast :: [a] -> a
myLast xs = head (reverse xs)

--Problem 2
--Find the second to last element of a list
myButLast :: [a] -> a
myButLast xs = head (tail (reverse xs))

--Problem 3
-- Find the k'th element of the list
elementAt :: [a] -> Int -> a
elementAt xs pos = xs !! (pos-1)

--Problem 4
--Find the number of elements in a list
myLength :: [a] -> Int
myLength xs = sum [1 | a<- xs]

--Problem 5
--Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = [(xs !! (length xs - 1))] ++ myReverse (take (length xs -1) xs)

--Problem 6
--Find out whether a list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = (reverse xs == xs)

--Problem 7
--Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List [x]) = flatten x
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten _ = []

--Problem 8
--Eliminate consecutive duplicates of list elements
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
    | (x == y) = compress (y:xs)
    | otherwise = [x] ++ compress (y:xs)
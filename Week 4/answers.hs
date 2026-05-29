--List comprehensions

--Exercise 1

pyths :: Int -> [(Int, Int, Int)]
--Use list comprehensions to generate triples (x,y,z) where they are pythagorean x^2+y^2=z^2
pyths max = [(x,y,z) | x<-[1..max], y<-[1..max], z<-[1..max], (x^2 + y^2) == z^2]

--Exercise 2
isPerfectNum :: Int -> Bool
-- We check if it is equal to 2*num since the sum will include the number itself as a factor
isPerfectNum num = (sum[a | a<-[1..num], num `mod` a == 0]) == 2*num

perfects :: Int -> [Int]
--Return a list of all perfect numbers in the given range - perfect nuumber equals the sum of all its factors excluding the number itself
perfects max = [x | x<-[1..max], isPerfectNum x]

--Exercise 3
scalarProduct :: [Int] -> [Int] -> Int
--Scalar Product of two lists of integers is given by the sum of the products of the corresponding integers
scalarProduct xs ys = sum [x * y | (x,y) <- (zip xs ys)]

--Exercise 4 (Hard)
matrix_mul :: Int -> Int -> Int -> [[Int]] -> [[Int]] -> [[Int]]
--Multiply 2 matrices together using list comprehensions
matrix_mul cols1 cols2 rows2 mat1 mat2= undefined

--Recursive Functions
--Exercise 1
and :: [Bool] -> Bool
and [False] = False
and[True] = True
and (x:xs) = x && Main.and xs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate count x = [x] ++ myReplicate (count -1) x

(!!!) :: [a] -> Int -> a
(!!!) list 0 = head list
(!!!) list index = (!!!) (tail list) (index - 1)

myElem :: Eq a => a -> [a] -> Bool
myElem item [] = False
myElem item list | (head list) == item = True
    | otherwise = myElem item (tail list)

--Exercise 2
split :: [a] -> [[a]]
split list = [take (length list `div` 2) list, drop (length list `div`2) list]

merge :: Ord a => [a] -> [a] -> [a]
--Merges two sorted lists of values into a single sorted list
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

--List Comprehensions and higher-order functions
--Exercise 1
fun :: Num a => (a->a) -> (a->Bool) -> [a] -> [a]
fun mf ff = filter ff . map mf

--Exercise 2
myMap :: (a->b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a->Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x:acc else acc) []

--Exercise 3
altMap :: (a->b) -> (a->b) -> [a] -> [b]
altMap _ _ [] = []
altMap f _ [x] = [f x]
altMap f g (x:y:xs) = f x : g y : (altMap f g xs)

--Exercise 4 (Hard)

zero :: (a -> a) -> (a -> a)
zero f x = x

one :: (a -> a) -> (a -> a)
one f x = f x

two :: (a -> a) -> (a -> a)
two f x = f (f x)

three :: (a -> a) -> (a -> a)
three f x = f (f (f x))


mult m n = m . n
add :: ((a->a) -> (a->a)) -> ((a->a) -> (a->a)) -> ((a->a) -> (a->a))
add m n f x = m f (n f x)

--Define Prelude `Concat` in four different ways
--Exercise 1 - Using comprehensions + NO recrusion
concat1 :: [[a]] -> [[a]]
concat1 xss = [[x | x<-xs] | xs <- xss]
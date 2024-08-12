module Part1 where

emptyListError :: String
emptyListError = "List is empty !"

-- Problem 1
-- Find the last element of a list.
myLast :: [a] -> a
myLast [] = error emptyListError
myLast [x] = x
myLast (x : xs) = myLast xs

-- Problem 2
-- Find the last-but-one (or second-last) element of a list.

myButLast :: [a] -> a
myButLast [] = error emptyListError
myButLast [x] = error "List only has a single element !"
myButLast (x : xs)
  | length xs == 1 = x
  | otherwise = myButLast xs

-- Problem 3
-- Find the K'th element of a list.

elementAt :: [a] -> Int -> a
elementAt (x : _) 1 = x
elementAt (_ : xs) i = elementAt xs (i - 1)
elementAt _ _ = error "Index out of bounds !"

-- Problem 4
-- Find the number of elements in a list.

myLength :: [a] -> Int
myLength [] = 0
myLength [x] = 1
myLength (x : xs) = 1 + myLength xs

-- Problem 5
-- Reverse a list.

myReverse :: [a] -> [a]
myReverse = foldl flippedCons []
  where
    flippedCons xs x = x : xs

-- Problem 6
-- Find out whether a list is a palindrome.

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome l = myReverse l == l
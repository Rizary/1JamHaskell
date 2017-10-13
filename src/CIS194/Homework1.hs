module CIS194.Homework1 where

-- Exercise 1

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0    = []
  | otherwise = (mod x 10) : toDigitsRev (div x 10)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []          = []
doubleEveryOther (x:[])      = [x]
doubleEveryOther xs'@(y:x:xs) =
  if even (length xs')
  then y*2 : x : doubleEveryOther xs
  else y : x*2 : doubleEveryOther xs

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits []       = 0
sumDigits (x : xs) =
  if x >= 10 then (div x 10) + (mod x 10) + sumDigits xs
  else x + sumDigits xs

-- Exercise 4
validate :: Integer -> Bool
validate num =
   let result = mod (sumDigits (doubleEveryOther (toDigits num))) 10
   in if result == 0 then True else False

test = do
  print $ validate 4012888888881881
  print $ validate 4012888888881882

-- Exercise 5
type Peg = String
type Move = (Peg,Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _    = []
hanoi disk a b c = hanoi (disk-1) a c b ++ [(a,b)] ++ hanoi (disk-1) c b a

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

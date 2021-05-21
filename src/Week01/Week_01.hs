{-# OPTIONS_GHC -Wall #-}

module Week01.Week_01
    ( week01Test
    ) where

week01Test :: IO ()
week01Test = do
    print (toDigits 1234 == [1, 2, 3, 4])
    print (toDigitsRev 1234 == [4, 3, 2, 1])
    print (doubleEveryOther [1, 2, 3, 4] == [2, 2, 6, 4])
    print (doubleEveryOther [1, 2, 3] == [1, 4, 3])
    print (doubleEveryOther [1] == [1])
    print (doubleEveryOther [] == [])
    print (sumDigits [16, 7, 12, 5] == 22)
    print (validate 4012888888881881 == True)
    print (validate 4012888888881882 == False)
    print (hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")])


toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
  | n < 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]


toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
  | n < 0 = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)


doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper [] = []
doubleEveryOtherHelper [x] = [x]
doubleEveryOtherHelper (x:y:zs) = [x, y * 2] ++ (doubleEveryOtherHelper zs)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherHelper (reverse xs))

sumDigit :: Integer -> Integer
sumDigit n = sum (toDigits n)

sumDigits :: [Integer] -> Integer
sumDigits lst = sum (map sumDigit lst)

validate :: Integer -> Bool
validate n
  | ((sumDigits (doubleEveryOther (toDigits n))) `mod` 10) == 0 = True
  | otherwise = False


type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 1 = [(a, b)]
  | otherwise = (hanoi (n - 1) a c b) ++ [(a, b)] ++ (hanoi (n - 1) c b a)
{-# OPTIONS_GHC -Wall #-}

module Week_01
    ( someFunc,
        toDigits,
        toDigitsRev
    ) where

someFunc :: IO ()
someFunc = putStrLn "someWeek"


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

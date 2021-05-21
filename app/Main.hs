{-# OPTIONS_GHC -Wall #-}

module Main where

import Week_01

main :: IO()
main = do 
    print (toDigits 1234)
    print (toDigitsRev 1234)
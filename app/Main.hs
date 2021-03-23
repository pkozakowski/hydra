{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Data.Record.Hom
import Numeric.Algebra
import Numeric.Field.Fraction
import Market.Types
import Prelude hiding ((+), (*))

portfolio :: Portfolio '["eth", "btc", "ada"]
portfolio = set #btc (Amount $ 2 % 3) zero + half .* (#eth := (Amount $ 3 % 4) & zero) where
    half :: Fraction Integer
    half = 1 % 2

main :: IO ()
main = putStrLn $ show portfolio

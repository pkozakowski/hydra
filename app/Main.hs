{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Data.Record.Hom
import Numeric.Algebra
import Numeric.Delta
import Numeric.Field.Fraction
import Market.Types
import Prelude hiding ((+), (*))

portfolio :: Portfolio '["eth", "btc", "ada"]
portfolio = set #btc (Amount $ 2 % 3) zero + half .* (#eth := (Amount $ 3 % 4) & zero) where
    half :: Fraction Integer
    half = 1 % 2

main :: IO ()
main = do
    putStrLn $ show portfolio
    let x = Amount $ 1 % 2
        y = Amount $ 2 % 3
        z = delta x y :: AmountDelta
    putStrLn $ show z
    let z10 = (10 :: Natural) .* z
        y' = sigma y z
    putStrLn $ show y'
    let y'' = sigma y z10
    putStrLn $ show y''

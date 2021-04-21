{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Data.Record.Hom
import Numeric.Algebra
import Numeric.Delta
import Numeric.Field.Fraction
import Numeric.Kappa
import Market.Types
import Prelude hiding ((+), (*), pi)

portfolio :: Portfolio '["eth", "btc", "ada"]
portfolio = set #btc (Amount $ 2 % 3) zero + half .* (#eth := (Amount $ 3 % 4) & zero) where
    half :: Fraction Integer
    half = 1 % 2

prices :: Prices '["eth", "btc", "ada"]
prices
    = #eth := (Price $ 3 % 4)
    & #btc := (Price $ 4 % 3)
    & #ada := (Price $ 5 % 2)
    & empty

main :: IO ()
main = do
    putStrLn $ show portfolio
    let x = Amount $ 1 % 2
        y = Amount $ 2 % 3
        z = delta x y
    putStrLn $ show z
    let z10 = (10 :: Natural) .* z
        y' = sigma y z
    putStrLn $ show y'
    let y'' = sigma y z10
    putStrLn $ show y''
    let tenThirds = 10 % 3 :: Fraction Integer
        portfolioDelta = delta portfolio $ tenThirds .* portfolio
    putStrLn $ show portfolioDelta
    let portfolio' = sigma portfolio portfolioDelta
    putStrLn $ show portfolio'
    let x = Amount $ 1 % 2
        y = Price $ 3 % 1
        z = pi x y
    putStrLn $ show z
    let x' = kappa' z y
    putStrLn $ show x'
    let y' = kappa z x
    putStrLn $ show y'
    let values = pi portfolio prices
    putStrLn $ show values

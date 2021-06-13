{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Data.Record.Hom
import Numeric.Algebra
import Numeric.Delta
import Numeric.Field.Fraction
import Numeric.Kappa
import Numeric.Normalizable
import Market.Ops
import Market.Types
import Prelude hiding ((+), (*), pi)

portfolio :: Portfolio '["eth", "btc", "ada"]
portfolio = Portfolio $
    set #btc (Amount $ 2 % 3) zero +
    half .* (#eth := (Amount $ 3 % 4) :& zero)
    where
        half :: Fraction Integer
        half = 1 % 2

prices :: Prices '["eth", "btc", "ada"]
prices = Prices
     $ #eth := (Price $ 3 % 4)
    :& #btc := (Price $ 4 % 3)
    :& #ada := (Price $ 5 % 2)
    :& Empty

main :: IO ()
main = do
    print portfolio
    let x = Amount $ 1 % 2
        y = Amount $ 2 % 3
        z = delta x y
    print z
    let z10 = (10 :: Natural) .* z
        y' = sigma y z
    print y'
    let y'' = sigma y z10
    print y''
    let tenThirds = 10 % 3 :: Fraction Integer
        portfolioDelta = delta portfolio $ tenThirds .* portfolio
    print portfolioDelta
    let portfolio' = sigma portfolio portfolioDelta
    print portfolio'
    let x = Amount $ 1 % 2
        y = Price $ 3 % 1
        z = pi x y
    print z
    let x' = kappa' z y
    print x'
    let y' = kappa z x
    print y'
    let values = pi portfolio prices
    print values
    let n = norm values
        dist = normalize values
    print dist
    let values' = unnormalize n dist
    print values'
    let d1 = Distribution
            $  #eth := Share (2 % 5)
            :& #btc := Share (2 % 5)
            :& #ada := Share (1 % 5)
            :& Empty
        d2 = Distribution
            $  #eth := Share (1 % 3)
            :& #btc := Share (1 % 3)
            :& #ada := Share (1 % 3)
            :& Empty
    print $ balancingTransfers (1 % 10) d1 d2

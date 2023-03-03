{-# LANGUAGE NumericUnderscores, OverloadedStrings #-}

module Main where

import Test.QuickCheck 


import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
-- import Data.Vector.Unboxed  ((!))
import Data.Time (fromGregorian, UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import qualified Data.HashMap.Strict as Hm
import Data.Text (Text)
import qualified Data.Complex as Cx
import Data.Complex (Complex ( (:+) ) )

import Data.Approx 



main :: IO ()
main = do 

  -- print "Float equality test"
  quickCheck $ (0.0 :: Float) =~ (-1e-9)
  quickCheck $ (0.0 :: Double) =~ (-0.0)
  quickCheck $ (1.0e+7 :: Double) =~ 10_000_000.05

  quickCheck $ Just (10.0000000000000007 :: Double) =~ Just 10.0
  quickCheck $ Just (10 :: Double) /~ Just 1.0
  quickCheck $ Just (10 :: Double) /~ Nothing
  quickCheck $ (Nothing :: Maybe Double) /~ Just 1.0
  quickCheck $ (Nothing :: Maybe Double) =~ Nothing

  quickCheck $ ([1.2, 3.4, 5.6] :: [Double]) /~ [1.2, 3.4, 5.65]
  quickCheck $ ([2.3, 1,2, 1000] :: [Double]) =~ [2.3, 1,2, 1e+3]
  quickCheck $ ([1.0,1.0,1.0,1.0] :: [Double]) /~ [1.0,1.0,1.0,1.0,2.0]

  quickCheck $ ((1.2, 3.4) :: (Double, Double)) =~ (1.20000008, 3.399999999)
  quickCheck $ ((1.2, 3.5) :: (Double, Double)) /~ (1.20000008, 3.399999999)

  quickCheck $ ((1.2, 3.4, 2.5) :: (Double, Double, Double)) =~ (1.20000008, 3.399999999, 2.5000001)
  quickCheck $ ((1.2, 3.4, 2.5) :: (Double, Double, Double)) /~ (1.2, 3.399999999, 2.4)

  quickCheck $ ((1.2, 3.4, 2.5) :: (Float, Float, Float)) =~ (1.20000008, 3.399999999, 2.5000001)
  quickCheck $ ((1.2, 3.4, 2.5) :: (Float, Float, Float)) /~ (1.2, 3.399999999, 2.4)

  quickCheck $ ((1, 3, 2) :: (Int, Int, Int)) =~ (1, 3, 2)
  quickCheck $ ((1, 3, 2) :: (Int, Int, Int)) /~ (1, 3, 5)

  quickCheck $ ((2,5.35) :: (Int,Double)) =~ (2,5.35)

  quickCheck $ ((3.0 :+ 5.0)::(Cx.Complex Float)) =~ (3.0 :+ 5.0)

  quickCheck $ ((2,5.35,"happ",[1,2],["ret","we"],[6.78,3.5]) 
    :: (Int,Double,Text,[Int],[[Char]],[Double])) 
    
    =~ (2,5.35,"happ",[1,2],["ret","we"],[6.78,3.5])

  quickCheck $ ((1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0) :: (Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double)) =~ (1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0)

  quickCheck $ ((1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.1) :: (Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double))   /~ (1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0)

  quickCheck $ ((1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0) :: (Double,Double,Double,Double,Double,Double,Double,Double,Double,Double)) =~ (1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0)

  quickCheck $ ((1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0) :: (Double,Double,Double,Double,Double,Double,Double,Double,Double,Double))   /~ (1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.1)

  quickCheck $ False /~ True
  quickCheck $ True =~ True
  quickCheck $ False =~ False

  quickCheck $ ("star"::Text) =~ ("star"::Text)
  quickCheck $ ("star"::Text) /~ ("start"::Text)
  quickCheck $ ("start"::Text) /~ ("star"::Text)

  quickCheck $ ("star"::String) =~ ("star"::String)
  quickCheck $ ("star"::String) /~ ("start"::String)
  quickCheck $ ("start"::String) /~ ("star"::String)

  quickCheck $ fromGregorian 2018 3 30 =~ fromGregorian 2018 3 30 
  quickCheck $ fromGregorian 2018 3 31 /~ fromGregorian 2018 3 30

  quickCheck $ (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2020-01-15 15:02:15" :: Maybe UTCTime) /~ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2020-01-15 14:02:15"

  quickCheck $ (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2020-01-15 15:02:15" :: Maybe UTCTime) =~ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2020-01-15 15:01:50"

  quickCheck $ (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2020-01-15 15:01:50" :: Maybe UTCTime) =~ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2020-01-15 15:02:15"
  quickCheck $ (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2020-01-16 15:02:15" :: Maybe UTCTime) /~ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2020-01-15 15:02:15"

  quickCheck $ (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2019-01-15 15:02:15" :: Maybe UTCTime) /~ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2020-01-15 15:02:15"

  quickCheck $ (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2020-01-15 15:02:15" :: Maybe UTCTime) =~ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2020-01-15 15:02:15"

  let rb = Hm.fromList [("Cash", 10.23), ("CurrentAdvances", 56.25), ("AccountPayables", 0.0)] :: Hm.HashMap String Double
  let tb = Hm.fromList [("Cash", 10.23), ("CurrentAdvances", 56.25)] :: Hm.HashMap String Double

  quickCheck $ rb /~ tb
  quickCheck $ tb /~ rb

  let rb = Hm.fromList [("Cash", 10.23), ("CurrentAdvances", 56.25)] :: Hm.HashMap String Double

  quickCheck $ rb =~ tb
  quickCheck $ tb =~ rb

  let rb = Hm.fromList [("Cash", 10.21), ("CurrentAdvances", 56.25)] :: Hm.HashMap String Double

  quickCheck $ rb /~ tb
  quickCheck $ tb /~ rb

  let rb = Hm.fromList [("Cash", Just 10.23), ("CurrentAdvances", Just 56.25), ("AccountPayables", Just 0.0)] :: Hm.HashMap String (Maybe Double)
  let tb = Hm.fromList [("Cash", Just 10.23), ("CurrentAdvances", Just 56.25)] :: Hm.HashMap String (Maybe Double)

  quickCheck $ rb /~ tb
  quickCheck $ tb /~ rb

  let rb = Hm.fromList [("Cash", Just 10.23), ("CurrentAdvances", Just 56.25)] :: Hm.HashMap String (Maybe Double)

  quickCheck $ rb =~ tb
  quickCheck $ tb =~ rb

  let rb = V.fromList [("Cash", Just 10.23), ("CurrentAdvances", Just 56.25), ("AccountPayables", Just 0.0)] :: V.Vector (String, Maybe Double)
  let tb = V.fromList [("Cash", Just 10.23), ("CurrentAdvances", Just 56.25)] :: V.Vector (String, Maybe Double)

  quickCheck $ rb /~ tb

  let rb = V.fromList [("Cash", Just 10.24), ("CurrentAdvances", Just 56.25)] :: V.Vector (String, Maybe Double)

  quickCheck $ rb /~ tb

  quickCheck $ U.fromList ([2.5, 1.5, 3.56] :: [Double]) =~ U.fromList [2.500000001, 1.5, 3.56]

  quickCheck $ (2,4) `inRange` (3::Int) 
  quickCheck $ not $ (2,4) `inRange` (5::Int) 

  quickCheck $ (2.5, 2.75) `inRange` (2.62::Double)
  quickCheck $ not $ (2.5, 2.75) `inRange` (2.8::Double)

  quickCheck $ (4,2) `safeInRange` (3::Int) 
  quickCheck $ not $ (2,4) `safeInRange` (5::Int) 

  quickCheck $ (2.5, 2.75) `safeInRange` (2.62::Double)
  quickCheck $ not $ (2.75, 2.5) `safeInRange` (2.8::Double)

  quickCheck $ inTol 0.25 6.25 (6.1::Double) 
  quickCheck $ not $ inTol 0.125 6.25 (6.1::Double) 
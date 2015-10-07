{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad
import System.Random
import Data.IP
import Data.Aeson
import Data.IP.Aeson

import qualified Data.ByteString.Lazy as B

rndIPv4 :: StdGen -> IPv4
rndIPv4 gen = toIPv4 ip
    where ip = rndSelect gen [0..256] 4 :: [Int]

rndSelect :: StdGen -> [a] -> Int -> [a]
rndSelect gen lst i = take i [ lst !! x | x <- randomRs (0, (length lst) - 1) gen]

-- 4 Ints
instance Arbitrary IPv4 where
  arbitrary = do
    let a = choose (0,255)
    let b = choose (0,255)
    let c = choose (0,255)
    let d = choose (0,255)
    liftM toIPv4 (sequence [a,b,c,d])


prop_parseJSONIPv4 :: IPv4 -> Property
prop_parseJSONIPv4 ip = True ==>
  (Just ip) == decode ("{\"ip\": " `B.append` (encode ip) `B.append` " }")

main :: IO ()
main = quickCheck prop_parseJSONIPv4

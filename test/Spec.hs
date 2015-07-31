import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import System.Random
import Data.IP

rndIPv4 :: StdGen -> IPv4
rndIPv4 gen = toIPv4 ip
    where ip = rndSelect gen [0..256] 4 :: [Int]

rndSelect :: StdGen -> [a] -> Int -> [a]
rndSelect gen lst i = take i [ lst !! x | x <- randomRs (0, (length lst) - 1) gen]

main :: IO ()
main = do
    gen <- getStdGen
    let ip = rndIPv4 gen
    putStrLn $ show ip

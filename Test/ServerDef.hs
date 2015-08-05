------------------------------------------------------------
-- Copyright : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
------------------------------------------------------------

module Test.ServerDef
    ( PortsDef (..)
    , portsDef
    ) where

import Data.List (sort)
import System.IO.Unsafe (unsafePerformIO)
import System.Random

data PortsDef = PortsDef
    { httpTestPort :: Int
    , httpsTestPort :: Int
    , proxyTestPort :: Int
    }
    deriving Show


-- Yeah, yeah, unsafePerformIO! Worst thing that can happen is that the tests
-- fail.
portsDef :: PortsDef
portsDef = unsafePerformIO getPortsDef


-- Grab three unique Ints in the range (30000, 60000) and stick them in a
-- PortsDef constructor.
getPortsDef :: IO PortsDef
getPortsDef = do
    vals <- randomRL []
    case sort vals of
        [a, b, c] -> return $ PortsDef a b c
        _ -> getPortsDef
  where
    randomRL :: [Int] -> IO [Int]
    randomRL xs
        | length xs == 3 = return $ sort xs
        | otherwise = do
            x <- randomRIO portRange
            if x `elem` xs
                then randomRL xs
                else randomRL (x:xs)

portRange :: (Int, Int)
portRange = (30000, 60000)

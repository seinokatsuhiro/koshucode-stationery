{-# OPTIONS_GHC -Wall #-}

-- | Koshu regression test driver.

module Main (main) where

import qualified Koshucode.Stationery.Regress.Main as Reg

-- | Entry point of the @koshu-regress@ program.
main :: IO ()
main = Reg.regressMain


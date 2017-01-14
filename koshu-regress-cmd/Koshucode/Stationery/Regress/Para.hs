{-# OPTIONS_GHC -Wall #-}

-- | Command-line parameter.

module Koshucode.Stationery.Regress.Para
 ( Para (..),
   version,
   usage,
   options,
   parsePara,
 ) where

import qualified Koshucode.Baala.System                as Z
import qualified Koshucode.Baala.DataPlus              as K
import qualified Paths_koshu_regress_cmd               as Ver

version :: String
version = "Koshu regression test driver " ++ Z.showVersion Ver.version

usage :: [String]
usage = [ "DESCRIPTION"
        , "  Execute regression test"
        , ""
        , "USAGE"
        , "  koshu-regress [OPTION] REGRESS"
        , ""
        ]

options :: [Z.Option]
options =
  [ Z.help
  , Z.version
  , Z.flag [] ["batch"]           "Batch mode"
  , Z.req  [] ["prompt"] "TEXT"   "Change prompt to TEXT"
  , Z.req  [] ["target"] "DIR"    "Target directory"
  ]

data Para = Para
    { pError    :: [String]

    -- Command line parameter
    , pHelp     :: Bool
    , pVersion  :: Bool
    , pBatch    :: Bool
    , pPrompt   :: String
    , pTarget   :: FilePath
    , pFiles    :: [FilePath]

    -- Counter
    , pTotal    :: Int
    , pNew      :: Int
    , pOk       :: Int
    , pDiff     :: Int
    , pDiffs    :: [Int]
    } deriving (Show)

para :: Para
para = Para
       { pError    = []
       , pHelp     = False
       , pVersion  = False
       , pBatch    = False
       , pPrompt   = ""
       , pTarget   = ""

       , pFiles    = []

       , pTotal    = 0
       , pNew      = 0
       , pOk       = 0
       , pDiff     = 0
       , pDiffs    = []
       }

parsePara :: IO Para
parsePara =
    do args <- Z.parseCommand options
       return $ case args of
         Left errs -> para { pError = errs }
         Right (ps, fs)
             | flag "help"      -> para { pHelp     = True }
             | flag "version"   -> para { pVersion  = True }
             | otherwise        -> para { pBatch    = flag "batch"
                                        , pPrompt   = req "prompt" K.|?| ">> "
                                        , pTarget   = req "target" K.|?| "."
                                        , pFiles    = fs }
             where flag = Z.getFlag ps
                   req  = Z.getReqLast ps


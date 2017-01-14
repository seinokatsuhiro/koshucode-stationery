{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Koshu regression test driver.

module Koshucode.Stationery.Regress.Main
 ( regressMain
 ) where

import qualified Data.Algorithm.Diff                   as Diff
import qualified Data.ByteString.Lazy                  as Bz
import qualified System.Directory                      as Dir
import qualified System.FilePath                       as Path
import qualified Koshucode.Baala.System                as Z
import qualified Koshucode.Baala.DataPlus              as K
import qualified Koshucode.Stationery.Regress.Diff     as Diff
import qualified Koshucode.Stationery.Regress.DirTree  as Reg
import qualified Paths_koshu_regress_cmd               as Ver

ver :: String
ver = "Koshu regression test driver " ++ Z.showVersion Ver.version

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

-- | Entry point of regression test driver.
regressMain :: IO ()
regressMain = dispatch K.# parsePara

dispatch :: Para -> IO ()
dispatch Para {..}
    | K.some pError  = Z.printHelp pError options
    | pHelp          = Z.printHelp usage options
    | pVersion       = putStrLn ver
dispatch p@Para { pFiles = [regFile] }
    = let regDir = regFile Path.-<.> "d"
      in body p regFile regDir (regDir Path.</> "base")
dispatch _ = Z.printHelp usage options

body :: Para -> FilePath -> FilePath -> FilePath -> IO ()
body p@Para {..} regFile regDir baseDir =
    do Reg.checkRegressFile regFile
       Dir.createDirectoryIfMissing True baseDir
       paths <- Reg.preparePaths pTarget regFile regDir baseDir
       p' <- K.foldM (regressTo pTarget baseDir) p paths
       summary p'

regressTo :: FilePath -> FilePath -> Para -> FilePath -> IO Para
regressTo targetDir baseDir p@Para {..} path =
    let p' = p { pTotal = pTotal + 1 }
    in regress p' baseDir (targetDir Path.</> path) (baseDir Path.</> path) where

regress :: Para -> FilePath -> FilePath -> FilePath -> IO Para
regress p@Para {..} dir path path' = check where

    check = do
      exist <- Dir.doesFileExist path'
      case exist of
        False -> do putNew
                    Dir.copyFile path path'
                    return $ p { pNew = pNew + 1 }
        True  -> do bz  <- readBz path
                    bz' <- readBz path'
                    case bz == bz' of
                      True  -> do putOK
                                  return $ p { pOk = pOk + 1 }
                      False -> do comp bz bz'
                                  return $ p { pDiff  = pDiff + 1
                                             , pDiffs = pTotal : pDiffs }
                         
    putNew   = putX "NEW"
    putOK    = putX "OK"
    putDiff  = putX "DIFF"

    putX label = putStrLn $ "[" ++ show pTotal ++ "] " ++ label ++ " - " ++ path

    -- ----------------------  compare

    comp bz bz'
        | testTextFile bz
            = let ls   = K.linesCrlfBzNumbered bz
                  ls'  = K.linesCrlfBzNumbered bz'
                  wd   = K.digitsLength 10 $ 100 + length ls
              in diffText wd $ diff ls' ls
        | otherwise
            = diffBinary bz bz'

    -- ----------------------  binary

    diffBinary bz bz' = do
      K.putLn >> putDiff >> K.putLn
      putStrLn   "  Cannot display binary content."
      putStrLn $ "  Size of base file is " ++ size bz'
                      ++ " bytes, new is " ++ size bz ++ "."
      helpCmd

    size = show . Bz.length

    -- ----------------------  text

    diff xs ys = Diff.diffContext 2 $ Diff.getGroupedDiffBy sameLine xs ys
    sameLine x y = snd x == snd y

    diffText wd ds = do
      K.putLn >> putDiff >> hr
      Diff.printContextDiff wd ds
      hr >> putDiff
      helpCmd

    -- ----------------------  help and command

    hr = putStrLn $ replicate 70 '-'

    helpCmd | pBatch     = K.putLn
            | otherwise  = K.putLn >> help >> cmd

    help = do
      putStrLn   "Type 'a' to abort process"
      putStrLn $ "  or 'u' to update base file in " ++ qqString dir
      putStrLn   "  or 's' to skip this file"
      --putStrLn   "  or 'b' to skip this file and switch to batch mode"

    cmd = do
      s <- K.promptWith pPrompt
      case s of
        "s" -> K.putLn
        "a" -> K.putLn >> Z.putAbort
        "u" -> K.putLn >> Dir.copyFile path path'
        _   -> K.putLn >> help >> cmd

readBz :: FilePath -> IO K.Bz
readBz path = do
  file <- K.readBzFile path
  K.abortLeft $ K.bzFileContent file

qqString :: K.Map String
qqString s = "\"" ++ s ++ "\""

testTextFile :: K.Test K.Bz
testTextFile = Bz.null . Bz.filter bin where
    bin c = K.isControlCode c && not (K.isFormatCode c)

summary :: Para -> IO ()
summary Para {..} | pTotal == 0 = return ()
summary Para {..} = message where
    message = do
      K.putLn
      putStrLn "**"
      putStrLn $ "**  Summary"
      putCnt pNew   "**    NEW    = " ""
      putCnt pOk    "**    OK     = " ""
      putCnt pDiff  "**    DIFF   = " (diffs pDiffs)
      putCnt pTotal "**    TOTAL  = " ""
      putStrLn "**"

    putCnt c label note = K.when (c > 0) $ putStrLn (label ++ show c ++ note)

    diffs ds | null ds = ""
    diffs ds =
        let ds'  = take 5 $ reverse ds
            list = unwords ((show . K.list1) <$> ds')
            etc | length ds > 5 = " etc"
                | otherwise     = ""
        in "  --  See " ++ list ++ etc


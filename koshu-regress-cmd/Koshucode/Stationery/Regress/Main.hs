{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Koshu regression test driver.

module Koshucode.Stationery.Regress.Main
 ( regressMain
 ) where

import qualified Data.Algorithm.Diff                 as Diff
import qualified Data.ByteString.Lazy                as Bz
import qualified System.Directory                    as Dir
import qualified System.FilePath                     as Path
import qualified Koshucode.Baala.System              as Z
import qualified Koshucode.Baala.DataPlus            as K
import qualified Koshucode.Baala.Base.Message        as Msg
import qualified Koshucode.Stationery.Regress.Diff   as Diff
import qualified Paths_koshu_regress_cmd             as Ver

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
  ]

data Para = Para
    { pError    :: [String]
    , pHelp     :: Bool
    , pVersion  :: Bool
    , pBatch    :: Bool
    , pPrompt   :: String
    , pFiles    :: [FilePath]
    } deriving (Show)

para :: Para
para = Para
       { pError    = []
       , pHelp     = False
       , pVersion  = False
       , pBatch    = False
       , pPrompt   = ""
       , pFiles    = [] }

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
                                        , pFiles    = fs }
             where flag = Z.getFlag ps
                   req  = Z.getReqLast ps

-- | Entry point of regression test driver.
regressMain :: IO ()
regressMain = dispatch K.# parsePara

dispatch :: Para -> IO ()
dispatch Para { .. }
    | K.some pError  = Z.printHelp pError options
    | pHelp          = Z.printHelp usage options
    | pVersion       = putStrLn ver
dispatch p@Para { pFiles = [file] }
    = let dir = file Path.-<.> "d"
      in body p file dir (dir Path.</> "base")
dispatch _ = Z.printHelp usage options

body :: Para -> FilePath -> FilePath -> FilePath -> IO ()
body p file dir baseDir =
    do checkRegressFile file
       Dir.createDirectoryIfMissing True baseDir
       pats   <- readPatterns file
       trees  <- K.dirTrees [Path.takeFileName dir] "." pats
       K.withCurrentDirectory baseDir $ createDirTrees trees
       let paths = createPath <$> (K.treePaths K.<++> trees)
       regressTo p baseDir K.<#!> paths

readPatterns :: FilePath -> IO [K.SubtreePattern]
readPatterns file =
    do ls  <- K.readSubtreeClauses file
       ls2 <- K.abortLeft ls
       ps  <- K.abortLeft (createPatterns K.<#> ls2)
       return $ concat ps
       
createPatterns :: K.TokenClause -> K.Ab [K.SubtreePattern]
createPatterns cl = 
    do ts <- K.toTrees cl
       K.decodeSubtreePattern ts

checkRegressFile :: FilePath -> IO ()
checkRegressFile path =
    do exist <- Dir.doesFileExist path
       case exist of
         True   -> return ()
         False  -> K.abortLeft $ Msg.adlib "No regress file"

createPath :: ([String], String) -> FilePath
createPath (ys, z) = Path.joinPath ys Path.</> z

createDirTrees :: [K.Subtree String] -> IO ()
createDirTrees = mapM_ createDirTree

createDirTree :: K.Subtree String -> IO ()
createDirTree (K.TreeL _) = return ()
createDirTree (K.TreeB _ y xs) =
    do Dir.createDirectoryIfMissing True y
       K.withCurrentDirectory y $ createDirTrees xs

regressTo :: Para -> FilePath -> FilePath -> IO ()
regressTo p dir path = regress p dir path (dir Path.</> path)

regress :: Para -> FilePath -> FilePath -> FilePath -> IO ()
regress p dir path path' = check where

    check = do
      exist <- Dir.doesFileExist path'
      case exist of
        False -> do putNew
                    Dir.copyFile path path'
        True  -> do bz  <- readBz path
                    bz' <- readBz path'
                    case bz == bz' of
                      True  -> putOK
                      False -> comp bz bz'
                         
    putNew = putStrLn $ "NEW - " ++ path
    putOK  = putStrLn $ "OK - " ++ path

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
      K.putLn >> diffFound >> K.putLn
      putStrLn   "  Cannot display binary content."
      putStrLn $ "  Size of base file is " ++ size bz'
                      ++ " bytes, new is " ++ size bz ++ "."
      helpCmd

    size = show . Bz.length

    -- ----------------------  text

    diff xs ys = Diff.diffContext 2 $ Diff.getGroupedDiffBy sameLine xs ys
    sameLine x y = snd x == snd y

    diffText wd ds = do
      K.putLn >> diffFound >> hr
      Diff.printContextDiff wd ds
      hr >> diffFound
      helpCmd

    -- ----------------------  help and command

    hr = putStrLn $ replicate 70 '-'

    diffFound = putStrLn $ "DIFF - " ++ path

    helpCmd | pBatch p   = K.putLn
            | otherwise  = K.putLn >> help >> cmd

    help = do
      putStrLn   "Type 'a' to abort process"
      putStrLn $ "  or 'u' to update base file in " ++ qqString dir
      putStrLn   "  or 's' to skip this file"
      --putStrLn   "  or 'b' to skip this file and switch to batch mode"

    cmd = do
      s <- K.promptWith $ pPrompt p
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

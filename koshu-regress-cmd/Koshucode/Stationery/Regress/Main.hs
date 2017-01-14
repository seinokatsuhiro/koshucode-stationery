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
  ]

data Para = Para
    { pError    :: [String]
    , pHelp     :: Bool
    , pVersion  :: Bool
    , pFiles    :: [FilePath]
    } deriving (Show)

para :: Para
para = Para
       { pError    = []
       , pHelp     = False
       , pVersion  = False
       , pFiles    = [] }

parsePara :: IO Para
parsePara =
    do args <- Z.parseCommand options
       return $ case args of
         Left errs -> para { pError = errs }
         Right (ps, fs)
             | flag "help"      -> para { pHelp     = True }
             | flag "version"   -> para { pVersion  = True }
             | otherwise        -> para { pFiles    = fs }
             where flag = Z.getFlag ps

-- | Entry point of regression test driver.
regressMain :: IO ()
regressMain = dispatch K.# parsePara

dispatch :: Para -> IO ()
dispatch Para { .. }
    | K.some pError  = Z.printHelp pError options
    | pHelp          = Z.printHelp usage options
    | pVersion       = putStrLn ver
dispatch Para { pFiles = [file] }
    = let dir = file Path.-<.> "d"
      in body file dir (dir Path.</> "base")
dispatch _ = Z.printHelp usage options

body :: FilePath -> FilePath -> FilePath -> IO ()
body file dir baseDir =
    do checkRegressFile file
       Dir.createDirectoryIfMissing True baseDir
       pats   <- readPatterns file
       trees  <- K.dirTrees [Path.takeFileName dir] "." pats
       K.withCurrentDirectory baseDir $ createDirTrees trees
       let paths = createPath <$> (K.treePaths K.<++> trees)
       regressTo baseDir K.<#!> paths

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

regressTo :: FilePath -> FilePath -> IO ()
regressTo dir path = regress dir path (dir Path.</> path)

regress :: FilePath -> FilePath -> FilePath -> IO ()
regress dir path path' = check where

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
      K.putLn
      help
      command

    size = show . Bz.length

    -- ----------------------  text

    diff xs ys = Diff.diffContext 2 $ Diff.getGroupedDiffBy sameLine xs ys
    sameLine x y = snd x == snd y

    diffText wd ds = do
      K.putLn >> diffFound >> hr
      Diff.printContextDiff wd ds
      hr >> diffFound >> K.putLn
      help
      command

    -- ----------------------  help and command

    hr = putStrLn $ replicate 70 '-'

    diffFound = do
      putStrLn $ "Differences found on " ++ qqString path

    help = do
      putStrLn   "Type 'a' to abort process"
      putStrLn $ "  or 'u' to update base file in " ++ qqString dir
      putStrLn   "  or 's' to skip this file"
      --putStrLn   "  or 'b' to skip this file and switch to batch mode"

    command = do
      s <- K.prompt
      case s of
        "s" -> K.putLn
        "a" -> K.putLn >> Z.putAbort
        "u" -> K.putLn >> Dir.copyFile path path'
        _   -> K.putLn >> help >> command

readBz :: FilePath -> IO K.Bz
readBz path = do
  file <- K.readBzFile path
  K.abortLeft $ K.bzFileContent file

qqString :: K.Map String
qqString s = "\"" ++ s ++ "\""

testTextFile :: K.Test K.Bz
testTextFile = Bz.null . Bz.filter bin where
    bin c = K.isControlCode c && not (K.isFormatCode c)

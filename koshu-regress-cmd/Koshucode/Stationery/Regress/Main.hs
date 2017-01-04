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

-- | Entry point of regression test driver.
regressMain :: IO ()
regressMain =
    do args <- Z.parseCommand options
       case args of
         Left errs -> Z.printHelp errs options
         Right (ps, [file])
             | flag "help"    -> Z.printHelp usage options
             | flag "version" -> putStrLn ver
             | otherwise      -> let dir = file Path.-<.> "d"
                                 in body file dir (dir Path.</> "base")
             where flag = Z.getFlag ps
         Right (_, _)         -> Z.printHelp usage options

body :: FilePath -> FilePath -> FilePath -> IO ()
body file dir baseDir =
    do checkRegressFile file
       Dir.createDirectoryIfMissing True baseDir
       pats   <- readPatterns file
       trees  <- K.dirTrees [Path.takeFileName dir] "." pats
       Dir.withCurrentDirectory baseDir $ createDirTrees trees
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

createDirTrees :: [K.Subtree] -> IO ()
createDirTrees = mapM_ createDirTree

createDirTree :: K.Subtree -> IO ()
createDirTree (K.TreeL _) = return ()
createDirTree (K.TreeB _ y xs) =
    do Dir.createDirectoryIfMissing True y
       Dir.withCurrentDirectory y $ createDirTrees xs

regressTo :: FilePath -> FilePath -> IO ()
regressTo dir path = regress dir path (dir Path.</> path)

regress :: FilePath -> FilePath -> FilePath -> IO ()
regress dir path path' = check where

    check = do
      exist <- Dir.doesFileExist path'
      case exist of
        False -> Dir.copyFile path path'
        True  -> do bz  <- readBz path
                    bz' <- readBz path'
                    comp bz bz'
                         
    -- ----------------------  compare

    comp bz bz'
        | testBinaryFile bz
            = case bz == bz' of
                True   -> putOK
                False  -> diffBinary bz bz'
        | otherwise
            = let ls   = K.linesCrlfBzNumbered bz
                  ls'  = K.linesCrlfBzNumbered bz'
                  wd   = K.digitsLength 10 $ 100 + length ls
              in case diff ls' ls of
                   ds | ok ds      -> putOK
                      | otherwise  -> diffText wd ds

    putOK = putStrLn $ "OK - " ++ path

    -- ----------------------  binary

    diffBinary bz bz' = do
      K.putLn >> diffFound >> K.putLn
      putStrLn   "  Cannot display non-textual content of the file."
      putStrLn $ "  Size of base file is " ++ size bz'
                      ++ " bytes, new is " ++ size bz ++ "."
      K.putLn
      help
      command

    size = show . Bz.length

    -- ----------------------  text

    diff xs ys = Diff.diffContext 2 $ Diff.getGroupedDiffBy sameLine xs ys
    sameLine x y = snd x == snd y

    ok []               = True
    ok [Diff.Both _ _]  = True
    ok _                = False

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
        _   -> command

readBz :: FilePath -> IO K.Bz
readBz path = do
  file <- K.readBzFile path
  K.abortLeft $ K.bzFileContent file

qqString :: K.Map String
qqString s = "\"" ++ s ++ "\""

testBinaryFile :: K.Test K.Bz
testBinaryFile = Bz.any (< K.integralSpace)
                 . Bz.take 1024
                 . Bz.takeWhile K.isAsciiCode
                 . Bz.filter (not . K.isNewlineCode)


{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Koshu regression test driver.

module Koshucode.Stationery.Regress.DirTree
 ( checkRegressFile,
   preparePaths,
 ) where

import qualified System.Directory                    as Dir
import qualified System.FilePath                     as Path
import qualified Koshucode.Baala.DataPlus            as K
import qualified Koshucode.Baala.Base.Message        as Msg

checkRegressFile :: FilePath -> IO ()
checkRegressFile path =
    do exist <- Dir.doesFileExist path
       case exist of
         True   -> return ()
         False  -> K.abortLeft $ Msg.adlib "No regress file"

preparePaths :: FilePath -> FilePath -> FilePath -> FilePath -> IO [FilePath]
preparePaths targetDir regFile regDir baseDir =
  do pats   <- readPatterns regFile
     trees  <- K.dirTrees [Path.takeFileName regDir] targetDir pats
     K.withCurrentDirectory baseDir $ createDirTrees trees
     let paths = createPath <$> (K.treePaths K.<++> trees)
     return paths

readPatterns :: FilePath -> IO [K.SubtreePattern]
readPatterns regFile =
    do ls  <- K.readSubtreeClauses regFile
       ls2 <- K.abortLeft ls
       ps  <- K.abortLeft (createPatterns K.<#> ls2)
       return $ concat ps
       
createPatterns :: K.TokenClause -> K.Ab [K.SubtreePattern]
createPatterns cl = 
    do ts <- K.toTrees cl
       K.decodeSubtreePattern ts

createPath :: ([String], String) -> FilePath
createPath (ys, z) = Path.joinPath ys Path.</> z

createDirTrees :: [K.Subtree String] -> IO ()
createDirTrees = mapM_ createDirTree

createDirTree :: K.Subtree String -> IO ()
createDirTree (K.TreeL _) = return ()
createDirTree (K.TreeB _ y xs) =
    do Dir.createDirectoryIfMissing True y
       K.withCurrentDirectory y $ createDirTrees xs


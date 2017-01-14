{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Koshu regression test driver.

module Koshucode.Stationery.Regress.Test
 ( regressTo,
 ) where

import qualified Data.Algorithm.Diff                   as Diff
import qualified Data.ByteString.Lazy                  as Bz
import qualified System.Directory                      as Dir
import qualified System.FilePath                       as Path
import qualified Koshucode.Baala.System                as Z
import qualified Koshucode.Baala.DataPlus              as K
import qualified Koshucode.Stationery.Regress.Diff     as Diff
import qualified Koshucode.Stationery.Regress.Para     as Rg

regressTo :: FilePath -> FilePath -> Rg.Para -> FilePath -> IO Rg.Para
regressTo targetDir baseDir p@Rg.Para {..} path =
    let p' = p { Rg.pTotal = pTotal + 1 }
    in regress p' baseDir (targetDir Path.</> path) (baseDir Path.</> path) where

regress :: Rg.Para -> FilePath -> FilePath -> FilePath -> IO Rg.Para
regress p@Rg.Para {..} dir path path' = check where

    check = do
      exist <- Dir.doesFileExist path'
      case exist of
        False -> do putNew
                    Dir.copyFile path path'
                    return $ p { Rg.pNew = pNew + 1 }
        True  -> do bz  <- readBz path
                    bz' <- readBz path'
                    case bz == bz' of
                      True  -> do putOK
                                  return $ p { Rg.pOk = pOk + 1 }
                      False -> do comp bz bz'
                                  return $ p { Rg.pDiff  = pDiff + 1
                                             , Rg.pDiffs = pTotal : pDiffs }
                         
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


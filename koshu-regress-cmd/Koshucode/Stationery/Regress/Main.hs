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
import qualified Koshucode.Stationery.Regress.Para     as Reg

-- | Entry point of regression test driver.
regressMain :: IO ()
regressMain = dispatch K.# Reg.parsePara where

    dispatch Reg.Para {..}
        | K.some pError  = help pError
        | pHelp          = help Reg.usage
        | pVersion       = putStrLn Reg.version
    dispatch p@Reg.Para { pFiles = [regFile] } = dir p regFile
    dispatch _ = help Reg.usage

    help msg = Z.printHelp msg Reg.options

    dir p regFile =
        let regDir  = regFile Path.-<.> "d"
            baseDir = regDir Path.</> "base"
        in body p regFile regDir baseDir

    body p@Reg.Para {..} regFile regDir baseDir =
        do Reg.checkRegressFile regFile
           Dir.createDirectoryIfMissing True baseDir
           paths <- Reg.preparePaths pTarget regFile regDir baseDir
           p' <- K.foldM (regressTo pTarget baseDir) p paths
           summary p'

    summary Reg.Para {..} | pTotal == 0 = return ()
    summary Reg.Para {..} = 
        do K.putLn
           putStrLn "**"
           putStrLn $ "**  Summary"
           putCnt pNew   "**    NEW    = " ""
           putCnt pOk    "**    OK     = " ""
           putCnt pDiff  "**    DIFF   = " (diffs pDiffs)
           putCnt pTotal "**    TOTAL  = " ""
           putStrLn "**"

    putCnt c label note =
        K.when (c > 0) $ putStrLn (label ++ show c ++ note)

    diffs ds | null ds = ""
    diffs ds = let ds'  = take 5 $ reverse ds
                   list = unwords ((show . K.list1) <$> ds')
                   etc | length ds > 5 = " etc"
                       | otherwise     = ""
               in "  --  See " ++ list ++ etc

regressTo :: FilePath -> FilePath -> Reg.Para -> FilePath -> IO Reg.Para
regressTo targetDir baseDir p@Reg.Para {..} path =
    let p' = p { Reg.pTotal = pTotal + 1 }
    in regress p' baseDir (targetDir Path.</> path) (baseDir Path.</> path) where

regress :: Reg.Para -> FilePath -> FilePath -> FilePath -> IO Reg.Para
regress p@Reg.Para {..} dir path path' = check where

    check = do
      exist <- Dir.doesFileExist path'
      case exist of
        False -> do putNew
                    Dir.copyFile path path'
                    return $ p { Reg.pNew = pNew + 1 }
        True  -> do bz  <- readBz path
                    bz' <- readBz path'
                    case bz == bz' of
                      True  -> do putOK
                                  return $ p { Reg.pOk = pOk + 1 }
                      False -> do comp bz bz'
                                  return $ p { Reg.pDiff  = pDiff + 1
                                             , Reg.pDiffs = pTotal : pDiffs }
                         
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


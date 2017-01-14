{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Koshu regression test driver.

module Koshucode.Stationery.Regress.Main
 ( regressMain
 ) where

import qualified System.Directory                      as Dir
import qualified System.FilePath                       as Path
import qualified Koshucode.Baala.System                as Z
import qualified Koshucode.Baala.DataPlus              as K
import qualified Koshucode.Stationery.Regress.DirTree  as Rg
import qualified Koshucode.Stationery.Regress.Para     as Rg
import qualified Koshucode.Stationery.Regress.Test     as Rg

-- | Entry point of regression test driver.
regressMain :: IO ()
regressMain = dispatch K.# Rg.parsePara where

    dispatch Rg.Para {..}
        | K.some pError  = help pError
        | pHelp          = help Rg.usage
        | pVersion       = putStrLn Rg.version
    dispatch p@Rg.Para { pFiles = [regFile] } = dir p regFile
    dispatch _ = help Rg.usage

    help msg = Z.printHelp msg Rg.options

    dir p regFile =
        let regDir  = regFile Path.-<.> "d"
            baseDir = regDir Path.</> "base"
        in body p regFile regDir baseDir

    body p@Rg.Para {..} regFile regDir baseDir =
        do Rg.checkRegressFile regFile
           Dir.createDirectoryIfMissing True baseDir
           paths <- Rg.preparePaths pTarget regFile regDir baseDir
           p' <- K.foldM (Rg.regressTo pTarget baseDir) p paths
           summary p'

    summary Rg.Para {..} | pTotal == 0 = return ()
    summary Rg.Para {..} = 
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


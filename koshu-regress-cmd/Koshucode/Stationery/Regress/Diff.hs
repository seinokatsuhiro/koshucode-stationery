{-# OPTIONS_GHC -Wall #-}

-- | Contextual diff.

module Koshucode.Stationery.Regress.Diff
 ( diffContext,
   printContextDiff,
 ) where

import qualified Data.Algorithm.Diff             as Diff
import qualified Koshucode.Baala.DataPlus        as K

-- | Edit diff result with context of /N/ lines.
diffContext :: Int -> [Diff.Diff [a]] -> [Diff.Diff [a]]
diffContext n = loop where
    loop (Diff.Both xs _ : ds) =
        case takeEdge n xs of
          Right (low, up)  -> Diff.Both low up : loop ds
          Left  _          -> Diff.Both xs []  : loop ds
    loop (diff : ds) = diff : loop ds
    loop [] = []

-- | Take beginning and ending /N/ elements of list.
--   If the list is too short, whole list is returned in 'Left'.
--
--   >>> takeEdge 2 "abcdefg"
--   Right ("ab","fg")
--
--   >>> takeEdge 2 "abc"
--   Left "abc"
--
takeEdge :: Int -> [a] -> Either [a] ([a], [a])
takeEdge n xs
    | length xs > 2 * n = Right (take n xs, take n K./$/ xs)
    | otherwise         = Left xs

-- | Print result of contextual diff.
printContextDiff :: Int -> [Diff.Diff [K.NumberedLine]] -> IO ()
printContextDiff wd = mapM_ p where
    p (Diff.First  xs)   = old "- " K.<#!> xs
    p (Diff.Second ys)   = new "+ " K.<#!> ys
    p (Diff.Both xs [])  = old "  " K.<#!> xs
    p (Diff.Both low up) = do old "  " K.<#!> low
                              ell "  "
                              old "  " K.<#!> up

    new sym (_, s) = putStrLn $ sym ++ space  ++ " | " ++ trunc s
    old sym (l, s) = putStrLn $ sym ++ zero l ++ " | " ++ trunc s
    ell sym        = putStrLn $ sym ++ ellip  ++ " |"

    trunc  = K.truncateString 200
    zero   = K.padBeginWith '0' wd . show
    ellip  = replicate wd cdot
    space  = replicate wd ' '

-- | Centered dot character (@'·'@).
cdot :: Char
cdot = '·'

-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | An interpreter for Copilot specifications.

{-# LANGUAGE Safe #-}

module Copilot.Interpret
  ( Format (..)
  , interpret
  , paint
  ) where

import Data.List
import Copilot.Core
import Copilot.Interpret.Eval
import Copilot.Interpret.Render

-- | Output format for the results of a Copilot spec interpretation.
data Format = Table | CSV

-- | Interpret a Copilot specification.
interpret :: Format  -- ^ Format to be used for the output.
          -> Int     -- ^ Number of steps to interpret.
          -> Spec    -- ^ Specification to interpret.
          -> String
interpret format k spec =
  case format of
    Table   -> renderAsTable e
    CSV     -> renderAsCSV e
  where
    e = eval Haskell k spec

paint :: Int     -- ^ Number of steps to interpret.
      -> Spec    -- ^ Specification to interpret.
      -> IO ()
paint k spec =
    paintEvaluation e
  where
    e = eval Haskell k spec

paintEvaluation :: ExecTrace
                -> IO ()
paintEvaluation e = do
    putStrLn "\\documentclass{standalone}"
    putStrLn "\\usepackage{tikz}"
    putStrLn "\\usepackage{tikz-timing}"
    putStrLn "\\begin{document}"
    putStrLn "\\begin{tikztimingtable}"
    printTriggerOutputs
    putStrLn "\\end{tikztimingtable}"
    putStrLn "\\end{document}"
    printObserverOutputs
  where

    printTriggerOutputs = mapM_ (printTriggerOutput) trigs

    printTriggerOutput (name, ls) = putStrLn $ name ++ " & "
                                                 ++ concatMap printTriggerOutputListElem ls
                                                 ++ "\\\\"

    printTriggerOutputListElem Nothing  = ""
    printTriggerOutputListElem (Just x) = showValues x

    showValues [] = ""
    showValues ls@(x:_)
      | isBoolean x = showValuesBoolean ls
      | otherwise   = showValuesNumber  ls

    showValuesBoolean ls = concatMap showValuesBooleanSegment $ group ls

    showValuesBooleanSegment ls = show (length ls) ++ highOrLow (head ls)
      where
        highOrLow "true"  = "H"
        highOrLow "false" = "L"

    showValuesNumber ls = concatMap showValuesNumberSegment $ group ls

    showValuesNumberSegment ls =
       show (length ls) ++ "D{" ++ (head ls) ++ "}"

    printObserverOutputs = return ()

    trigs = interpTriggers e
    obsvs = interpObservers e

isBoolean "true" = True
isBoolean "false" = True
isBoolean _ = False

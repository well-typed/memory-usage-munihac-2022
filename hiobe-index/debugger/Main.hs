{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

module Main where

import System.Environment

import GHC.Debug.Client
import GHC.Debug.Client.Search
import GHC.Debug.Retainers
import GHC.Debug.Snapshot

main :: IO ()
main = getArgs >>= \case
  ("snapshot":snapshotName:_) -> do
    sock <- getEnv "GHC_DEBUG_SOCKET"
    withDebuggeeConnect sock $
      \d -> makeSnapshot d snapshotName
  ("debug":snapshotName:_) -> do
    putStrLn "debugging away.."
    snapshotRun snapshotName analysis
  _ -> putStrLn "bad arguments"

analysis :: Debuggee -> IO ()
analysis d = do
  (g,rets) <- run d $ do
    rs <- gcRoots
    [rets] <- findRetainersOfConstructor (Just 1) rs "HiobeState"
    loc <- addLocationToStack rets

    let root = last rets
    g <- buildHeapGraph Nothing root
    return (g,[("HiobeState retainers", loc)])

  displayRetainerStack rets
  putStrLn "\n"

  let numIntegers = length $ findConstructors "IS" g
      numBins = length $ findConstructors "Bin" g
  putStrLn $ "found " ++ show numIntegers ++ " integers"
  putStrLn $ "found " ++ show numBins ++ " map nodes"

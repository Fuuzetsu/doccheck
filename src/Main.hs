-- |
-- Copyright   :  (c) Mateusz Kowalczyk 2013,
-- License     :  GPLv3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
-- Portability :  portable

module Main where

import Control.Applicative ((<$>))
import Control.Monad (unless)
import Documentation.DocCheck
import GHC (runGhc)
import GHC.Paths (libdir)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

-- | Exits with 'exitFailure' if any warnings are found or any files provided
-- do not exist. Note that it will not exit on any parse errors.
main :: IO ()
main = do
  files <- parseArgs
  allFiles <- concat <$> mapM getHaskellFiles files
  sources <- mapM readFile allFiles
  (fs, ps) <- runGhc (Just libdir) (extractDocs $ zip allFiles sources)
  let issues = concat $ findIssues ps
  unless (null fs) (putStrLn $ "Following files failed to parse:\n"
                    ++ unlines (map (\(f, m) -> f ++ " - " ++ m) fs))
  unless (null issues) (putStr issues >> exitFailure)

-- | Reports usage if it sees @-h@ as the first argument, otherwise treats any
-- input as files.
parseArgs :: IO [String]
parseArgs = do
  args <- getArgs
  case args of
    "-h":_ -> putStrLn "usage: doccheck [-h] [file1 ...]" >> exitSuccess
    xs -> return xs

-- |
-- Module      :  Documentation.DocCheck
-- Copyright   :  (c) Mateusz Kowalczyk 2013,
-- License     :  GPLv3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
-- Portability :  portable

{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Bag (bagToList)
import           Control.Applicative ((*>), (<$>))
import           Control.Monad (unless, liftM2)
import qualified Data.Attoparsec.Text as A (takeWhile, Parser, parseOnly)
import           Data.Text (Text, pack)
import           DynFlags (DynFlag(Opt_Haddock), getDynFlags, dopt_set)
import           FastString (unpackFS)
import           GHC (Ghc, runGhc, HsDocString(..), DocDecl(..), HsDecl(..),
                      GenLocated(L), hsmodDecls, parser)
import           GHC.Paths (libdir)
import           System.Directory (doesFileExist, doesDirectoryExist)
import           System.Directory.Tree (AnchoredDirTree(..), DirTree(..),
                                        filterDir, readDirectoryWith,
                                        flattenDir)
import           System.Environment (getArgs)
import           System.Exit (exitFailure, exitSuccess)
import           System.FilePath (takeExtension)

-- | Extracts Haddock documentation from all the files provided. Note that any
-- modules that fail to parse will be reported as such. Note that the program
-- will still exit succesfully even if some files don't parse but no potential
-- warnings are found in regards to documentation.
extractDocs
  :: [(FilePath, String)] -- ^ Files and their sources to extract the docs from
     -> Ghc ([(FilePath, String)], [(FilePath, [String])])
extractDocs files = do
  dflags' <- flip dopt_set Opt_Haddock <$> getDynFlags
  let psed = map (\(f, s) -> (f, parser s dflags' f)) files
  return (failedParses psed, map stripLoc $ rightLocs psed)
  where
    -- Determine which modules failed to parse
    failedParses xs = [ (f, show $ bagToList m) | (f, Left m) <- xs ]


    -- Pull out module declarations from successful parses, drop location
    rightLocs xs = [ (f, hsmodDecls x) | (f, Right (_, L _ x)) <- xs ]

    -- Get out Haddock comment strings from amongst other module declarations
    stripLoc (f, ls) = (f, [ docDeclToString x | (L _ (DocD x)) <- ls ])

-- | Extracts a 'String' from 'DocDecl's by unpacking the 'FastString's
-- with 'unpackFs' packed in the 'HsDocString' data type.
docDeclToString :: DocDecl -> String
docDeclToString (DocCommentNext (HsDocString x)) = unpackFS x
docDeclToString (DocCommentPrev (HsDocString x)) = unpackFS x
docDeclToString (DocCommentNamed _ (HsDocString x)) = unpackFS x
docDeclToString (DocGroup _ (HsDocString x)) = unpackFS x

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

parseArgs :: IO [String]
parseArgs = do
  args <- getArgs
  case args of
    ("-h":_) -> putStrLn "usage: doccheck [-h] [file1 ...]" >> exitSuccess
    xs -> return xs

-- | Finds potential problems for each comment in each file and
-- formats the warning messages.
findIssues :: [(FilePath, [String])] -> [String]
findIssues fs = filter (not . null) $ map warn fs
  where
    warn :: (FilePath, [String]) -> String
    warn (p, docs) =
      case [ (d, x) |
             (d, Just x) <- map (\d' -> (d', runParsers $ pack d')) docs ] of
        [] -> []
        xs -> "Potential problems in " ++ p
              ++ " :\n" ++ unlines (map formatProblems xs)
          where
            formatProblems :: (String, [String]) -> String
            formatProblems (doc, issues) =
              unlines $ map (++ " in ‘" ++ doc ++ "’") issues

-- | Runs multiple parsers on each of the strings and collects results of any
-- parsers that succeed. Note that these results will are used as the warning
-- messages so each parser should be in form of
-- @p = someParsing *> return "warning message for this parser"@
runParsers :: Text -> Maybe [String]
runParsers d = case [ x | Right x <- map (`A.parseOnly` d) parsers ] of
  [] -> Nothing
  xs -> Just xs
  where
    parsers :: [A.Parser String]
    parsers = [escapingEmph, htmlEmph]
      where
        escapingEmph = A.takeWhile (/= '/') *> "/"
                       *> A.takeWhile (`notElem` "\n\\/") *> "\\"
                       *> return ("attempting to escape a character "
                                  ++ "inside of emphasis")
        htmlEmph = A.takeWhile (/= '/') *> "/"
                   *> A.takeWhile (`notElem` "\n&/") *> "&#"
                   *> return "HTML sequence inside of emphasis"

-- | Walks the file system looking for Haskell source files, starting from the
-- given file.
getHaskellFiles :: FilePath -> IO [FilePath]
getHaskellFiles s = do
  exists <- liftM2 (||) (doesFileExist s) (doesDirectoryExist s)
  unless exists $ putStrLn ("no such file or directory: " ++ s) >> exitFailure
  _:/tree <- readDirectoryWith return s
  return $ map flattenFiles $ filter f $ flattenDir $ filterDir myPred tree
  where
    myPred (Dir ('.':_) _) = False
    myPred (File n _) = e == ".hs" || e == ".lhs"
      where e = takeExtension n
    myPred _ = True

    f (File _ _) = True
    f _ = False

    flattenFiles (File _ path) = path
    flattenFiles x = error $ "Got non-file into flattenFiles: " ++ show x

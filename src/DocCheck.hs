{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import GHC.Paths
import GHC
import Lexer

import Data.Traversable (traverse)
import System.Directory.Tree (AnchoredDirTree(..), DirTree(..),
                              filterDir, readDirectoryWith, flattenDir)
import SrcLoc
import StringBuffer
import System.FilePath (takeExtension)

import Debug.Trace
import System.FilePath
import Control.Applicative
import System.Environment
import System.Directory
import Data.Attoparsec.Text
import Data.Text (pack)
import FastString
import DynFlags
import HsDecls
import qualified Data.Attoparsec.Text as A
import Data.Text (pack, Text(..))
import System.Exit


extractDocs :: [FilePath] -> [String] -> Ghc [(FilePath, [String])]
extractDocs files sources = do
  dflags <- flip dopt_set Opt_Haddock <$> getDynFlags
  let psed = map (\(f, s) -> (f, parser s dflags f)) (zip files sources)
      hsm = [ (f, hsmodDecls x)  | (f, Right (_, L _ x)) <- psed ]
      extract (f, ls) = (f, [ x | (L _ x) <- ls ])
      extract' (f, ls) = (f, [ x | (DocD x) <- ls ])
      extract'' (f, ls) = (f, map docDeclToString ls)
      docDeclToString (DocCommentNext (HsDocString x)) = init . tail $ show x
      docDeclToString (DocCommentPrev (HsDocString x)) = init . tail $ show x
      docDeclToString (DocCommentNamed _ (HsDocString x)) = init . tail $ show x
      docDeclToString (DocGroup _ (HsDocString x)) = init . tail $ show x
      decls = map extract hsm :: [(FilePath, [HsDecl RdrName])]
      docsDecl = map extract' decls :: [(FilePath, [DocDecl])]
      docs = map extract'' docsDecl
  return docs

test = do
  let f = "/tmp/Test.hs"
  s <- readFile f
  foo <- runGhc (Just libdir) (extractDocs [f] [s])
  return foo

main :: IO ()
main = do
  files <- getArgs
  allFiles <- concat <$> mapM getHaskellFiles files
  sources <- mapM readFile allFiles
  foo <- runGhc (Just libdir) (extractDocs allFiles sources)
  let issues = unlines $ findIssues foo
  when (not $ null issues) (putStr issues >> exitFailure)

findIssues :: [(FilePath, [String])] -> [String]
findIssues fs = filter (not . null) $ map warn fs
  where
    warn :: (FilePath, [String]) -> String
    warn (p, docs) =
      case [ (d, x) |
             (d, Just x) <- map (\d' -> (d', runParsers $ pack d')) docs ] of
        [] -> []
        xs -> "Potential problems in " ++ p
              ++ " :\n" ++ (unlines $ map formatProblems xs)
          where
            formatProblems :: (String, [String]) -> String
            formatProblems (doc, issues) = concat $ map (++ " in " ++ doc) issues

runParsers :: Text -> Maybe [String]
runParsers d = case [ x | Right x <- map (flip parseOnly d) parsers ] of
  [] -> Nothing
  xs -> Just xs
  where
    parsers :: [Parser String]
    parsers = [onetwothree]
      where
        onetwothree = A.takeWhile (/= '1') *> "123" *> return "123 found!"

getHaskellFiles s = do
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

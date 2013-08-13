{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad
import qualified Data.Attoparsec.Text as A
import           Data.Text (Text, pack)
import           DynFlags
import           FastString (unpackFS)
import           GHC
import           GHC.Paths
import           System.Directory
import           System.Directory.Tree (AnchoredDirTree(..), DirTree(..),
                                        filterDir, readDirectoryWith,
                                        flattenDir)
import           System.Environment
import           System.Exit
import           System.FilePath


extractDocs :: [FilePath] -> [String] -> Ghc [(FilePath, [String])]
extractDocs files sources = do
  dflags' <- flip dopt_set Opt_Haddock <$> getDynFlags
  let psed = map (\(f, s) -> (f, parser s dflags' f)) (zip files sources)
  return $ map stripLoc $ rightLocs psed
  where
    -- Pull out module declarations from successful parses, drop location
    rightLocs xs = [ (f, hsmodDecls x)  | (f, Right (_, L _ x)) <- xs ]

    -- Get out Haddock comment strings from amongst other module declarations
    stripLoc (f, ls) = (f, [ docDeclToString x | (L _ (DocD x)) <- ls ])


docDeclToString :: DocDecl -> String
docDeclToString (DocCommentNext (HsDocString x)) = unpackFS x
docDeclToString (DocCommentPrev (HsDocString x)) = unpackFS x
docDeclToString (DocCommentNamed _ (HsDocString x)) = unpackFS x
docDeclToString (DocGroup _ (HsDocString x)) = unpackFS x


main :: IO ()
main = do
  files <- getArgs
  allFiles <- concat <$> mapM getHaskellFiles files
  sources <- mapM readFile allFiles
  foo <- runGhc (Just libdir) (extractDocs allFiles sources)
  let issues = concat $ findIssues foo
  unless (null issues) (putStr issues >> exitFailure)

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

-- |
-- Module      :  Documentation.DocCheck.Parsers
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPLv3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
-- Portability :  portable

{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}
module Documentation.DocCheck.Parsers
    (escapingEmph, htmlEmph, WarningMessage, WarningParser(..), skipUntil)
    where

import           Control.Applicative ((*>), some, empty, many, (<|>), (<*))
import qualified Data.Attoparsec.Text as A
import           Data.Text           (Text, head, cons)

type Parser = A.Parser

-- | Warning message used when parsing succeeds.
type WarningMessage = String

-- | Existentially quantified parser with a 'WarningMessage' to produce on
-- success.
data WarningParser = forall a. WarningParser (A.Parser a) WarningMessage

-- | Looks for the escape character inside of emphasis pairs
escapingEmph :: WarningParser
escapingEmph = WarningParser emphBlock'
             "escape inside of emphasis"

-- | HTML escape sequence inside of emphasis.
htmlEmph :: WarningParser
htmlEmph = WarningParser htmlEmph'
           "HTML sequence inside of emphasis"

escapingEmph' :: A.Parser Text
escapingEmph' = A.takeWhile (/= '/') *> "/"
                *> A.takeWhile (`notElem` "\n\\/") *> "\\"

emphBlock' :: A.Parser Char
emphBlock' = A.takeWhile (/= '/') *> block *> A.anyChar
  where
    block = A.scan False p
      where
        p inside c
          | c == '/' = Just (not inside)
          | c == '\n' = Just False
          | c == '\\' && inside = Nothing
          | otherwise = Just inside

-- | Returns sections of text delimited by specified text.
-- getBlocks :: Text -> Text -> Parser [Text]
getBlocks
  :: Text -- ^ Opening delimiter
     -> Text -- ^ Closing delimiter
     -> A.Parser [Text]
getBlocks op ed = some $ skipUntil (Data.Text.head op) *> (start *> mid)
  where
    start = A.string op <|> A.anyChar *> start
    mid :: A.Parser Text
    mid = A.string ed *> return "" <|> do
      c <- A.anyChar
      m <- mid
      return $ c `cons` m

-- | Skip all characters until the specified one. Skips over
-- the character if it's escaped.
skipUntil :: Char -> A.Parser ()
skipUntil c = A.char '\\' *> A.anyChar *> skipUntil c -- honour escapes
              <|> peekTerminate -- if we get what we want, terminate
              <|> A.anyChar *> skipUntil c -- skip next char, recurse
  where
    peekTerminate = do
      x <- A.peekChar
      case x of
        Nothing -> fail "eof"
        Just x' -> if x' == c then return () else fail "not goal"

htmlEmph' :: A.Parser Text
htmlEmph' = A.takeWhile (/= '/') *> "/"
            *> A.takeWhile (`notElem` "\n&/") *> "&#"

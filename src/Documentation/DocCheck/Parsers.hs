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
import           Data.Text           (Text, head, cons, any, isInfixOf)

type Parser = A.Parser

-- | Warning message used when parsing succeeds.
type WarningMessage = String

-- | Existentially quantified parser with a 'WarningMessage' to produce on
-- success.
data WarningParser = forall a. WarningParser (A.Parser a) WarningMessage

-- | Looks for the escape character inside of emphasis pairs
escapingEmph :: WarningParser
escapingEmph = WarningParser emphEscape'
             "escape inside of emphasis"

-- | HTML escape sequence inside of emphasis.
htmlEmph :: WarningParser
htmlEmph = WarningParser htmlEmph'
           "HTML sequence inside of emphasis"

htmlEmph' :: A.Parser ()
htmlEmph' = do
  bs <- getBlocks "/" "/"
  if or $ map ("&#" `Data.Text.isInfixOf`) bs
    then return ()
    else fail "all clean"

emphEscape' :: A.Parser ()
emphEscape' = do
  bs <- getBlocks "/" "/"
  if or $ map (Data.Text.any (== '\\')) bs
    then return ()
    else fail "all clean"

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

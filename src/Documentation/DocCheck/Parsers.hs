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
    (escapingEmph, htmlEmph, WarningMessage, WarningParser(..))
    where

import           Control.Applicative ((*>))
import qualified Data.Attoparsec.Text as A
import           Data.Text (Text)

-- | Warning message used when parsing succeeds.
type WarningMessage = String

-- | Existentially quantified parser with a 'WarningMessage' to produce on
-- success.
data WarningParser = forall a. WarningParser (A.Parser a) WarningMessage

-- | Escape character \\\\ inside of emphasis.
escapingEmph :: WarningParser
escapingEmph = WarningParser escapingEmph'
               "attempting to escape a character inside of emphasis"

-- | HTML escape sequence inside of emphasis.
htmlEmph :: WarningParser
htmlEmph = WarningParser htmlEmph'
           "HTML sequence inside of emphasis"

escapingEmph' :: A.Parser Text
escapingEmph' = A.takeWhile (/= '/') *> "/"
                *> A.takeWhile (`notElem` "\n\\/") *> "\\"

htmlEmph' :: A.Parser Text
htmlEmph' = A.takeWhile (/= '/') *> "/"
            *> A.takeWhile (`notElem` "\n&/") *> "&#"

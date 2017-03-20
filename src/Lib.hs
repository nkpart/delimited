{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
module Text.Parser.CSV where

import Text.Parser.Char
import Text.Parser.Combinators
import Data.Char
import Control.Applicative
import Data.Monoid

--  https://tools.ietf.org/html/rfc4180#page-2

-- TODO:
--  cassava parses newlines as either \n, \r\n or \r. This might be a good choice.

fileP
  :: (Monad m, CharParsing m) =>
     m [[String]]
fileP =
  do h <- headerP <* crlfP
     rs <- sepEndBy1 recordP crlfP
     -- TODO: this is a hack to deal with a trailing newline
     let rs' = reverse rs
     case rs' of
       ([""]: rest) -> pure (h: reverse rest)
       _ -> pure (h: reverse rs)

headerP :: CharParsing m => m [[Char]]
headerP =
  sepBy1 nameP commaP

recordP :: CharParsing m => m [[Char]]
recordP =
  sepBy1 fieldP commaP

nameP :: CharParsing f => f [Char]
nameP =
  fieldP

fieldP :: CharParsing f => f [Char]
fieldP =
  escapedP <|> nonEscapedP

escapedP :: CharParsing f => f [Char]
escapedP =
  dquoteP *> many (textDataP <|> commaP <|> crP <|> lfP) <* dquoteP

nonEscapedP :: CharParsing f => f [Char]
nonEscapedP = many textDataP

textDataP :: CharParsing m => m Char
textDataP =
  oneOf $ fmap chr ([0x20, 0x21] <> [0x23..0x2B] <> [0x2D..0x7E])

dquoteP :: CharParsing m => m Char
dquoteP = char' 0x22

commaP :: CharParsing m => m Char
commaP = char' 0x2C

crP :: CharParsing m => m Char
crP = char' 0x0D

lfP :: CharParsing m => m Char
lfP = char' 0x0A

crlfP :: CharParsing f => f Char
crlfP = crP *> lfP

char' :: CharParsing m => Int -> m Char
char' = char . chr

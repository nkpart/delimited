{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
module Text.Delimited.CSV.Parser where

import           Control.Applicative
import           Control.Lens
import           Data.Functor
import           Data.List.NonEmpty
import           Data.Monoid
import           Text.Delimited.CSV
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Trifecta

-- https://tools.ietf.org/html/rfc4180#page-2

fileP
  :: (Monad m, DeltaParsing m)
  => m CSV
fileP = do
  h <- (,) <$> headerP <*> newlineP
  rs <- many ((,) <$> recordP <*> newlineP)
  pure $ CSV (h :| rs)

headerP :: DeltaParsing f => f Record
headerP = Record <$> spanned (sepBy1NE (spanned nameP) commaP)

recordP :: DeltaParsing f => f Record
recordP = Record <$> spanned (sepBy1NE (spanned field) commaP)

nameP :: (Monad f, CharParsing f) => f Field
nameP = field

field :: (CharParsing f, Monad f) => f Field
field = (Quoted '"' <$> escapedP) <|> (Unquoted <$> nonEscapedP)

escapedP :: (Monad f, CharParsing f) => f [QuotedData]
escapedP =
  char '"' *>
  many
    ((TextDataC <$> textDataP) <|> (Comma' <$ commaP) <|> (CR' <$ char '\r') <|>
     (LF' <$ char '\n') <|>
     (DoubleQuote <$ quoteQuote)) <*
  char '"'

quoteQuote :: CharParsing m => m String
quoteQuote = string "\"\""

nonEscapedP :: (CharParsing f, Monad f) => f [TextData]
nonEscapedP = many textDataP

textDataP :: (Monad m, CharParsing m) => m TextData
textDataP =
  try
    (do c <- anyChar
        filterP (c ^? _TextData) ("Invalid csv character: " <> pure c)) <?>
  "textdata"
  where
    filterP z msg = maybe (fail msg) pure z

commaP :: CharParsing m => m Char
commaP = char ','

newlineP :: CharParsing f => f EOL
newlineP =
  (try (string "\r\n") $> CRLF) <|> (char '\r' $> CR) <|> (char '\n' $> LF)

sepBy1NE :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepBy1NE p sep = (:|) <$> p <*> many (sep *> p)
{-# INLINE sepBy1NE #-}

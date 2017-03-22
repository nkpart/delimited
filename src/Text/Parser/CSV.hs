{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Text.Parser.CSV where

import           Control.Applicative
import           Control.Lens
import           Data.Char
import           Data.Functor
import           Data.List
import           Data.List.NonEmpty
import           Data.Monoid
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Trifecta

-- https://tools.ietf.org/html/rfc4180#page-2

data CSV =
  CSV (NonEmpty (Record, EOL))
  deriving (Eq, Show)

data Record =
  Record (NonEmpty (Spanned Field))
  deriving (Eq, Show)

data EOL
  = CRLF
  | CR
  | LF
  deriving (Eq, Show)

data Field
  = Quoted Char
           [CSVChar]
  | Unquoted [TextData]
  deriving (Eq, Show)

fieldS :: Field -> String
fieldS (Quoted _ cs) =
  let f (TextDataC td) = _TextData # td
      f (Comma')       = ','
      f (CR')          = '\r'
      f (LF')          = '\n'
      f DQuote         = '"'
   in fmap f cs
fieldS (Unquoted tds) = fmap (_TextData #) tds

data CSVChar
  = TextDataC TextData
  | Comma'
  | CR'
  | LF'
  | DQuote
  deriving (Eq, Show)


newtype TextData =
  TextData Char
  deriving (Eq, Show)

_TextData :: Prism' Char TextData
_TextData =
  prism'
    (\(TextData c) -> c)
    (\c ->
       if f c
         then Just (TextData c)
         else Nothing)
  where
    f c =
      c == chr 0x20 ||
      c == chr 0x21 ||
      (c >= chr 0x23 && c <= chr 0x2B) || (c >= chr 0x2D && c <= chr 0x7E)
  -- As specified in the RFC:
  -- !#$%&'()*+-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~
  -- oneOf $ fmap chr ([0x20, 0x21] <> [0x23..0x2B] <> [0x2D..0x7E])

printCsv :: CSV -> String
printCsv (CSV records) = toList records >>= printRecord
  where
    printRecord (Record fields, eol) =
      intercalate "," (toList $ fmap printField fields) <> printEol eol
    printEol CRLF = "\r\n"
    printEol CR   = "\r"
    printEol LF   = "\n"
    printField (Quoted q content :~ _) = q : (=<<) printCSVChar content <> [q]
    printField (Unquoted content :~ _) = fmap (_TextData #) content
    printCSVChar (TextDataC textdata) = pure $ _TextData # textdata
    printCSVChar DQuote               = "\"\""
    printCSVChar Comma'               = pure ','
    printCSVChar CR'                  = pure '\r'
    printCSVChar LF'                  = pure '\n'

fileP
  :: (Monad m, DeltaParsing m)
  => m CSV
fileP = do
  h <- (,) <$> headerP <*> newlineP
  rs <- many ((,) <$> recordP <*> newlineP)
  pure $ CSV (h :| rs)

headerP :: DeltaParsing f => f Record
headerP = Record <$> sepBy1NE nameP commaP

recordP :: DeltaParsing f => f Record
recordP = Record <$> sepBy1NE fieldP commaP

nameP :: DeltaParsing m => m (Spanned Field)
nameP = fieldP

fieldP :: DeltaParsing m => m (Spanned Field)
fieldP = spanned $ (Quoted '"' <$> escapedP) <|> (Unquoted <$> nonEscapedP)

escapedP :: (Monad f, CharParsing f) => f [CSVChar]
escapedP =
  dquoteP *>
  many ((TextDataC <$> textDataP) <|> (Comma' <$ commaP) <|> (CR' <$ crP) <|> (LF' <$ lfP) <|> (DQuote <$ quoteQuote)) <*
  dquoteP

quoteQuote :: CharParsing m => m String
quoteQuote = string "\"\""

nonEscapedP :: (CharParsing f, Monad f) => f [TextData]
nonEscapedP = many textDataP

textDataP :: (Monad m, CharParsing m) => m TextData
textDataP =
  try (
  do c <- anyChar
     filterP (c ^? _TextData) ("Invalid csv character: " <> pure c )
  ) <?> "textdata"

filterP :: Monad f => Maybe a -> String -> f a
filterP z msg =
  maybe (fail msg) pure z

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

newlineP :: CharParsing f => f EOL
newlineP = (try crlfP $> CRLF) <|> (crP $> CR) <|> (lfP $> LF)

char' :: CharParsing m => Int -> m Char
char' = char . chr

sepBy1NE :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepBy1NE p sep = (:|) <$> p <*> many (sep *> p)
{-# INLINE sepBy1NE #-}

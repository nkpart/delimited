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

-- https://tools.ietf.org/html/rfc4180#page-2

data CSV =
  CSV (NonEmpty (Record, EOL))
  deriving (Eq, Show)

data EOL
  = CRLF
  | CR
  | LF
  deriving (Eq, Show)

data Field =
    Quoted Char [CSVChar]
  | Unquoted [CSVChar]
  deriving (Eq, Show)

data CSVChar =
  TextDataC TextData
  | Comma' | CR' | LF'
  deriving (Eq, Show)

newtype TextData = TextData Char deriving (Eq, Show)

_TextData :: Prism' Char TextData
_TextData = prism' (\(TextData c) -> c) (\c -> if f c then Just (TextData c) else Nothing)
  where f c = c == chr 0x20
            || c == chr 0x21
            || (c >= chr 0x23 && c <= chr 0x2B)
            || (c >= chr 0x2D && c <= chr 0x7E)
  -- As specified in the RFC:
  -- !#$%&'()*+-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~
  -- oneOf $ fmap chr ([0x20, 0x21] <> [0x23..0x2B] <> [0x2D..0x7E])

data Record =
  Record (NonEmpty Field)
  deriving (Eq, Show)

printCsv :: CSV -> String
printCsv (CSV records) = toList records >>= printRecord
  where
    printRecord (Record fields, eol) =
      intercalate "," (toList $ fmap printField fields) <> printEol eol
    printEol CRLF = "\r\n"
    printEol CR   = "\r"
    printEol LF   = "\n"
    printField (Quoted q content) = q : fmap printCSVChar content <> [q]
    printField (Unquoted content) = fmap printCSVChar content
    printCSVChar (TextDataC textdata) = _TextData # textdata
    printCSVChar Comma' = ','
    printCSVChar CR' = '\r'
    printCSVChar LF' = '\n'

fileP
  :: (Monad m, CharParsing m)
  => m CSV
fileP = do
  h <- (,) <$> headerP <*> newlineP
  rs <- many ((,) <$> recordP <*> newlineP)
  pure $ CSV (h :| rs)

headerP :: (CharParsing f, Monad f) => f Record
headerP = Record <$> sepBy1NE nameP commaP

recordP :: (Monad f, CharParsing f) => f Record
recordP = Record <$> sepBy1NE fieldP commaP

nameP :: (Monad f, CharParsing f) => f Field
nameP = fieldP

fieldP :: (CharParsing f, Monad f) => f Field
fieldP = (Quoted '"' <$> escapedP) <|> (Unquoted <$> nonEscapedP)

escapedP :: (Monad f, CharParsing f) => f [CSVChar]
escapedP = dquoteP *> many ((TextDataC <$> textDataP) <|> (Comma' <$ commaP ) <|> (CR' <$ crP ) <|> (LF' <$ lfP )) <* dquoteP

nonEscapedP :: (CharParsing f, Monad f) => f [CSVChar]
nonEscapedP = many (TextDataC <$> textDataP)

textDataP :: (Monad m, CharParsing m) => m TextData
textDataP =
  try $
  do c <- anyChar
     filterP (^? _TextData) ("Invalid csv character: " <> pure c ) c

filterP :: Monad f => (t -> Maybe a) -> String -> t -> f a
filterP f msg z =
     case f z of
       Just v -> pure v
       Nothing -> fail msg

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

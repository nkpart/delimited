{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Text.Parser.CSV where

import           Control.Applicative
import           Data.Char
import           Data.Functor
import           Data.List
import           Data.Monoid
import           Text.Parser.Char
import           Text.Parser.Combinators

-- https://tools.ietf.org/html/rfc4180#page-2

data CSV =
  CSV [(Record, EOL)]
  deriving (Eq, Show)

data EOL
  = CRLF
  | CR
  | LF
  deriving (Eq, Show)

data Field =
  Field (Maybe Char)
        String
  deriving (Eq, Show)

data Record =
  Record [Field]
  deriving (Eq, Show)

printCsv :: CSV -> String
printCsv (CSV records) = records >>= printRecord
  where
    printRecord (Record fields, eol) =
      intercalate "," (fmap printField fields) <> printEol eol
    printEol CRLF = "\r\n"
    printEol CR   = "\r"
    printEol LF   = "\n"
    printField (Field quote content) =
      case quote of
        Just q  -> q : content <> [q]
        Nothing -> content

fileP
  :: (Monad m, CharParsing m) =>
     m CSV
fileP =
  do h <- (,) <$> headerP <*> newlineP
     rs <- many ((,) <$> recordP <*> newlineP)
     -- TODO: this is a hack to deal with a trailing newline
     pure $ CSV (h: rs)

headerP :: CharParsing m => m Record
headerP =
  Record <$> sepBy1 nameP commaP

recordP :: CharParsing m => m Record
recordP =
  Record <$> sepBy1 fieldP commaP

nameP :: CharParsing f => f Field
nameP =
  fieldP

fieldP :: CharParsing f => f Field
fieldP =
  (Field (Just '"') <$> escapedP) <|> ( Field Nothing <$> nonEscapedP )

escapedP :: CharParsing f => f [Char]
escapedP =
  dquoteP *> many (textDataP <|> commaP <|> crP <|> lfP) <* dquoteP

nonEscapedP :: CharParsing f => f [Char]
nonEscapedP = many textDataP

textDataP :: CharParsing m => m Char
textDataP =
  -- As specified in the RFC:
  -- !#$%&'()*+-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~
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

newlineP :: CharParsing f => f EOL
newlineP = (try crlfP $> CRLF) <|> (crP $> CR) <|> (lfP $> LF)

char' :: CharParsing m => Int -> m Char
char' = char . chr

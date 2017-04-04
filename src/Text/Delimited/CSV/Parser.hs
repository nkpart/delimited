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

-- $setup
-- >>>

-- https://tools.ietf.org/html/rfc4180#page-2

fileP
  :: (Monad m, DeltaParsing m)
  => m CSV
fileP = do
  h <- headerP
  rs <- many recordP
  pure $ csvRows # (h :| rs)

headerP :: DeltaParsing f => f Record
headerP =
     makeRecord <$> spanned (sepBy1NE (spanned nameP) commaP) <*> newlineP

recordP :: DeltaParsing f => f Record
recordP =
     makeRecord <$> spanned (sepBy1NE (spanned nameP) commaP) <*> newlineP

makeRecord :: Spanned (NonEmpty (Spanned Field)) -> EOL -> Record
makeRecord (fields :~ sp) eol = Record sp (fmap (\(fld :~ s) -> (s, fld)) fields) eol

nameP :: (Monad f, CharParsing f) => f Field
nameP = field

-- |
-- >>> parseTest (fieldContent <$> field) "\"this, is fine\""
-- "this, is fine"
-- >>> parseTest (fieldContent <$> field) "this is fine"
-- "this is fine"
field :: (CharParsing f, Monad f) => f Field
field = ((_Quoted #) <$> escapedP) <|> ((_Unquoted #) <$> nonEscapedP)

escapedP :: (Monad f, CharParsing f) => f [QuotedData]
escapedP =
  char '"' *>
  many
    ((_TextDataC <#> textDataP) <|> (_Comma' <# commaP) <|> (_CR' <# char '\r') <|>
     (_LF' <# char '\n') <|>
     (_DoubleQuote <# string "\"\"")) <* char '"'

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
  (try (string "\r\n") #> _CRLF) <|> (char '\r' #> _CR) <|> (char '\n' #> _LF)

sepBy1NE :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepBy1NE p sep = (:|) <$> p <*> many (sep *> p)
{-# INLINE sepBy1NE #-}

(<#>) :: Functor f => AReview a b -> f b -> f a
l <#> c = (l #) <$> c

(<#) :: Functor f => AReview a () -> f b -> f a
l <# c = (l # ()) <$ c

(#>) :: Functor f => f a -> AReview b () -> f b
c #> l = c $> (l # ())

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
module Text.Delimited.CSV where

import           Control.Lens
import           Data.List
import           Data.List.NonEmpty
import           Data.Monoid
import           Text.Trifecta

-- https://tools.ietf.org/html/rfc4180#page-2

data CSV =
  CSV (NonEmpty (Record, EOL))
  deriving (Eq, Show)

data Record =
  Record (Spanned (NonEmpty (Spanned Field)))
  deriving (Eq, Show)

data EOL
  = CRLF
  | CR
  | LF
  deriving (Eq, Show)

data Field
  = Quoted Char
           [QuotedData]
  | Unquoted [TextData]
  deriving (Eq, Show)

fieldContent :: Field -> String
fieldContent (Quoted _ cs) =
  let f (TextDataC td) = _TextData # td
      f Comma'         = ','
      f CR'            = '\r'
      f LF'            = '\n'
      f DoubleQuote         = '"'
  in fmap f cs
fieldContent (Unquoted tds) = fmap (_TextData #) tds

data QuotedData
  = TextDataC TextData
  | Comma'
  | CR'
  | LF'
  | DoubleQuote
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
    -- Valid CSV characters according to:
    -- https://tools.ietf.org/html/rfc4180#page-2
    f c =
      c == ' ' ||
      c == '!' ||
      (c >= '#' && c <= '+') || (c >= '-' && c <= '~')

printCsv :: CSV -> String
printCsv (CSV records) = toList records >>= printRecord
  where
    printRecord (Record (fields :~ _), eol) =
      intercalate "," (toList $ fmap printField fields) <> printEol eol
    printEol CRLF = "\r\n"
    printEol CR   = "\r"
    printEol LF   = "\n"
    printField (Quoted q content :~ _) = q : (=<<) printQuotedData content <> [q]
    printField (Unquoted content :~ _) = fmap (_TextData #) content
    printQuotedData (TextDataC textdata) = pure $ _TextData # textdata
    printQuotedData DoubleQuote               = "\"\""
    printQuotedData Comma'               = pure ','
    printQuotedData CR'                  = pure '\r'
    printQuotedData LF'                  = pure '\n'

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell        #-}
module Text.Delimited.CSV
  (
    CSV, csvRows,
    Record, recordFields,
    EOL, _CRLF, _CR, _LF,
    Field, _Quoted, _Unquoted,
    QuotedData, _TextDataC, _Comma', _CR', _LF', _DoubleQuote,
    TextData, _TextData,
    renderCsv, fieldContent
  )
 where

import           Control.Lens
import           Data.List          (intercalate)
import           Data.List.NonEmpty (NonEmpty, toList)
import           Data.Monoid        ((<>))
import           Prelude            (Eq, Show)
import           Prelude            (Char, String)
import           Prelude            (Maybe (Just, Nothing))
import           Prelude            (fmap, pure, (=<<), (>>=), ($))
import           Prelude            ((&&), (<=), (==), (>=), (||))
import           Text.Trifecta      (Spanned ((:~)))

-- https://tools.ietf.org/html/rfc4180#page-2

-- $setup
-- >>> import Data.Char (chr)

newtype CSV =
  CSV { _csvRows :: NonEmpty (Record, EOL) }
  deriving (Eq, Show)

newtype Record =
  Record { _recordFields :: Spanned (NonEmpty (Spanned Field)) }
  deriving (Eq, Show)

data EOL
  = CRLF
  | CR
  | LF
  deriving (Eq, Show)

data Field
  = Quoted [QuotedData]
  | Unquoted [TextData]
  deriving (Eq, Show)

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

makeLenses ''CSV
makeLenses ''Record
makePrisms ''EOL
makePrisms ''Field
makePrisms ''QuotedData

-- | Valid csv characters
-- >>> [chr 0x20, chr 0x21] ^.. traverse . _TextData
-- [TextData ' ',TextData '!']
-- >>> lengthOf (traverse . _TextData) "#$%&'()*+"
-- 9
-- >>> (0x7e - 0x2d + 1) == lengthOf (traverse . _TextData) (fmap chr [0x2d..0x7e])
-- True
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
    -- TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E
    f c =
      c == ' ' ||
      c == '!' ||
      (c >= '#' && c <= '+') || (c >= '-' && c <= '~')

fieldContent :: Field -> String
fieldContent (Quoted cs) =
  let f (TextDataC td) = _TextData # td
      f Comma'         = ','
      f CR'            = '\r'
      f LF'            = '\n'
      f DoubleQuote    = '"'
  in fmap f cs
fieldContent (Unquoted tds) = fmap (_TextData #) tds

-- | Render CSV
-- Should obey: `renderCSV . parseCSV ≅ id`
renderCsv :: CSV -> String
renderCsv (CSV records) = toList records >>= printRecord
  where
    printRecord (Record (fields :~ _), eol) =
      intercalate "," (toList $ fmap printField fields) <> printEol eol
    printEol CRLF = "\r\n"
    printEol CR   = "\r"
    printEol LF   = "\n"
    printField (Quoted content :~ _) = '"' : (=<<) printQuotedData content <> ['"']
    printField (Unquoted content :~ _) = fmap (_TextData #) content
    printQuotedData (TextDataC textdata) = pure $ _TextData # textdata
    printQuotedData DoubleQuote          = "\"\""
    printQuotedData Comma'               = pure ','
    printQuotedData CR'                  = pure '\r'
    printQuotedData LF'                  = pure '\n'

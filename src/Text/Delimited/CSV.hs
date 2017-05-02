{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}

module Text.Delimited.CSV
  ( CSV
  , csvRows
  , Record(Record)
  , recordAnn
  , recordFields
  , recordEOL
  , EOL
  , _CRLF
  , _CR
  , _LF
  , Field
  , _Quoted
  , _Unquoted
  , makeField
  , QuotedData
  , _TextDataC
  , _Comma'
  , _CR'
  , _LF'
  , _DoubleQuote
  , TextData
  , _TextData
  , renderCsv
  , fieldContent
  ) where

import           Control.Applicative ((<$>), (<|>))
import           Control.Lens
import           Data.CharSet        (CharSet, empty, member, range, singleton,
                                      union)
import           Data.Data
import           Data.List           (intercalate)
import           Data.List.NonEmpty  (NonEmpty, toList)
import           Data.Monoid         ((<>))
import           Prelude             (Char, Eq, Maybe (Just, Nothing), Show,
                                      String, fmap, foldr, pure, ($), (=<<),
                                      (>>=))
import           Text.Trifecta       (Span)

-- https://tools.ietf.org/html/rfc4180#page-2
-- $setup
-- >>> import Data.Char (chr)
-- >>> import Prelude
type CSV = CSV' Span

newtype CSV' ann = CSV
  { _csvRows :: NonEmpty (Record ann)
  } deriving (Eq, Show, Data, Typeable)

data Record ann = Record
  { _recordAnn    :: ann
  , _recordFields :: NonEmpty (ann, Field)
  , _recordEOL    :: EOL
  } deriving (Eq, Show, Data, Typeable)

data EOL
  = CRLF
  | CR
  | LF
  deriving (Eq, Show, Data, Typeable)

data Field
  = Quoted [QuotedData]
  | Unquoted [TextData]
  deriving (Eq, Show, Data, Typeable)

data QuotedData
  = TextDataC TextData
  | Comma'
  | CR'
  | LF'
  | DoubleQuote
  deriving (Eq, Show, Data, Typeable)

newtype TextData =
  TextData Char
  deriving (Eq, Show, Data, Typeable)

makeLenses ''CSV'

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
    -- Valid CSV characters according to:
    -- https://tools.ietf.org/html/rfc4180#page-2
    -- TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E
  where
    f c = member c textDataCS

-- | Can encode a char into quoted data, and decode.
-- This is inappropriate for parsing, as we need to use up 2 double quotes when parsing them. However,
-- it's a useful function when constructing records from scratch.
_QuotedData :: Prism' Char QuotedData
_QuotedData =
  prism'
    (\v ->
       case v of
         TextDataC (TextData c) -> c
         Comma'                 -> ','
         LF'                    -> '\n'
         CR'                    -> '\r'
         DoubleQuote            -> '"')
    (\c -> (TextDataC `fmap` (c ^? _TextData)) <|> checkIt c)
  where
    checkIt ','  = Just Comma'
    checkIt '\n' = Just LF'
    checkIt '\r' = Just CR'
    checkIt '"'  = Just DoubleQuote
    checkIt _    = Nothing

makeField :: String -> Maybe Field
-- TODO: can we do this in one pass?
makeField c = (Unquoted <$> traverse (preview _TextData) c) <|> (Quoted <$> traverse (preview _QuotedData) c)

textDataCS :: CharSet
textDataCS = foldr union empty [singleton ' ', singleton '!', range '#' '+', range '-' '~']

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
renderCsv :: CSV' a -> String
renderCsv (CSV records) = toList records >>= printRecord
  where
    printRecord (Record _ fields eol) = intercalate "," (toList $ fmap printField fields) <> printEol eol
    printEol CRLF = "\r\n"
    printEol CR   = "\r"
    printEol LF   = "\n"
    printField (_, Quoted content) = '"' : (=<<) printQuotedData content <> ['"']
    printField (_, Unquoted content) = fmap (_TextData #) content
    printQuotedData (TextDataC textdata) = pure $ _TextData # textdata
    printQuotedData DoubleQuote          = "\"\""
    printQuotedData Comma'               = pure ','
    printQuotedData CR'                  = pure '\r'
    printQuotedData LF'                  = pure '\n'

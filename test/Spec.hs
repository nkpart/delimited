{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}
import qualified Text.Trifecta as T
import qualified Text.Trifecta.Parser as T
import Control.Monad
import Control.Lens

import Text.Parser.CSV
import Text.Parser.CSV.Record
import Text.Parser.Token

testy =  "age,name\r\n33,nick\r\n"

data P = P Integer String deriving (Eq, Show)

pp :: RecordParser P
pp =
  do age <- field integer
     name <- field (T.manyTill ',')
     pure $ P age name

main :: IO ()
main =
  do let T.Success v = T.parseString fileP mempty testy
     when (length v /= 2) $ error "included trailing line"

     let v = T.parseString ( customFileP pp ) mempty testy
     print v
     pure ()



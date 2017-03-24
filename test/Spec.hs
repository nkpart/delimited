{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -Wwarn #-}
import qualified Text.Trifecta as T
-- import qualified Text.Trifecta.Parser as T
import Control.Lens ()
import Control.Monad
import Control.Applicative.Free
import Text.Delimited.CSV.Parser
import Text.Delimited.CSV
import Text.Trifecta
import Data.List.NonEmpty hiding (length)

testy :: String
testy =  "age,name\r\n23,nick\r\n33,\"mark\"\"\ncuban\"\r\n"

main :: IO ()
main =
  do let T.Success csv@(CSV (_:|rs)) = T.parseString fileP mempty testy
     print rs
     when (printCsv csv /= testy) $ do
       putStrLn (printCsv csv)
       error "print . parse /= id"
     when (length rs /= 2) $ do
       error "record parse count fail"

     pure ()

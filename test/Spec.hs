{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall -Wwarn #-}
import qualified Text.Trifecta as T
-- import qualified Text.Trifecta.Parser as T
import Control.Lens
import Control.Monad
import Text.Delimited.CSV.Parser
import Text.Delimited.CSV
import Text.Trifecta
import Data.List.NonEmpty hiding (length)

testy :: String
testy =  "age,name\r\n23,nick\r\n33,\"mark\"\"\ncuban\"\r\n"

main :: IO ()
main =
  do let v = T.parseString fileP mempty testy
     let (_ :| rs) = v ^?! _Success . csvRows
         csv = v ^?! _Success
     when (renderCsv csv /= testy) $ do
       putStrLn (renderCsv csv)
       error "print . parse /= id"
     when (length rs /= 2) $ do
       error "record parse count fail"

     pure ()

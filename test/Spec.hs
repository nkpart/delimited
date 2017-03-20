{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StandaloneDeriving #-}
import qualified Text.Trifecta as T
-- import qualified Text.Trifecta.Parser as T
import Control.Monad
import Text.Parser.CSV

testy :: String
testy =  "age,name\r\n33,nick\r\n33,\"mark\ncuban\"\r\n"

-- data P = P Integer String deriving (Eq, Show)
-- pp :: RecordParser P
-- pp =
--   do age <- field integer
--      name <- field (T.manyTill T.anyChar (( T.char ',' $> () ) <|> T.eof) T.<?> "xxx")
--      pure $ P age name

-- instance Show ( Ap f a ) where
--   show (Pure _) = "Done"
--   show (Ap _ r) = "Ap:" `mappend` show r

-- pp2 =
--   do age <- field (Just 3)
--      name <- field (Just "five")
--      pure $ P age name

main :: IO ()
main =
  do let T.Success csv@(CSV rs) = T.parseString fileP mempty testy
     when (printCsv csv /= testy) $ do
       putStrLn (printCsv csv)
       error "print . parse /= id"
     when (length rs /= 3) $ do
       error "record parse count fail"
     -- print pp2
     -- let v = T.parseString ( customFileP pp ) mempty testy
     -- print v
     pure ()

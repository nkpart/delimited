{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
module Text.Parser.CSV.Record where

import Text.Parser.CSV
import Text.Parser.Char
import Text.Parser.Token
import Text.Parser.Combinators
import Control.Applicative.Free

data Foo = Foo Integer Integer

type RecordParser t = forall f. TokenParsing f => Ap f t

customFileP
  :: (Monad m, TokenParsing m) =>
     Ap m a ->
     m [a]
customFileP rowP =
  let xxx = runMe rowP
  in
  do _ <- headerP <* crlfP <?> "header"
     sepEndBy1 xxx crlfP <?> "records"

-- | Interleave ',' parsing between sequenced parsers
runMe :: (Monad f, TokenParsing f ) => Ap f a -> f a
runMe (Pure a) = pure a
runMe (Ap fa fa2b) =
  do a <- fa
     case fa2b of
       Pure cont -> pure (cont a)
       Ap fr cont ->
         do _ <- char ','
            r <- fr
            xxx <- runMe cont
            pure $ xxx r a

field :: f a -> Ap f a
field = liftAp

recordP :: CharParsing m => m [String]
recordP =
  sepBy1 fieldP commaP

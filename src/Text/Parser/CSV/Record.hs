{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Text.Parser.CSV.Record where

import Text.Parser.CSV
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta
import Control.Monad (join)
import Control.Applicative.Free

recordsWithHeader
  :: (Monad m, DeltaParsing m, Errable m) =>
     (Record -> Ap Parser a) ->
     m [a]
recordsWithHeader rowP =
  do h <- headerP <* crlfP  <?> "header"
     sepEndBy1 (parseRow (rowP h)) crlfP <?> "records"

parseRow :: (Errable f, DeltaParsing f ) => Ap Parser a -> f a
parseRow ap =
  let xxx = parseWithin (fieldS . (\(a :~ _) -> a) <$> fieldP)
   in interleaveAp (char ',') $ hoistAp xxx ap

-- | Run the second parser on the content parsed by the first
parseWithin :: (Errable m, DeltaParsing m ) => m String -> Parser b -> m b
parseWithin pa pb = join $ slicedWith (\(a :~ _) _ ->
                          case parseString pb mempty a of
                            Success x -> pure x
                            -- TODO: use the span to scope this error?
                            Failure _ -> raiseErr (failed "wat")
              ) (spanned pa)

-- | Interleave an action between layers
interleaveAp :: (Applicative f) => f b -> Ap f a -> f a
interleaveAp _ (Pure a) = pure a
interleaveAp sep (Ap fa fa2b) =
  do case fa2b of
       Pure cont -> cont <$> fa
       Ap fr cont ->
         do a <- fa
            _ <- sep
            r <- fr
            f <- interleaveAp sep cont
            pure $ f r a

field :: f a -> Ap f a
field = liftAp

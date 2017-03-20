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
  do h <- headerP <* crlfP <?> "header"
     rs <- sepEndBy1 xxx crlfP <?> "records"
     pure rs
     -- TODO: this is a hack to deal with a trailing newline
     -- let rs' = reverse rs
     -- case rs' of
     --   ([""]: rest) -> pure (h: reverse rest)
     --   _            -> pure (h: reverse rs)

-- | Interleave ',' parsing between sequenced parsers
runMe :: (Monad f, TokenParsing f ) => Ap f a -> f a
runMe (Pure a) = pure a
runMe (Ap fa fa2b) =
  do a <- fa
     case fa2b of
       Pure cont -> pure (cont a)
       x@(Ap fr cont) ->
         do char ','
            r <- fr
            xxx <- runMe cont
            pure $ xxx r a

field :: f a -> Ap f a
field fp = liftAp fp

fooooP =
  do a <- field $ integer
     b <- field $ integer
     pure $ Foo a b

recordP :: CharParsing m => m [String]
recordP =
  sepBy1 fieldP commaP

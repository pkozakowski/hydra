module Parser where

import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Class
import Data.Maybe
import Data.Time
import Market.Types
import Numeric.Algebra
import Numeric.Field.Fraction
import Numeric.Truncatable
import Options.Applicative hiding (Parser, (<|>))
import Prelude hiding ((*))
import Text.Parsec hiding (option)
import Text.Parsec.Char hiding (Parser)
import Text.Parsec.Language
import Text.Parsec.String hiding (Parser)
import qualified Text.Parsec.Token as P
import qualified Text.Parsec as Parsec
import Text.Read

import Types

-- Parsec parsers.

type Parser a = Parsec String () a

float :: Parser Double
float = P.float haskell

lexeme :: Parser a -> Parser a
lexeme = P.lexeme haskell

symbol :: String -> Parser String
symbol = P.symbol haskell

identifier :: Parser String
identifier = P.identifier haskell

someAmount :: Parser SomeAmount
someAmount = flip (,)
    <$> fmap (Amount . realToFraction) float
    <*> fmap Asset (lexeme $ some upper)

parsecReader :: Parsec String () a -> ReadM a
parsecReader parser = eitherReader $ first show . parse parser ""

-- Options.Applicative parsers.

date :: ReadM UTCTime
date
    = maybeReader
    $ parseTimeM True defaultTimeLocale "%Y-%-m-%-d"

portfolio :: ReadM Portfolio
portfolio
    = parsecReader
    $ fromList <$> someAmount `sepBy1` symbol "+" where
        symbol = P.symbol haskell

fees :: ReadM Fees
fees = parsecReader $ Fees
    <$> (fmap ((1 % 100 *) . realToFraction) float <* symbol "%")
    <*> (symbol "+" *> fmap Just someAmount)

periodP :: Parser Period
periodP
    = pure Hourly  <* symbol "hourly"
  <|> pure Daily   <* symbol "daily"
  <|> pure Monthly <* symbol "monthly"

metrics :: ReadM (NonEmpty Metric)
metrics = parsecReader $ fromJust . nonEmpty <$> metric `sepBy` symbol "," where
    metric = Metric <$> periodP <*> identifier

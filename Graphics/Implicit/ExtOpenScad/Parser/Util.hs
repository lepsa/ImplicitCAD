-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016 Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.Implicit.ExtOpenScad.Parser.Util ((*<|>), (?:), tryMany, patternMatcher, sourcePosition, number, variable, boolean, scadString, scadUndefined) where

import Prelude (String, ($), fmap, (.), return, Bool(True, False), (*), (<$>), (<$), (<*>), Foldable, Functor, Maybe (Nothing, Just), flip, notElem, foldr, Integral, (+), fromIntegral, RealFloat, Integer, Int, (-), Num, negate, id, pure)
import qualified Data.Scientific as Sci
import Data.Foldable (fold, foldl')

import Text.Megaparsec (SourcePos, (<|>), (<?>), try, sepBy, option, between, MonadParsec, many, takeWhileP, unPos, takeWhile1P, chunkToTokens)

import qualified Text.Megaparsec as P (sourceLine, sourceColumn, sourceName)
import Control.Applicative (empty)
import Text.Megaparsec.Char (char, string, char')
import Text.Megaparsec.Char.Lexer (lexeme)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Char (isDigit, digitToInt)

import Graphics.Implicit.ExtOpenScad.Definitions (Pattern(Wild, Name, ListP), SourcePosition(SourcePosition), Symbol(Symbol), Expr(LitE, Var), OVal(ONum, OString, OBool, OUndefined))

import Graphics.Implicit.Definitions (toFastℕ)

-- The lexer.
import Graphics.Implicit.ExtOpenScad.Parser.Lexer (matchIdentifier, matchTok, matchUndef, matchTrue, matchFalse, surroundedBy, matchComma)

import Data.Functor (($>), void)

-- TODO
-- try forces the parser to keep extra state hanging around, and perform extra
-- back tracking when an error occurs. If we can, we should refactor the parser
-- to avoid the use of try where possible.
infixr 1 *<|>
(*<|>) :: MonadParsec e s m => m a -> m a -> m a
a *<|> b = try a <|> b

infixr 2 ?:
(?:) :: MonadParsec e s m => String -> m a -> m a
l ?: p = p <?> l

tryMany :: (Functor f, Foldable f, MonadParsec e s m) => f (m a) -> m a
tryMany = foldr (<|>) empty . fmap try

-- | A pattern parser
patternMatcher :: MonadParsec e Text m => m Pattern
patternMatcher = "pattern" ?:
          (Wild <$ matchTok '_')
      <|> (Name . Symbol <$> matchIdentifier)
      <|> (ListP <$> surroundedBy '[' (patternMatcher `sepBy` matchComma) ']' )

-- expression parsers

-- | Parse a number.
number :: MonadParsec e Text m => m Expr
number = ("number" ?:) $ LitE . ONum <$> float
{-
  h <- ("0" <>) <$> decimal
    <|> (<>) <$> some digitChar <*> option "" decimal 
  d <- option "0" $ oneOf "eE" *> exponent
  _ <- whiteSpace
  return . LitE $ ONum $ if d == "0"
                         then read h
                         else read h * (10 ** read d)
  where
    decimal = (:) <$> char '.' <*> some digitChar
    sign = string "-" <|> "" <$ optional (string "+")
    exponent = (<>) <$> sign <*> some digitChar
-}


data SP = SP !Integer {-# UNPACK #-} !Int

float :: (MonadParsec e Text m, RealFloat a) => m a
float = do
  c' <- decimalOptional0_
  Sci.toRealFloat <$>
    ((do SP c e' <- dotDecimal_ (Proxy :: Proxy Text) c'
         e       <- option e' (try $ exponent_ e')
         return (Sci.scientific c e))
     <|> (Sci.scientific c' <$> exponent_ 0))
{-# INLINEABLE float #-}

decimalOptional0_
  :: (MonadParsec e Text m, Integral a)
  => m a
decimalOptional0_ = mkNum <$> takeWhile1P (Just "digit") isDigit
                          <|> pure 0
  where
    mkNum    = foldl' step 0 . chunkToTokens (Proxy :: Proxy Text)
    step a c = a * 10 + fromIntegral (digitToInt c)
{-# INLINE decimalOptional0_ #-}

decimal_
  :: (MonadParsec e Text m, Integral a)
  => m a
decimal_ = mkNum <$> takeWhile1P (Just "digit") isDigit 
  where
    mkNum    = foldl' step 0 . chunkToTokens (Proxy :: Proxy Text)
    step a c = a * 10 + fromIntegral (digitToInt c)
{-# INLINE decimal_ #-}

dotDecimal_ :: (MonadParsec e Text m)
  => Proxy Text
  -> Integer
  -> m SP
dotDecimal_ pxy c' = do
  void $ char '.'
  let mkNum    = foldl' step (SP c' 0) . chunkToTokens pxy
      step (SP a e') c = SP
        (a * 10 + fromIntegral (digitToInt c))
        (e' - 1)
  mkNum <$> takeWhile1P (Just "digit") isDigit
{-# INLINE dotDecimal_ #-}

exponent_ :: (MonadParsec e Text m)
  => Int
  -> m Int
exponent_ e' = do
  void $ char' 'e'
  (+ e') <$> signed (return ()) decimal_
{-# INLINE exponent_ #-}

signed :: (MonadParsec e Text m, Num a)
  => m ()              -- ^ How to consume white space after the sign
  -> m a               -- ^ How to parse the number itself
  -> m a               -- ^ Parser for signed numbers
signed spc p = option id (lexeme spc sign) <*> p
  where
    sign = (id <$ char '+') <|> (negate <$ char '-')
{-# INLINEABLE signed #-}

-- | Parse a variable reference.
--   NOTE: abused by the parser for function calls.
variable :: MonadParsec e Text m => m Expr
variable = "variable" ?:
  Var . Symbol <$> matchIdentifier

-- | Parse a true or false value.
boolean :: MonadParsec e Text m => m Expr
boolean = "boolean" ?:
  LitE . OBool <$> (matchTrue $> True <|> matchFalse $> False)

-- | Parse a quoted string.
--   FIXME: no \u unicode support?
scadString :: MonadParsec e Text m => m Expr
scadString = "string" ?: LitE . OString <$>
    between
      (char '"')
      (matchTok '"')
      (fmap fold . many $
        (string "\\\"" $> "\"") <|>
        (string "\\n"  $> "\n") <|>
        (string "\\r"  $> "\r") <|>
        (string "\\t"  $> "\t") <|>
        (string "\\\\" $> "\\") <|>
        takeWhileP Nothing (flip notElem ['\"', '\n'])
      )

scadUndefined :: MonadParsec e Text m => m Expr
scadUndefined = "undefined" ?:
  LitE OUndefined <$ matchUndef

sourcePosition :: SourcePos -> SourcePosition
sourcePosition pos = SourcePosition (toFastℕ . unPos $ P.sourceLine pos) (toFastℕ . unPos $ P.sourceColumn pos) (P.sourceName pos)

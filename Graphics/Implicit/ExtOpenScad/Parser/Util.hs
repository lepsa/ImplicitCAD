-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016 Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
module Graphics.Implicit.ExtOpenScad.Parser.Util
  ( (*<|>)
  , (?:)
  , tryMany
  , patternMatcher
  , sourcePosition
  , number
  , variable
  , boolean
  , scadString
  , scadUndefined
  ) where

import           Control.Applicative                        ((<**>), (<|>))
import           Data.Char                                  (digitToInt)
import           Data.Foldable                              (foldl')
import           Data.Functor                               (($>))
import qualified Data.Scientific                            as Sci
import           Graphics.Implicit.Definitions              (toFastℕ)
import           Graphics.Implicit.ExtOpenScad.Definitions  (Expr (LitE, Var), OVal (OBool, ONum, OString, OUndefined),
                                                             Pattern (ListP, Name, Wild),
                                                             SourcePosition (SourcePosition),
                                                             Symbol (Symbol))
import           Graphics.Implicit.ExtOpenScad.Parser.Lexer (matchFalse,
                                                             matchIdentifier,
                                                             matchTok,
                                                             matchTrue,
                                                             matchUndef)
import           Prelude                                    (Char, String,
                                                             either, fmap,
                                                             foldl1,
                                                             fromInteger,
                                                             fromIntegral, id,
                                                             map, negate, pure,
                                                             ($), (*), (*>),
                                                             (+), (-), (.),
                                                             (<$), (<$>), (<*),
                                                             (<*>))
import           Text.Parsec                                (SourcePos,
                                                             sourceColumn,
                                                             sourceLine,
                                                             sourceName)
import           Text.Parsec.String                         (GenParser)
import           Text.Parser.Char                           (char, digit,
                                                             noneOf, oneOf,
                                                             string)
import           Text.Parser.Combinators                    (Parsing, between,
                                                             choice, many,
                                                             option, sepBy,
                                                             some, try, (<?>))
import           Text.Parser.Token                          (decimal, highlight,
                                                             integerOrDouble,
                                                             whiteSpace)
import           Text.Parser.Token.Highlight                (Highlight (Operator))

infixr 1 *<|>
(*<|>) :: Parsing m => m a -> m a -> m a
a *<|> b = try a <|> b

infixr 2 ?:
(?:) :: Parsing m => String -> m a -> m a
l ?: p = p <?> l

tryMany :: Parsing m => [m a] -> m a
tryMany = foldl1 (<|>) . map try

-- | A pattern parser
patternMatcher :: GenParser Char st Pattern
patternMatcher =
    (Wild <$ char '_') <|>
    (Name . Symbol <$> matchIdentifier) <|>
    (ListP <$> between (matchTok '[') (matchTok ']') (patternMatcher `sepBy` try (matchTok ',')))

-- expression parsers

-- | Parse a number.
-- number :: TokenParsing m => m Expr
-- number = "number" ?: LitE . ONum <$> double
number :: GenParser Char st Expr
number = ("number" ?:) $
  -- Very similar to `double` from parsers, but can handle an implicit leading 0
  LitE . ONum <$> choice
    [ either fromIntegral id <$> integerOrDouble
    , fmap Sci.toRealFloat $ pure 0 <**> fractExponent
    ]
  <* whiteSpace
  where
    sign = highlight Operator $ negate <$ char '-' <|> id <$ char '+' <|> pure id
    fractExponent = (\fract expo n -> (fromInteger n + fract) * expo) <$> fraction <*> option 1 exponent'
                <|> (\expo n -> fromInteger n * expo) <$> exponent'
      where
        fraction = foldl' op 0 <$> (char '.' *> (some digit <?> "fraction"))
        op f d = f + Sci.scientific (fromIntegral (digitToInt d)) (Sci.base10Exponent f - 1)
        exponent' = ((\f e -> power (f e)) <$ oneOf "eE" <*> sign <*> (decimal <?> "exponent")) <?> "exponent"
        power = Sci.scientific 1 . fromInteger


-- | Parse a variable reference.
--   NOTE: abused by the parser for function calls.
variable :: GenParser Char st Expr
variable = "variable" ?: Var . Symbol <$> matchIdentifier

-- | Parse a true or false value.
boolean :: GenParser Char st Expr
boolean = "boolean" ?: LitE . OBool <$> (matchTrue *<|> matchFalse)

-- | Parse a quoted string.
--   FIXME: no \u unicode support?
scadString :: GenParser Char st Expr
scadString = "string" ?: LitE . OString <$>
  between
  (char '"')
  (matchTok '"')
  (many $
    (string "\\\"" $> '\"') *<|>
    (string "\\n"  $> '\n') *<|>
    (string "\\r"  $> '\r') *<|>
    (string "\\t"  $> '\t') *<|>
    (string "\\\\" $> '\\') *<|>
     noneOf "\"\n"
  )

scadUndefined :: GenParser Char st Expr
scadUndefined = "undefined" ?: LitE OUndefined <$ matchUndef

sourcePosition :: SourcePos -> SourcePosition
sourcePosition pos = SourcePosition (toFastℕ $ sourceLine pos) (toFastℕ $ sourceColumn pos) $ sourceName pos

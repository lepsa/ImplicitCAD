-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Kelvin Cookshaw (kelvin@cookshaw.com)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ExtOpenScad.Parser.Lexer (whiteSpace, matchTrue, matchFalse, matchFunction, matchInclude, matchUse, matchEcho, matchIf, matchElse, matchFor, matchModule, matchLet, matchUndef, matchTok, matchColon, matchSemi, matchComma, matchIdentifier, surroundedBy, matchLT, matchLE, matchGT, matchGE, matchEQ, matchNE, matchCAT, matchOR, matchAND, matchEach) where

import           Control.Monad.Identity  (Identity)
import           Data.Functor            (void)
import           Prelude                 (Bool (False, True), Char, String,
                                          return, ($), (.), (<$), (>>))
import           Text.Parsec             (between, char)
import           Text.Parsec.Char        (noneOf)
import           Text.Parsec.Language    (GenLanguageDef, emptyDef)
import           Text.Parsec.String      (GenParser)
import           Text.Parsec.Token       (GenTokenParser, caseSensitive, colon,
                                          comma, commentEnd, commentLine,
                                          commentStart, identLetter, identStart,
                                          makeTokenParser, nestedComments,
                                          reservedNames, reservedOpNames, semi)
import qualified Text.Parsec.Token       as Pr
import qualified Text.Parser.Char        as P
import qualified Text.Parser.Combinators as P
import qualified Text.Parser.Token       as P

-- The definition of openscad used by parsec.
openScadStyle :: GenLanguageDef String u0 Identity
openScadStyle
    = emptyDef
    { commentStart = "/*"
    , commentEnd = "*/"
    , commentLine = "//"
    , nestedComments = True
    , identStart =  noneOf " ,|[]{}()+-*&^%#@!~`'\"\\/;:.,<>?=1234567890"
    , identLetter = noneOf " ,|[]{}()+-*&^%#@!~`'\"\\/;:.,<>?="
    , reservedNames = ["module", "function", "if", "else", "let", "for", "each", "true", "false", "undef", "include", "use"]
    , reservedOpNames= ["<=", ">=", "==", "!=", "&&", "||"]
    , caseSensitive = True
    }

lexer :: GenTokenParser String st Identity
lexer = makeTokenParser openScadStyle

-- | Consume whitespace.

whiteSpace_ :: P.TokenParsing m => m ()
whiteSpace_ = P.whiteSpace

whiteSpace :: GenParser Char st ()
whiteSpace = Pr.whiteSpace lexer

matchTrue_ :: P.TokenParsing m => m Bool
matchTrue_ = True <$ P.symbol "true"

-- | Match boolean true.
matchTrue :: GenParser Char st Bool
matchTrue = True <$ Pr.reserved lexer "true"

-- | Match boolean false
matchFalse :: GenParser Char st Bool
matchFalse = False <$ Pr.reserved lexer "false"

matchFalse_ :: P.TokenParsing m => m Bool
matchFalse_ = False <$ P.symbol "false"

-- | Match the function keyword.
matchFunction :: GenParser Char st ()
matchFunction = Pr.reserved lexer "function"

matchFunction_ :: P.TokenParsing m => m ()
matchFunction_ = void $ P.symbol "function"

-- | Match the include keyword.
matchInclude :: GenParser Char st ()
matchInclude = Pr.reserved lexer "include"

matchInclude_ :: P.TokenParsing m => m ()
matchInclude_ = void $ P.symbol "include"

-- | Match the use keyword.
matchUse :: GenParser Char st ()
matchUse = Pr.reserved lexer "use"

matchUse_ :: P.TokenParsing m => m ()
matchUse_ = void $ P.symbol "use"

-- | Match the echo keyword.
matchEcho :: GenParser Char st ()
matchEcho = Pr.reserved lexer "echo"

-- | Match the if keyword.
matchIf :: GenParser Char st ()
matchIf = Pr.reserved lexer "if"

-- | Match the else keyword.
matchElse :: GenParser Char st ()
matchElse = Pr.reserved lexer "else"

-- | Match the for keyword.
matchFor :: GenParser Char st ()
matchFor = Pr.reserved lexer "for"

-- | Match the module keyword.
matchModule :: GenParser Char st ()
matchModule = Pr.reserved lexer "module"

-- | Match the let keyword.
matchLet :: GenParser Char st ()
matchLet = Pr.reserved lexer "let"

-- | Match the undef keyword.
matchUndef :: GenParser Char st ()
matchUndef = Pr.reserved lexer "undef"

-- | Match the each keyword.
matchEach :: GenParser Char st ()
matchEach = Pr.reserved lexer "each"

-- | match a single character token followed by whitespace.
matchTok :: Char -> GenParser Char st String
matchTok x = do
  y <- char x
  _ <- Pr.whiteSpace lexer
  return [y]
--matchTok tok = lexeme lexer $ symbol lexer [tok]

-- | match a colon.
matchColon :: GenParser Char st String
matchColon = colon lexer

-- | match a semicolon.
matchSemi :: GenParser Char st String
matchSemi = semi lexer

-- | match a comma.
matchComma :: GenParser Char st String
matchComma = comma lexer

-- | Match operators.
matchLE :: GenParser Char st String
matchLE = Pr.reservedOp lexer "<=" >> return "<="
matchLT :: GenParser Char st String
matchLT = matchTok '<'
matchGE :: GenParser Char st String
matchGE = Pr.reservedOp lexer ">=" >> return ">="
matchGT :: GenParser Char st String
matchGT = matchTok '>'
matchEQ :: GenParser Char st String
matchEQ = Pr.reservedOp lexer "==" >> return "=="
matchNE :: GenParser Char st String
matchNE = Pr.reservedOp lexer "!=" >> return "!="
matchAND :: GenParser Char st String
matchAND = Pr.reservedOp lexer "&&" >> return "&&"
matchOR :: GenParser Char st String
matchOR = Pr.reservedOp lexer "||" >> return "||"
matchCAT :: GenParser Char st String
matchCAT = Pr.reservedOp lexer "++" >> return "++"


-- | match something between two ends.
surroundedBy :: Char -> GenParser Char st a -> Char -> GenParser Char st a
surroundedBy leftTok middle rightTok = between (matchTok leftTok) (matchTok rightTok) middle

matchIdentifier :: GenParser Char st String
matchIdentifier = Pr.identifier lexer

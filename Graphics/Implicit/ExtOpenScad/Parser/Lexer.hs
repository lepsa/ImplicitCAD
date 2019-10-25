-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Kelvin Cookshaw (kelvin@cookshaw.com)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Implicit.ExtOpenScad.Parser.Lexer (whiteSpace, matchTrue, matchFalse, matchFunction, matchInclude, matchUse, matchIf, matchElse, matchModule, matchLet, matchUndef, matchTok, matchColon, matchSemi, matchComma, matchIdentifier, surroundedBy, matchLT, matchLE, matchGT, matchGE, matchEQ, matchNE, matchCAT, matchOR, matchAND, matchEach, symbol) where

import Prelude (Char, (*>), (<*), ($), (.), (<>), (<$>), (<*>), pure, flip, notElem)
import Text.Megaparsec (satisfy, MonadParsec, noneOf, takeWhileP)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char (isSpace)
import Data.Text (Text, singleton)
import Data.Functor (void)

-- | Consume whitespace.
whiteSpace :: MonadParsec e Text m => m ()
whiteSpace = L.space
  (void $ satisfy isSpace)
  (L.skipLineComment "//")
  (L.skipBlockCommentNested "/*" "*/")

symbol :: MonadParsec e Text m => Text -> m Text
symbol = L.symbol whiteSpace

symbol_ :: MonadParsec e Text m => Text -> m ()
symbol_ = void . symbol


-- | Match boolean true.
matchTrue :: MonadParsec e Text m => m ()
matchTrue = symbol_ "true"

-- | Match boolean false
matchFalse :: MonadParsec e Text m => m ()
matchFalse = symbol_ "false"

-- | Match the function keyword.
matchFunction :: MonadParsec e Text m => m ()
matchFunction = symbol_ "function"

-- | Match the include keyword.
matchInclude :: MonadParsec e Text m => m ()
matchInclude = symbol_ "include"

-- | Match the use keyword.
matchUse :: MonadParsec e Text m => m ()
matchUse = symbol_ "use"

-- | Match the if keyword.
matchIf :: MonadParsec e Text m => m ()
matchIf = symbol_ "if"

-- | Match the else keyword.
matchElse :: MonadParsec e Text m => m ()
matchElse = symbol_ "else"

-- | Match the module keyword.
matchModule :: MonadParsec e Text m => m ()
matchModule = symbol_ "module"
-- | Match the let keyword.
matchLet :: MonadParsec e Text m => m ()
matchLet = symbol_ "let"

-- | Match the undef keyword.
matchUndef :: MonadParsec e Text m => m ()
matchUndef = symbol_ "undef"

-- | Match the each keyword.
matchEach :: MonadParsec e Text m => m ()
matchEach = symbol_ "each"

-- | match a single character token followed by whitespace.
matchTok :: MonadParsec e Text m => Char -> m Text
matchTok = symbol . singleton
--matchTok tok = lexeme lexer $ symbol lexer [tok]

-- | match a colon.
matchColon :: MonadParsec e Text m => m Text 
matchColon = symbol ":"

-- | match a semicolon.
matchSemi :: MonadParsec e Text m => m Text 
matchSemi = symbol ";"

-- | match a comma.
matchComma :: MonadParsec e Text m => m Text 
matchComma = symbol ","

-- | Match operators.
matchLE :: MonadParsec e Text m => m Text
matchLE = symbol "<="
matchLT :: MonadParsec e Text m => m Text
matchLT = symbol "<"
matchGE :: MonadParsec e Text m => m Text
matchGE = symbol ">="
matchGT :: MonadParsec e Text m => m Text
matchGT = symbol ">"
matchEQ :: MonadParsec e Text m => m Text
matchEQ = symbol "=="
matchNE :: MonadParsec e Text m => m Text
matchNE = symbol "!="
matchAND :: MonadParsec e Text m => m Text
matchAND = symbol "&&"
matchOR :: MonadParsec e Text m => m Text
matchOR = symbol "||"
matchCAT :: MonadParsec e Text m => m Text
matchCAT = symbol "++"


-- | match something between two ends.
surroundedBy :: MonadParsec e Text m => Char -> m a -> Char -> m a
surroundedBy leftTok middle rightTok = matchTok leftTok *> middle <* matchTok rightTok

-- | match an identifier. variable name, function name, module name, etc.
matchIdentifier :: MonadParsec e Text m => m Text 
matchIdentifier = L.lexeme whiteSpace $ (<>)
  <$> (singleton <$> noneOf identStart)
  <*> takeWhileP (pure "identLetter") (flip notElem identLetter)
  where
    identStart :: [Char]
    identStart = " ,|[]{}()+-*&^%#@!~`'\"\\/;:.,<>?=1234567890"
    identLetter :: [Char]
    identLetter = " ,|[]{}()+-*&^%#@!~`'\"\\/;:.,<>?="

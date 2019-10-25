-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use a shorter form of Name.
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

-- The entry point for parsing an ExtOpenScad program.
module Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram) where

import Prelude(Char, Either, String, return, ($), (*>), Bool(False, True), (<$>), (<*>), (.), (<$), flip, fmap, filter, not, pure, notElem)

import Data.Maybe(Maybe(Nothing))

import Graphics.Implicit.ExtOpenScad.Definitions (Statement(DoNothing, NewModule, Include, If, ModuleCall, (:=)), Expr(LamE), StatementI(StatementI), Symbol(Symbol), SourcePosition)

import qualified Graphics.Implicit.ExtOpenScad.Definitions as GIED (Pattern(Name))

import Graphics.Implicit.ExtOpenScad.Parser.Util ((*<|>), patternMatcher, sourcePosition)

-- the top level of the expression parser.
import Graphics.Implicit.ExtOpenScad.Parser.Expr (expr0)

-- The lexer.
import Graphics.Implicit.ExtOpenScad.Parser.Lexer (whiteSpace, matchFunction, matchInclude, matchUse, matchIf, matchElse, matchModule, matchTok, matchComma, matchSemi, matchIdentifier)

-- We use parsec to parse.
import Text.Megaparsec ((<?>), sepBy, parse, eof, many, option, between, optional, MonadParsec, ParseErrorBundle, takeWhileP, getSourcePos)
import Text.Megaparsec.Char (char)
import Data.Text (Text)
import Control.Applicative ((<*), (<|>))
import Data.Void (Void)

import Data.Functor (($>))

-- Let us use the old syntax when defining Names.
pattern Name :: Text -> GIED.Pattern
pattern Name n = GIED.Name (Symbol n)

-- | all of the token parsers are lexemes which consume all trailing spaces nicely.
-- | This leaves us to deal only with the first spaces in the file.
parseProgram :: String -> Text -> Either (ParseErrorBundle Text Void) [StatementI]
parseProgram = parse $ removeNoOps <$> (whiteSpace *> many (computationA1) <* eof)


-- | A computable block of code in our openscad-like programming language.
computationA1 :: MonadParsec e Text m => m StatementI
computationA1 = computationA2 <|> throwAway

computationA2 :: MonadParsec e Text m => m StatementI
computationA2 =
  -- suite statements: no semicolon...
      userModule
  <|> ifStatementI
  <|> userModuleDeclaration
  -- Non suite statements. Semicolon needed...
  <|> (include <|> function) <* matchSemi
  *<|> assignment <* matchSemi

-- | A suite of s!
--   What's a suite? Consider:
--
--      union() {
--         sphere(3);
--      }
--
--  The suite was in the braces ({}). Similarily, the
--  following has the same suite:
--
--      union() sphere(3);
--
--  We consider it to be a list of computables which
--  are in turn StatementI s.
suite :: MonadParsec e Text m => m [StatementI]
suite = removeNoOps <$> computations <?> "suite"
  where
    computations =
      pure <$> computationA1
      <|> braces (many computationA1)

braces :: MonadParsec e Text m => m a -> m a
braces = between (matchTok '{') (matchTok '}')

-- | Every StatementI requires a source position, thus we can build a combinator.
statementI :: MonadParsec e Text m => m (Statement StatementI) -> m StatementI
statementI p = StatementI <$> sourcePos <*> p

-- | Commenting out a computation: use % or * before the statement, and it will not be run.
throwAway :: MonadParsec e Text m => m StatementI
throwAway = statementI $ DoNothing <$ p <* whiteSpace <* computationA2
  where
    p = char '%' <|> char '*'

-- | An include! Basically, inject another extopenscad file here...
include :: MonadParsec e Text m => m StatementI
include = statementI p <?> "include/use"
  where
    p :: MonadParsec e Text m => m (Statement StatementI)
    p = flip Include
      <$> (matchInclude $> True <|> matchUse $> False)
      -- FIXME: better definition of valid filename characters.
      <*> between (char '<') (matchTok '>') (takeWhileP Nothing (flip notElem l))
    l :: [Char]
    l = "<> "

-- | An assignment (parser)
assignment :: MonadParsec e Text m => m StatementI
assignment = statementI p <?> "assignment"
  where
    p :: MonadParsec e Text m => m (Statement StatementI)
    p = (:=) <$> patternMatcher <* matchTok '=' <*> expr0

-- | A function declaration (parser)
function :: MonadParsec e Text m => m StatementI
function = statementI p <?> "function"
  where
    p :: MonadParsec e Text m => m (Statement StatementI)
    p = (:=) <$> lval <*> rval
    lval :: MonadParsec e Text m => m GIED.Pattern
    lval = Name <$> (matchFunction *> matchIdentifier)
    rval :: MonadParsec e Text m => m Expr
    rval = LamE <$> parens (sepBy patternMatcher matchComma) <*> (matchTok '=' *> expr0)

-- | An if statement (parser)
ifStatementI :: MonadParsec e Text m => m StatementI
ifStatementI = statementI p <?> "if"
  where
    p :: MonadParsec e Text m => m (Statement StatementI)
    p = If <$> (matchIf *> parens expr0) <*> suite <*> option [] (matchElse *> suite)

-- | parse a call to a module.
userModule :: MonadParsec e Text m => m StatementI
userModule = statementI p <?> "module call"
  where
    p :: MonadParsec e Text m => m (Statement StatementI)
    p = ModuleCall <$> fmap Symbol matchIdentifier <*> moduleArgsUnit <*> (suite <|> matchSemi $> [])

-- | declare a module.
userModuleDeclaration :: MonadParsec e Text m => m StatementI
userModuleDeclaration = statementI p <?> "module declaration"
  where
    p :: MonadParsec e Text m => m (Statement StatementI)
    p = NewModule <$> fmap Symbol (matchModule *> matchIdentifier) <*> moduleArgsUnitDecl <*> suite

-- | parse the arguments passed to a module.
moduleArgsUnit :: MonadParsec e Text m => m [(Maybe Symbol, Expr)]
moduleArgsUnit = parens $
  sepBy (arg *<|> args *<|> constant) matchComma
  where
    -- eg. a = 12
    arg = do
      symb <- pure . Symbol <$> matchIdentifier
      expr <- matchTok '=' *> expr0
      pure (symb, expr)
    -- eg. a(x,y) = 12
    args = do
      symb    <- pure . Symbol <$> matchIdentifier
      argVars <- parens $ sepBy matchIdentifier matchComma
      expr    <- matchTok '=' *> expr0
      return (symb, LamE (Name <$> argVars) expr)
    -- eg. 12
    constant = (Nothing,) <$> expr0

parens :: MonadParsec e Text m => m a -> m a
parens = between (matchTok '(') (matchTok ')')

-- | parse the arguments in the module declaration.
moduleArgsUnitDecl :: MonadParsec e Text m => m [(Symbol, Maybe Expr)]
moduleArgsUnitDecl = parens $ sepBy arg matchComma
  where
    arg = (,) 
      <$> fmap Symbol matchIdentifier
      <*> optional (matchTok '=' *> expr0)

-- | Find the source position. Used when generating errors.
sourcePos :: MonadParsec e Text m => m SourcePosition
sourcePos = sourcePosition <$> getSourcePos

isNoOp :: StatementI -> Bool
isNoOp (StatementI _ DoNothing) = True
isNoOp _                        = False

-- | Remove statements that do nothing.
removeNoOps :: [StatementI] -> [StatementI]
removeNoOps = filter $ not . isNoOp

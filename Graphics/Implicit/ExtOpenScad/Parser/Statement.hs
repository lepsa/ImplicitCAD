-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}

-- The entry point for parsing an ExtOpenScad program.
module Graphics.Implicit.ExtOpenScad.Parser.Statement
  ( parseProgram
  ) where

import           Control.Applicative                        ((<*), (<|>))
import           Control.Monad.Identity                     (Identity)
import           Data.Functor                               (void, ($>))
import           Data.Maybe                                 (Maybe (Nothing))
import           Graphics.Implicit.ExtOpenScad.Definitions  (Expr (LamE),
                                                             Pattern (Name),
                                                             SourcePosition,
                                                             Statement ((:=), DoNothing, Echo, For, If, Include, ModuleCall, NewModule),
                                                             StatementI (StatementI),
                                                             Symbol (Symbol))
import           Graphics.Implicit.ExtOpenScad.Parser.Expr  (expr0)
import           Graphics.Implicit.ExtOpenScad.Parser.Lexer (matchComma,
                                                             matchEcho,
                                                             matchElse,
                                                             matchFor,
                                                             matchFunction,
                                                             matchIdentifier,
                                                             matchIf,
                                                             matchInclude,
                                                             matchModule,
                                                             matchTok, matchUse,
                                                             surroundedBy,
                                                             whiteSpace)
import           Graphics.Implicit.ExtOpenScad.Parser.Util  (patternMatcher,
                                                             sourcePosition,
                                                             tryMany, (*<|>))
import           Prelude                                    (Bool (False, True),
                                                             Char, Either,
                                                             String, filter,
                                                             flip, fmap, not,
                                                             pure, ($), (*>),
                                                             (.), (<$), (<$>),
                                                             (<*>))
import           Text.Parsec                                (ParseError,
                                                             ParsecT,
                                                             SourceName,
                                                             getPosition, parse)
import           Text.Parsec.String                         (GenParser)
import           Text.Parser.Char                           (char, noneOf,
                                                             oneOf)
import           Text.Parser.Combinators                    (Parsing, between,
                                                             eof, many, option,
                                                             sepBy, try, (<?>))

-- all of the token parsers are lexemes which consume all trailing spaces nicely.
-- This leaves us to deal only with the first spaces in the file.
parseProgram :: SourceName -> String -> Either ParseError [StatementI]
parseProgram = parse $ removeNoOps <$> program
  where
    program = whiteSpace *> many computationOrComment <* eof


-- | A block of code in our openscad-like programming language.
computationOrComment :: ParsecT String st Identity StatementI
computationOrComment = computation *<|> throwAway

-- | A computable block of code in our openscad-like programming language.
computation :: ParsecT String st Identity StatementI
computation =
    -- suite statements: no semicolon...
      tryMany
        [ ifStatementI
        , forStatementI
        , userModuleDeclaration
        ]
    *<|> -- Non suite statements. Semicolon needed...
      tryMany
        [ echo
        , include -- also handles use
        , function
        , assignment
        ]
      <* matchTok ';'
    *<|> -- Modules. no semicolon...
      userModule

{-
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
-}
suite :: GenParser Char st [StatementI]
suite = p <?> " suite"
  where
    p = pure <$> computationOrComment <|>
        removeNoOps <$> surroundedBy '{' (many computationOrComment) '}'


-- Every StatementI requires a source position, thus we can build a combinator.
statementI :: ParsecT s u Identity (Statement StatementI)-> ParsecT s u Identity StatementI
statementI p = StatementI <$> sourcePos <*> p

-- | commenting out a computation: use % or * before the statement, and it will not be run.
throwAway :: GenParser Char st StatementI
throwAway = statementI $ DoNothing <$ oneOf "%*" <* whiteSpace <* computation

-- | An include! Basically, inject another extopenscad file here...
include :: GenParser Char st StatementI
include = statementI p <?> "include "
  where
    p = flip Include
      <$> (matchInclude $> True <|> matchUse $> False)
      -- FIXME: better definition of valid filename characters.
      <*> between (char '<') (matchTok '>') (many $ noneOf "<> ")

-- | An assignment (parser)
assignment :: GenParser Char st StatementI
assignment = statementI p <?> "assignment "
  where
    p = (:=)
     <$> patternMatcher <* matchTok '='
     <*> expr0

-- | A function declaration (parser)
function :: GenParser Char st StatementI
function = statementI p <?> "function "
  where
    name = do
      void matchFunction
      Name . Symbol <$> matchIdentifier
    lam = LamE
      <$> between (matchTok '(') (matchTok ')') (sepBy patternMatcher (try matchComma))
      <*> (matchTok '=' *> expr0)
    p = (:=) <$> name <*> lam

-- | An echo (parser)
echo :: GenParser Char st StatementI
echo = statementI (Echo <$> p) <?> "echo "
  where
    p = matchEcho *> between (matchTok '(') (matchTok ')') (sepBy expr0 (try matchComma))

ifStatementI :: GenParser Char st StatementI
ifStatementI = statementI p <?> "if "
  where
    p = matchIf *> if_
    if_ = If
      <$> between (matchTok '(') (matchTok ')') expr0
      <*> suite
      <*> option [] (matchElse *> suite)

forStatementI :: GenParser Char st StatementI
forStatementI = statementI for <?> "for "
  -- a for loop is of the form:
  --      for ( vsymb = vexpr   ) loops
  -- eg.  for ( a     = [1,2,3] ) {echo(a);   echo "lol";}
  -- eg.  for ( [a,b] = [[1,2]] ) {echo(a+b); echo "lol";}
  where
    lvalue = matchFor *> matchTok '(' *> patternMatcher <* matchTok '='
    vexpr  = expr0 <* matchTok ')'
    for    = For <$> lvalue <*> vexpr <*> suite

-- | parse a call to a module.
userModule :: GenParser Char st StatementI
userModule = statementI $ ModuleCall
  <$> fmap Symbol matchIdentifier
  <*> moduleArgsUnit
  <*> (suite *<|> (matchTok ';' $> []))

-- | declare a module.
userModuleDeclaration :: GenParser Char st StatementI
userModuleDeclaration = statementI $ matchModule *> newModule
  where
    newModule = NewModule
      <$> fmap Symbol matchIdentifier
      <*> moduleArgsUnitDecl
      <*> suite

-- | parse the arguments passed to a module.
moduleArgsUnit :: GenParser Char st [(Maybe Symbol, Expr)]
moduleArgsUnit = between (matchTok '(') (matchTok ')') $
  sepBy
    (simple *<|> complex *<|> literal)
    (try matchComma)
  where
    simple = (,) -- eg. a = 12
      <$> (pure . Symbol <$> matchIdentifier)
      <*> (matchTok '=' *> expr0)
    complex = do -- eg. a(x,y) = 12
      symb    <- matchIdentifier
      argVars <- between (matchTok '(') (matchTok ')') (sepBy matchIdentifier (try matchComma))
      expr    <- matchTok '=' *> expr0
      pure (pure $ Symbol symb, LamE (Name . Symbol <$> argVars) expr)
    literal = (Nothing, ) <$> expr0

-- | parse the arguments in the module declaration.
moduleArgsUnitDecl :: GenParser Char st [(Symbol, Maybe Expr)]
moduleArgsUnitDecl = between (matchTok '(') (matchTok ')') $
  sepBy (one *<|> list *<|> none) (try matchComma)
  where
    one = (,)
      <$> (Symbol <$> matchIdentifier <* matchTok '=')
      <*> (pure <$> expr0)
    list = do
      symb <- Symbol <$> matchIdentifier
      -- FIXME: why match this content, then drop it?
      _ <- between (matchTok '(') (matchTok ')') (sepBy matchIdentifier $ try matchComma) <* matchTok '='
      expr <- pure <$> expr0
      pure (symb, expr)
    none = (, Nothing) . Symbol <$> matchIdentifier

-- | Find the source position. Used when generating errors.
sourcePos :: ParsecT s u Identity SourcePosition
sourcePos = sourcePosition <$> getPosition

isNoOp :: StatementI -> Bool
isNoOp (StatementI _ DoNothing) = True
isNoOp _                        = False

-- | Remove statements that do nothing.
removeNoOps :: [StatementI] -> [StatementI]
removeNoOps = filter $ not . isNoOp

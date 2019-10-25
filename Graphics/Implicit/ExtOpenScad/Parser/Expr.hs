-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014-2019 , Julia Longtin (julial@turinglace.com)
-- Copyright (C) 2016, Kelvin Cookshaw (kelvin@cookshaw.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use shorter forms of Var and Name.
{-# LANGUAGE PatternSynonyms #-}
-- Allow the compiler to rewrite do syntax to maximise applicative operations
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- A parser for a numeric expressions.
module Graphics.Implicit.ExtOpenScad.Parser.Expr(expr0) where

import Prelude (Maybe(Nothing, Just), ($), (<>), id, foldl, foldr, pure , (.), (<$>), (<*>), (*>), (<*), flip, maybe, Eq, (<$))

import Graphics.Implicit.ExtOpenScad.Definitions (Expr(LamE, LitE, ListE, (:$)), OVal(ONum, OUndefined), Symbol(Symbol))

import qualified Graphics.Implicit.ExtOpenScad.Definitions as GIED (Expr(Var), Pattern(Name))

import Graphics.Implicit.ExtOpenScad.Parser.Util ((?:), number, boolean, scadString, scadUndefined, variable)

-- The lexer.
import Graphics.Implicit.ExtOpenScad.Parser.Lexer (matchLet, matchTok, matchColon, matchComma, matchIdentifier, matchEQ, matchNE, matchLE, matchLT, matchGE, matchGT, matchCAT, matchAND, matchOR, symbol)

-- The parsec parsing library.
import Text.Megaparsec (many, sepBy, option, (<|>), between, MonadParsec, optional)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR, Prefix, TernR), makeExprParser)
import Data.Text (Text)

import Data.Function ((&))
import Data.Functor (void, ($>))

-- Let us use the old syntax when defining Vars and Names.
pattern Var :: Text -> Expr
pattern Var  s = GIED.Var  (Symbol s)
pattern Name :: Text -> GIED.Pattern
pattern Name n = GIED.Name (Symbol n)

-- Borrowed the pattern from http://compgroups.net/comp.lang.functional/parsing-ternary-operator-with-parsec/1052460
-- In the levels list, the first element is the lowest precedent, and the last is the highest.
-- "higher" represents the higher precedence parser, ie. the next one in the levels list.
-- "fix $ \self ->..." is used to consume all expressions in the same level, "self" being the current level.
--

expr0 :: MonadParsec e Text m => m Expr
expr0 = makeExprParser
  nonAssociativeExpr
  [ [ Prefix $ flip (foldr bindLets) <$> (matchLet *> parens (assignment `sepBy` matchTok ',')) ]
  , [ Prefix $ 
      -- Unary -. applied to strings is undefined, but handle that in the interpreter.
      (\e -> Var "negate" :$ [e]) <$ matchTok '-'
      -- Unary +. Handle this by ignoring the +
      <|> id <$ matchTok '+'
    ]
  , [ Prefix $ do -- unary ! operator. OpenSCAD's YACC parser puts '!' at the same level of precedence as '-' and '+'.
      (\op right -> case right of
        -- when noting a not, just skip both of them.
        Var "!" :$ [deepright] -> deepright
        _                      -> Var op :$ [right]
        ) <$> matchTok '!'
    ]
  -- *, /, % operators
  , [ InfixL $ binaryOperation <$> (symbol "*" <|> symbol "/" <|> symbol "%") ]
  -- ^ exponent operator. This is not available in OpenSCAD.
  , [ InfixR $ binaryOperation <$> symbol "^" ]
  -- ++ string/list concatenation operator. This is not available in OpenSCAD.
  , [ InfixL $ binaryOperation <$> matchCAT ]
  -- + and - operators
  , [ InfixL $ binaryOperation <$> (symbol "+" <|> symbol "-") ]
  -- <, <=, >= and > operators
  , [ InfixL $ binaryOperation <$> (matchLE <|> matchLT <|> matchGE <|> matchGT) ]
  -- == and != operators
  , [ InfixL $ binaryOperation <$> (matchEQ <|> matchNE) ]
  -- && boolean AND operator
  , [ InfixL $ binaryOperation <$> matchAND ]
  -- || boolean OR operator
  , [ InfixL $ binaryOperation <$> matchOR ]
  -- ?: ternary operator.
  , [ TernR $ symbol "?" $> (matchColon $> (\c t f -> Var "?" :$ [c, t, f]))
    ]
  ]

{-
expr0 :: MonadParsec e Text m => m Expr
expr0 = foldr ($) nonAssociativeExpr levels
  where
    levels :: [m Expr -> m Expr]
    levels =
      [ id
      , \higher -> fix $ \self -> do -- ?: ternary operator.
        condition <- higher
        do
          trueExpr  <- matchTok '?' *> self
          falseExpr <- matchColon   *> self
          pure $ Var "?" :$ [condition, trueExpr, falseExpr]
         <|> pure condition
      -- || boolean OR operator
      , \higher -> chainl1 higher $ binaryOperation <$> matchOR
      -- && boolean AND operator
      , \higher -> chainl1 higher $ binaryOperation <$> matchAND
      -- == and != operators
      , \higher -> chainl1 higher $ binaryOperation <$> (matchEQ <|> matchNE)
      -- <, <=, >= and > operators
      , \higher -> chainl1 higher $ binaryOperation <$> (matchLE <|> matchLT <|> matchGE <|> matchGT)
      -- + and - operators
      , \higher -> chainl1 higher $ binaryOperation . pure <$> (oneOf "+-" <* whiteSpace)
      -- ++ string/list concatenation operator. This is not available in OpenSCAD.
      , \higher -> chainl1 higher $ binaryOperation <$> matchCAT
      -- ^ exponent operator. This is not available in OpenSCAD.
      , \higher -> chainr1 higher $ binaryOperation <$> matchTok '^'
      -- *, /, % operators
      , \higher -> chainl1 higher $ binaryOperation . pure <$> (oneOf "*/%" <* whiteSpace)
      , \higher ->
        fix $ \self -> do -- unary ! operator. OpenSCAD's YACC parser puts '!' at the same level of precedence as '-' and '+'.
          op <- matchTok '!'
          right <- self
          -- when noting a not, just skip both of them.
          pure $ case right of
            Var "!" :$ [deepright] -> deepright
            _                      -> Var op :$ [right]
        <|> higher
      , \higher -> -- leading positive or negative sign.
          fix $ \self ->
              do -- Unary -. applied to strings is undefined, but handle that in the interpreter.
                right <- matchTok '-' *> self
                pure $ Var "negate" :$ [right]
          -- Unary +. Handle this by ignoring the +
          <|> matchTok '+' *> self
        <|> higher
      , \higher -> -- "let" expression
        flip (foldr bindLets) <$> (matchLet *> parens (assignment `sepBy` matchTok ',')) <*> expr0
        <|> higher
      ]
-}

-- | parse expressions that don't associate, either because they are not operators or because they are operators
--   that contain the expressions they operate on in start and end tokens, like parentheses, and no other operator can associate with their expressions.
nonAssociativeExpr :: MonadParsec e Text m => m Expr
nonAssociativeExpr = do
       number
   <|> vectorListParentheses
   <|> variableish
   <|> scadString
   <|> boolean
   <|> scadUndefined

brackets :: MonadParsec e Text m => m a -> m a
brackets = between (matchTok '[') (matchTok ']')

parens :: MonadParsec e Text m => m a -> m a
parens = between (matchTok '(') (matchTok ')')

listIndexing :: MonadParsec e Text m => m (Expr -> Expr)
listIndexing = "list indexing" ?: listIndex <$> expr0
  where
    listIndex i l = Var "index" :$ [l, i]

listSplicing :: MonadParsec e Text m => m (Expr -> Expr)
listSplicing = "list splicing" ?:
  listSplice
    <$> optional expr0 <* matchColon
    <*> optional expr0
  where
    listSplice start end = case (start, end) of
      (Nothing, Nothing) -> id
      (Just s,  Nothing) -> \l -> Var "splice" :$ [l, s, LitE OUndefined]
      (Nothing, Just e ) -> \l -> Var "splice" :$ [l, LitE $ ONum 0, e]
      (Just s,  Just e ) -> \l -> Var "splice" :$ [l, s, e]

functionApplication :: MonadParsec e Text m => m [Expr -> Expr]
functionApplication = "function application" ?: do
  args <- parens $ sepBy expr0 matchComma
  pure [(:$ args)]

-- | parse operations that start with a variable name,
-- including variable reference, function calling, variable list indexing, and variable list splicing.
variableish :: MonadParsec e Text m => m Expr
variableish = "variable" ?: do
  obj  <- variable
  args <- option [] functionApplication 
  mods <- many . brackets $ listIndexing <|> listSplicing
  pure . foldl (&) obj $ args <> mods

data Parens = OpenParen | CloseParen
  deriving Eq

data Brackets = OpenBracket | CloseBracket
  deriving Eq

{-
matchedParenBrackets :: MonadParsec e Text m => m a -> GenParser Char s a
matchedParenBrackets m = do
  close <- open
  m <* close
  where
    open = matchTok ')' <$ matchTok '('
       <|> matchTok ']' <$ matchTok '['
-}

vectorListParens :: MonadParsec e Text m => m Expr
vectorListParens = "vector/list/parentheses" ?:
  -- eg. ( 1, 2, 3) - list
  --     (a+1) - parenthesized expression.
  matchTok '(' *> oneOrMany <* matchTok ')'
  where
    f [e] = e
    f l   = ListE l
    oneOrMany = f <$> sepBy expr0 matchComma

vectorListBrackets :: MonadParsec e Text m => m Expr
vectorListBrackets = "vector/list/brackets" ?: do
   -- eg. [ 3, a, a+1, b, a*b] - list
   --     [ a : 1 : a + 10 ]
   --     [ a : a + 10 ]
   -- FIXME: clearly, these have a numeric context, and should fail to parse for non-numeric contents.
  void $ matchTok '['
  emptyList <|> do
    e <- expr0
    generator e <|> list e
  where
    emptyList = ListE [] <$ matchTok ']'
    generator a = collector "list_gen" <$> do
      b <- matchColon *> expr0
      c <- optional (matchColon *> expr0) <* matchTok ']'
      pure $ maybe [a, LitE $ ONum 1.0, b] (\n -> [a, b, n]) c
    list e =
      ListE [e] <$ matchTok ']' <|>
      ListE . (e:) <$> (matchComma *> sepBy expr0 matchComma <* matchTok ']')

-- | Parse parentheses, lists, vectors, and vector/list generators.
vectorListParentheses :: MonadParsec e Text m => m Expr
vectorListParentheses = vectorListParens <|> vectorListBrackets 

-- | Apply a symbolic operator to a list of expressions, returning one big expression.
--   Accepts a string for the operator, to simplify callers.
collector :: Text -> [Expr] -> Expr
collector _ [x] = x
collector s  l  = Var s :$ [ListE l]

-- | Apply a symbolic operator to two expressions, combining left and right operands with an binary operator
binaryOperation :: Text -> Expr -> Expr -> Expr
binaryOperation sym left right = Var sym :$ [left, right]

-- | An assignment expression within a let's bindings list
assignment :: MonadParsec e Text m => m Expr
assignment = do
    ident       <- matchIdentifier
    expression  <- matchTok '=' *> expr0
    pure $ ListE [Var ident, expression]

-- | build nested let statements when foldr'd.
bindLets :: Expr -> Expr -> Expr
bindLets (ListE [Var boundName, boundExpr]) nestedExpr = LamE [Name boundName] nestedExpr :$ [boundExpr]
bindLets _ e = e

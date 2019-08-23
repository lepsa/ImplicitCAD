-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use shorter forms of Var and Name.
{-# LANGUAGE PatternSynonyms #-}

-- A parser for a numeric expressions.
module Graphics.Implicit.ExtOpenScad.Parser.Expr(expr0) where

import           Prelude                                    (Char, Maybe (Just, Nothing),
                                                             String, foldl,
                                                             foldl1, foldr,
                                                             head, id, length,
                                                             map, mod, return,
                                                             tail, unzip,
                                                             zipWith3, ($),
                                                             (&&), (++), (.),
                                                             (==), (>>))

-- The parsec parsing library.
import           Text.Parsec                                (many, many1, oneOf,
                                                             option,
                                                             optionMaybe, sepBy,
                                                             sepBy1, string,
                                                             try)

import           Text.Parsec.String                         (GenParser)

import           Graphics.Implicit.ExtOpenScad.Definitions  (Expr ((:$), LamE, ListE, LitE),
                                                             OVal (ONum, OUndefined),
                                                             Symbol (Symbol),
                                                             collector)

import qualified Graphics.Implicit.ExtOpenScad.Definitions  as GIED (Expr (Var), Pattern (Name))

import           Graphics.Implicit.ExtOpenScad.Parser.Util  (boolean, number,
                                                             scadString,
                                                             scadUndefined,
                                                             variable, (*<|>),
                                                             (?:))

-- The lexer.
import           Graphics.Implicit.ExtOpenScad.Parser.Lexer (matchColon,
                                                             matchComma,
                                                             matchEQ, matchGE,
                                                             matchGT,
                                                             matchIdentifier,
                                                             matchLE, matchLT,
                                                             matchLet, matchNE,
                                                             matchTok,
                                                             surroundedBy,
                                                             whiteSpace)

-- Let us use the old syntax when defining Vars and Names.
pattern Var :: String -> Expr
pattern Var  s = GIED.Var  (Symbol s)
pattern Name :: String -> GIED.Pattern
pattern Name n = GIED.Name (Symbol n)

-- We represent the priority or 'fixity' of different types of expressions
-- by the order of parser application.

expr0 :: GenParser Char st Expr
expr0 = ternary

-- match the ternary (1?2:3) operator.
ternary :: GenParser Char st Expr
ternary =
    "ternary" ?: do
        a <- logic
        _ <- matchTok '?'
        b <- ternary
        _ <- matchColon
        c <- ternary
        return $ Var "?" :$ [a,b,c]
    *<|> logic

-- | Match the logical And and Or (&&,||) operators.
logic :: GenParser Char st Expr
logic =
    "logical and/or" ?: do
        a <- negation
        symb <-      string "&&"
                *<|> string "||"
        _ <- whiteSpace
        b <- logic
        return $ Var symb :$ [a,b]
    *<|> negation

-- | Match the logical negation operator.
negation :: GenParser Char st Expr
negation =
    "logical-not" ?: do
        a <- many1 $ matchTok '!'
        b <- comparison
        return $ if length a `mod` 2 == 0
                 then b
                 else Var "!" :$ [b]
    *<|> comparison

-- match comparison operators.
comparison :: GenParser Char st Expr
comparison =
    do
        firstExpr <- chainedAddition
        otherComparisonsExpr <- many $ do
            comparisonSymb <-
                   matchEQ
              *<|> matchNE
              *<|> matchLE
              *<|> matchLT
              *<|> matchGE
              *<|> matchGT
            expr <- chainedAddition
            return (Var comparisonSymb, expr)
        let
            (comparisons, otherExprs) = unzip otherComparisonsExpr
            exprs = firstExpr:otherExprs
        return $ case comparisons of
            []  -> firstExpr
            [x] -> x :$ exprs
            _   -> collector "all" $ zipWith3 (\c e1 e2 -> c :$ [e1,e2]) comparisons exprs (tail exprs)
    *<|> chainedAddition

-- match sequences of addition and subtraction.
chainedAddition :: GenParser Char st Expr
chainedAddition =
    "addition/subtraction" ?: do
        -- Similar to multiply & divide
        -- eg. "1+2+3-4-5+6-7"
        --     [[1],[2],[3,4,5],[6,7]]
        exprs <- sepBy1
            (sepBy1 append (try $ matchTok '-'))
            (try $ matchTok '+')
        let sub a b = Var "-" :$ [a, b]
        return . collector "+" $ map (foldl1 sub) exprs
    *<|> append

-- match string addition (++) operator.
append :: GenParser Char st Expr
append =
    "append" ?: do
        exprs <- sepBy1 modulo (try $ string "++" >> whiteSpace)
        return $ collector "++" exprs
    *<|> modulo

-- match remainder (%) operator.
modulo :: GenParser Char st Expr
modulo =
    "modulo" ?: do
        exprs <- sepBy1 multiplication (try $ matchTok '%')
        let mod' a b = Var "%" :$ [a, b]
        return $ foldl1 mod' exprs
    *<|> multiplication

-- match sequences of multiplication and division.
multiplication :: GenParser Char st Expr
multiplication =
    "multiplication/division" ?: do
        -- outer list is multiplication, inner division.
        -- eg. "1*2*3/4/5*6*7/8"
        --     [[1],[2],[3,4,5],[6],[7,8]]
        exprs <- sepBy1
            (sepBy1 power (try $ matchTok '/'))
            (try $ matchTok '*')
        let div' a b = Var "/" :$ [a, b]
        return . collector "*" $ map (foldl1 div') exprs
    *<|> power

-- match power-of (^) operator.
power :: GenParser Char st Expr
power =
    "exponentiation" ?: do
        a <- signs
        b <- matchTok '^' >> power
        return $ Var "^" :$ [a,b]
    *<|> signs

-- match a leading (+) or (-) operator.
signs :: GenParser Char st Expr
signs =
    "negation" ?: do
        expr <- matchTok '-' >> variables
        return $ Var "negate" :$ [expr]
    *<|> do
        matchTok '+' >> variables
    *<|> variables

-- | parse operations that start with a variable name, including variable reference..
variables :: GenParser Char st Expr
variables =
    do
        obj <- variable
        args <- option [] (
          "function application" ?: do
              args <- surroundedBy '(' (sepBy expr0 matchComma) ')'
              return [(:$ args)]
          )
        mods <- many (
               "list indexing" ?: do
                   i <- surroundedBy '[' expr0 ']'
                   return $ \l -> Var "index" :$ [l, i]
          *<|> "list splicing" ?: do
                   _     <- matchTok '['
                   start <- optionMaybe expr0
                   _     <- matchColon
                   end   <- optionMaybe expr0
                   _     <- matchTok ']'
                   return $ case (start, end) of
                              (Nothing, Nothing) -> id
                              (Just s,  Nothing)  -> \l -> Var "splice" :$ [l, s, LitE OUndefined ]
                              (Nothing, Just e )  -> \l -> Var "splice" :$ [l, LitE $ ONum 0, e]
                              (Just s,  Just e )  -> \l -> Var "splice" :$ [l, s, e]
                 )
        return $ foldl (\a b -> b a) obj (args ++ mods)
    *<|> bracketed

-- | Parse parentheses, lists, vectors, and vector/list generators.
bracketed :: GenParser Char st Expr
bracketed =
         literals
    *<|> "vector/list/brackets" ?: do
            -- eg. [ 3, a, a+1, b, a*b] or ( 1, 2, 3)
            o <- oneOf "[("
            _ <- whiteSpace
            exprs <- sepBy expr0 matchComma
            _ <- if o == '['
                 then matchTok ']'
                 else matchTok ')'
            return $ if o == '(' && length exprs == 1
                     then head exprs
                     else ListE exprs
    *<|> "vector/list generator" ?: do
        -- eg.  [ a : 1 : a + 10 ]
        _ <- matchTok '['
        e1 <- expr0
        _     <- matchColon
        exprs <- do
                   e2 <- expr0
                   e3 <- optionMaybe (matchColon >> expr0)
                   return $ case e3 of
                      Just n  -> [e1, e2, n]
                      Nothing -> [e1, LitE $ ONum 1.0, e2]
        _ <- matchTok ']'
        return $ collector "list_gen" exprs
    *<|> letExpr

-- | Parse literals.
literals :: GenParser Char st Expr
literals =
       number
  *<|> boolean
  *<|> scadString
  *<|> scadUndefined

letExpr :: GenParser Char st Expr
letExpr = "let expression" ?: do
  _ <- matchLet >> matchTok '('
  bindingPairs <- sepBy ( do
    boundName <- matchIdentifier
    boundExpr <- matchTok '=' >> expr0
    return $ ListE [Var boundName, boundExpr])
    matchComma
  _ <- matchTok ')'
  expr <- expr0
  let bindLets (ListE [Var boundName, boundExpr]) nestedExpr = LamE [Name boundName] nestedExpr :$ [boundExpr]
      bindLets _ e = e
  return $ foldr bindLets expr bindingPairs


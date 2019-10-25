-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.Implicit.ExtOpenScad.Eval.Constant (addConstants, runExpr) where

import Prelude (Maybe(Just, Nothing), IO, ($), return, (+), Either (Left, Right), Bool(False), (.))

import Graphics.Implicit.Definitions (Fastℕ)

import Graphics.Implicit.ExtOpenScad.Definitions (
                                                  Pattern,
                                                  Expr,
                                                  VarLookup,
                                                  Message,
                                                  MessageType(SyntaxError),
                                                  StateC,
                                                  ScadOpts(ScadOpts),
                                                  CompState(CompState),
                                                  SourcePosition(SourcePosition),
                                                  OVal(OUndefined),
                                                  varUnion
                                                 )

import Graphics.Implicit.ExtOpenScad.Util.StateC (modifyVarLookup, addMessage)

import Graphics.Implicit.ExtOpenScad.Parser.Expr (expr0)

import Graphics.Implicit.ExtOpenScad.Eval.Expr (evalExpr, matchPat)

import Graphics.Implicit.ExtOpenScad.Default (defaultObjects)

import Control.Monad.State (liftIO, runStateT)

import System.Directory (getCurrentDirectory)

import Text.Megaparsec (parse, MonadParsec, ParseErrorBundle, errorBundlePretty)
import System.IO (FilePath)
import Data.Text (Text, pack)
import Data.Void (Void)

import Graphics.Implicit.ExtOpenScad.Parser.Util (patternMatcher)

import Graphics.Implicit.ExtOpenScad.Parser.Lexer (matchTok)

-- | Define variables used during the extOpenScad run.
addConstants :: [Text] -> IO (VarLookup, [Message])
addConstants constants = do
  path <- getCurrentDirectory
  let scadOpts = ScadOpts False False
  (_, CompState (varLookup, _, _, messages, _)) <- liftIO $ runStateT (execAssignments constants 0) (CompState (defaultObjects, [], path, [], scadOpts))
  return (varLookup, messages)
  where
    execAssignments :: [Text] -> Fastℕ  -> StateC ()
    execAssignments [] _ = return ()
    execAssignments (assignment:xs) count = do
      let
        pos = (SourcePosition count 1 "cmdline_constants")
        show' = pack . errorBundlePretty
      case parseAssignment "cmdline_constant" assignment of
        Left e -> do
          addMessage SyntaxError pos $ show' e
        Right (key, expr) -> do
          res <- evalExpr pos expr
          case matchPat key res of
            Nothing -> return ()
            Just pat -> modifyVarLookup $ varUnion pat
      execAssignments xs (count+1)
    parseAssignment :: FilePath -> Text -> Either (ParseErrorBundle Text Void) (Pattern, Expr)
    parseAssignment = parse assignment
      where
        assignment :: MonadParsec e Text m => m (Pattern, Expr)
        assignment = do
          key <- patternMatcher
          _ <- matchTok '='
          expr <- expr0
          return (key, expr)

-- | Evaluate an expression, returning only it's result.
runExpr :: Text -> IO (OVal, [Message])
runExpr expression = do
  path <- getCurrentDirectory
  let scadOpts = ScadOpts False False
  (res, CompState (_, _, _, messages, _)) <- liftIO $ runStateT (execExpression expression) (CompState (defaultObjects, [], path, [], scadOpts))
  return (res, messages)
  where
    execExpression :: Text -> StateC OVal
    execExpression expr = do
      let
        pos = (SourcePosition 1 1 "raw_expression")
        show' = pack . errorBundlePretty 
      case parseExpression "raw_expression" expr of
        Left e -> do
          addMessage SyntaxError pos $ show' e
          return OUndefined
        Right parseRes -> evalExpr pos parseRes
    parseExpression :: FilePath -> Text -> Either (ParseErrorBundle Text Void) Expr
    parseExpression = parse expr0


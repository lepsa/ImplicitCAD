{- ORMOLU_DISABLE -}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use string literals for Text
{-# LANGUAGE OverloadedStrings #-}

module Graphics.Implicit.ExtOpenScad.Eval.Expr (evalExpr, rawRunExpr, matchPat, StateE, ExprState(ExprState), addMessage) where

import Prelude (String, Maybe(Just, Nothing), ($), pure, zip, (!!), const, (<>), foldr, foldMap, (.), (<$>), traverse)

import Graphics.Implicit.ExtOpenScad.Definitions (
                                                  Pattern(Name, ListP, Wild),
                                                  OVal(OList, OError, OFunc, OUndefined),
                                                  Expr(LitE, ListE, LamE, Var, (:$)),
                                                  Symbol(Symbol),
                                                  VarLookup(VarLookup),
                                                  SourcePosition,
                                                  Message(Message),
                                                  MessageType(Error),
                                                  StateC, ImplicitCadM, runImplicitCadM
                                                 )

import Graphics.Implicit.ExtOpenScad.Util.OVal (oTypeStr, getErrors)

import Graphics.Implicit.ExtOpenScad.Util.StateC (getVarLookup)

import qualified Graphics.Implicit.ExtOpenScad.Util.StateC as GIEUS (addMessage)

import Data.List (elemIndex)

import Data.Map (fromList, lookup)

import Data.Foldable (fold, traverse_)

import Data.Traversable (for)

import Control.Monad (zipWithM)

import Data.Text.Lazy (Text, unpack)

import Data.Eq (Eq)
import Text.Show (Show)
import Control.Monad.Writer.Class (tell)
import Control.Monad.State.Lazy (get)
import Control.Monad.State.Class (modify)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ask)

-- Patterns is the only thing being modified, so
-- it is the only on in the state structure.
newtype ExprState = ExprState
  { patterns  :: [String]
  } deriving (Eq, Show)

-- varLookup and sourcePos are only ever read from
-- so we can put them into a reader, so they can never
-- accidentally be written to.
data Input = Input
  { varLookup :: VarLookup
  , sourcePos :: SourcePosition
  } deriving (Eq, Show)

-- Check Graphics.Implicit.ExtOpenScad.Definitions for an explanation
-- of why we are using a transformer stack.
type StateE a = ImplicitCadM Input [Message] ExprState Identity a

runStateE :: Input -> ExprState -> StateE a -> (a, [Message], ExprState)
runStateE r s m = runIdentity $ runImplicitCadM r s m

-- Add a message to our list of messages contained in the StatE monad.
addMessage :: MessageType -> SourcePosition -> Text -> StateE ()
addMessage mtype pos text = addMesg $ Message mtype pos text
  where
    addMesg :: Message -> StateE ()
    addMesg = tell . pure

-- Log an error condition.
errorE :: SourcePosition -> Text -> StateE ()
errorE = addMessage Error

-- | The names of all of the patterns in the given pattern.
patVars :: Pattern -> [Text]
patVars (Name (Symbol name)) = [name]
patVars (ListP pats) = foldMap patVars pats
patVars Wild = []

-- | Match patterns and ovals, returning a list of all of the OVals matched.
patMatch :: Pattern -> OVal -> Maybe [OVal]
patMatch (Name _) val = Just [val]
patMatch (ListP pats) (OList vals) = fold <$> zipWithM patMatch pats vals
patMatch Wild _ = Just []
patMatch _ _ = Nothing

-- | Construct a VarLookup from the given Pattern and OVal, if possible.
matchPat :: Pattern -> OVal -> Maybe VarLookup
matchPat pat val = VarLookup . fromList . zip (Symbol <$> patVars pat) <$> patMatch pat val

-- | The entry point from StateC. evaluates an expression, pureing the result, and moving any error messages generated into the calling StateC.
evalExpr :: SourcePosition -> Expr -> StateC OVal
evalExpr pos expr = do
    vars <- getVarLookup
    let
      input = Input vars pos
      initState = ExprState []
      (valf, messages, _) = runStateE input initState (evalExpr' expr)
      moveMessage (Message mtype mpos text) = GIEUS.addMessage mtype mpos text
    traverse_ moveMessage messages
    pure $ valf []

-- A more raw entry point, that does not depend on IO.
rawRunExpr :: SourcePosition -> VarLookup -> Expr -> (OVal, [Message])
rawRunExpr pos vars expr = do
  let
    input = Input vars pos
    initState = ExprState []
    (valf, messages, _) = runStateE input initState (evalExpr' expr)
  (valf [], messages)

-- The expression evaluators.
evalExpr' :: Expr -> StateE ([OVal] -> OVal)

-- Evaluate a variable lookup.
evalExpr' (Var (Symbol name)) = do
  Input (VarLookup varlookup) spos <- ask
  (ExprState namestack) <- get
  case (lookup (Symbol name) varlookup, elemIndex (unpack name) namestack) of
        (_, Just pos) -> pure (!! pos)
        (Just val, _) -> pure $ const val
        _             -> do
          errorE spos ("Variable " <> name <> " not in scope")
          pure $ const OUndefined

-- Evaluate a literal value.
evalExpr' (LitE  val) = pure $ const val

-- Evaluate a list of expressions.
evalExpr' (ListE exprs) = do
    valFuncs <- traverse evalExpr' exprs
    pure $ \s -> OList $ ($ s) <$> valFuncs

-- Evaluate application of a function.
evalExpr' (fexpr :$ argExprs) = do
    fValFunc <- evalExpr' fexpr
    argValFuncs <- traverse evalExpr' argExprs
    pure $ \s -> app (fValFunc s) (($ s) <$> argValFuncs)
        where
            app f l = case (getErrors f, getErrors $ OList l) of
                (Nothing, Nothing) -> app' f l
                    where
                        -- apply function to the list of its arguments until we run out
                        -- of them
                        app' (OFunc f') (x:xs) = app (f' x) xs
                        app' a [] = a
                        app' x _ = OError $ "Can't apply arguments to " <> oTypeStr x
                (Just err, _     ) -> OError err
                (_,      Just err) -> OError err

-- Evaluate a lambda function.
evalExpr' (LamE pats fexpr) = do
    fparts <- for pats $ \pat -> do
        modify $ \s -> s { patterns = (unpack <$> patVars pat) <> patterns s}
        pure $ \f xss -> OFunc $ \val -> case patMatch pat val of
            Just xs -> f (xs <> xss)
            Nothing -> OError "Pattern match failed"
    fval <- evalExpr' fexpr
    pure $ foldr ($) fval fparts
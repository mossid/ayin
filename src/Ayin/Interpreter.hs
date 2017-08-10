{-# LANGUAGE RankNTypes, GADTs #-}

module Ayin.Interpreter where

import Ayin.Language hiding ((+), Expr(..), Bool, Lit(..))
import qualified Ayin.Language as A

import Numeric.Natural

import Control.Monad.State

import qualified Data.Map as M


data ExpandEnv = ExpandEnv { varID :: Int
                           , fns   :: M.Map String XExpr
                           } deriving (Show)

data XLit = BoolLit Bool
          | SIntLit Integer
          | UIntLit Natural
    deriving (Show)

xLit :: A.Lit a -> XLit 
xLit (A.BoolLit x) = BoolLit x
xLit (A.SIntLit x) = SIntLit x
xLit (A.UIntLit x) = UIntLit x

data XExpr = Lit XLit
           | Fn Int XExpr
           | Call String
           | Arith A.Arith XExpr XExpr
           | Comp A.Comp XExpr XExpr
           | Logic A.Logic XExpr XExpr
           | If XExpr XExpr XExpr
           | App XExpr XExpr
           | Arg Int
    deriving (Show)

expand :: A.Expr a -> State ExpandEnv XExpr
expand (A.Lit x) = return $ Lit (xLit x)
expand (A.Dec name x) = do
    fns' <- gets fns
    if M.member name fns'
        then return $ Call name
        else do
            -- insert dummy value
            modify (\env -> env{ fns = M.insert name (Call name) (fns env) })
            res <- expand x 
            modify (\env -> env{ fns = M.insert name res (fns env) })
            return res
expand (A.Fn f) = do
    varID' <- gets varID
    modify (\env -> env { varID = varID env + 1 })
    res <- Fn varID' <$> expand (f $ A.Arg varID')
    modify (\env -> env { varID = varID' })
    return res
expand (A.Arith a x y) = Arith a <$> expand x <*> expand y
expand (A.Comp c x y) = Comp c <$> expand x <*> expand y
expand (A.Logic l x y) = Logic l <$> expand x <*> expand y
expand (A.If b x y) = If <$> expand b <*> expand x <*> expand y
expand (A.App f x) = App <$> expand f <*> expand x
expand (A.Arg x) = return $ Arg x
{-
expand (App (Fn name (Lit f)) x) = do
    
expand (App (Lit f) x) = do
-}

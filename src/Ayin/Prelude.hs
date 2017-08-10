{-# LANGUAGE NoImplicitPrelude, RankNTypes, FlexibleInstances #-}
{-# LANGUAGE GADTs, KindSignatures, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances, TypeOperators, TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Ayin.Prelude (
    ($), (.),
    Comparable(..),
    Arithmetic(..),
    Bool, SInt, UInt,
    true, false, 
    not, (&&), (||), xor,
    (-->), cond, (?),
    (#), (:>), fn,
) where

import Prelude (($), (.), Show(..))
import qualified Prelude as P
import qualified Numeric.Natural as N

class Comparable a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    (/=) = (not .) . (==)
    (<)  :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    (>)  :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    
class Arithmetic a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a 
    (/) :: a -> a -> a
    (%) :: a -> a -> a

data Arith = Add
           | Sub
           | Mul
           | Div
           | Mod
    deriving (Show)

data Comp = Lt
          | Gt
          | Eq
    deriving (Show)

data Logic = And
           | Or
           | Xor
    deriving (Show)

data Expr :: * -> * where
    Lit   ::                          a                         -> Expr a
    Fn    ::                          P.String -> (Expr a :> Expr b) -> (Expr a :> Expr b)
    Arith :: (Arithmetic (Expr a)) => Arith -> Expr a -> Expr a -> Expr a
    Comp  :: (Comparable (Expr a)) => Comp  -> Expr a -> Expr a -> Bool
    Logic ::                          Logic -> Bool   -> Bool   -> Bool
    If    ::                          Bool  -> Expr a -> Expr a -> Expr a
    App   ::                          (Expr a :> Expr b) -> Expr a -> Expr b
    Arg   ::                          P.Int                     -> Expr a

type Bool = Expr P.Bool
type SInt = Expr P.Integer
type UInt = Expr N.Natural

true :: Bool
true = Lit P.True

false :: Bool
false = Lit P.False

not :: Bool -> Bool
not = Logic Xor true

(&&), (||), xor :: Bool -> Bool -> Bool
(&&) = Logic And
(||) = Logic Or
xor  = Logic Xor

instance (P.Ord a) => Comparable (Expr a) where
    (==)   = Comp Eq
    (<)    = Comp Lt
    x <= y = Comp Lt x y || Comp Eq x y
    (>)    = Comp Gt
    x >= y = Comp Gt x y || Comp Eq x y 

instance (P.Num a) => Arithmetic (Expr a) where
    (+) = Arith Add
    (-) = Arith Sub
    (*) = Arith Mul
    (/) = Arith Div
    (%) = Arith Mod 

instance (P.Num a, Arithmetic (Expr a), Comparable (Expr a)) => P.Num (Expr a) where
    (+) = (+)
    (-) = (-)
    (*) = (*)
    abs x = x >= 0 ? x $ -x
    signum x = x / P.abs x
    fromInteger = Lit . P.fromInteger

(-->) = (,)

cond :: [(Bool, Expr a)] -> Expr a -> Expr a
cond []          def = def
cond ((b, t):cs) def = b ? t $ cond cs def

(?) :: Bool -> Expr a -> Expr a -> Expr a
(?) = If

type family FnLit a :: *
type instance FnLit (Expr a)      = Expr a
type instance FnLit (Expr a -> b) = Expr (Expr a -> FnLit b)

type a :> b = Expr (a -> FnLit b)

class Fn a where
    fn' :: a -> FnLit a

instance Fn (Expr a) where
    fn' x = x

instance (Fn b) => Fn (Expr a -> b) where
    fn' f = Lit (\x -> fn' (f x))

fn name f = Fn name $ fn' f

(#) = App

{-# LANGUAGE NoImplicitPrelude, RankNTypes, FlexibleInstances #-}
{-# LANGUAGE GADTs, KindSignatures, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances, TypeOperators, TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Ayin.Language{- (
    ($), (.),
    Comparable(..),
    Arithmetic(..),
    Bool, SInt, UInt,
    true, false, 
    not, (&&), (||), xor,
    (-->), cond, (?),
    (#), (:>), fn,
)-} where

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

class FromSInt a where
    fromSInt :: SInt -> a

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

data Lit :: * -> * where
    BoolLit :: P.Bool -> Lit P.Bool
    SIntLit :: P.Integer -> Lit P.Integer
    UIntLit :: N.Natural -> Lit N.Natural

data Expr :: * -> * where
    Lit   ::                          Lit a                     -> Expr a
    Fn    ::                          (Expr a -> Expr b)        -> Expr (Expr a -> Expr b)
    Dec   ::                          P.String -> Expr a        -> Expr a
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
true = Lit $ BoolLit P.True

false :: Bool
false = Lit $ BoolLit P.False

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
{-
instance P.Num UInt where
    (+) = (+)
    (-) = (-)
    (*) = (*)
    abs x = x >= 0 ? x $ -x
    signum x = x / P.abs x
    fromInteger = Lit . UIntLit . P.fromInteger
-}


--instance (P.Num a, Arithmetic (Expr a), Comparable (Expr a)) => P.Num (Expr a) where
instance P.Num SInt where
    (+) = (+)
    (-) = (-)
    (*) = (*)
    abs x = x >= 0 ? x $ -x
    signum x = x / P.abs x
    fromInteger = Lit . SIntLit

instance P.Num UInt where
    (+) = (+)
    (-) = (-)
    (*) = (*)
    abs x = x >= 0 ? x $ -x
    signum x = x / P.abs x
    fromInteger = Lit . UIntLit . P.fromInteger


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

instance (Fn (Expr b)) => Fn (Expr a -> Expr b) where
    fn' f = Fn (\x -> fn' (f x))

fn name f = Dec name $ fn' f

(#) = App
{-
fn "a" \ a b -> f a b
Fn "a" $ fn' \ a b -> f a b
Fn "a" $ Lit (\x -> fn' (\b -> f x b))
Fn "a" $ Lit (\x -> Lit \y -> fn' (f x y))
Fn "a" $ Lit (\x -> Lit \y -> f x y)-}

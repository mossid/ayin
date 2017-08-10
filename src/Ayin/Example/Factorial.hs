{-# LANGUAGE NoImplicitPrelude, TypeOperators #-}

module Ayin.Example.Factorial where

import Ayin.Language
import Ayin.Interpreter
import Prelude (($))
{-
[struct|
struct Maybe a = Just { just :: a }
               | Nothing
|]
-}
factorial :: UInt :> UInt
factorial = fn "factorial" $ \x -> cond [
    x == 0 --> 0,
    x == 1 --> 1] 
    $ x * (factorial # x - 1)

{-
sfactorial :: SInt :> Maybe SInt
sfactorial = fn "sfactorial" $ \x -> cond [
    x <  0 --> 
    x == 0 -->
    x == 1 -->] $ 
    x * (factorial # x - 1)
-}
-- Expr (Expr Int -> Expr Int)

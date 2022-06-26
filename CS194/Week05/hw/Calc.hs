{-# LANGUAGE FlexibleInstances #-}
module Calc where
    import ExprT
    import Parser
    import qualified StackVM
    import qualified Data.Map as M

    eval :: ExprT -> Integer
    eval (ExprT.Lit i) = i
    eval (ExprT.Add e1 e2) = (eval e1) + (eval e2)
    eval (ExprT.Mul e1 e2) = (eval e1) * (eval e2)

    applyMaybe :: (a -> b) -> Maybe a -> Maybe b
    applyMaybe _ Nothing = Nothing
    applyMaybe f (Just a) = Just (f a)

    evalStr :: String -> Maybe Integer
    evalStr = (applyMaybe eval) . (parseExp ExprT.Lit ExprT.Add ExprT.Mul)

    class Expr a where
        lit :: Integer -> a
        add :: a -> a -> a
        mul :: a -> a -> a

    instance Expr ExprT where
        lit = ExprT.Lit
        add = ExprT.Add
        mul = ExprT.Mul

    instance Expr Integer where
        lit = id
        add = (+)
        mul = (*)

    instance Expr Bool where
        lit = (>0)
        add = (||)
        mul = (&&)

    newtype MinMax = MinMax Integer deriving (Eq, Show)
    newtype Mod7 = Mod7 Integer deriving (Eq, Show)

    instance Expr MinMax where
        lit i = MinMax i
        add (MinMax i1) (MinMax i2) = MinMax (max i1 i2)
        mul (MinMax i1) (MinMax i2) = MinMax (min i1 i2)

    instance Expr Mod7 where
        lit i = Mod7 i
        add (Mod7 i1) (Mod7 i2) = Mod7 ((i1 + i2) `mod` 7)
        mul (Mod7 i1) (Mod7 i2) = Mod7 ((i1 * i2) `mod` 7)

    instance Expr StackVM.Program where
        lit i = [StackVM.PushI i]
        add p1 p2 = p1 ++ p2 ++ [StackVM.Add]
        mul p1 p2 = p1 ++ p2 ++ [StackVM.Mul]

    compile :: String -> Maybe StackVM.Program
    compile = parseExp lit add mul

    class HasVars a where
        var :: String -> a

    data VarExprT = Lit Integer
            | Add VarExprT VarExprT
            | Mul VarExprT VarExprT
            | Var String
        deriving (Show, Eq)

    instance Expr VarExprT where
        lit = Calc.Lit
        add = Calc.Add
        mul = Calc.Mul

    instance HasVars VarExprT where
        var = Calc.Var

    instance HasVars (M.Map String Integer -> Maybe Integer) where
        var = M.lookup

    instance Expr (M.Map String Integer -> Maybe Integer) where
        lit i _ = Just i
        add (f1) (f2) m = case ((f1 m), (f2 m)) of
            (Just i1, Just i2) -> Just (i1 + i2)
            _ -> Nothing
        mul (f1) (f2) m = case ((f1 m), (f2 m)) of
            (Just i1, Just i2) -> Just (i1 * i2)
            _ -> Nothing

    withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
    withVars vs exp = exp $ M.fromList vs


module Expr where

import Parsing
import MaybeHandling
import EitherHandling
import BinarySearchTree

type Name = String

-- | All possible expressions including number of arguments required, including variables and values 
data Expr = Divide Expr Expr
          | Multiply Expr Expr
          | Add Expr Expr
          | Subtract Expr Expr
          | Div Expr Expr
          | Modulus Expr Expr
          | Power Expr Expr
          | Absolute Expr
          | Val Float
          | Var Name
  deriving Show

-- | All possible calculator commands
data Command = Set Name Expr
             | Eval Expr
             | Quit
             | Repeat Int
             | File Name
             | Help
  deriving Show

-- | 
eval :: BiTree String Float -> -- Variable name to value mapping
       Expr -> -- Expression to evaluate
       {- Maybe Int -- Result (if no errors such as missing variables) -}
       Either String Float
eval vars (Val x) = Right x -- For values, just give the value directly
eval vars (Add x y) = eitherAdd (eval vars x) (eval vars y) 
eval vars (Subtract x y) = eitherSubtract (eval vars x) (eval vars y) 
eval vars (Divide x y) = eitherDivide (eval vars x) (eval vars y)
eval vars (Multiply x y) = eitherMultiply (eval vars x) (eval vars y)
eval vars (Div x y) = eitherDiv (eval vars x) (eval vars y)
eval vars (Modulus x y) = eitherModulus (eval vars x) (eval vars y) 
eval vars (Power x y) = eitherPower (eval vars x) (eval vars y) 
eval vars (Absolute x) = eitherAbsolute (eval vars x) 
eval vars (Var v) = retrieve vars v

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

pCommand :: Parser Command
pCommand = do var <- many1 letter
              char '='
              e <- pExpr
              return (Set var e)
            ||| do e <- pExpr
                   return (Eval e)
            ||| do 
                   string ":q"
                   return Quit
            ||| do
                   char '!'
                   i <- nat
                   return (Repeat i)
            ||| do
                   string ":f"
                   i <- fileName
                   return (File i)
            ||| do
                   string ":h"
                   return Help


pExpr :: Parser Expr                       
pExpr = do t <- pTerm
           do char '+'
              e <- pExpr
              return (Add t e)
            ||| do char '-'
                   e <- pExpr
                   return (Subtract t e)
            ||| do char '%' --Modulus
                   e <- pExpr
                   return (Modulus t e)
            ||| do string "div" --Modulus
                   e <- pExpr
                   return (Div t e)
            ||| return t

pFactor :: Parser Expr
pFactor = do d <- nfloat
             return (Val d)
           ||| do var <- many1 letter
                  return (Var var)
           ||| do char '('
                  e <- pExpr
                  char ')'
                  return e
           ||| do char '|'
                  e <- pExpr
                  char '|'
                  return (Absolute e)

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do char '*'
              t <- pTerm
              return (Multiply f t)
            ||| do 
                   char '/'
                   t <- pTerm
                   return (Divide f t)
              ||| do 
                     char '^'
                     t <- pTerm
                     return (Power f t)
                 ||| return f


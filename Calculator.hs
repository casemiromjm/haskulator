{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
-}
module Main where

import Parsing
import Data.Char

-- defining an Env type for handling variables
type Name = String
type Env = [(Name, Integer)]

-- | update curr Env by appending a (k,v) tuple
updateEnv :: Name -> Integer -> Env -> Env
updateEnv k v [] = [(k, v)]
updateEnv k' v' ((k,v):ks)
  | k' == k = (k', v'):ks
  | otherwise = (k, v) : updateEnv k' v' ks

--
-- a data type for expressions
-- made up from integer numbers, + and *
--
data Expr = Num Integer
          | Var Name
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Rem Expr Expr
          deriving Show

-- a recursive evaluator for expressions
--
eval :: Env -> Expr -> Integer
eval env (Num n) = n
eval env (Var v) = case lookup v env of
                    Just val -> val
                    Nothing -> error ("undefined variable: " ++ v)    -- not ideal, but it works in this case
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Div e1 e2) = eval env e1 `div` eval env e2
eval env (Rem e1 e2) = eval env e1 `mod` eval env e2

-- | a parser for expressions
-- Grammar rules:
--
-- expr ::= term exprCont
-- exprCont ::= '+' term exprCont | '-' term exprCont | epsilon

-- term ::= factor termCont
-- termCont ::= '*' factor termCont | '/' factor termCont | '%' factor termCont | epsilon

-- factor ::= variable | natural | '(' expr ')'

-- command ::= variable '=' expr | expr

expr :: Parser Expr
expr = do t <- term
          exprCont t

exprCont :: Expr -> Parser Expr
exprCont acc = ( do 
                  char '+'
                  t <- term
                  exprCont (Add acc t)
                )
              <|> ( do
                      char '-'
                      t <- term
                      exprCont (Sub acc t)
                    )
              <|> return acc
              
term :: Parser Expr
term = do f <- factor
          termCont f

termCont :: Expr -> Parser Expr
termCont acc = ( do 
                  char '*'
                  f <- factor  
                  termCont (Mul acc f)
                )
                <|> ( do
                        char '/'
                        f <- factor
                        termCont (Div acc f)
                    )
                <|> ( do
                        char '%'
                        f <- factor
                        termCont (Rem acc f)
                    )
                <|> return acc

factor :: Parser Expr
factor = ( do 
            n <- natural
            return (Num n)
          )
          <|> ( do 
                v <- variable
                return (Var v)
              )

          <|> ( do 
                char '('
                e <- expr
                char ')'
                return e
              )

natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit)
             return (read xs)

variable :: Parser String
variable = do xs <- many1 (satisfy isAlpha)
              return xs

data Command = Assign Name Expr
              | ExprOnly Expr
              deriving Show

command :: Parser Command
command = do
            v <- variable
            char '='
            e <- expr
            return (Assign v e)
        <|> do
          e <- expr
          return (ExprOnly e)

----------------------------------------------------------------             

main :: IO ()
main
  = do txt <- getContents
       calculator [] (lines txt)

-- | read-eval-print loop
calculator :: Env -> [String] -> IO ()
calculator env []  = return ()
calculator env (l:ls) = 
  let (out, env2) = execute env l
  in
    do 
      putStrLn out
      calculator env2 ls  

execute :: Env -> String -> (String, Env)
execute env txt =
  case parse command txt of
    [(ExprOnly e, "")] ->
      let val = eval env e
      in (show val, env)
    
    [(Assign v e, "")] ->
      let val = eval env e
          env' = updateEnv v val env
      in (show val, env')
    _ ->
      ("parse error; try again", env)

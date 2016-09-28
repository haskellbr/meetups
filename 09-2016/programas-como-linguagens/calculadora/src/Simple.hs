module Main where

import Control.Exception
import Control.Monad
import System.IO (hSetBuffering, stdout, BufferMode(..))

data Expr = Soma Expr Expr
          | Mult Expr Expr
          | Lit Int
  deriving(Read, Show)

evalExpr :: Expr -> Int
evalExpr e = case e of
    Lit i -> i
    Soma e1 e2 -> evalExpr e1 + evalExpr e2
    Mult e1 e2 -> evalExpr e1 * evalExpr e2

parseExpr :: String -> Expr
parseExpr str = read str

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    forever $ do
        putStr "> "
        exprStr <- getLine
        print (evalExpr (read exprStr)) -- May throw

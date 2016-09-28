{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           System.IO (hFlush, hSetBuffering, stdout, BufferMode(..))

data Expr = Soma Expr Expr
          | Mult Expr Expr
          | Lit Int
          | Var String
          | Dec String Expr
  deriving(Read, Show)

type IState = Map String Int

evalExpr
    :: ( MonadState IState m
       , MonadError String m
       , Monad m
       )
    => Expr
    -> m Int
evalExpr e = case e of
    Lit i -> return i
    Soma e1 e2 -> do
        r1 <- evalExpr e1
        r2 <- evalExpr e2
        return (r1 + r2)
    Mult e1 e2 -> do
        r1 <- evalExpr e1
        r2 <- evalExpr e2
        return (r1 * r2)
    Var n -> do
        variableMap <- get
        case Map.lookup n variableMap of
            Nothing -> throwError "Variable not defined"
            Just i -> return i
    Dec n e -> do
        i <- evalExpr e
        modify (Map.insert n i)
        return i

parseExpr :: String -> Expr
parseExpr str = read str

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    let initialState = Map.empty
    void $ runStateT loop initialState
  where
    loop :: StateT (Map String Int) IO ()
    loop = do
        s <- get
        exprStr <- lift $ do
            putStrLn $ show s
            putStr "> "
            hFlush stdout
            getLine
        case exprStr of
            "" -> return ()
            _ -> do
                eret <- runExceptT (evalExpr (read exprStr))
                lift $ print eret
        loop

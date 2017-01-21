{-# LANGUAGE ExistentialQuantification #-}

module Language.Grass.Interpreter
    (
    ) where

import Data.Char
import Data.Word

import Control.Monad.Except

data Pos = forall p. Show p => Pos p

data RuntimeError = RuntimeError {
        errPos :: Pos,
        errMsg :: String
    }

type VM = ExceptT RuntimeError IO

type Index = Int
type Arity = Int
type Code  = [Inst]
data Inst  = App Pos Index Index
           | Abs Pos Arity Code

type Name  = String
type Env   = [Value]
data Value = Char Word8
           | Prim Name (Value -> VM Value)
           | Clos Env Code

data VMState = VMState {
        code :: Code,
        env  :: Env,
        dump :: [(Code, Env)]
    }

vmPos :: Pos
vmPos = Pos "VM"

valId :: Value
valId = Clos [] []

valFalse :: Value
valFalse = Clos [] [Abs vmPos 1 []]

valTrue :: Value
valTrue = Clos [valId] [Abs vmPos 1 [App vmPos 2 1]]

valLowerW :: Value
valLowerW = Char 119

valIn :: Value
valIn = Prim "IN" (\_ -> undefined)

valOut :: Value
valOut = Prim "OUT" (\val -> case val of
        Char c -> liftIO (putChar (chr $ fromEnum c)) >> return val
        _      -> undefined
    )

valSucc :: Value
valSucc = Prim "SUCC" (\val -> case val of
        Char c -> return $ Char (succ c)
        _      -> undefined
    )

initEnv :: Env
initEnv = [valOut, valSucc, valLowerW, valIn]

initDump :: [(Code, Env)]
initDump = [([App vmPos 0 0], []), ([], [])]

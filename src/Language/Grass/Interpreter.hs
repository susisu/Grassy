{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Grass.Interpreter
    (
    ) where

import Data.Char
import Data.Word
import Control.Exception
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


closId :: Value
closId = Clos [] []

closFalse :: Value
closFalse = Clos [] [Abs vmPos 1 []]

closTrue :: Value
closTrue = Clos [closId] [Abs vmPos 1 [App vmPos 2 1]]


charLowerW :: Value
charLowerW = Char 119


charToWrod8 :: Char -> Word8
charToWrod8 c = toEnum (ord c `mod` 0x100)

word8ToChar :: Word8 -> Char
word8ToChar w = chr $ fromEnum w

periodicSucc :: Word8 -> Word8
periodicSucc w
    | w == maxBound = minBound
    | otherwise     = w + 1

optionIO :: a -> IO a -> IO a
optionIO x m = m `catch` (\(e :: IOException) -> return x)

primIn :: Value
primIn = Prim "IN" (\val -> liftIO (optionIO val (Char . charToWrod8 <$> getChar)))

primOut :: Value
primOut = Prim "OUT" (\val -> case val of
        Char w -> liftIO (putChar $ word8ToChar w) >> return val
        _      -> throwError $ RuntimeError (Pos "VM::OUT") "not a character"
    )

primSucc :: Value
primSucc = Prim "SUCC" (\val -> case val of
        Char w -> return $ Char (periodicSucc w)
        _      -> throwError $ RuntimeError (Pos "VM::SUCC") "not a character"
    )


initEnv :: Env
initEnv = [primOut, primSucc, charLowerW, primIn]

initDump :: [(Code, Env)]
initDump = [([App vmPos 0 0], []), ([], [])]

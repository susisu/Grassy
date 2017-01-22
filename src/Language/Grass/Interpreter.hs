{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Grass.Interpreter
    (
    ) where

import Data.Char
import Data.Word
import Control.Exception
import Control.Monad.Except
import Safe

data Pos = forall p. Show p => Pos p

instance Show Pos where
    show (Pos p) = show p

data RuntimeError = RuntimeError Pos String

instance Show RuntimeError where
    show (RuntimeError pos msg) = "RuntimeError at " ++ show pos ++ ":\n" ++ msg

type VM = ExceptT RuntimeError IO

type Index = Int
type Arity = Int
type Code  = [Inst]
data Inst  = App Pos Index Index
           | Abs Pos Arity Code

type Env   = [Value]
data Value = Char Word8
           | Prim (Value -> VM Value)
           | Clos Env Code

data VMState = VMState Code Env [(Code, Env)]

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
primIn = Prim (\x -> liftIO (optionIO x (Char . charToWrod8 <$> getChar)))

primOut :: Value
primOut = Prim (\x -> case x of
        Char w -> liftIO (putChar $ word8ToChar w) >> return x
        _      -> throwError $ RuntimeError vmPos "OUT: not a character"
    )

primSucc :: Value
primSucc = Prim (\x -> case x of
        Char w -> return $ Char (periodicSucc w)
        _      -> throwError $ RuntimeError vmPos "SUCC: not a character"
    )


initEnv :: Env
initEnv = [primOut, primSucc, charLowerW, primIn]

initDump :: [(Code, Env)]
initDump = [([App vmPos 0 0], []), ([], [])]


envAt :: Pos -> Env -> Int -> VM Value
envAt pos env i = case env `atMay` i of
    Nothing -> throwError $ RuntimeError pos ("index out of range: " ++ show (i + 1))
    Just x  -> return x

eqChar :: Word8 -> Value -> Value
eqChar w (Char v)
    | w == v    = closTrue
    | otherwise = closFalse
eqChar _ _ = closFalse

eval :: VMState -> VM Value
eval (VMState [] (x : []) []) = return x
eval (VMState [] (x : _) ((c', e') : d)) = eval (VMState c' (x : e') d)
eval (VMState (App pos m n : c) e d) = do
    f <- envAt pos e m
    a <- envAt pos e n
    case f of
        Char w     -> eval (VMState c (eqChar w a : e) d)
        Prim p     -> p a >>= \x -> eval (VMState c (x : e) d)
        Clos em cm -> if null c
            then eval (VMState cm (a : em) d)
            else eval (VMState cm (a : em) ((c, e) : d))
eval (VMState (Abs pos n c' : c) e d)
    | n == 0    = eval (VMState c (Clos e c' : e) d)
    | otherwise = eval (VMState c (Clos e [Abs pos (n - 1) c'] : e) d)
eval _ = throwError $ RuntimeError vmPos "unknown VM state"

run :: Code -> IO (Either RuntimeError Value)
run code = do
    let initState = VMState code initEnv initDump
    runExceptT $ eval initState

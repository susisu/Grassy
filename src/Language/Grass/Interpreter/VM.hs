{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Grass.Interpreter.VM
    (
    ) where

import qualified Data.ByteString as BS
import Data.Word
import Control.Monad.Except
import Safe
import System.IO

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


getWord8 :: IO (Maybe Word8)
getWord8 = do
    b <- BS.hGet stdin 1
    if BS.null b
        then return Nothing
        else return $ Just (BS.head b)

putWord8 :: Word8 -> IO ()
putWord8 w = BS.hPut stdout (BS.singleton w)

periodicSucc :: Word8 -> Word8
periodicSucc w
    | w == maxBound = minBound
    | otherwise     = w + 1

primIn :: Value
primIn = Prim op
    where
        op x = maybe x Char <$> liftIO getWord8

primOut :: Value
primOut = Prim op
    where
        op x@(Char w) = liftIO (putWord8 w) >> return x
        op _          = throwError $ RuntimeError vmPos "OUT: not a character"

primSucc :: Value
primSucc = Prim op
    where
        op (Char w) = return $ Char (periodicSucc w)
        op _        = throwError $ RuntimeError vmPos "SUCC: not a character"

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
        Clos em cm -> if null c && not (null d)
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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}

module Language.Grass.Interpreter.VM
  ( Pos(..)
  , Inst(..)
  , RuntimeError(..)
  , Code
  , run
  ) where

import           Control.Monad.Except
import qualified Data.ByteString      as BS
import           Data.Word
import           Safe
import           System.IO

-- instructions
data Pos = forall p. Show p => Pos p

instance Show Pos where
  show (Pos p) = show p

data Inst =
    App Pos Int Int  -- application
  | Abs Pos Int Code -- abstraction

-- values
data Value =
    Char Word8               -- character
  | Prim (Value -> VM Value) -- primitive operation
  | Clos Env Code            -- closure

-- VM
data RuntimeError = RuntimeError Pos String

instance Show RuntimeError where
  show (RuntimeError pos msg) = show pos ++ ":\n" ++ msg

type VM = ExceptT RuntimeError IO

type Code = [Inst]

type Env = [Value]

data VMState = VMState Code Env [(Code, Env)]

-- dummy position
vmPos :: Pos
vmPos = Pos "VM"

-- boolean values
closFalse :: Value
closFalse = Clos [] [Abs vmPos 1 []]

closTrue :: Value
closTrue = Clos [closId] [Abs vmPos 1 [App vmPos 2 1]]
  where
    closId = Clos [] []

-- character 'w'
charLowerW :: Value
charLowerW = Char 119

-- primitive operations
getWord8 :: IO (Maybe Word8)
getWord8 = do
    b <- BS.hGet stdin 1
    if BS.null b then
      return Nothing
    else
      return $ Just (BS.head b)

putWord8 :: Word8 -> IO ()
putWord8 w = BS.hPut stdout (BS.singleton w)

periodicSucc :: Word8 -> Word8
periodicSucc w
    | w == maxBound = minBound
    | otherwise = w + 1

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

-- evaluation
-- get a value in the environment
envAt :: Pos -> Env -> Int -> VM Value
envAt pos env i = case env `atMay` i of
    Nothing -> throwError $ RuntimeError pos ("index out of range: " ++ show (i + 1))
    Just x  -> return x

-- compare characters
eqChar :: Word8 -> Value -> Value
eqChar w (Char v)
    | w == v = closTrue
    | otherwise = closFalse
eqChar _ _ = closFalse

-- set error position
withPos :: Pos -> VM a -> VM a
withPos pos = withExceptT setPos
  where
    setPos (RuntimeError _ msg) = RuntimeError pos msg

eval :: VMState -> VM Value
eval (VMState [] [x] []) =
    -- terminate
    return x
eval (VMState [] (x : _) ((c', e') : d)) =
    -- back to the call site
    eval (VMState c' (x : e') d)
eval (VMState (App pos m n : c) e d) = do
    f <- envAt pos e m -- function
    a <- envAt pos e n -- argument
    case f of
      Char w ->
        -- compare the character with the argument
        let x = eqChar w a in
        eval (VMState c (x : e) d)
      Prim op -> do
        -- execute the primitive operation
        x <- withPos pos (op a)
        eval (VMState c (x : e) d)
      Clos em cm ->
        -- call the function
        if null c && not (null d) then
          -- tail call optimization
          eval (VMState cm (a : em) d)
        else
          eval (VMState cm (a : em) ((c, e) : d))
eval (VMState (Abs pos n c' : c) e d) =
    -- create a closure
    let
      clos
        | n <= 1 = Clos e c'
        | otherwise = Clos e [Abs pos (n - 1) c']
    in
    eval (VMState c (clos : e) d)
eval _ = throwError $ RuntimeError vmPos "unknown VM state"

run :: Code -> IO (Either RuntimeError ())
run code = runExceptT $ do
    let initState = VMState code initEnv initDump
    _ <- eval initState
    return ()
  where
    initEnv = [primOut, primSucc, charLowerW, primIn]
    initDump = [([App vmPos 0 0], []), ([], [])]

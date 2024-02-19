module BFLib
  ( strip,
    construct,
    simplify,
    runProg,
    instrToString,
  )
where

import Data.Array.IO
import Data.Char
import Data.IORef
import Data.Word
import System.IO

data Instr a = Move Int | Add a | Output | Input | While [Instr a] deriving (Show, Eq)

-- functions for constructing an AST
toInstr :: (Num a) => Char -> Instr a
toInstr '>' = Move 1
toInstr '<' = Move (-1)
toInstr '+' = Add 1
toInstr '-' = Add (-1)
toInstr '.' = Output
toInstr ',' = Input
toInstr x = error ("unrecognised character: " ++ [x])

-- this is ass but I just wanted to get it to work
construct :: (Num a) => String -> [Instr a]
construct [] = []
construct s = leftInstrs ++ midInstrs ++ construct remainder
    where
        leftInstrs = map toInstr $ takeWhile (/= '[') s
        removeOuter = reverse . drop 1 . reverse . drop 1
        midString = matchBracket $ drop (length leftInstrs) s
        midInstrs = [While (construct (removeOuter midString)) | not (null midString)]
        remainder = drop (length leftInstrs + length midString) s

matchBracket :: String -> String
matchBracket s = findBr (s, 0)
    where
        findBr :: (String, Int) -> String
        findBr ('[' : xs, v) = '[' : findBr (xs, succ v)
        findBr (']' : _, 1) = "]"
        findBr (']' : xs, v) = ']' : findBr (xs, pred v)
        findBr (_, 0) = []
        findBr ([], _) = []
        findBr (x : xs, v) = x : findBr (xs, v)

simplify :: (Num a) => [Instr a] -> [Instr a]
simplify i = reverse $ foldl absorb [] i

absorb :: (Num a) => [Instr a] -> Instr a -> [Instr a]
absorb (Move x : xs) (Move v) = Move (x + v) : xs
absorb (Add x : xs) (Add v) = Add (x + v) : xs
absorb xs (While e) = While (simplify e) : xs
absorb xs v = v : xs

-- functions for reproducing the original string from an [Instr]
instrToString :: (Integral a) => [Instr a] -> String
instrToString = concatMap toStr

repeatStr :: (Integral a) => a -> String -> String
repeatStr v = concat . replicate (fromIntegral v)

toStr :: (Integral a) => Instr a -> String
toStr (Move v)
    | v > 0 = repeatStr v ">"
    | v < 0 = repeatStr (-v) "<"
    | otherwise = " "
toStr (Add v)
    | v > 0 = repeatStr v "+"
    | v < 0 = repeatStr (-v) "-"
    | otherwise = " "
toStr Output = "."
toStr Input = ","
toStr (While v) = "[" ++ concatMap toStr v ++ "]"

-- functions for running input
-- these are hardcoded to use Word8 due to limitations with polymorphic arrays
type State a = (IOUArray Int a, IORef Int)

strip :: String -> String
strip = filter (`elem` "><+-.,[]")

runInstr :: State Word8 -> Instr Word8 -> IO ()
runInstr (arr, p) i = case i of
    Move v -> do
        old <- readIORef p
        writeIORef p (old + v)
    Add v -> do
        point <- readIORef p
        old <- readArray arr point
        writeArray arr point (old + v)
    Output -> do
        hFlush stdout
        point <- readIORef p
        val <- readArray arr point
        putChar (chr (fromIntegral val))
    Input -> do
        hFlush stdout
        point <- readIORef p
        char <- getChar
        writeArray arr point (fromIntegral (fromEnum char))
    While v -> do
        point <- readIORef p
        val <- readArray arr point
        if val == 0
            then return ()
            else do
                runProg (arr, p) v
                runInstr (arr, p) i

runProg :: State Word8 -> [Instr Word8] -> IO ()
runProg (arr, p) = mapM_ (runInstr (arr, p))

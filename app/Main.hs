
{-
This program:
Constructs an AST consisting of a list of instructions
Maps each to an IO action
Sequences those actions
-}

module Main (main) where

import BFLib
import System.Environment
import System.IO
import Data.Array.IO
import Data.Word
import Data.IORef
import Control.Exception

memsize :: Int
memsize = 30000

main :: IO ()
main = do
    args <- getArgs
    case args of
        (file:_) -> runFile file
        _ -> putStrLn "Usage: bf-hs [filename]"


runFile :: String -> IO ()
runFile file = do
    fileHandle <- openFile file ReadMode
    contents <- hGetContents fileHandle

    -- state
    arr <- newArray (0, memsize) 0 :: IO (IOUArray Int Word8)
    p <- newIORef 0 :: IO (IORef Int)

    -- the program
    let text = strip contents
        prog = simplify $ construct text

    runProg (arr, p) prog

    hClose fileHandle
    

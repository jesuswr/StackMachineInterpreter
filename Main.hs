module Main where

import System.Environment
import System.Directory

import qualified Data.Map as M


import qualified MiniParser as P
import qualified StackMachine as SM

main = do
    args <- getArgs
    case args of
        [fileName] -> 
            go fileName
        _          -> 
            error "Error in the values passed by command line, only file name expected"


-- receives file name
go :: String -> IO()
go fileName = do
    exists <- doesFileExist fileName
    if not $ exists then
        error "The given file doesnt exist"
    else do
        _input <- readFile fileName
        let input = map words $ lines _input
        let (toks, labelMp) = P.parse input
        SM.runStackMachine toks toks [] M.empty labelMp
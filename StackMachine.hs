module StackMachine where

import qualified Data.Map as M

data Instructions = 
    Push {val :: Int}
    | Pop
    | AritBinOp     {aritOp :: (Int -> Int -> Int)}    -- +,-,*,/
    | BoolBinOp     {boolOp :: (Bool -> Bool -> Bool)} -- and or
    | RelBinOp      {relOp :: (Int -> Int -> Bool)}   -- < <= > >= == !=
    | UMinus
    | Not
    | RValue        {varName :: String}
    | LValue        {varName :: String}
    | Assign
    | Go            {label :: String}
    | GoCond        {label :: String, cond :: (Bool -> Bool)}
    | Read          {varName :: String}
    | Print         {varName :: String}
    | Exit
--  | Reset         Ignoring this because of endless loop

instance Show Instructions where
    show (Push v)       = "Push " ++ (show v)
    show Pop            = "Pop"
    show (AritBinOp f)  = "AritBinOp " ++ "3 op 2 = " ++ (show (f 3 2))
    show (BoolBinOp f)  = "BoolBinOp " ++ "true op false = " ++ (show (f True False))
    show (RelBinOp f)   = "RelBinOp " ++ "1 op 0 = " ++ (show (f 1 0)) ++ "; 1 op 1 = " ++ (show (f 1 1))
    show UMinus         = "UMinus"
    show Not            = "Not"
    show (RValue s)     = "RValue " ++ s
    show (LValue s)     = "LValue " ++ s
    show Assign         = "Assign"
    show (Go s)         = "Go " ++ s
    show (GoCond s f)   = "GoCond " ++ s ++ " f(true) = " ++ (show (f True))
    show (Read s)       = "Read " ++ s
    show (Print s)      = "Print" ++ s
    show Exit           = "Exit"


type LabelToInd = M.Map String Int
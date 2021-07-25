module MiniParser where

import qualified StackMachine as SM

import qualified Data.Map as M

parse :: [[String]] -> ([SM.Instructions], SM.LabelToInd)
parse s = _parse s 0

_parse :: [[String]] -> Int -> ([SM.Instructions], SM.LabelToInd)
_parse [] _ = ([], M.empty)
_parse (x:xs) i
    | null x            = 
        _parse xs i
    | (isLabel $ head x) && (null $ tail x) =
        (instrs2, M.insert (init $ head x) i mp2) 
    | isLabel $ head x  = 
        (getInstruction (tail x):instrs, M.insert (init $ head x) i mp)    
    | otherwise         = 
        (getInstruction x:instrs, mp)
    where
        (instrs, mp) = _parse xs (i+1)
        (instrs2, mp2) = _parse xs i

isLabel :: String -> Bool
isLabel s = last s == ':'

getInstruction :: [String] -> SM.Instructions
getInstruction ("PUSH":"true":[])   = SM.Push (SM.Boolean True)
getInstruction ("PUSH":"false":[])  = SM.Push (SM.Boolean False)
getInstruction ("PUSH":v:[])        = SM.Push (SM.Num $ read v)
getInstruction ("POP":[])           = SM.Pop 
getInstruction ("ADD":[])           = SM.AritBinOp (+) 
getInstruction ("SUB":[])           = SM.AritBinOp (-) 
getInstruction ("MUL":[])           = SM.AritBinOp (*) 
getInstruction ("DIV":[])           = SM.AritBinOp (div) 
getInstruction ("AND":[])           = SM.BoolBinOp (&&)
getInstruction ("OR":[])            = SM.BoolBinOp (||)
getInstruction ("LT":[])            = SM.RelBinOp (<)
getInstruction ("LE":[])            = SM.RelBinOp (<=)
getInstruction ("GT":[])            = SM.RelBinOp (>)
getInstruction ("GE":[])            = SM.RelBinOp (>=)
getInstruction ("EQ":[])            = SM.Eq 
getInstruction ("NEQ":[])           = SM.NotEq
getInstruction ("UMINUS":[])        = SM.UMinus
getInstruction ("NOT":[])           = SM.Not
getInstruction ("RVALUE":name:[])   = SM.RValue name
getInstruction ("LVALUE":name:[])   = SM.LValue name
getInstruction ("ASSIGN":[])        = SM.Assign
getInstruction ("GOTO":lab:[])      = SM.Go lab
getInstruction ("GOTRUE":lab:[])    = SM.GoCond lab (== True)
getInstruction ("GOFALSE":lab:[])   = SM.GoCond lab (== False)
getInstruction ("READ":name:[])     = SM.Read name
getInstruction ("PRINT":name:[])    = SM.Print name
getInstruction ("EXIT":[])          = SM.Exit
getInstruction s                    = error $ "Error parsing: " ++ (unwords s)

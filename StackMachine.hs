module StackMachine where

import qualified Data.Map as M

import Data.Char

data Instructions = 
    Push            {val :: StackElements}
    | Pop
    | AritBinOp     {aritOp :: (Int -> Int -> Int)}    -- +,-,*,/
    | BoolBinOp     {boolOp :: (Bool -> Bool -> Bool)} -- and or
    | RelBinOp      {relOp :: (Int -> Int -> Bool)}   -- < <= > >= == !=
    | Eq         
    | NotEq         
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

type LabelToInd = M.Map String Int
type IdToVal = M.Map String StackElements

data StackElements = 
    Num Int
    | Boolean Bool
    | LVal String
    deriving (Eq, Show)

type Stack = [StackElements]

-- asumi que un id puede apuntar a un lvalor, que lvalor == lvalor es valido, que 
-- gotrue y gofalse no hacen pop del stack, que el read solo funciona para int y bool,
-- que reset no debe estar ya que si esta seria un ciclo infinito

-- Param:
--      Complete Instruction List
--      A suffix of the Complete Instruction List, that represents the instructions left
--      Stack of values
--      Map to save values of ids
--      Map to save ind of labels
runStackMachine :: [Instructions] -> [Instructions] -> Stack -> IdToVal -> LabelToInd -> IO()
runStackMachine _ [] _ _ _ = return ()
runStackMachine a (i:is) st idMp labelMp = do
    case i of
        Push v          -> 
            runStackMachine a is (v:st) idMp labelMp

        Pop             -> do
            if null st then do
                putStrLn $ "Error: pop operation with an empty stack on instrucion number " ++ show((length a) - (length is))
                runStackMachine a is st idMp labelMp
            else do
                runStackMachine a is (tail st) idMp labelMp

        AritBinOp f     ->
            case st of
                ((Num x):(Num y):sts) ->
                    runStackMachine a is (Num (f y x):sts) idMp labelMp
                _                     -> do
                    putStrLn $ "Error: artihmetic operation without enough values or with booleans/lvalues on instrucion number " ++ show((length a) - (length is))
                    runStackMachine a is st idMp labelMp

        BoolBinOp f     -> 
            case st of
                ((Boolean x):(Boolean y):sts)   ->
                    runStackMachine a is (Boolean (f y x):sts) idMp labelMp
                _                               -> do
                    putStrLn $ "Error: boolean operation without enough values or with integers/lvalues on instrucion number " ++ show((length a) - (length is))
                    runStackMachine a is st idMp labelMp

        RelBinOp f      ->
            case st of
                ((Num x):(Num y):sts) ->
                    runStackMachine a is (Boolean (f y x):sts) idMp labelMp
                _                     -> do
                    putStrLn $ "Error: relational operation without enough values or with booleans/lvalues on instrucion number " ++ show((length a) - (length is))
                    runStackMachine a is st idMp labelMp

        Eq              ->
            case st of
                ((Num x):(Num y):sts)           ->
                    runStackMachine a is (Boolean (y == x):sts) idMp labelMp
                ((Boolean x):(Boolean y):sts)   ->
                    runStackMachine a is (Boolean (y == x):sts) idMp labelMp
                ((LVal x):(LVal y):sts)         ->
                    runStackMachine a is (Boolean (y == x):sts) idMp labelMp
                _                               -> do
                    putStrLn $ "Error: eq operation without enough values " ++ show((length a) - (length is))
                    runStackMachine a is st idMp labelMp

        NotEq           ->
            case st of
                ((Num x):(Num y):sts)           ->
                    runStackMachine a is (Boolean (y /= x):sts) idMp labelMp
                ((Boolean x):(Boolean y):sts)   ->
                    runStackMachine a is (Boolean (y /= x):sts) idMp labelMp
                ((LVal x):(LVal y):sts)         ->
                    runStackMachine a is (Boolean (y /= x):sts) idMp labelMp
                _                               -> do
                    putStrLn $ "Error: neq operation without enough values " ++ show((length a) - (length is))
                    runStackMachine a is st idMp labelMp

        UMinus          ->
            case st of
                ((Num x):sts)       ->
                    runStackMachine a is (Num (-x):sts) idMp labelMp
                _                   -> do
                    putStrLn $ "Error: uminus operation without enough values or with booleans/lvalues on instrucion number " ++ show((length a) - (length is))
                    runStackMachine a is st idMp labelMp

        Not             ->
            case st of
                ((Boolean x):sts)   ->
                    runStackMachine a is (Boolean (not x):sts) idMp labelMp
                _                   -> do
                    putStrLn $ "Error: not operation without enough values or with integers/lvalues on instrucion number " ++ show((length a) - (length is))
                    runStackMachine a is st idMp labelMp

        RValue s        -> 
            case M.lookup s idMp of
                Just sVal   ->
                    runStackMachine a is (sVal:st) idMp labelMp
                Nothing     -> do
                    putStrLn $ "Error: rvalue operation with unassigned id on instrucion number " ++ show((length a) - (length is))
                    runStackMachine a is st idMp labelMp
        
        LValue s        -> 
            runStackMachine a is (LVal s:st) idMp labelMp
        
        Assign          ->
            case st of
                ((LVal s):x:sts)            ->
                    runStackMachine a is sts (M.insert s x idMp) labelMp
                _                           -> do
                    putStrLn $ "Error: Assign operation without enough values on instrucion number " ++ show((length a) - (length is))
                    runStackMachine a is st idMp labelMp
        
        Go s            -> 
            case M.lookup s labelMp of
                Just ind    ->
                    runStackMachine a (drop ind a) st idMp labelMp
                Nothing     -> do
                    putStrLn $ "Error: goto operation with unexisting label on instrucion number " ++ show((length a) - (length is))
                    runStackMachine a is st idMp labelMp
       
        GoCond s f      -> 
            case M.lookup s labelMp of
                Just ind    ->
                    if null st then do
                        putStrLn $ "Error: gotrue/gofalse operation with empty stack on instrucion number " ++ show((length a) - (length is))
                        runStackMachine a is st idMp labelMp
                    else
                        case head st of
                            Boolean x ->
                                if f x then
                                    runStackMachine a (drop ind a) st idMp labelMp
                                else
                                    runStackMachine a is st idMp labelMp
                            _         -> do
                                putStrLn $ "Error: gotrue/gofalse operation without boolean on stack on instrucion number " ++ show((length a) - (length is))
                                runStackMachine a is st idMp labelMp
                Nothing     -> do
                    putStrLn $ "Error: gotrue/gofalse operation with unexisting label on instrucion number " ++ show((length a) - (length is))
                    runStackMachine a is st idMp labelMp

        Read s          -> do 
            putStrLn $ "Give a value for variable: " ++ s
            inp <- getLine
            case words inp of
                ("true":[])     ->
                    runStackMachine a is st (M.insert s (Boolean True) idMp) labelMp
                ("false":[])    ->
                    runStackMachine a is st (M.insert s (Boolean False) idMp) labelMp
                (v:[])          ->
                    if validNum v then
                        runStackMachine a is st (M.insert s (Num (read v)) idMp) labelMp
                    else do
                        putStrLn $ "Error: read operation with unknow input format on instrucion number " ++ show((length a) - (length is))
                        runStackMachine a is st idMp labelMp
                _               -> do
                    putStrLn $ "Error: read operation with unknow input format on instrucion number " ++ show((length a) - (length is))
                    runStackMachine a is st idMp labelMp

        Print s         -> 
            case M.lookup s idMp of
                Just val    -> do
                    putStrLn $ "Print operation: " ++ s ++ " = " ++ (show val)
                    runStackMachine a is st idMp labelMp
                Nothing     -> do
                    putStrLn $ "Error: print operation with unexisting label on instrucion number " ++ show((length a) - (length is))
                    runStackMachine a is st idMp labelMp
        Exit            -> 
            return ()


validNum :: String -> Bool
validNum ('-':n) = all isDigit n
validNum n = all isDigit n
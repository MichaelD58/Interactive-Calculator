module REPL where

import Control.Monad.IO.Class

import Data.Either
import Data.List

import System.IO
import System.Directory
import System.Console.Haskeline

import Expr
import Parsing
import EitherHandling
import BinarySearchTree 

data State = State { vars :: BiTree String Float,
                     history :: [Command] }

initState :: State
initState = State Leaf []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Float -> BiTree String Float -> BiTree String Float
updateVars name float vars = insertBST vars (name, float)

-- Add a command to the command history in the state
addHistory :: State -> Command -> State
addHistory state command = State {vars = vars state,  history = (history state ++ [command]) }

runFile :: Name -> State -> IO ()
runFile file st = runInputTBehavior (useFile file) defaultSettings (repl st)

processEval :: State -> Command -> InputT IO () 
processEval st (Eval e) = 
     let computation = (eval (vars st) e) in 
          case isRight computation of 
               True -> do
                    let st' = addHistory st (Eval e) in
                         let value = (fromRight 0 computation) in
                              let new_vars = updateVars "it" (value) (vars st') in
                                   let st'' = State {vars = new_vars, history = history st'} in do
                                        let result = show(value)
                                        if (last result) == '0' && result!!((length result) - 2) == '.' then
                                             outputStrLn(show (round value))
                                        else do
                                             outputStrLn(show value)
                                        repl st''
               False -> do
                    outputStrLn(fromLeft "?" computation)
                    repl st

process :: State -> Command -> InputT IO ()
process st (Set var e) =
     do let st' = addHistory st (Set var e) in
          let value = fromRight 0 (eval (vars st') e) in 
               let new_vars = updateVars var (value) (vars st') in
                    let st'' = State {vars = new_vars, history = history st'} in do
                         outputStrLn("OK")
                         repl st''
process st (Eval e) = 
     processEval st (Eval e)
process st (Quit) = 
     outputStrLn("Bye")
process st (Repeat i) = 
     do 
          if length (history st) <= i then do
               outputStrLn("Invalid history fetch") 
               repl st
               else 
                    process st ((history st) !! i)
process st (File i)
     = do x <- liftIO(doesFileExist i)
          if x then do outputStrLn "Reading file"
                       liftIO(runFile i st)
          else do outputStrLn ("'" ++ i ++ "'" ++ " does not exist")
                  repl st  
process st Help = 
     do
          outputStrLn("These are the supported operations:")
          outputStrLn("    x + y : addition")
          outputStrLn("    x - y : subtraction")
          outputStrLn("    x * y : multiplication")
          outputStrLn("    x / y : division")
          outputStrLn("    x % y : modulo (x and y will be truncated)")
          outputStrLn("    x \\ y : div (x and y will be truncated)")
          outputStrLn("    | x | : abs")
          outputStrLn("    x ^ y : power of (y will be truncated)")
          outputStrLn("    x = y : variable assignment")
          outputStrLn("    it    : holds the result of the last calulation")
          outputStrLn("These are the non-arithmetic commands:")
          outputStrLn("    !n          : repeats the nth command")
          outputStrLn("    :q          : quits the program")
          outputStrLn("    :f filename : reads the file")
          outputStrLn("    :h          : displays the help message")
          outputStrLn("Inline comments are denoted with '#' and continue to the end of the line")
          repl st


removeComments :: Maybe String -> String 
removeComments (Just []) = []
removeComments (Just ('#':_)) = [] 
removeComments (Just (x:xs)) = x : (removeComments (Just xs))
removeComments Nothing = [] 

clean :: Maybe String -> Maybe String 
clean Nothing = Nothing
clean inp = Just (filter (/=' ') (removeComments inp))

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.
repl :: State -> InputT IO ()
repl st = do inp <- getInputLine (show (length (history st)) ++ " > ")
             case (clean inp) of
                  (Just x) -> do case parse pCommand x of
                                   [(cmd, "")] -> 
                                        process st cmd
                                   _ -> do outputStrLn "Parse Error"
                                           repl st
                  Nothing -> do outputStrLn "End of file"

module Main where

import Syntax
import Parser
import Eval
import Pretty

import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline
import qualified Data.Map as Map

-- 1
-- на вході ім"я файлу котрий містить програму: об"яви + вираз
--  вводиться файл -- аналізується 
--  якщо немає промилок  -- виконується вираз!! 
process :: String -> IO ()
process line = do
--  let fname = (reverse(tail(reverse line)))
  let fname = reverse $ tail $ reverse line
--  print fname
  text <- readFile fname
--  putStrLn "==input============================================"
--  sequence_ (map putStrLn (lines text))
--  putStrLn "==end input========================================"
  let res = parseTerm text 
  case res of 
    Left err  -> print err 
    Right prog@(Program ctx te)  -> do 
 --       print (  prog)
        sequence_ (map putStrLn (ppprog  prog))
 --       print te
 --       evalStep 30 prog
 --       eval 500 prog

--  покрокове обчислення і друк 
evalStep :: Int -> Program -> IO()
evalStep mx prog@(Program ctx te) = do
    let (t,stl, tl, res) = runEvalStep mx prog
    let steplist = map ppstep (reverse stl)
    let stepterm = map ppterm (reverse tl)
    let redlist  = concat [["     " ++ s,t] | (s,t) <- (zip steplist stepterm)]
    sequence_ (map putStrLn redlist)
--    putStrLn ("==" ++ (ppterm t))
    if res == "Max" then putStrLn ("Abort: all step = " ++ (show mx)) else do
      putStrLn ("Step:" ++ (show (length stl)))
      putStrLn (ppterm t)
      putStrLn ("comp: " ++ ppterm(compress ctx t)) 
{-	
      let (tsF,resF) = runFullStep mx (Program ctx t)
      let t1 = if null tsF then t else (head tsF)
      if resF == "No step Full" then  print resF else do
         sequence_ (map putStrLn (map ppterm (reverse tsF)))
         putStrLn (ppterm t1)
      putStrLn (ppterm(compress ctx t1)) 
-}
--  лише обчислення і друк 		
eval :: Int -> Program -> IO()
eval mx prog@(Program ctx te) = do
    let (res,t,i) = runEval mx prog
    putStrLn ("Step " ++ res ++ " " ++ (show i))
    if (res /= "Full") then putStrLn "--" else do 
      putStrLn (ppterm t)
      putStrLn ("comp: " ++ ppterm(compress ctx t))
{-	  
      let (rres,rt, ri) = runFull mx (Program ctx t)
      putStrLn (rres ++ ":" ++ (show ri) ++ ": " ++ (ppterm rt))   
      putStrLn ("compr: " ++ ppterm(compress ctx rt))
-}    

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Exuntyp> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just ('w':_) -> outputStrLn "Goodbye!!"
      Just input -> (liftIO $ process input) >> loop
module Main where

import Syntax
import Parser
import Eval
import Pretty

import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline
import qualified Data.Map as Map
--import Data.List ( foldl)
{-
showStep :: (Int, Int, Expr) -> IO ()
   --showStep :: (Int, String) -> IO ()
--showStep (d, x) = putStrLn ((replicate d ' ') ++ "=> " ++ ppexpr x)
showStep (d,c, x) = putStrLn ((replicate d ' ') ++ ":" ++ (show c) ++ "=> " ++ show x)
--showStep (d, x) = putStrLn ((show d ) ++ " => " ++ show x)

showDecls :: [(String, Expr)] -> IO()
showDecls []             = return()
showDecls ((nm,ex): dcl) = do 
         putStrLn  (".." ++ nm ++ "  " ++ (show ex)) --  ppexpr ex)
         showDecls dcl

showEnvShort :: TermEnv -> String
showEnvShort st  = "["++ tail (concat (map showObShort (Map.toList st))) ++"]"

showObShort :: (String,Value) -> String
showObShort (nm,val) = "," ++ nm ++ "=>" ++ (show val) 
-}
-- 1
-- на вході ім"я файлу котрий містить вираз (програму)
--  вводиться файл -- аналізується 
--  якщо немає промилок  -- виконується !! 
process :: String -> IO ()
process line = do
--  let fname = (reverse(tail(reverse line)))
  let fname = reverse $ tail $ reverse line
--  print fname
  text <- readFile fname
--  putStrLn "==input============================================"
--  sequence_ (map putStrLn (lines text))
--  putStrLn "==end input========================================"
   -- Parser ( returns AST )
   
  let res = parseTerm text 
  case res of 
    Left err  -> print err 
    Right prog@(Program ctx te)  -> do 
 --       print (  prog)
        sequence_ (map putStrLn (ppprog  prog))
 --       print te
 --       evalStep 10 prog
        eval 500 prog

--  покрокове обчислення і друк 
--  тимчасово НЕ використовується 		
evalStep :: Int -> Program -> IO()
evalStep mx prog@(Program ctx te) = do
    let (t,stl, tl, res) = runEvalStep mx prog
    let steplist = map ppstep (reverse stl)
    let stepterm = map ppterm (reverse tl)
    let redlist  = concat [["     " ++ s,t] | (s,t) <- (zip steplist stepterm)]
--    let redlist = [(steplist!!i)++ "--->" ++ (stepterm!!i) | i <- [0..(length steplist)-1] ]
    sequence_ (map putStrLn redlist)
--    sequence_ (map putStrLn (map ppstep (reverse stl)))
--    putStrLn ("==" ++ (ppterm t))
    if res == "Max" then putStrLn res else do
      let (tsF,resF) = runFullStep mx (Program ctx t)
      let t1 = if null tsF then t else (head tsF)
      if resF == "No step Full" then  print resF else do
         sequence_ (map putStrLn (map ppterm (reverse tsF)))
         putStrLn (ppterm t1)
      putStrLn (ppterm(compress ctx t1)) 
{-   	
    print (head tsF)
    print (isNumber (head tsF))
    putStrLn (ppterm(head tsF))
    putStrLn (ppterm(compress ctx (head tsF)))
-}
--  лише обчислення і друк 		
eval :: Int -> Program -> IO()
eval mx prog@(Program ctx te) = do
    let (res,t,i) = runEval mx prog
    putStrLn ("Step " ++ (show i) ++ " " ++ res)
    if (res /= "Full") then putStrLn "--" else do 
      putStrLn (ppterm t)
      let (rres,rt, ri) = runFull mx (Program ctx t)
      putStrLn (rres ++ ":" ++ (show ri) ++ ": " ++ (ppterm rt))   
      putStrLn ("compr: " ++ ppterm(compress ctx rt))
    
--        putStrLn (ppterm te)		
--        print te
--        sequence_ (map print ts)
{-	
    Right tm -> do  
       -- print tm
       putStrLn (ppterm tm)
       let ts = runEval tm 
       let vs = map ppterm ts
       sequence_ (map putStrLn vs)
-}
{-	
	do 
    --   print mod 
       putStrLn "---decl------------------------------------"
       showDecls mod 
       putStrLn "---end decl--------------------------------"
  -- Create the new environment
  --     let st = foldl evalDef emptyTmenv mod 
  
       case evalDef emptyTmenv mod of 
          Left err1 -> print err1 
          Right st  -> do
-- !            print st
    --        print (showEnvShort st)			
            -- If a value is entered, print it. 			
            case lookup "it" mod of
              Nothing -> return ()
              Just ex -> do 
                case runEvalStep st "it"  ex of
                  (Right val, (steps, all)) -> do
                       putStrLn ("work App " ++ (show all))
                       print ex
                       mapM_ showStep steps
                       print val
                  (Left er,(_,_))        ->  do 
                      print er
                      print ex
 -}
{-  
evalDef :: TermEnv -> (String, Expr) -> TermEnv
evalDef env (nm, ex) = tmctx
  where (val, tmctx) = runEval env nm ex
-}
{-
evalDef :: TermEnv -> [(String, Expr)] ->  Either String TermEnv 
evalDef tm [] = Right tm 
evalDef tm ((nm,ex): ml) = 
     case tm `seq` runEval tm nm ex of 
      Right (_,tm1) -> evalDef tm1 ml 
      Left  er      -> Left er
-}
{-
evalDef :: TermEnv -> [(String, Expr)] ->  Either String TermEnv 
evalDef tm [] = Right tm 
evalDef tm (("it",_): ml) = evalDef tm ml
evalDef tm ((nm,ex): ml) = 
      case runEval tm nm ex  of 
        Right(_, tm1) -> evalDef tm1 ml
        Left er       -> Left er
-}
{-  
  let res = parseTerm text
  case res of
    Left err -> print err
    Right tm -> do  
       print tm
       print (runEval tm)
-}   
-- 1

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Exuntyp> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just ('w':_) -> outputStrLn "Goodbye!!"
      Just input -> (liftIO $ process input) >> loop
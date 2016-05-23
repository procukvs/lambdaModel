module Eval (
  runEval,
  runEvalStep,
  compress, 
  isNumber  
) where

-- main model for work

import Syntax
import Control.Monad.Except

getBinding :: Contex -> Int -> Binding
getBinding ctx i =
  case ctx !! i of 
   (_, NmBind)   -> NmBind 
   (_, TmBind t) -> TmBind (termShift (i+1) t) 

termShift :: Int -> Term -> Term 
termShift d t  = walk 0 t 
  where 
    walk c (Nmb nm x)     = Nmb nm x 
    walk c (Var nm x n)   = if x >= c then Var nm (x+d) (n+d) 
                                    else Var nm x (n+d)
    walk c (Abs nm t1)    = Abs nm  (walk (c+1) t1)
    walk c (App t1 t2)    = App (walk c t1) (walk c t2)
    walk c (Let nm t1 t2) = Let nm (walk c t1) (walk (c+1) t2)

termSubst :: Int -> Term -> Term -> Term 
termSubst j s t  = walk 0 t 
  where 
    walk c (Nmb nm x )  = Nmb nm x   
    walk c (Var nm x n) = if x == j+c then termShift c s  else Var nm x n
    walk c (Abs nm t1)  = Abs nm  (walk (c+1) t1)
    walk c (App t1 t2)  = App (walk c t1) (walk c t2) 
    walk c (Let nm t1 t2) = Let nm (walk c t1) (walk (c+1) t2)

termSubstTop :: Term -> Term -> Term 
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

isval :: Contex -> Term -> Bool
isval _ (Abs _ _) = True
isval _  _       = False

integerTerm :: Int ->  Term
integerTerm n  = (Abs "s" (Abs "z" (buildTerm n))) 
  where buildTerm 0 = Var "z" 0 2 
        buildTerm n = (App (Var "s" 1 2) (buildTerm (n-1)))  

-- реалізація через хвостову рекурсію 
runEval :: Int -> Program -> (String, Term, Int)
runEval mx (Program ctx et) =  runAll et 0
 where runAll tm i = case eval ctx tm of
                Just t -> if i <= mx then runAll t (i+1) else ("Max", t, i)
                Nothing-> ("Full", tm,i)

-- просто використовую монаду Maybe!!! 
-- Нормальний порядок
eval :: Contex -> Term -> Maybe Term 
eval ctx (Nmb x i )    = Just (integerTerm i) 
eval ctx (Var x i lc ) = case getBinding ctx i of 
                              (TmBind t) -> Just t 
                              _          -> Nothing
eval ctx (App (Abs x t12) t2) = Just (termSubstTop t2 t12)
eval ctx (App t1 t2)   = case eval ctx t1 of 
                              Just v1 -> Just (App v1 t2)
                              Nothing ->  case eval ctx t2 of
                                Just v2 -> Just (App t1 v2) 
                                Nothing -> Nothing 
eval ctx (Let _ t1 t2) = Just (termSubstTop t1 t2)
eval ctx (Abs nm t)    = case eval ((nm,NmBind):ctx) t of
                            Just v -> Just (Abs nm v) 
                            Nothing -> Nothing

-- повертає - крок редукції (Step) + результат (Term)
evalStep :: Contex -> Term -> Maybe (Step,Term)
evalStep ctx (Nmb x i) = Just((NmbSt x (integerTerm i)),(integerTerm i))
evalStep ctx (Var x i lc ) = case getBinding ctx i of 
                               (TmBind t) -> Just ((VarSt x t), t) 
                               _          -> Nothing
evalStep ctx (App (Abs x t12) t2) = Just ((AppSt x t2 t12),(termSubstTop t2 t12))
evalStep ctx (App t1 t2)   = case evalStep ctx t1 of 
                               Just (st,v1) -> Just (st,(App v1 t2))
                               Nothing ->  case evalStep ctx t2 of
                                            Just (st,v2) -> Just(st, (App t1 v2) )
                                            Nothing -> Nothing 
evalStep ctx (Let x t1 t2) = Just ((LetSt x t1 t2) ,(termSubstTop t1 t2))
evalStep ctx (Abs nm t)    = case evalStep ((nm,NmBind):ctx) t of
                               Just (st, v) -> Just (st,(Abs nm v) )
                               Nothing -> Nothing

-- реалізація через хвостову рекурсію 
runEvalStep :: Int -> Program -> (Term, [Step], [Term], String)
runEvalStep mx (Program ctx et) =  runAllStep et [] []
 where runAllStep tm lst lt = case evalStep ctx tm of
                Just (st,t) -> if (length lst) < mx then runAllStep t (st:lst) (t:lt) else (t,lst, lt,"Max")
                Nothing     -> (tm, lst, lt, "No step")

compress :: Contex -> Term -> Term 
compress ctx (App t1 t2) = 
  let t = (App (compress ctx t1) (compress ctx t2)) in 
  case findEqTerm t ctx of 
    Just var -> var
    Nothing  -> t 
compress ctx x@(Abs nm t1) = 
  case findEqTerm x ctx of    
    Just var1 -> var1
    Nothing   ->
       let t = (Abs nm (compress ctx t1)) in 
       case findEqTerm t ctx of 
         Just var -> var
         Nothing  -> t
compress ctx (Let nm t1 t2) = 
  let t = (Let nm (compress ctx t1) (compress ctx t2)) in 
  case findEqTerm t ctx of 
    Just var -> var
    Nothing  -> t  
compress _ t1  = t1 

findEqTerm :: Term -> Contex -> Maybe Term
findEqTerm t ctx = if (isNumber t) then inNumber t else find t ctx 0 
  where find t [] i  = Nothing 
        find t ((id,NmBind):ct) i = find t ct (i+1)  
        find t ((id,(TmBind v)):ct) i | (v==t) = (Just (Var id i (length ctx)))
        find t (_:ct) i  = find t ct (i+1)

isNumber :: Term -> Bool 
isNumber (Abs _ (Abs _ t))| isNumberList t = True 
isNumber _  = False 

isNumberList :: Term -> Bool 
isNumberList (Var _ 0 _) = True 
isNumberList (App (Var _ 1 _) t) = isNumberList t  
isNumberList _    = False

inNumber :: Term -> Maybe Term --Int
inNumber (Abs _ (Abs _ t)) = Just (Var (show (toInteger t)) (-1) (-1))
  where toInteger (Var _ 0 _ ) = 0
        toInteger (App (Var _ 1 _) t1) = (toInteger t1  ) + 1

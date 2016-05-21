module Syntax where

type Name = String

data Binding = NmBind | TmBind Term deriving (Show, Eq)
type Contex = [(Name, Binding)]


-- (Var "asc" i l)  i >= 0  l >0  --- задає змінну з номером i в контексті довжини j
-- (Nmb "34" 34) --- задає  число (Number) -- при редукції розвертається в терм  Не входить в контекст!!!! 
data Term
  = Var Name Int Int 
  | App Term Term
  | Abs Name Term
  | Let Name Term Term 
  | Nmb Name Int  
  deriving (Show)

data Step 
  = VarSt Name Term
  | AppSt Name Term Term
  | LetSt Name Term Term 
  | NmbSt Name Term         deriving (Eq, Show)

type Decl = (String, Term)
data Program = Program Contex Term deriving (Show, Eq) 

instance Eq Term where
  Var _ i _   == Var _ j _   = i==j 
  App t1 t2   == App v1 v2   = t1==v1 && t2==v2 
  Abs _ t     == Abs _ v     = t==v 
  Let _ t1 t2 == Let _ v1 v2 = t1==v1 && t2==v2  
  _           == _           = False
  



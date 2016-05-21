{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pretty (
  ppterm,
  ppbind,
  ppprog, 
  ppstep
) where

import Syntax

import Text.PrettyPrint

class Pretty p where
  ppr :: Int -> p -> Doc

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

instance Pretty Name where
  ppr _ x = text x

instance Pretty Term where
  ppr _ (Var x _ _)         = text x
  ppr p (Let a b c) = text "let" <> ppr p a <+> text  "=" <+> ppr p b <+> text "in" <+> ppr p c
  ppr _ (Nmb x _ )         = text x
 -- ppr _ (Lit (LBool b)) = text (show b)
  ppr p e@(App _ _)     = parensIf (p>0) (ppr p f <+> sep (map (ppr (p+1)) xs))
    where (f, xs) = viewApp e
  ppr p e@(Abs _ _)     = parensIf (p>0) $ char '\\' <> hsep vars <+> text "." <+> body
    where
      vars = map (ppr 0) (viewVars e)
      body = ppr (p+1) (viewBody e)

instance Pretty Step where
  ppr p (NmbSt x t) = text "..Nmb " <> text x <+> text  "=>" <+> ppr 0 t
  ppr p (VarSt x t) = text "..Var " <> text x <+> text  "=>" <+> ppr 0 t
  ppr p (LetSt nm v1 t2) = text "..Let [" <> text nm <+> text "->" <+> ppr p v1 <+> text  "]" <+> ppr p t2
  ppr p (AppSt nm t1 t2) = text "..App [" <> text nm <+> text "->" <+> ppr p t1 <+> text  "]" <+> ppr p t2

viewVars :: Term -> [Name]
viewVars (Abs n a) = n : viewVars a
viewVars _ = []

viewBody :: Term -> Term
viewBody (Abs _ a) = viewBody a
viewBody x = x

viewApp :: Term -> (Term, [Term])
viewApp (App e1 e2) = go e1 [e2]
  where
    go (App a b) xs = go a (b : xs)
    go f xs = (f, xs)
viewApp _ = error "not application"

ppterm :: Term -> String
ppterm = render . ppr 0

ppstep :: Step -> String
ppstep = render . ppr 0

ppdecl :: Term -> String 
ppdecl (Abs n a) = " " ++ n ++ ppdecl a 
ppdecl t         = " = " ++ ppterm t ++ ";"

ppbind :: (Name, Binding) -> String
ppbind (a, TmBind t) = "let " ++ a ++ ppdecl t
ppbind (a, _)        = "variable " ++ a

ppprog :: Program -> [String]
ppprog (Program ctx t) = 
   let dcls = map ppbind ctx in  (reverse dcls) ++ [ppterm t]
    
{-# LANGUAGE TemplateHaskell #-}
module Midivis.Util.THUtils where
import Language.Haskell.TH

{-# INLINE qkInfixNNN #-}
qkInfixNNN :: Name -> Name -> Name -> Exp
qkInfixNNN a op b = InfixE (Just (VarE a)) (VarE op) (Just (VarE b))

{-# INLINE qkInfixNNE #-}
qkInfixNNE :: Name -> Name -> Exp -> Exp
qkInfixNNE a op e = InfixE (Just (VarE a)) (VarE op) (Just (e))

{-# INLINE qkInfixENE #-}
qkInfixENE :: Exp -> Name -> Exp -> Exp
qkInfixENE e1 op e2 = InfixE (Just e1) (VarE op) (Just e2)

{-# INLINE qkInfixENN #-}
qkInfixENN :: Exp -> Name -> Name -> Exp
qkInfixENN e op b = InfixE (Just e) (VarE op) (Just (VarE b))

{-# INLINE qkListDouble #-}
qkListDouble :: Name -> [Double] -> Dec
qkListDouble name lst = ValD (VarP name) (NormalB listExp) []
  where
    toLitE x= LitE (RationalL (toRational x)) -- uhhh
    listExp = ListE (map toLitE lst)

{-# INLINE qkExpToValD #-}
qkExpToValD :: Name -> Exp -> Dec
qkExpToValD name e = ValD (VarP name) (NormalB e) []  
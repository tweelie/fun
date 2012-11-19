-- | Written by Morten Winther Olsson, (c) 2012

module CTGen
       ( OpenType (..)
       , OpenEnv
       , Constraint (..)
       , ConstraintWriter
       , genConstraints
       , solve
       , findtype
       , typex
       , cnsex
       , ex
       , typex2
       , cnsex2
       , ex2
       , typtest
       , cnstest
       , test
       ) where


import Ast
import Parser
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Error

data OpenType = OTInt
              | OTBool
              | OTPair OpenType OpenType
              | OTFun OpenType OpenType
              | OTSum OpenType OpenType
              | OTUniVar Int
              deriving (Eq)

showOT :: OpenType -> String
showOT OTInt = "int"
showOT OTBool = "bool"
showOT (t1 `OTPair` t2) = "(" ++ showOT t1 ++ " x " ++ showOT t2 ++ ")"
showOT (t1 `OTFun` t2) = "(" ++ showOT t1 ++ " -> " ++ showOT t2 ++ ")"
showOT (t1 `OTSum` t2) = "(" ++ showOT t1 ++ " + " ++ showOT t2 ++ ")"
showOT (OTUniVar x) = "[" ++ show x ++ "]"

instance Show OpenType where
  show = showOT
  
type OpenEnv = Map.Map String OpenType

data Constraint = CMustBe OpenType OpenType
                | CIs Int OpenType
                | CFail

showCS :: Constraint -> String
showCS (t1 `CMustBe` t2) = show t1 ++ " ?= " ++ show t2
showCS (x `CIs` t1) = "[" ++ show x ++ "] == " ++ show t1
showCS CFail = "Fail"

instance Show Constraint where
  show = showCS

type ConstraintWriter = ErrorT String (Writer [Constraint])

generate :: OpenEnv -> Int -> Term -> ConstraintWriter (OpenType, Int)
generate env i (TVar x) =
  if x `Map.member` env
  then return (env Map.! x, i)
  else throwError $ "Unbound var: " ++ x
generate env i (TNum _) =
  return (OTInt, i)
generate env i TTrue =
  return (OTBool, i)
generate env i TFalse =
  return (OTBool, i)
generate env i (t0 `TPlus` t1) =
  do (tau0, ipp) <- generate env i t0
     (tau1, ip) <- generate env ipp t1
     tell [tau0 `CMustBe` OTInt, tau1 `CMustBe` OTInt]
     return (OTInt, ip)
generate env i (t0 `TLeq` t1) =
  do (tau0, ipp) <- generate env i t0
     (tau1, ip) <- generate env ipp t1
     tell [tau0 `CMustBe` OTInt, tau1 `CMustBe` OTInt]
     return (OTBool, ip)
generate env i (TIf t0 t1 t2) =
  do (tau0, ipp) <- generate env i t0
     (tau1, ippp) <- generate env ipp t1
     (tau2, ip) <- generate env ippp t2
     tell [tau0 `CMustBe` OTBool, tau1 `CMustBe` tau2]
     return (tau1, ip)
generate env i (t1 `TPair` t2) =
  do (tau1, ipp) <- generate env i t1
     (tau2, ip) <- generate env ipp t2
     return (tau1 `OTPair` tau2, ip)
generate env i (TFst t0) =
  do (tau0, ip) <- generate env (i+2) t0
     tell [tau0 `CMustBe` (OTUniVar i `OTPair` OTUniVar (i+1))]
     return (OTUniVar i, ip)
generate env i (TSnd t0) =
  do (tau0, ip) <- generate env (i+2) t0
     tell [tau0 `CMustBe` (OTUniVar i `OTPair` OTUniVar (i+1))]
     return (OTUniVar (i+1), ip)
generate env i (TLam x t0) =
  do (tau0, ip) <- generate (($env)$Map.insert x $ OTUniVar i) (i+1) t0
     return (OTUniVar i `OTFun` tau0, ip)
generate env i (t1 `TApp` t2) =
  do (tau1, ipp) <- generate env (i+1) t1
     (tau2, ip) <- generate env ipp t2
     tell [tau1 `CMustBe` (tau2 `OTFun` OTUniVar i)]
     return (OTUniVar i, ip)
generate env i (TLet x t1 t2) =
  do (tau1, ipp) <- generate env i t1
     (tau2, ip) <- generate (($env)$Map.insert x tau1) ipp t2
     return (tau2, ip)
generate env i (TRec x t0) =
  do (tau0, ip) <- generate (($env)$Map.insert x $ OTUniVar i) (i+1) t0
     tell [OTUniVar i `CMustBe` tau0]
     return (tau0, ip)
generate env i (TCase t0 x t1 y t2) =
  do (tau0, ipp) <- generate env (i+2) t0
     (tau1, ippp) <- generate (($env)$Map.insert x $ OTUniVar i) ipp t1
     (tau2, ip) <- generate (($env)$Map.insert y $ OTUniVar (i+1)) ippp t2
     tell [tau0 `CMustBe` (OTUniVar i `OTSum` OTUniVar (i+1)),
           tau1 `CMustBe` tau2]
     return (tau1, ip)
generate env i (TInl t0) =
  do (tau0, ip) <- generate env (i+1) t0
     return (tau0 `OTSum` OTUniVar i, ip)
generate env i (TInr t0) =
  do (tau0, ip) <- generate env (i+1) t0
     return (OTUniVar i `OTSum` tau0, ip)

genConstraints = (runWriter.) $ (runErrorT.) $ generate Map.empty 0

runCs t = case genConstraints t of
  (Left err, _) -> error err
  (Right (ot, _), cs) -> (ot, cs)

getUV :: OpenType -> [Int]
getUV (t1 `OTPair` t2) = (getUV t1)++(getUV t2)
getUV (t1 `OTFun` t2) = (getUV t1)++(getUV t2)
getUV (t1 `OTSum` t2) = (getUV t1)++(getUV t2)
getUV (OTUniVar i) = [i]
getUV _ = []

substUVinOT :: (Int, OpenType) -> OpenType -> OpenType
substUVinOT (x, tau) (OTUniVar n) | n==x = tau
substUVinOT _ t1 = t1

mapOT :: (OpenType -> OpenType) -> OpenType -> OpenType
mapOT f (t1 `OTPair` t2) = mapOT f t1 `OTPair` mapOT f t2
mapOT f (t1 `OTFun` t2) = mapOT f t1 `OTFun` mapOT f t2
mapOT f (t1 `OTSum` t2) = mapOT f t1 `OTSum` mapOT f t2
mapOT f ot = f ot

substRecUVinOT = mapOT.substUVinOT

mapCS :: (OpenType -> OpenType) -> Constraint -> Constraint
mapCS sf (i `CIs` t2) = i `CIs` sf t2
mapCS sf (t1 `CMustBe` t2) = sf t1 `CMustBe` sf t2

substUVinAllOTs = (map.) $ mapCS.substRecUVinOT

solve :: [Constraint] -> [Constraint]
solve [] = []
solve ((c1@(CIs _ _):cs)) = cs ++ [c1]
solve ((tau1 `OTPair` tau2) `CMustBe` (tau1p `OTPair` tau2p):c) =
  [tau1 `CMustBe` tau1p, tau2 `CMustBe` tau2p] ++ c
solve ((tau1 `OTSum` tau2) `CMustBe` (tau1p `OTSum` tau2p):c) =
  [tau1 `CMustBe` tau1p, tau2 `CMustBe` tau2p] ++ c
solve ((tau1 `OTFun` tau2) `CMustBe` (tau1p `OTFun` tau2p):c) =
  [tau1 `CMustBe` tau1p, tau2 `CMustBe` tau2p]++c
solve ((t1 `CMustBe` t2):c) | t1 == t2 = c
solve ((OTUniVar i `CMustBe` t2):c)
  | any (\x->x==i) (getUV t2) = [CFail]
  | otherwise = (substUVinAllOTs (i, t2) c)++[CIs i t2]
solve ((t1 `CMustBe` OTUniVar i):c)
  | any (\x->x==i) (getUV t1) = [CFail]
  | otherwise = (substUVinAllOTs (i, t1) c)++[CIs i t1]
solve _ = [CFail]

is_solved :: Constraint -> Bool
is_solved (_ `CIs` _) = True
is_solved _ = False

solveCs = (until.all) is_solved solve

applySolvedCs :: Constraint -> OpenType -> OpenType
applySolvedCs (i `CIs` tau) = substRecUVinOT (i, tau)
applySolvedCs _ = id

findtype = (.solveCs) $ flip $ foldl $ flip applySolvedCs

(typex, cnsex) = runCs (p_parse testex)
(typex2, cnsex2) = runCs (p_parse testex2)
(typtest, cnstest) = runCs (p_parse testprog)

ex = findtype cnsex typex
ex2 = findtype cnsex2 typex2
test = findtype cnstest typtest

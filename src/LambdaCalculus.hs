module LambdaCalculus where
import Data.List (nub, (\\))

type Id = String

data Term = Var Id      -- Variables
    | Abs Id Term       -- Abstractions
    | App Term Term deriving Show    -- Applications

-- 1. Implement a function freeVars t that returns a set of all free variables within a lambda term t. You may want to represent sets as lists without duplicates. 
-- Appendix A.2 of the lecture notes contains recursive definitions of non-freeness that you may find useful.

freeVars :: Term -> [Id]
freeVars (Var id) = [id]
freeVars (Abs id term) = nub (freeVars term) \\ [id]
freeVars (App lterm rterm) = freeVars lterm ++ freeVars rterm

-- 2. substitute (x,tx) t
-- Implement a function substitute (x,tx) t that replaces all free occurrences of the variable x within the term t with the term tx. 
-- Take care to avoid capturing substitutions (you will have to do some alpha renaming with fresh variables to avoid this).
-- Appendix A.2 of the lecture notes contains a recursive definition of substitution that you may find useful. 
-- In case you find the task of avoiding variable capture too challenging, skip this and only use terms with unique bound and free variable names.

substitute :: (Id, Term) -> Term -> Term
substitute (x, tx) (Var a) | x == a = tx 
                           | otherwise = Var a
substitute (x, tx) (Abs id term) | x == id = Abs id tx
                                 | x `elem` freeVars term = substitute (x, tx) term
                                 | otherwise = Abs id term
substitute (x, tx) (App lterm rterm) = App (substitute (x, tx) lterm) (substitute (x, tx) rterm)

-- 3. Implement a function isBetaRedex t which returns True if the top level of the term t is a beta redex.

isBetaRedex :: Term -> Bool
isBetaRedex (App lterm rterm) = isAbs lterm && (isVar rterm || isAbs rterm)
isBetaRedex _ = False

isAbs :: Term -> Bool
isAbs (Abs _ _) = True 
isAbs _ = False 

isVar :: Term -> Bool
isVar (Var _) = True 
isVar _ = False 

-- 4. Use substitute to implement a function betaReduce t that applies a beta reduction to top level of the term t.

betaReduce :: Term -> Term
betaReduce (Var id) = Var id
betaReduce (Abs id term) = Abs id term
betaReduce (App (Abs id term) rterm) = substitute (id, rterm) term
betaReduce (App lterm rterm) = App (betaReduce lterm) rterm

-- 5. leftmostOutermost, leftmostInnermost that perform a single reduction step using the appropriate evaluation strategy.

leftmostOutermost :: Term -> Term
leftmostOutermost (Var id) = Var id
leftmostOutermost (Abs id term) = Abs id term
leftmostOutermost (App lterm rterm) = App (betaReduce lterm) rterm

leftmostInnermost :: Term -> Term
leftmostInnermost (Var id) = Var id
leftmostInnermost (Abs id term) = Abs id term 
leftmostInnermost (App lterm rterm) | isBetaRedex lterm = leftmostInnermost (App (getLeft lterm) rterm) 
                                    | otherwise = App (betaReduce lterm) rterm

--leftmostInnermost (App lterm rterm) = App (betaReduce (lterm)) rterm 

getLeft :: Term -> Term
getLeft (Var id) = Var id
getLeft (Abs id term) = Abs id term
getLeft (App lterm rterm) = lterm


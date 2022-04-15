module LambdaCalculus where
import Data.List (nub, (\\))

type Id = String

data Term = Var Id      -- Variables
    | Abs Id Term       -- Abstractions
    | App Term Term     -- Applications

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
                                 | x `elem ` freeVars term = substitute (x, tx) term
                                 | otherwise = Abs id term
substitute (x, tx) (App lterm rterm) = App (substitute (x, tx) lterm) (substitute (x, tx) rterm)

-- 3. Implement a function isBetaRedex t which returns True if the top level of the term t is a beta redex.

isBetaRedex :: Term -> Bool
isBetaRedex (Var a) = False 
isBetaRedex (Abs id term) = undefined
isBetaRedex (App lterm rterm) = isBetaRedex lterm || isBetaRedex rterm 

-- 4. Use substitute to implement a function betaReduce t that applies a beta reduction to top level of the term t.

betaReduce :: Term -> Term
betaReduce t = undefined
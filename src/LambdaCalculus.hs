module LambdaCalculus where
import Data.List ( nub )

type Id = String

data Term = Var Id      -- Variables
    | Abs Id Term       -- Abstractions
    | App Term Term     -- Applications


freeVars :: Term -> [Id]
freeVars (Var id) = [id]
freeVars (Abs id term) = nub (id : freeVars term)
freeVars (App lterm rterm) = freeVars lterm ++ freeVars rterm


-- 2. App (substitute (x, tx) (Var id)) (substitute (x, tx) term)

substitute :: (Id, Term) -> Term -> Term
substitute (x, tx) (Var a) | x == a = tx 
                           | otherwise = Var a
substitute (x, tx) (Abs id term) | x == id = Abs id term
                                 | x /= id = undefined -- todo
                                 | otherwise = Abs id term

substitute (x, tx) (App lterm rterm) = App (substitute (x, tx) lterm) (substitute (x, tx) rterm)

-- 3. Implement a function isBetaRedex t which returns True if the top level of the term t is a beta redex.

isBetaRedex :: Term -> Bool
isBetaRedex t = undefined 

-- 4. Use substitute to implement a function betaReduce t that applies a beta reduction to top level of the term t.

betaReduce :: Term -> Term
betaReduce t = undefined
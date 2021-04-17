module Core.Utils (aggregateApplications) where

import Core.AST

-- (...((f x1) x2) ... xn) = (f, [x1, ..., xn])
-- (f x1) (g x2) = (f x1, [g x2])
aggregateApplications :: Expr f -> (Expr f, [Expr f])
aggregateApplications (App e1@App {} e2) = 
    let (fn, args) = aggregateApplications e1 in
    (fn, args ++ [e2])
aggregateApplications (App e1 e2) = (e1, [e2])
aggregateApplications e = (e, [])
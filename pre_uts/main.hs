main = putStrLn "a"

-- 3
data Expr = C Float
          | Expr :+ Expr
          | Expr :- Expr
          | Expr :* Expr
          | Expr :/ Expr
          | V String
          | Let String Expr Expr
        deriving Show

e1 = (C 10 :+ (C 8 :/ C 2)) :* (C 7 :- C 4)
e2 = Let "x" (C 10) (V "x" :+ (C 11))

e4 = (V "y" :+ C 21)
e3 = (C 10) :+ (Let "y" (C 20) e4)

e5 = (V "y" :+ V "x")
e6 = (C 10) :+ (Let "y" (C 20) (Let "x" (C 22) e5))

-- Flow:
-- e6 = (C 10) + (Let "y" (C 20) (Let "x" (C 22) (V "y" :+ V "x")))
-- e6 = (10) + (subst "y" (C 20) (Let "x" (C 22) (V "y" :+ (V "x"))))
-- e6 = (10) + (Let "x" (C 22) (subst "y" (C 20) (V "y" :+ (V "x"))))
-- e6 = (10) + (Let "x" (C 22) ((subst "y" (C 20) (V "y")) :+ (subst "y" (C 20) (V "x")))
-- e6 = (10) + (Let "x" (C 22) ((C 20) :+ (V "x")))
-- ...
-- e6 = (10) + ((C 20) :+ (C 22))
-- e6 = (10) + (20) + (22)
-- e6 = 52


subst :: String -> Expr -> Expr -> Expr
subst v0 e0 (V v1) = if (v0 == v1) then e0 else (V v1)
subst _ _ (C c) = (C c)
subst v0 e0 (e1 :+ e2) = subst v0 e0 e1 :+ subst v0 e0 e2
subst v0 e0 (e1 :- e2) = subst v0 e0 e1 :- subst v0 e0 e2
subst v0 e0 (e1 :* e2) = subst v0 e0 e1 :* subst v0 e0 e2
subst v0 e0 (e1 :/ e2) = subst v0 e0 e1 :/ subst v0 e0 e2
subst v0 e0 (Let v1 e1 e2) = Let v1 e1 (subst v0 e0 e2) -- perlu koreksi ???
-- Sepertinya tidak perlu

evaluate :: Expr -> Float
evaluate (C x) = x
evaluate (e1 :+ e2) = evaluate e1 + evaluate e2
evaluate (e1 :- e2) = evaluate e1 - evaluate e2
evaluate (e1 :* e2) = evaluate e1 * evaluate e2
evaluate (e1 :/ e2) = evaluate e1 / evaluate e2
evaluate (Let v e0 e1) = evaluate (subst v e0 e1)
evaluate (V _) = 0.0 -- is this correct?
-- benar jika diasumsikan undefined variable = 0
-- atau bisa raise error jika diperlukan
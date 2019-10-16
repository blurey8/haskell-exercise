main = putStrLn "a"

-- 1
data Expr = C Float 
          | Expr :+ Expr 
          | Expr :- Expr
          | Expr :* Expr 
          | Expr :/ Expr

e1 = (C 10 :+ (C 8 :/ C 2)) :* (C 7 :- C 4)
e2 = 
evaluate           :: Expr -> Float
evaluate (C x)      = x
evaluate (e1 :+ e2) = evaluate e1 + evaluate e2
evaluate (e1 :- e2) = evaluate e1 - evaluate e2
evaluate (e1 :* e2) = evaluate e1 * evaluate e2
evaluate (e1 :/ e2) = evaluate e1 / evaluate e2

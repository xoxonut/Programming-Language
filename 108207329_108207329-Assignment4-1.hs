type Name = String
type Env a = [(String,a)]
data Error a = S a
             | Error String
             deriving (Eq, Show)
data Expr = Var Name
          | Num Int  --constant
          | Expr :+: Expr
          | Expr :*: Expr
          | Let [Name] Expr Expr 
          deriving (Eq, Show)
---find
find :: String -> Env Int -> Maybe Int  
find t ((var,val):env) =
  if t == var then Just val else find t env
find t [] = Nothing
---ev1
ev1 :: Env Int -> Expr -> Error Int
ev1 env (Num x) = S x
ev1 env (l :+: r) = case (ev1 env l) of 
  S x -> case (ev1 env r) of
    S y -> S(x+y)
    Error s1 -> Error s1
  Error s -> Error s
ev1 env (l :*: r) = case (ev1 env l) of 
  S x -> case (ev1 env r) of
    S y -> S(x*y)
    Error s1 -> Error s1
  Error s -> Error s
ev1 env (Let [x] e be) = case (ev1 env e) of
  S v -> ev1 ([(x,v)]++env) be
  Error s -> Error s
ev1 env (Var x) = case (find x env) of 
  Just x -> S x
  Nothing -> Error ("unbounded  variable : " ++ x)

main = do
  let global = [("x", 1), ("y", 2)] :: [(Name, Int)] in
    pretest "ev1" $ ev1 global (Let ["z"] (Num 3) (Num 6 :+: Var "z")) 
  where
  pretest p a = putStrLn $ p ++ " = " ++ show a
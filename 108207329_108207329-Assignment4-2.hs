type Name = String
type Env a = [(String,a)]
data Error a = S a
             | Error String
             deriving (Eq, Show)
data Exp2 = Var2 Name 
          | Num2 Int  
          | B2 Bool--constant
          | Exp2 :+: Exp2
          | Exp2 :*: Exp2
          | Exp2 :=: Exp2
          | If Exp2 Exp2 Exp2
          | Let2 [String] Exp2 Exp2 
          deriving (Eq, Show)
--Val
data Val = VN Int | VB Bool deriving (Eq, Show)
---find
find :: String -> Env Val -> Maybe Val
find t ((var,val):env) =
  if t == var then Just val else find t env
find t [] = Nothing
---ev2
ev2 :: Env Val -> Exp2 -> Error Val
ev2 env (Num2 x) = S(VN x)
ev2 env (B2 x) = S(VB x)
ev2 env (l :+: r) = case (ev2 env l) of
  S(VN x) -> case (ev2 env r) of
    S(VN xx) -> S(VN(x+xx))
    S(VB xy) -> Error "right side is wrong type"
    Error e -> Error e
  S(VB y) -> Error "left side is wrong type"
  Error e -> Error e
ev2 env (l :*: r) = case (ev2 env l) of
  S(VN x) -> case (ev2 env r) of
    S(VN xx) -> S(VN(x*xx))
    S(VB xy) -> Error "right side is wrong type"
    Error e -> Error e
  S(VB y) -> Error "left side is wrong type"
  Error e -> Error e
ev2 env (l :=: r) = case (ev2 env l) of
  S(VN x) -> case (ev2 env r) of
    S(VN xx) -> S(VB(xx==x))
    S(VB xy) -> Error "differnt type"
    Error e -> Error e
  S(VB y) -> case (ev2 env r) of
    S(VN yx) ->Error "different type"
    S(VB yy) -> S(VB(y==yy))
    Error e -> Error e
  Error e -> Error e
ev2 env (Let2 [v] e be) = case (ev2 env e) of
  S x -> ev2 ([(v,x)]++env) be
  Error e -> Error e
ev2 env (If fi th el) = case (ev2 env fi) of
  S(VB True) -> ev2 env th
  S(VB False) -> ev2 env el
  S(VN _) -> Error "if is wrong type"
  Error e -> Error e
ev2 env (Var2 v) = case (find v env) of
  Nothing -> Error ("unbounded  variable : " ++ v)
  Just x -> S x
main = do
  let global = [("x", VN 1), ("y", VN 2)] :: [(Name, Val)] in
    pretest "ev2" $ ev2 global (Num2 1 :=: Num2 1)
  where
  pretest p a = putStrLn $ p ++ " = " ++ show a
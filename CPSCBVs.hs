import System.IO.Unsafe
import Parser
--継続渡し値呼び静的スコープ
-- Value
data Val = VInt Int
         | VBool Bool
         | VClosure Variable Expr Env
         deriving (Eq, Show)

showVal :: Val -> [Char]
showVal (VInt m) = show m
showVal (VBool b) = show b
showVal (VClosure v e r) =
  "Closure [lambda " ++ show v ++ " . " ++ show e ++ "]"

unwrapInt :: Val -> Int
unwrapInt (VInt m) = m
upwrapInt _ = error "not an integer"

unwrapBool :: Val -> Bool
unwrapBool (VBool b) = b
upwrapBool _ = error "not a boolean"

-- Environment
type Assoc a b = [(a,b)]

emptyAssoc :: Assoc a b
emptyAssoc = []

lookupAssoc :: (Eq a) => a -> Assoc a b -> Maybe b
lookupAssoc x [] = Nothing
lookupAssoc x ((k,v):ps) | x == k    = Just v
                         | otherwise = lookupAssoc x ps

updateAssoc :: (Eq a) => a -> b -> Assoc a b -> Assoc a b
updateAssoc k v ps = (k,v):ps

type Env = Assoc Variable Val

emptyEnv :: Env
emptyEnv = emptyAssoc

lookupEnv :: Variable -> Env -> Maybe Val
lookupEnv = lookupAssoc

updateEnv :: Variable -> Val -> Env -> Env
updateEnv = updateAssoc

-- Continuation
type Cont = Val -> Val

endCont :: Cont
-- endCont x = x
endCont x =
  (unsafePerformIO $ do { putStrLn "endcont"; return x; }) `seq` x

apValToCont :: Cont -> Val -> Val
apValToCont cont val = val `seq` cont val

-- Evaluation
expval :: Expr -> Env -> Cont -> Val
expval (Num n) env cont =
  n `seq` apValToCont cont (VInt n)

expval (Var x) env cont =
  apValToCont cont (getval x (lookupEnv x env))

expval (Bexpr o e1 e2) env cont =
  expval e1 env bexprCont1
  where bexprCont1 :: Cont
        bexprCont1 v1 =
          v1 `seq` expval e2 env bexprCont2
          where bexprCont2 :: Cont
                bexprCont2 v2 =
                  v2 `seq` r `seq` apValToCont cont (VInt r)
                  where r = binop o (unwrapInt v1) (unwrapInt v2)

expval (Rexpr o e1 e2) env cont =
  expval e1 env rexprCont1
  where rexprCont1 :: Cont
        rexprCont1 v1 =
          v1 `seq` expval e2 env rexprCont2
          where rexprCont2 :: Cont
                rexprCont2 v2 =
                  v2 `seq` r `seq` apValToCont cont (VBool r)
                  where r = relop o (unwrapInt v1) (unwrapInt v2)



expval (Fun x e) env cont = apValToCont cont (VClosure x e env)

expval (Apply e1 e2) env cont =
  expval e1 env applyCont1
  where applyCont1 :: Cont
        applyCont1 (VClosure x body env') =
          expval e2 env applyCont2
          where applyCont2 :: Cont
                applyCont2 v =
                  v `seq` expval body newenv cont
                  where newenv = updateEnv x v env'

expval (Let (Decl x e1) e2) env cont =
  expval e1 env letCont
  where letCont :: Cont
        letCont v =
          v `seq` expval e2 newenv cont
          where newenv = updateEnv x v env

expval (Letrec (Decl f (Fun x e1)) e2) env cont =
  v `seq` expval e2 newenv cont
  where v = VClosure x e1 newenv
        newenv = updateEnv f v env

expval (Letrec _ _) env cont = error "letrec"

expval (If e1 e2 e3) env cont =
  expval e1 env ifCont
  where ifCont :: Cont
        ifCont v = v `seq` expval (if unwrapBool v then e2 else e3) env cont

expval (Pr e) env cont =
  expval e env prCont
  where prCont :: Cont
        prCont v =
          unsafePerformIO $
          do { putStrLn (showVal v); return (apValToCont cont v); }

getval :: Variable -> Maybe Val -> Val
getval var (Just val) = val
getval var Nothing = error ("getval: " ++ var)

binop :: BinOp -> Int -> Int -> Int
binop Add = (+)
binop Sub = (-)
binop Mul = (*)
binop Div = div

relop :: RelOp -> Int -> Int -> Bool
relop Equal = (==)
relop NotEqual = (/=)
relop LessThan = (<)
relop LessThanEqual = (<=)

-- Test
-- ev e = showVal (expval e emptyEnv)

p1 = "(lambda x . x + 2) 100"
p2 = "let x = 100 + 200 in x < 500"
p3 = "letrec f = lambda x . if x == 0 then 0 else x + f (x - 1) in f 100"

--
ev :: [Char] -> Val
ev s = expval (parseProg s) emptyEnv endCont

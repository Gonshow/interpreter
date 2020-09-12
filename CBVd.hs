import System.IO.Unsafe
import Parser
--値呼び動的スコープ
-- Value
data Val = VInt Int
         | VBool Bool
         | VClosure Variable Expr Env
         | VFuns Char
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
lookupEnv var env = lookupAssoc var env

updateEnv :: Variable -> Val -> Env -> Env
updateEnv var val env = updateAssoc var val env

--globalsub :: Env
--globalsub = let (k,v) = updateGlobalAssoc (head builtinFns) emptyAssoc
--            in parseProg v

globalAssoc :: Assoc [([Char], [Char])] (Assoc a b)
                           -> Assoc [([Char], [Char])] (Assoc a b)
globalAssoc = updateAssoc builtinFns emptyAssoc

updateGlobalAssoc ::(Eq a) => (a,b) -> Assoc a b -> Assoc a b
updateGlobalAssoc (k,v) ps = (k,v):ps

builtinFns :: [([Char], [Char])]
builtinFns =
  [("square", "lambda x . x * x"),
  ("fourthPower", "lambda x . square (square x)"),
  ("abs", "lambda x . if x < 0 then 0 - x else x"),
  ("evenp", "lambda n . if n == 0 then True else oddp (n - 1)"),
  ("oddp", "lambda n . if n == 0 then False else evenp (n - 1)"),
  ("True", "0 == 0"),
  ("False", "0 == 1")]

--chang :: [([Char], [Char])] -> Assoc a b -> Assoc a b
chang (x,y) ps = (x,y):ps


--globalAssoc :: Assoc [char] [char]
--globalAssoc = updateAssoc [head builtinFns] emptyAssoc



-- Evaluation
expval :: Expr -> Env -> Val
expval (Num n) env = n `seq` VInt n
expval (Var x) env = getval x (lookupEnv x env)
expval (Bexpr o e1 e2) env =
  v1 `seq` v2 `seq` r `seq` VInt r
  where v1 = expval e1 env
        v2 = expval e2 env
        r = binop o (unwrapInt v1) (unwrapInt v2)
expval (Rexpr o e1 e2) env =
  v1 `seq` v2 `seq` r `seq` VBool r
  where v1 = expval e1 env
        v2 = expval e2 env
        r = relop o (unwrapInt v1) (unwrapInt v2)
expval (Fun x e) env = VClosure x e emptyEnv
expval (Apply e1 e2) env =
  f `seq` v `seq` expval body newenv
  where f = expval e1 env
        v = expval e2 env
        VClosure x body env' = f
        newenv = updateEnv x v env
expval (Let (Decl x e1) e2) env =
  v `seq` expval e2 newenv
  where v = expval e1 env
        newenv = updateEnv x v env
expval (Letrec (Decl x e1) e2) env =
  v `seq` expval e2 newenv
  where v = expval e1 newenv
        newenv = updateEnv x v env
expval (If e1 e2 e3) env =
  v `seq` expval (if unwrapBool v then e2 else e3) env
  where v = expval e1 env
expval (Pr e) env =
  unsafePerformIO $ do { putStrLn (showVal v); return v;}
  where v = expval e env

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
ev s = expval (parseProg s) emptyEnv

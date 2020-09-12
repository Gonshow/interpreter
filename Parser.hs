module Parser where

import Data.Char

-- <prog> ::= <expr>
-- <expr> ::= <number>
--          | <variable>
--          | <expr> <expr>
--          | <expr> <binop> <expr>
--          | <expr> <relop> <expr>
--          | lambda <identifier> . <expr>
--          | let <decl> in <expr>
--          | letrec <decl> in <expr>
--          | if <expr> then <expr> else <expr>

-- <expr> ::= <expr1>
--          | lambda <identifier> . <expr>
--          | let <decl> in <expr>
--          | letrec <decl> in <expr>
--          | if <expr> then <expr> else <expr>


-- <expr1>  ::= <expr2> <expr1c>
-- <expr1c> ::= <relop> <expr2>
--           | epsilon
-- <expr2>  ::= <expr3> <expr2c>
-- <expr2c> ::= { <addop> <expr3> }
-- <expr3>  ::= <expr4> <expr3c>
-- <expr3c> ::= { <mulop> <expr4> }
-- <expr4> ::= [ <aexpr> ]
-- <aexpr>  ::= <variable>
--           |  <number>
--           |  ( <expr> )
--           |  !<aexpr>    -- for debugging

-- <decl> ::= <variable> = <expr>

-- Expression
data Expr = Num Int
          | Var Variable
          | Apply Expr Expr
          | Bexpr BinOp Expr Expr
          | Rexpr RelOp Expr Expr
          | Fun Variable Expr
          | Let Decl Expr
          | Letrec Decl Expr
          | If Expr Expr Expr
          | Pr Expr
          deriving (Eq, Show)

-- Program
type Prog = Expr

type Variable = [Char]

data Decl = Decl Variable Expr
            deriving (Eq, Show)

data BinOp = Add | Sub | Mul | Div
             deriving (Eq, Show)

data RelOp = Equal | NotEqual | LessThan | LessThanEqual
             deriving (Eq, Show)

-- Lexer

type Token = [Char]

lexProg :: [Char] -> [Token]
lexProg [] = []
lexProg (c:cs)
  | isSpace c = lexProg cs
  | isDigit c = let ns = c : takeWhile isDigit cs
                    cs' = dropWhile isDigit cs
                in ns : lexProg cs'
  | isLetter c = let vs = c : takeWhile isVarChar cs
                     cs' = dropWhile isVarChar cs
                 in vs : lexProg cs'
  | cs /= [] && elem [c, head cs] twoCharOps
               = [c, head cs] : lexProg (tail cs)
  | otherwise = [c] : lexProg cs

isVarChar :: Char -> Bool
isVarChar c = isLetter c || isDigit c || (c == '_')

twoCharOps :: [[Char]]
twoCharOps = ["==", "/=", "<=", ">="]

--- Parser

type Parser a b = [a] -> [(b, [a])]

-- parser combinaters

pSucceed :: b -> Parser a b
pSucceed v xs = [(v, xs)]

pFail :: Parser a b
pFail xs = []

pSatisfy :: (a -> Bool) -> Parser a a
pSatisfy q [] = pFail []
pSatisfy q (x:xs) | q x       = pSucceed x xs
                  | otherwise = pFail xs

infixr 4 `pAlt`
pAlt :: Parser a b -> Parser a b -> Parser a b
pAlt p1 p2 xs = p1 xs ++ p2 xs

pFoldAlt :: [Parser a b] -> Parser a b
pFoldAlt = foldr1 pAlt

infixr 6 `pSeq`, `pSeqFst`, `pSeqSnd`
pSeq :: Parser a b -> Parser a c -> Parser a (b,c)
pSeq p1 p2 xs =
  [((v,v'), xs'') | (v, xs') <- p1 xs, (v', xs'') <- p2 xs']

pSeqFst :: Parser a b -> Parser a c -> Parser a b
pSeqFst p1 p2 = (p1 `pSeq` p2) `pUsing` fst

pSeqSnd :: Parser a b -> Parser a c -> Parser a c
pSeqSnd p1 p2 = (p1 `pSeq` p2) `pUsing` snd

infixl 5 `pUsing`
pUsing :: Parser a b -> (b -> c) -> Parser a c
pUsing p f xs = [(f v, xs') | (v, xs') <- p xs]

pMany :: Parser a b -> Parser a [b]
pMany p = pSome p `pAlt` pSucceed []

pSome :: Parser a b -> Parser a [b]
pSome p = (p `pSeq` pMany p) `pUsing` uncurry (:)

pLiteral :: Eq a => a -> Parser a a
pLiteral x = pSatisfy (x ==)

pCheck :: Eq a => a -> b -> Parser a b
pCheck x y = pLiteral x `pSeqSnd` pSucceed y

pVariable :: Parser Token Token
pVariable = pSatisfy varName
            where varName :: [Char] -> Bool
                  varName x = isLetter (head x) && not (elem x resWords)

resWords :: [[Char]]
resWords = ["lambda", "let", "letrec", "in", "if", "then", "else"]

pNumber :: Parser Token Token
pNumber = pSatisfy (isDigit . head)

-- parser for Prog

pProg, pExpr :: Parser Token Expr
pProg = pExpr
pExpr = pFoldAlt [pExpr1, pFun, pLet, pLetrec, pIf]

pExpr1 :: Parser Token Expr
pExpr1 = (pExpr2 `pSeq` pExpr1c) `pUsing` opExp Rexpr
         where
         pExpr1c = (pFoldAlt [pCheck "==" Equal, pCheck "/=" NotEqual,
                              pCheck "<" LessThan, pCheck "<=" LessThanEqual]
                   --  (pFoldAlt (map (uncurry pCheck) relOps)
                   `pSeq` pExpr2 `pUsing` (\x -> [x]))
                   `pAlt` pSucceed []

-- relOps :: [([Char], RelOp)]
-- relOps =
--  [("==", Equal), ("/=", NotEqual), ("<", LessThan), ("<=", LessThanEqual)]

opExp :: (a -> b -> b -> b) -> (b, [(a, b)]) -> b
opExp con (e1, []) = e1
opExp con (e1, [(op, e2)]) = con op e1 e2

pExpr2 :: Parser Token Expr
pExpr2 = (pExpr3 `pSeq` pExpr2c) `pUsing` opChain
         where
         pExpr2c = pMany ((pCheck "+" Add `pAlt` pCheck "-" Sub) `pSeq` pExpr3)

pExpr3 :: Parser Token Expr
pExpr3 = (pExpr4 `pSeq` pExpr3c) `pUsing` opChain
         where
         pExpr3c = pMany ((pCheck "*" Mul `pAlt` pCheck "/" Div) `pSeq` pExpr4)

pExpr4 :: Parser Token Expr
pExpr4 = pSome pAexpr `pUsing` apChain
         where apChain = foldl1 Apply

opChain :: (Expr, [(BinOp, Expr)]) -> Expr
opChain (e, xs) = foldl f e xs
                  where f e (op,e') = Bexpr op e e'
-- opChain (e, []) = e
-- opChain (e, ((op, e'):xs)) = Bexpr op e (opChain (e',xs))

pAexpr :: Parser Token Expr
pAexpr =
  pFoldAlt [pNumber `pUsing` Num . read,
            pVariable `pUsing` Var,
            pLiteral "(" `pSeqSnd` pExpr `pSeqFst` pLiteral ")",
            pLiteral "!" `pSeqSnd` pAexpr `pUsing` Pr ]

pFun :: Parser Token Expr
pFun =
  (pLiteral "lambda" `pSeqSnd` pVariable `pSeq` pLiteral "." `pSeqSnd` pExpr)
  `pUsing` uncurry Fun

pLet :: Parser Token Expr
pLet = (pLiteral "let" `pSeqSnd` pDecl `pSeq` pLiteral "in" `pSeqSnd` pExpr)
       `pUsing` uncurry Let

pLetrec :: Parser Token Expr
pLetrec =
  (pLiteral "letrec" `pSeqSnd` pDecl `pSeq` pLiteral "in" `pSeqSnd` pExpr)
  `pUsing` uncurry Letrec

pDecl :: Parser Token Decl
pDecl = (pVariable `pSeq` pLiteral "=" `pSeqSnd` pExpr) `pUsing` uncurry Decl

pIf :: Parser Token Expr
pIf = (pLiteral "if" `pSeqSnd` pExpr `pSeq`
       pLiteral "then" `pSeqSnd` pExpr `pSeq`
       pLiteral "else" `pSeqSnd` pExpr)
      `pUsing` unc If
      where unc f (a,(b,c)) = f a b c

--

parseProg :: [Char] -> Prog
parseProg = fst . head . pProg . lexProg

module MathParse where

import Control.Monad (liftM2, foldM)
import Control.Arrow (left)
import Data.Bits

-- ======================== TYING IT ALL TOGETHER =============================

data Error = C ReduceError
           | P ParseError
    deriving (Show, Eq)

-- Entry point for calculation
-- Either 
-- * exits with an error, or 
-- * returns the integer from parsing the expression
calculate :: String -> Either Error Integer
calculate s = do
    e <- left P $ parseToExpr s
    i <- left C $ reduce e
    return i

-- ======================== EXPRESSIONS MANIPULATION ==========================
data Expr = Num Integer
          | Expr :**: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :&: Expr
          | Expr :^: Expr
          | Expr :|: Expr
          | Expr :=: Expr
    deriving (Eq, Show, Read)

data ReduceError = TooLarge
                 | NegativePower
    deriving (Show, Eq)

-- Reduces an Expression into a final integer, or exits with a ReduceError
reduce :: Expr -> Either ReduceError Integer
reduce (Num i)   = Right i
reduce (a :**: b) = do
    a' <- reduce a
    b' <- reduce b
    if or [ b' > 1000000
          , a' > 100 && b' > 100000
          ] 
    then Left TooLarge
    else if b' < 0 
    then Left NegativePower
    else liftM2 (^) (reduce a) (reduce b)
reduce (a :*: b) = liftM2 (*) (reduce a) (reduce b)
reduce (a :/: b) = liftM2 div (reduce a) (reduce b)
reduce (a :+: b) = liftM2 (+) (reduce a) (reduce b)
reduce (a :-: b) = liftM2 (-) (reduce a) (reduce b)
reduce (a :&: b) = liftM2 (.&.) (reduce a) (reduce b)
reduce (a :^: b) = liftM2 xor (reduce a) (reduce b)
reduce (a :|: b) = liftM2 (.|.) (reduce a) (reduce b)
reduce (a :=: b) = liftM2 (\x y -> toInteger $ fromEnum $ x == y) (reduce a) (reduce b)

-- ============================ OPERATOR MANIPULATION =========================
data Operator = Equals
              | Or
              | Xor
              | And
              | Subtract
              | Add
              | Divide
              | Times
              | Exponentiate
              | OpenParen | CloseParen
    deriving (Eq, Enum, Ord, Bounded)

enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound .. maxBound]

rassoc, lassoc :: Operator -> Bool
rassoc Exponentiate = True
rassoc _            = False
lassoc = not . rassoc

prec :: Operator -> Int
prec = fromEnum

instance Show Operator where
    show CloseParen   = ")"
    show OpenParen    = "("
    show Exponentiate = "**"
    show Times        = "*"
    show Divide       = "/"
    show Add          = "+"
    show Subtract     = "-"
    show And          = "&"
    show Xor          = "^"
    show Or           = "|"
    show Equals       = "="

instance Read Operator where
    readsPrec _ s = [ (o,rest) 
                    | (s,rest) <- lex s
                    , o <- enumerate
                    , s == show o
                    ]

-- ================================== PARSING =================================

-- | Specialized reading utilities
-- Only read in positive integers
readsInt :: ReadS Integer
readsInt = filter ((>=0) . fst) . reads

-- Read in operations
readsOp :: ReadS Operator
readsOp = reads

data ParseError = UnusedOperators   [Operator]
                | UnusedOperands    [Expr]
                | NotEnoughOperators
                | NotEnoughOperands Operator
                | EmptyInput
                | UnidentifiedToken String
    deriving (Show, Eq)

parseToExpr :: String -> Either ParseError Expr
parseToExpr = shuntingYard True [] []

-- Shunting Yard Algorithm
-- Look upon, ye mortals, and despair
shuntingYard :: Bool -> [Expr] -> [[Operator]] -> String -> Either ParseError Expr
shuntingYard unary []     []        "" = Left EmptyInput
shuntingYard unary []     operators "" = Left $ UnusedOperators $ concat operators
shuntingYard unary [expr] operators "" = Right $ expr
shuntingYard unary exprs  []        "" = Left $ UnusedOperands exprs
shuntingYard unary exprs  operators "" = head <$> applyAllOps exprs operators
shuntingYard unary exprs  operators input
    | not $ null $ readsInt input 
    = let (i,rest):_ = readsInt input
          exprs'     = Num i : exprs
       in shuntingYard False exprs' operators rest
    | not $ null $ readsOp input 
    = let (op,rest):_ = readsOp input
          ops' = insertOps operators op
       in if op == Subtract && unary == True
          then if not $ null $ readsInt input
              then do
                  let (i,rest'):_ = readsInt rest
                  let exprs'      = Num (-i) : exprs
                  shuntingYard False exprs' operators rest'
              else Left $ NotEnoughOperands Subtract
          else if opNeedsPopping operators op
          then do
              (nextOps, remOps) <- extractNextOps ops'
              exprs' <- applyOps exprs nextOps
              shuntingYard True exprs' remOps rest
          else shuntingYard True exprs ops' rest
    | otherwise                   
    = Left $ UnidentifiedToken $ fst $ head $ lex input

insertOps :: [[Operator]] -> Operator -> [[Operator]]
insertOps [] newOp = [[newOp]]
insertOps ops OpenParen  = [OpenParen]  : ops
insertOps ops CloseParen = [CloseParen] : ops
insertOps ([OpenParen]:ops) op = [op] : [OpenParen] : ops
insertOps (op:ops) newOp 
    | newOp == head op && rassoc newOp && rassoc (head op) = (newOp:op):ops
    | newOp > head op = [newOp] : op : ops
    | otherwise  = op : insertOps ops newOp

opNeedsPopping :: [[Operator]] -> Operator -> Bool
opNeedsPopping [] _                       = False
opNeedsPopping ([OpenParen]:_) newOp      = False
opNeedsPopping _               CloseParen = True
opNeedsPopping ops newOp                  
  = let op = head $ head ops
     in or [ newOp < op 
           , and [ newOp == op 
                 , not $ rassoc newOp
                 , not $ rassoc op
                 ]
           ]

extractNextOps :: [[Operator]] -> Either ParseError ([Operator], [[Operator]])
extractNextOps []                  = Left NotEnoughOperators
extractNextOps xs@([CloseParen]:x) = extractWithinParen xs
extractNextOps (x:xs)              = Right (x,xs)

extractWithinParen :: [[Operator]] -> Either ParseError ([Operator], [[Operator]])
extractWithinParen ([CloseParen]:x:[OpenParen]:xs) 
  = Right $ (x,xs)
extractWithinParen ([CloseParen]:xs)               
  = Left $ UnusedOperators $ concat $ takeWhile (/=[CloseParen]) xs

applyAllOps :: [Expr] -> [[Operator]] -> Either ParseError [Expr]
applyAllOps = foldM applyOps

applyOps :: [Expr] -> [Operator] -> Either ParseError [Expr]
applyOps = foldM applyOp

applyOp :: [Expr] -> Operator -> Either ParseError [Expr]
applyOp = applyBinaryOp

applyBinaryOp :: [Expr] -> Operator -> Either ParseError [Expr]
applyBinaryOp (a:b:xs) op = Right $ case op of
                              Exponentiate -> (b :**: a):xs
                              Times        -> (b :*: a):xs
                              Divide       -> (b :/: a):xs
                              Add          -> (b :+: a):xs
                              Subtract     -> (b :-: a):xs
                              And          -> (b :&: a):xs
                              Xor          -> (b :^: a):xs
                              Or           -> (b :|: a):xs
                              Equals       -> (b :=: a):xs
applyBinaryOp _        op = Left $ NotEnoughOperands op

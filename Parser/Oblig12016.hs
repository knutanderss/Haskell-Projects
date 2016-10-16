module Oblig12016 where
import Data.Char
import Data.List
import Data.Maybe

-- Shell --
main = prelude emptyCont emptyMem
prelude c m = do
  putStr "> "; l <- getLine; let (r, c', m') = eval (parse l) c m;
  putStrLn $ show r
  do prelude c' m'

run :: String -> Ast
run s = let (r, _, _) = eval (parse s) emptyCont emptyMem in r

-- Tokenizer --
type Tokens = [String]
doubleChars = ["==", "!=", "->"]
tokenize :: String -> [String]
tokenize [] = [];
tokenize tokens@(x:xs)
    | x == ' ' = tokenize xs -- skip char
    | length (x:xs) >= 2 && take 2 tokens `elem` doubleChars = take 2 tokens : tokenize (drop 2 tokens)
    | x `elem` "()=!<>*-+/;,." = [x]:tokenize xs -- token with 1 char
    | isAlpha x = let (w, rest) = span isAlpha tokens in w:tokenize rest
    | isPrefixOf "case" tokens = "case":tokenize (drop 4 tokens)
    | isPrefixOf "otherwise" tokens = "otherwise":tokenize (drop 9 tokens)
    | isDigit x = let (i,rest) = span isDigit tokens in i:tokenize rest -- token is a digit
    | otherwise = error ("Not a valid token: " ++ [x]) -- token is invalid

data Ast =  Number Integer | Name String | App Ast [Ast] | Block [Ast] | Case Ast [Ast] | Bool Ast Ast Ast
            | Default | Set String Ast | Lambda String Ast | Function String Ast Context deriving (Eq, Show, Ord)

-- Memory --
type Memory = (Integer, Integer -> Maybe Ast)
newtype Context = Context (String -> Maybe Integer)
instance Show Context where show _ = ""
instance Eq   Context where (==) _ _ = True
instance Ord  Context where (<=) _ _ = True

emptyMem = (0, const Nothing)
emptyCont = Context (const Nothing)

lookupMem :: Memory -> Integer -> Maybe Ast
lookupMem (_, f) = f

lookupCtx :: Context -> String -> Maybe Integer
lookupCtx (Context f) = f

addToMem :: Memory -> Ast -> Memory
addToMem (c, f) ast = (succ c, \k -> if k==c then Just ast else f k)

updateMem :: String -> Ast -> Context -> Memory -> Memory
updateMem var ast (Context c) (memI, memF) =
    let Just i = c var in (memI, \k -> if k==i then Just ast else memF k)

addToCtx :: Context -> String -> Integer -> Context
addToCtx (Context f) s i = Context (\k -> if k==s then Just i else f k)

addVarToMem :: String -> Ast -> Context -> Memory -> (Context, Memory)
addVarToMem s a c m@(i, f) = (addToCtx c s i, addToMem m a)

lookupVar :: String -> Context -> Memory -> Ast
lookupVar s c m
    | isNothing i = error (errorMsg ++ " ctx.")
    | isNothing a = error (errorMsg ++ " mem.")
    | otherwise   = fromJust a
    where   i = lookupCtx c s
            a = lookupMem m (fromJust i)
            errorMsg = "Variable " ++ s ++ " is not initialized yet."

-- Parser --
parse :: String -> Ast
parse [] = error "Missing expression"
parse tokens = fst (parseBlock (tokenize tokens))

parseBlock :: Tokens -> (Ast, Tokens)
parseBlock tokens
    | last tokens /= ";" = error "Missing ending ';'"
    | otherwise = (Block (getExprFromBlock tokens), [])

getExprFromBlock :: Tokens -> [Ast]
getExprFromBlock [] = [];
getExprFromBlock (";":xs) = error "Expression cannot start with ';'"
getExprFromBlock tokens =
    let exp = parseExpr (takeWhile (/=";") tokens)
        rest = tail(dropWhile (/=";") tokens)
    in  fst exp : getExprFromBlock rest

parseExpr :: Tokens -> (Ast, Tokens)
parseExpr exp@("(":xs) = parseApp exp
parseExpr exp@("case":xs) = parseCase exp
parseExpr set@("set":xs) = parseSet set
parseExpr lam@("lambda":xs) = parseLambda lam
parseExpr (x:xs)
    | isDigit (head x) = (Number (read x), xs)
    | isLetter (head x) = if not(null xs) && head xs == "(" then parseApp (x:xs) else (Name x, xs)
    | otherwise = error (x ++ " is not a valid beginning of expression. Rest: " ++ concat xs)
parseExpr [] = error "Tried to parse an empty expression"

parseApp :: Tokens -> (Ast, Tokens)
-- In case of function application
parseApp ("(":xs) =
    let (exp1, komma:ys) = parseExpr xs
        (exp2, closing:f:zs) = parseExpr ys
    in  if komma /= "," then error ("Expected ',' instead of: " ++ komma)
        else if closing /= ")" then error ("Expected ')' instead of: " ++ closing)
        else (App (Name f) [exp1, exp2], zs)
-- In case of <Var> (<Expr>)
parseApp (x:lP:xs) =
    let (exp, rP:rest) = parseExpr xs
    in  if lP /= "(" || rP /= ")"
        then error "Expected expression inside parenthesis"
        else (App (Name x) [exp], rest)

parseCase :: Tokens -> (Ast, Tokens)
parseCase ["case","otherwise","->"] = error "Expected expression after '->'"
parseCase ("case":"otherwise":"->":xs) =
    let (exp, expLeft) = parseExpr xs
    in  if null expLeft || head expLeft /= "." then error ("Expected '.' after case")
        else (Case Default [exp], tail expLeft)
parseCase ("case":"otherwise":_) = error "Exptected '->' after otherwise"
parseCase ["case"] = error "Expected expression after 'case'"
parseCase ("case":xs) =
    let (a, arrow:aLeft) = parseBool xs
        (b, komma:bLeft) = parseExpr aLeft
        (c, cLeft) = parseCase bLeft
    in  if arrow /= "->" then error ("Expected '->', was " ++ arrow)
        else if komma /= "," then error ("Expected ',', was " ++ komma)
        else (Case a [b, c], cLeft)

parseBool :: Tokens -> (Ast, Tokens)
parseBool (begPar:xs) =
    let (a, komma:aLeft) = parseExpr xs
        (b, closePar:op:bLeft) = parseExpr aLeft
    in  if begPar /= "(" then error ("Expected '(', was " ++ begPar)
        else if komma /= "," then error ("Expected ',', was " ++ komma)
        else if closePar /= ")" then error ("Expected ')', was " ++ closePar)
        else (Bool (Name op) a b, bLeft)

parseSet :: Tokens -> (Ast, Tokens)
parseSet ["set"] = error "Missing variable name after 'set'"
parseSet ("set":var@(y:ys):xs)
    | not(isLetter y) = error "Missing variable name after 'set'"
    | otherwise = let (exp, rest) = parseExpr xs in  (Set var exp, rest)

parseLambda :: Tokens -> (Ast, Tokens)
parseLambda ("lambda":var:lP:xs) =
    let (e, rest) = parseExpr xs
    in  if lP /= "(" || null rest || head rest /= ")"
        then error "Expected paranthesis around expression"
        else if null (tail rest) || head (tail rest) /= "(" then (Lambda var e, tail rest) -- Check if lambda has expression
        else    let (lP2:rest') = tail rest; (e2, rest2) = parseExpr rest'
                in  if null rest2 || head rest2 /= ")" then error "Expected parenthesis around expression"
                    else (App (Lambda var e) [e2], tail rest2)
parseLambda ["lambda", var] = error "Exptected '(' after variable"
parseLambda ["lambda"] = error "Expected variable after 'lambda'"

--------------------------------------------------------
-- Eval --

eval :: Ast -> Context -> Memory -> (Ast, Context, Memory)
eval (Block []) _ _ = error "No expression found."
eval (Block [a]) c m = eval a c m
eval (Block (a:as)) c m =
    let (_, c2, m2) = eval a c m
    in  eval (Block as) c2 m2
eval (App (Name op) [exp1, exp2]) c m = -- Arithmetic operation
    let (Number e1, c1, m1) = eval exp1 c m
        (Number e2, c2, m2) = eval exp2 c1 m1
    in  (Number (evalArithmetic op e1 e2), c2, m2)
eval (App (Lambda var lamExp) [exp]) c m = eval (App (Function var lamExp c) [exp]) c m
eval (App (Name n) [exp]) c m = eval (App (lookupVar n c m) [exp]) c m
eval (App (Function var lamExp lamC) [Name x]) c m = -- Evaluating lambda, call by ref
    let index = lookupCtx c x in  if isJust index then
            let c' = addToCtx lamC var (fromJust index); (r, c2, m2) = eval lamExp c' m
            in  (r, c, m2)
        else error ("Variable is not initialized yet: " ++ x)
eval (App (Function var lamExp lamC) [exp]) c m = -- Evaluating lambda, call by val
    let (exp', c2, m2) = eval exp c m
        (c3, m3) = addVarToMem var exp' lamC m2
        (exp'', _, m4) = eval lamExp c3 m3
    in  (exp'', c2, m4)
eval (Number i) c m = (Number i, c, m)
eval (Name v) c m = (lookupVar v c m, c, m)
eval (Lambda v e) c m = (Function v e c, c, m)
eval (Case bool [e1, e2]) c m | b = eval e1 c1 m1 | otherwise = eval e2 c1 m1
    where (b, c1, m1) = evalBool bool c m
eval (Case Default [e]) c m = eval e c m
eval (Set s a) c m =let (exp, c2, m2) = eval a c m; mI = lookupCtx c2 s
                    in  if isJust mI then let m3 = updateMem s exp c2 m2 in (exp, c2, m3)
                    else  let (c3, m3) = addVarToMem s exp c2 m2 in  (exp, c3, m3)
eval a _ _ = error ("Not valid App: " ++ show a)

evalArithmetic :: String -> Integer -> Integer -> Integer
evalArithmetic o = fromJust $ lookup o [("+",(+)),("-",(-)),("*",(*)),("/",div)]

evalBool :: Ast -> Context -> Memory -> (Bool, Context, Memory)
evalBool (Bool (Name op) a b) c m =
    let operator = lookup op [("==",(==)),("!=",(/=)),("<",(<)),(">",(>))]
    in  (fromJust operator n1 n2, c2, m2)
    where   (Number n1, c1, m1) = eval a c m
            (Number n2, c2, m2) = eval b c1 m1

import Data.Char
import Data.List
import Data.Maybe

run :: String -> Ast
run s = eval (parse s) emptyCont emptyMem

------------------------------------------------------
-- Tokenizer --

type Tokens = [String]

skipChars = " "
singleChars = "()=!<>*-+/;,.";
doubleChars = ["==", "!=", "->"]
keywords = ["case", "otherwise"]
tokenize :: String -> [String]
tokenize [] = [];
tokenize tokens@(x:xs)
    | x `elem` skipChars = (tokenize xs) -- skip char
    | length (x:xs) >= 2 && (take 2 tokens) `elem` doubleChars -- token with 2 chars
        = (take 2 tokens) : tokenize (drop 2 tokens)
    | x `elem` singleChars = ([x]:(tokenize xs)) -- token with 1 char
    | isLetter x = -- token is beginning of a word
        let (word, rest) = getWordFromString tokens
        in  (word:tokenize rest)
    | isJust (getKeywordFromString tokens keywords) = -- token is a keyword
        let Just(s, sLeft) = getKeywordFromString tokens keywords
        in s:(tokenize sLeft)
    | isDigit x = [digit] ++ (tokenize rest) -- token is a digit
    | otherwise = error ("Not a valid token: " ++ [x]) -- token is invalid
        where   digit = takeWhile isDigit tokens
                rest = dropWhile isDigit tokens

getKeywordFromString :: String -> [String] -> Maybe (String, String)
getKeywordFromString tokens [] = Nothing
getKeywordFromString tokens (word:xs)
    | word `isPrefixOf` tokens = Just(word, (drop (length word) tokens))
    | otherwise = getKeywordFromString tokens xs

getWordFromString :: String -> (String, String)
getWordFromString [] = ([], []) 
getWordFromString t@(x:xs)
    | isLetter x = 
        let (word, rest) = getWordFromString xs
        in  ((x:word), rest)
    | otherwise = ([], t)


------------------------------------------------------
-- Grammar --

data Ast =  Number Integer | Name String | App Ast [Ast] | Block [Ast] | 
            Case Ast [Ast] | Bool Ast Ast Ast | Default | Set String Ast |
            Lambda String Ast       deriving (Eq, Show, Ord)


------------------------------------------------------
-- Memory --

type Memory = (Integer, Integer -> Maybe Ast)
newtype Context = Context (String -> Maybe Integer)

emptyMem = (0, const Nothing)
emptyCont = Context (const Nothing)

lookupMem :: Memory -> Integer -> Maybe Ast
lookupMem (_, f) key = f key 

lookupCtx :: Context -> String -> Maybe Integer
lookupCtx (Context f) s = f s

addToMem :: Memory -> Ast -> Memory
addToMem (curKey, f) ast =
    (succ curKey, \k -> if k==curKey then Just ast else f k) 

addToCtx :: Context -> String -> Integer -> Context
addToCtx (Context f) s i = Context (\k -> if k==s then Just i else f s)

addVarToMem :: String -> Ast -> Context -> Memory -> (Context, Memory)
addVarToMem s a c m@(i, f) = (addToCtx c s i, addToMem m a)

lookupVar :: String -> Context -> Memory -> Ast
lookupVar s c m
    | isNothing i = error errorMsg 
    | isNothing a = error errorMsg
    | otherwise   = fromJust a
    where   i = lookupCtx c s 
            a = lookupMem m (fromJust i)
            errorMsg = ("Variable " ++ s ++ " is not initialized yet.")


------------------------------------------------------
-- Parser --

parse :: String -> Ast
parse [] = error "Missing expression"
parse tokens = fst (parseBlock (tokenize tokens))

parseBlock :: Tokens -> (Ast, Tokens)
parseBlock tokens
    | (last tokens) /= ";" = error "Missing ending ';'"
    | otherwise = (Block (getExprFromBlock tokens), [])

getExprFromBlock :: Tokens -> [Ast]
getExprFromBlock [] = [];
getExprFromBlock (";":xs) = error "Expression cannot start with ';'"
getExprFromBlock tokens = 
    let exp = (parseExpr (takeWhile (/=";") tokens))
        rest = tail(dropWhile (/=";") tokens)
    in  (fst exp) : (getExprFromBlock rest)

parseExpr :: Tokens -> (Ast, Tokens)
parseExpr exp@("(":xs) = parseApp exp
parseExpr exp@("case":xs) = parseCase exp
parseExpr set@("set":xs) = parseSet set
parseExpr (x:xs) 
    | isDigit (head x) = (Number (read x), xs)
    | isLetter (head x) = (Name x, xs)
    | otherwise = error (x ++ " is not a valid beginning of expression. Rest: " ++ (concat xs))

parseApp :: Tokens -> (Ast, Tokens)
parseApp ("(":xs) = 
    let (exp1, (komma:ys)) = parseExpr xs 
        (exp2, (closing:f:zs)) = parseExpr ys 
    in  if komma /= "," then error ("Expected ',' instead of: " ++ komma)
        else if closing /= ")" then error ("Expected ')' instead of: " ++ closing)
        else (App (Name f) [exp1, exp2], zs)

parseCase :: Tokens -> (Ast, Tokens)
parseCase ("case":"otherwise":"->":xs) =
    let (exp, (".":expLeft)) = parseExpr xs
    in  (exp, expLeft)
parseCase ("case":"otherwise":x:xs) = error ("Exptected '->', was " ++ x)
parseCase ("case":"otherwise":[]) = error ("Expected '->' at end of file")
parseCase ("case":xs) =
    let (a, (arrow:aLeft)) = parseBool xs
        (b, (komma:bLeft)) = parseExpr aLeft
        (c, cLeft) = parseCase bLeft
    in  if arrow /= "->" then error ("Expected '->', was " ++ arrow)
        else if komma /= "," then error ("Expected ',', was " ++ komma)
        else ((Case a [b, c]), cLeft)
        
parseBool :: Tokens -> (Ast, Tokens)
parseBool ("(":xs) = 
    let (a, (",":aLeft)) = parseExpr xs
        (b, (")":op:bLeft)) = parseExpr aLeft
    in  ((Bool (Name op) a b), bLeft)

parseSet :: Tokens -> (Ast, Tokens)
parseSet ("set":[]) = error "Missing variable name after 'set'"
parseSet ("set":var@(y:ys):xs) 
    | not(isLetter y) = error "Missing variable name after 'set'"
    | otherwise =
        let (exp, rest) = parseExpr xs
        in ((Set var exp), rest)


--------------------------------------------------------
-- Eval --

eval :: Ast -> Context -> Memory -> Ast
eval ast c m = let (a, c1, c2) = evalBlock ast c m in a

evalBlock :: Ast -> Context -> Memory -> (Ast, Context, Memory)
evalBlock (Block []) _ _ = error "No expression found."
evalBlock (Block (a:[])) c m = evalExp a c m
evalBlock (Block (a:as)) c m =
    let (_, c2, m2) = evalExp a c m
    in  evalBlock (Block as) c2 m2

evalApp :: Ast -> Context -> Memory -> (Ast, Context, Memory)
evalApp (App (Name op) [exp1, exp2]) c m = 
    let (Number e1, c1, m1) = evalExp exp1 c m
        (Number e2, c2, m2) = evalExp exp2 c1 m1
    in  ((Number (evalArithmetic op e1 e2)), c2, m2)
evalApp a b c = (error ("Not valid App: " ++ (show a)))

evalArithmetic :: String -> Integer -> Integer -> Integer
evalArithmetic "+" a b = a+b;
evalArithmetic "-" a b = a-b;
evalArithmetic "*" a b = a*b;
evalArithmetic "/" a b = a `div` b;

evalExp :: Ast -> Context -> Memory -> (Ast, Context, Memory)
evalExp (Number i) c m = (Number i, c, m)
evalExp (Case b expr) c m = evalCase (Case b expr) c m
evalExp (App n expr) c m = evalApp (App n expr) c m
evalExp (Name v) c m = ((lookupVar v c m), c, m)
evalExp set@(Set n a) c m = evalSet set c m

evalCase :: Ast -> Context -> Memory -> (Ast, Context, Memory)
evalCase (Case bool [e1, e2]) c m 
    | b = evalExp e1 c1 m1
    | otherwise = evalExp e2 c1 m1
    where (b, c1, m1) = evalBool bool c m 

evalBool :: Ast -> Context -> Memory -> (Bool, Context, Memory)
evalBool (Bool (Name op) a b) c m 
    | op == "==" = (n1 == n2, c2, m2)
    | op == "!=" = (n1 /= n2, c2, m2)
    | op == "<"  = (n1 <  n2, c2, m2)
    | op == ">"  = (n1 >  n2, c2, m2)
    | otherwise = error ("Illegal operator: " ++ op)
    where   (e1, c1, m1) = evalExp a c m
            (e2, c2, m2) = evalExp b c1 m1
            (Number n1) = e1
            (Number n2) = e2

evalSet :: Ast -> Context -> Memory -> (Ast, Context, Memory)
evalSet (Set s a) c m = 
    let (exp, c2, m2) = evalExp a c m
        (c3, m3) = addVarToMem s exp c m
    in  (exp, c3, m3)

























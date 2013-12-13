{- Position and errors -}

data E a = Success a | Error String

instance (Show a) => Show (E a) where
    show (Success v) = "Success: " ++ show v
    show (Error s)  = "Error: " ++ s

instance Monad E where
    return x = Success x
    (>>=) (Success x) f = f x
    (>>=) (Error s) f = Error s

errorE s = Error s

--data P a = P (Position -> E a)
newtype P a = Pos (Position -> E a)
appP (Pos f) = f

instance Monad P where
    return x = Pos $ \p -> return x
    (Pos m) >>= k = Pos $ \p -> m p >>= (\x -> appP (k x) p)

instance (Show a) => Show (P a) where
    show (Pos m) = show (m pos0)

errorP s = Pos (\p -> errorE (showpos p ++ ": " ++ s))

resetP      :: Position -> P x -> P x
resetP q m = Pos (\p -> appP m q)

-- base interpreter
type Name = String

type Position = Int
showpos = show
pos0 = 0

data Term = Var Name
            | Con Int
            | Add Term Term
            | Lam Name Term
            | App Term Term
            | At Position Term

instance Show Value where
    show Wrong = "<wrong>"
    show (Num i) = show i
    show (Fun f) = "<function>"

data Value = Wrong
            | Num Int
            | Fun (Value -> P Value)

type Environment = [(Name, Value)]

interp              :: Term -> Environment -> P Value
interp (Var x) e    = find x e
interp (Con i) e    = return (Num i)
interp (Add u v) e  = 
                interp u e >>= (\a ->
                interp v e >>= (\b ->
                add a b))

interp (Lam x v) e  = return (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e  = interp t e >>= (\f ->
                      interp u e >>= (\a ->
                      apply f a))
interp (At p t) e   = resetP p (interp t e)

apply               :: Value -> Value -> P Value
apply (Fun k) v     = k v
apply f a           = errorP $ "should be a function: " ++ show f

test                :: Term -> String
test t              = show (interp t [])

find                :: Name -> Environment -> P Value
find el l           =   case ans of
                            Just val -> return val
                            Nothing  -> errorP $ "unbound variable: " ++ el
                where ans = lookup el l

add                 :: Value -> Value -> P Value
add (Num i) (Num j) = return $ Num $ i + j
add a b             = errorP $ "should be numbers: " ++ show a
                                ++ ", " ++ show b

-- Tests ------------
term1               = Add (Con 1) (Con 2)
term2               = App (Lam ("x") (Add (Var "x") (Var "x"))) 
                        (Con 5)
term3               = (At 2 (Add (Con 1) (At 1 (Lam "x" (Var "x")))))
term4               = App (Lam ("x") (Add (Var "x") (Var "x")))
                        (Lam ("x") (Var "x"))
term5               = App (Lam ("x") (Add (Var "y") (Var "x"))) 
                        (Con 5)
{- Monad paper, Wadler -}

{- Error msgs -}

data M a = Success a | Error String

instance (Show a) => Show (M a) where
	show (Success v) = "Success: " ++ show v
	show (Error s) 	= "Error: " ++ s

instance Monad M where
	return x = Success x
	(>>=) (Success x) f = f x
	(>>=) (Error s) f = Error s

-- base interpreter
type Name = String

data Term = Var Name
			| Con Int
			| Add Term Term
			| Lam Name Term
			| App Term Term

instance Show Value where
	show Wrong = "<wrong>"
	show (Num i) = show i
	show (Fun f) = "<function>"

data Value = Wrong
			| Num Int
			| Fun (Value -> M Value)

type Environment = [(Name, Value)]

interp 				:: Term -> Environment -> M Value
interp (Var x) e 	= find x e
interp (Con i) e 	= return (Num i)
interp (Add u v) e 	= 
				interp u e >>= (\a ->
				interp v e >>= (\b ->
				add a b))

interp (Lam x v) e 	= return (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e 	= interp t e >>= (\f ->
					  interp u e >>= (\a ->
					  apply f a))

apply				:: Value -> Value -> M Value
apply (Fun k) v 	= k v
apply f a 			= Error $ "should be a function: " ++ show f

test 				:: Term -> String
test t				= show (interp t [])

find 				:: Name -> Environment -> M Value
find el l 			= 	case ans of
							Just val -> return val
							Nothing  -> Error $ "unbound variable: " ++ el
				where ans = lookup el l

add 			 	:: Value -> Value -> M Value
add (Num i) (Num j) = return $ Num $ i + j
add a b 			= Error $ "should be numbers: " ++ show a
								++ ", " ++ show b

-- Tests ------------
term1 				= Add (Con 1) (Con 2)
term2 				= App (Lam ("x") (Add (Var "x") (Var "x"))) 
						(Con 5)
term3				= Add (Con 1) (Lam "x" (Var "x"))
term4 				= App (Lam ("x") (Add (Var "x") (Var "x")))
						(Lam ("x") (Var "x"))
term5 				= App (Lam ("x") (Add (Var "y") (Var "x"))) 
						(Con 5)


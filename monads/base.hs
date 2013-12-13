{- identity Monad -}

newtype M a = Id a

instance Monad M where
	return x = Id x
	(>>=) (Id x) f = f x

instance (Show a) => Show (M a) where
	show (Id a) = show a

-- base interpreter
type Name = String

data Term = Var Name
			| Con Int
			| Add Term Term
			| Lam Name Term
			| App Term Term

data Value = Wrong
			| Num Int
			| Fun (Value -> M Value)

instance Show Value where
	show Wrong		= "<wrong>"
	show (Num i) 	= show i
	show (Fun f)	= "<function>"


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
apply f a 			= return Wrong

test 				:: Term -> String
test t				= show (interp t [])

find 				:: Name -> Environment -> M Value
find el l 			= 	case ans of
							Just val -> return val
							Nothing  -> return Wrong
				where ans = lookup el l

add 			 	:: Value -> Value -> M Value
add (Num i) (Num j) = return (Num $ i + j)
add a b 			= return Wrong

-- Tests ------------
term1 				= Add (Con 1) (Con 2)
term2 				= App (Lam ("x") (Add (Var "x") (Var "x"))) 
						(Con 5)
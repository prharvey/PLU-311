{- call-by-name -}

newtype S a = S (State -> (a, State))

instance Monad S where
	return x 		= S $ \s -> (x, s)
	(S x) >>= f 	= S (\s0 ->let 
								(a, s1) = x s0
								(b, s2) = app (f a) s1
								app (S x) arg = x arg
							in (b, s2))

instance (Show a) => Show (S a) where
	show (S x) = 	let (a, s1) = x 0
					in "Value: " ++ show a ++ "; " ++
						"Count: " ++ show s1

type State = Int

tickS		:: S ()
tickS		= S (\s -> ((), s+1))

fetchS		:: S State
fetchS		= S (\s -> (s, s))

-- base interpreter
type Name = String

data Term = Var Name
			| Con Int
			| Add Term Term
			| Lam Name Term
			| App Term Term
			| Count

data Value = Wrong
			| Num Int
			| Fun (S Value -> S Value)

instance Show Value where
	show Wrong		= "<wrong>"
	show (Num i) 	= show i
	show (Fun f)	= "<function>"


type Environment = [(Name, S Value)]

interp 				:: Term -> Environment -> S Value
interp (Var x) e 	= find x e
interp (Con i) e 	= return (Num i)
interp (Add u v) e 	= 
					interp u e >>= (\a ->
					interp v e >>= (\b ->
					add a b))

interp (Lam x v) e 	= return (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e 	= interp t e >>= (\f ->
					  apply f (interp u e))
interp Count e 		= fetchS >>= (\i -> return (Num i))

apply				:: Value -> S Value -> S Value
apply (Fun k) v 	= tickS >>= (\s -> k v)
apply f a 			= return Wrong

test 				:: Term -> String
test t				= show (interp t [])

find 				:: Name -> Environment -> S Value
find el l 			= 	case ans of
							Just val -> val
							Nothing  -> return Wrong
				where ans = lookup el l

add 			 	:: Value -> Value -> S Value
add (Num i) (Num j) = tickS >>= (\s -> return $ Num $ i + j)
add a b 			= return Wrong

-- Tests ------------
term1 				= Add (Con 1) (Con 2)
term2 				= App (Lam ("x") (Add (Var "x") (Var "x"))) 
						(Add (Con 5) (Con 1))
term3 				= Add term1 term1
term4 				= (Con 1)
term5 				= Add (Count) (Count)
term6 				= App (Lam ("x") (Add (Var "x") (Var "x"))) 
						(Count)
term7				= Add (Add (Con 1) (Con 2)) (Add (Con 3) (Con 4))
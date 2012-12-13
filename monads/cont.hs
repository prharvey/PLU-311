{- continuation Monad -}

--newtype K a = K ((a -> Answer) -> Answer)

type K a = (a -> Answer) -> Answer

{-instance Monad K where
	return x 		= K $ \c -> c x
	(K x) >>= f 	= \c -> x (\a -> app (f a) c)
					  where app (K cont) arg = cont arg

instance (Show a) => Show (K a) where
	show (K x) = show (x id)-}

unitK a		= \c -> c a
m `bindK` k = \c -> m (\a -> k a c)

--showK 		:: K Value -> String
showK a 	= show (a id)

type Answer = Value

callccK		:: ((a -> K b) -> K a) -> K a
callccK h 	= \c -> let k a = \d -> c a in h k c

-- cps interpreter
type Name = String

data Term = Var Name
			| Con Int
			| Add Term Term
			| Lam Name Term
			| App Term Term
			| Callcc Name Term

data Value = Wrong
			| Num Int
			| Fun (Value -> K Value)

instance Show Value where
	show Wrong		= "<wrong>"
	show (Num i) 	= show i
	show (Fun f)	= "<function>"


type Environment = [(Name, Value)]

interp 				:: Term -> Environment -> (Value -> Answer) -> Answer
interp (Var x) e 	= \c -> find x e c
interp (Con i) e 	= \c -> c (Num i)
interp (Add u v) e 	= 
				\c -> 
				interp u e (\a ->
				interp v e (\b ->
				add a b c))

interp (Lam x v) e 	= \c -> c (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e 	= \c -> 
					  interp t e (\f ->
					  interp u e (\a ->
					  apply f a c))
interp (Callcc x v) e = callccK (\k -> interp v ((x, Fun k):e))

apply				:: Value -> Value -> K Value
apply (Fun f) v c	= (f v) c
apply f a c 		= Wrong

test 				:: Term -> String
test t				= show (interp t [] id)

find 				:: Name -> Environment -> K Value
find el l 			= 	case ans of
							Just val -> unitK val
							Nothing  -> unitK Wrong
				where ans = lookup el l

add 			 	:: Value -> Value -> K Value
add (Num i) (Num j) c 	= c (Num $ i + j)
add a b c 			=  Wrong

-- Tests ------------
term1 				= Add (Con 1) (Con 2)
term2 				= App (Lam ("x") (Add (Var "x") (Var "x"))) 
						(Add (Con 5) (Con 1))
term3 				= Add term1 term1
term4 				= (Con 1)
term5				= Add (Con 1) (Callcc "k" (Add (Con 10) (App (Var "k") (Con 4))))
term6				= App (Lam ("x") (Callcc "k" (Var "k")))
						(Con 1)
module Suma where
import Numeric.Natural(Natural)
import Test.QuickCheck( Arbitrary(..), arbitrarySizedNatural, shrink )

--Datatype Nat (naturals)
data Nat = Zero | Succ Nat
        deriving (Eq, Show)


--Instance for Natural numbers (Function parameters)
fromNatural :: Natural -> Nat
fromNatural 0 = Zero
fromNatural n = Succ (fromNatural $ pred n)


--Instance of Datatype nat as list
instance Arbitrary Nat where
        arbitrary = fmap fromNatural arbitrarySizedNatural
        
        shrink Zero     = []
        shrink (Succ n) = n : shrink n


--Functor that produces recursion with Nats
--
recNat :: a -> (Nat -> a -> a) -> Nat -> a
recNat a _ Zero     = a											--F(a, h, 0) = a
recNat a h (Succ n) = h n (recNat a h n)		--F(a, h, n) = h(n-1, F(a, h, n-1)


--Addition Functions. 2 Implementations
--
addP :: Nat -> Nat -> Nat
addP  Zero      n = n												--Zero as module for addition 
addP  (Succ m)  n = Succ (addP m n)					--Recursive performance 1+1+...	

addR :: Nat -> Nat -> Nat
addR m n = recNat n (\ _ y -> Succ y) m			--Nest m successors after n


--Predecessor function. 2 implementations
--
predC :: Nat -> Nat
predC Zero			= Zero
predC (Succ n) = n

predR :: Nat -> Nat
predR Zero        = Zero
predR (Succ n)    = recNat n (\ x _ -> x) Zero
--Auxiliar function to create lists
--return list of size n with m as every element
--
auxList :: Nat -> Nat -> [Nat]
auxList m Zero			= [Zero]
auxList m (Succ n)	= m:auxList m n


--Product functios. 2 implementations
--
prodP :: Nat -> Nat -> Nat
prodP m n = foldl addR Zero (auxList m n)		--Folds over a list with addR

prodR :: Nat -> Nat -> Nat
prodR Zero a		 = Zero											--Base case for the recursion
		--Nest n succesors after n (m times)
prodR m n = recNat n (\ _ y -> Succ y) (prodR (predC m) n)


--Power functions. 2 implementations
--
powP :: Nat -> Nat -> Nat
powP x Zero = Succ Zero
powP x (Succ Zero) = x
powP x (Succ n)    = prodR x (powP x n)

powR :: Nat -> Nat -> Nat
powR a Zero        = Succ Zero
powR a (Succ Zero) = a
powR x (Succ n)=recNat (powP x n) (\ _ y -> Succ y) (prodR (predC x) (powP x n))



--Dummy test variables
t1 = fromNatural 2
t2 = fromNatural 3

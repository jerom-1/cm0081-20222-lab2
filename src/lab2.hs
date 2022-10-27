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


--Auxiliar function to create lists
--return list of size n with m as every element
--
auxList :: Nat -> Nat -> [Nat]
auxList m Zero			= [Zero]
auxList m (Succ n)	= m:auxList m n


--Product fucntios. 2 implementations
--
prodP :: Nat -> Nat -> Nat
prodP m n = foldl addR Zero (auxList m n)		--Folds over a list with addR

prodR :: Nat -> Nat -> Nat
prodR Zero a		 = Zero											--Base case for the recursion
		--Nest n succesors after n (m times)
prodR (Succ m) n = recNat n (\ _ y -> Succ y) (prodR m n)

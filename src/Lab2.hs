module Main where
import PRF
import Numeric.Natural()
import Test.QuickCheck(verboseCheck)


--Instances of functions to test
--
comparator :: [Nat->Nat->Nat]
comparator = [predP,idenP,prodP,powP,addP]

check :: [Nat->Nat->Nat]
check      = [predR,idenR,prodR,powR,addR]


--Property check function
--
propCheckBi :: [Nat -> Nat -> Nat] -> [Nat -> Nat -> Nat] ->  Nat -> Nat -> Bool 
propCheckBi [] [] _ _         = True
propCheckBi [] _ _ _          = True
propCheckBi _ [] _ _          = True
propCheckBi (x:xs) (y:ys) i j = (x i j == y i j) && propCheckBi xs ys i j

prop_dummy :: (Nat -> Nat -> Nat) -> Nat -> Nat -> Bool
prop_dummy x i j = x i j == powP i j 

main :: IO ()
main = (verboseCheck $ propCheckBi comparator check) 
--main = verboseCheck $ (propCheckBi binaryComp binaryCheck)
--main= verboseCheck $ prop_dummyuni facR

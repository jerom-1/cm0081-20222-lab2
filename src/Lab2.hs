module Main where
import PRF
import Numeric.Natural()
import Test.QuickCheck(verboseCheck)
--
unaryComp  = [predC,idenP]

binaryCheck = [prodR,powR]
unaryCheck  = [predR,idenR]

propCheckUn :: [Nat -> Nat] -> [Nat -> Nat] -> Nat -> Bool
--function, number, state
propCheckUn [] _ _ = True
propCheckUn (x:xs) (y:ys) i = (x i == y i) && propCheckUn xs ys i

propCheckBi :: [Nat -> Nat -> Nat] -> [Nat -> Nat -> Nat] ->  Nat -> Nat -> Bool 
propCheckBi [] _ _ _          = True
propCheckBi (x:xs) (y:ys) i j = (x i j == y i j) && propCheckBi xs ys i j

prop_dummy :: (Nat -> Nat -> Nat) -> Nat -> Nat -> Bool
prop_dummy x i j = x i j == powP i j 


prop_dummyuni :: (Nat -> Nat) -> Nat -> Bool
prop_dummyuni x i = x i == facP i 


main :: IO ()
main = (verboseCheck $ propCheckUn unaryComp unaryCheck) 
--main = verboseCheck $ (propCheckBi binaryComp binaryCheck)
--main =print (propCheckBi binaryComp binaryCheck (fromNatural 12) (fromNatural 12))
--main= verboseCheck $ prop_dummyuni facR

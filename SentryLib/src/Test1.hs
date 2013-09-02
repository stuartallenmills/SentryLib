{-# LANGUAGE GADTs, RankNTypes #-}

module Test1 where
import Data.Maybe
import Control.Monad
import Data.Functor

data K1 = K1 {name::String} deriving Show
data K2 = K2 {name2::String} deriving Show

class M a where 
    getName::a->String
    
instance M K1 where
    getName = name
    
instance M K2 where
    getName = name2
    

       
data Tiddwadle  = forall a. (Show a, M a )=> T a
 --    Tid1::(Show a)=> {val::a}  -> Tiddwadle  
--     Tid2:: {val ::K2}  -> Tiddwadle K2
--       deriving Show

instance M Tiddwadle where
   getName (T a) = getName a
   
k1 = K1 "k1"
k2 = K2 "k2"

tk1 = T k1
tk2 = T k2

z1::[Tiddwadle]
z1 = [tk1, tk2]

lm = getName (z1 !! 1)

sole:: [a]->Maybe a

sole xs = guard ((() <$ xs) == [()]) >> listToMaybe xs

tm = [1,2]

gb::[a]->Maybe ()
gb xs = guard ((() <$ xs) == [()])

testit::Maybe String

testit = do return mzero

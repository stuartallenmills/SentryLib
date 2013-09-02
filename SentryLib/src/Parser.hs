{-# LANGUAGE FlexibleInstances, RankNTypes #-}

module Parser where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

type Name = String -- variable names
data Exp = Lit Integer -- expressions
        | Var Name
        | Plus Exp Exp
        | Abs Name Exp
        | App Exp Exp
                deriving (Show)
data Value = IntVal Integer -- values
        |FunVal Env Name Exp
                deriving (Show)
type Env = Map.Map Name Value -- mapping from names to values


eval0                   ::  Env -> Exp -> Value
eval0 env (Lit i)       =   IntVal i
eval0 env (Var n)       =   fromJust (Map.lookup n env)
eval0 env (Plus e1 e2)  =   let  IntVal i1  = eval0 env e1
                                 IntVal i2  = eval0 env e2
                             in IntVal (i1 + i2)
eval0 env (Abs n e)     =   FunVal env n e
eval0 env (App e1 e2)   =   let  val1  = eval0 env e1
                                 val2  = eval0 env e2
                             in case val1 of
                                  FunVal env' n body -> eval0 (Map.insert n val2 env') body
                                  
--kt = FunVal Map.empty ( Abs "x" (Plus (Lit 2) (Lit 3) )   )            

v = Abs "x" (Plus (Var "x") (Lit 2))      

em::Env
em = Map.empty

kt = FunVal em "x" v
zt = eval0 em (App v (Lit 2))

newtype Reader2 r a = Reader2 { runReader2 :: r -> a }

instance Monad (Reader2 r) where
        return a = Reader2 $ \_ -> a
        m >>= k = Reader2 $ \r -> runReader2 (k (runReader2 m r)) r
        
--instance MonadReader r (Reader2 r) where
--        ask = Reader2 id
--        local f m = Reader2 $ runReader2 m . f

bub::forall r. (Show r)=>Reader2 r String
bub = Reader2  show
vb=runReader2 bub 3
vc =runReader2 bub "hell"
rfunc::forall r. (Show r)=>String->Reader2 r String
rfunc s = Reader2 (\x-> s++ " Oh No")

{-
(State h) >>= f = State $ \s -> let (a, newState) = h s  
                                    (State g) = f a  
                                    in  g newState  
                                    -}
                                    
bubba::State String Int
bubba = do
         s<-get 
         return ( length s)

zink::Int->State String Int
zink t =  do 
           s<-get
           let z= s++show t
           put z
           return (length z)
           
           
zink2 t = state (\x->let s= x++ show t 
                        in (length s, s))
                            

thom::State String Int
thom = state (\x->(length x, x++"bob"))

lst = thom >>= zink>>=zink

tick :: State Int Int
tick =  do n <- get
           put (n+1)
           return n

tip = runState tick  
 
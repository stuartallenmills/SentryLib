{-# LANGUAGE GADTs, RankNTypes, TypeFamilies , TemplateHaskell, DeriveGeneric, ExistentialQuantification, FlexibleInstances, TypeSynonymInstances #-}

module SentryPlay where

import qualified Data.Map as M
import Control.Lens
import qualified GHC.Generics as G
import Data.Text

data TTest  = TTest { _skey::Int}



--makeLenses ''Zap

data Tree a = Leaf a | Node a (Tree a) 

type  SentryKey = Int

data (SentryType a)=>SKey a  = SKey Int deriving (Eq, Ord, Show)

data Camera = Camera { cname::String, ckey::SKey Camera} deriving (Eq, Ord, Show)

data Manu = Manu {mName::String, mkey::SKey Manu} deriving (Eq, Ord, Show)

class SentryType a where
   key::a->SKey a
   name::a->String
   setKey::a->Int->a
    
        
        
        


data SentryMap = SM Int



instance SentryType Camera where
--   type KeyType Camera = SKey Camera
   key  = ckey  
   name = cname
   setKey a i = a {ckey = SKey i}

instance SentryType Manu where
--   type KeyType Manu = SKey Manu
   key  = mkey  
   name = mName
   setKey a i = a {mkey = SKey i}
   
    --type KeyType Int = Int
    
   
   

    
    


k0::SKey Camera    
k0 = SKey 0




--k1::Sentry Camera SentryKey
k1 = SKey 1

--dc = Data c1

c2::Camera 
c2 =  (Camera "tom" $ SKey 1)
c1::Camera 
c1 =  (Camera "bob" $ SKey 0)


--mp:: Sentry Camera SentryMap
--mp = MkMap (M.fromList [(k0, c1), (MkKey 1,c2 )])
i0::SKey Camera 
i0= SKey 0

m0::SKey Manu
m0 = SKey 1

--t::Camera->SKey Camera
t (x) = key x

z::SKey a->SentryKey
z (SKey x) = x

zb (SKeyType (SKey x) )= x

insert::(SentryType a)=>a->M.Map (SKey a) a->M.Map (SKey a) a
insert a tmap=  M.insert ( SKey ss ) na  tmap where
                        ss= M.size tmap
                        na = setKey a ss

data  SKeyType =   forall a. SKeyType {aKey::SKey a}

data Nix a where
   Cam::SKey Camera -> Nix Camera
   Man::SKey Manu->Nix Manu

eval::Nix a ->SKey a
eval (Cam x) = x
eval (Man x) = x

type KeyContainer = [SKeyType]

km::SKey Manu    
km = SKey 4

--bv::[Nix a]
bv = [Cam i0, Cam i0]
--tz::KeyContainer
--tz = [CamType k0, CamType km]


--getKey::Int->KeyContainer->SKey Camera
--getKey ind ks =  (aKey (ks !! ind))
                   
type CameraMap = M.Map (SKey Camera) (Camera)

tl:: M.Map (SKey Camera) (Camera)
--tl::M.Map (Sentry Camera MkKey) (Sentry Camera ())
tl = M.fromList [(key c2 ,  c2), (key c1,  c1)]


--kz::Sentry Camera SentryMap->M.Map (Sentry Camera SentryKey) (Sentry Camera ())

ts = [k1, k0]


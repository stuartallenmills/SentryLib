{-# LANGUAGE GADTs, RankNTypes, TypeFamilies , TemplateHaskell, DeriveGeneric, ExistentialQuantification, FlexibleInstances, TypeSynonymInstances #-}

module ExPlay where

import Data.Text

class AKey a where
   get::a->Int
   
   
instance AKey (SKey a) where
    get (SKey c) = c
    

data Test = Test {a::String, b::SKey Test} deriving (Eq, Ord, Show)

data Best = Best {k::String, m::SKey Best} deriving (Eq, Ord, Show)

type  SentryKey = Int

data (SentryType a)=>SKey a  = SKey Int deriving (Eq, Ord, Show)

--data Camera = Camera { cname::String, ckey::SKey Camera} deriving (Eq, Ord, Show)

--data Manu = Manu {mName::String, mkey::SKey Manu} deriving (Eq, Ord, Show)

data Keyness where
   C:: SKey Test -> Keyness
   D:: SKey Best -> Keyness
   
instance Eq Keyness where
   C (SKey ta) == C (SKey tb) = ta==tb
   D (SKey ta) == D (SKey tb) = ta==tb
   C _ == D _ = False
   D _ == C _ = False
   
      

instance SentryType Test where
    type KeyType Test = SKey Test
    key =  b
    name = a
    setKey obj int = obj {b= SKey int}
    
instance SentryType Best where
   type KeyType Best = SKey Best
   key = m
   name = k
   setKey obj int = obj {m=  SKey int}
   
        
class SentryType a where
   type KeyType a::  *
   key::a->KeyType a
   name::a->String
   setKey::a->Int->a
   
data SentryObj = forall a. (SentryType a)=>S a

kc::SKey Test
kc = SKey 0

kb::SKey Best
kb = SKey 1
    
ckc = C kc

ckb = D kb

--eval::forall a. Keyness -> a
--eval (C s) = s
--eval (D t) = t

{-
class Atest a where
   geta::a->Int
   getb::a->String
   
instance Atest Test where
  geta= a
  getb = b
  
instance Atest Best where
  geta= k
  getb = m
  
data Testies = forall a. (Atest a)=>T a

instance Atest Testies where
   geta (T s) = geta s
   getb (T s) = getb s
    
    
monk = [T (Test 1 "hi"), T (Best 2 "bye")]
-}
   
class StringLike s where
    toString :: s -> String
    fromString :: String -> s

instance StringLike String where
    toString = id
    fromString = id

instance StringLike Text where
    toString = unpack
    fromString = pack
    
data Stringish = forall a. StringLike a => Stringish a

bgt = [Stringish (pack "Hello as Text"), Stringish "Hello as String"]

instance StringLike Stringish where
    toString (Stringish s) = toString s
    fromString = Stringish
    
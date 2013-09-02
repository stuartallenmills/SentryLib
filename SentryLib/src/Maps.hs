{-# LANGUAGE TypeFamilies, FlexibleContexts, DataKinds, MultiParamTypeClasses, GADTs, RankNTypes, TemplateHaskell, ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Maps where

import qualified Data.Map as M
import Data.Text
import Control.Lens
--import SentryTH


type StringT = String
type Key = Int
type Name = String

-- | Define Item, which is used to typecheck entity specific datatypes


data  ( Show c,  Ord c)=> Item  a  c = Item  c  

instance forall a c. (Show c, Ord c)=>Show (Item a c) where
  show (Item  c) = show c

  
 
    
data KeyA a= KeyA {getkType::a->a,  kval::Key}

   

--getType a = ktype a

--toItem::forall a d. (Entity a, Ord d, Show d)=>a-> d->Item (EType a) d
--toItem c = Item c

fromItem::forall a d. (Entity a)=>( Ord d, Show d)=> Item a d->d
fromItem (Item tix) = tix

--itemType::(Entity a, Ord d, Show a, Show d)=>Item a d->a
--itemType (Item a _) = a
      
instance (Entity a, Ord b, Show b)=> Eq  (Item a b) where
   (==) (Item b) (Item c) = b Prelude.== c

instance (Entity a, Ord b, Show b)=> Ord  (Item a b) where
   (>) (Item  b) (Item  c) = b Prelude.> c
   
class Entity a where
   type EType a :: *
   type EType a = a 
   name:: a->StringT
   typeS::a->StringT
   key::(Eq a, Ord a)=>a->EType a
   
   
type EKey c= Item  (EType c) Key 

type EName c = Item (EType c) Name

class IsEntity a

instance IsEntity Int
instance IsEntity Char

instance (IsEntity a)=> IsEntity [a]


     
type KeyToEntityMap a= M.Map (EKey a)  a
type EntityToKey a= M.Map (EName a) (EKey a)
type Key2Key a b = M.Map (EKey a) (EKey b)



data EMaps c = EMaps {key2e::KeyToEntityMap c, e2key::EntityToKey c}


--data  AllMaps =  AllMaps [EMaps]

{-
addE::forall c. (Entity c)=> c->EMaps c->EMaps c
addE ent tEmaps=    EMaps {(key2e tMaps)=M.insert  tcount ent (key2e tEmaps)), e2key= M.insert (name ent) tcount  (e2key tEmaps)} where
                                      tcount= toItem ( M.size (key2e tEmaps))
                                      -}
                         
                  

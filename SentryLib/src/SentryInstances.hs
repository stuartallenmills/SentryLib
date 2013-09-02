
{-# LANGUAGE StandaloneDeriving, TypeFamilies #-}
module SentryInstances where

import Data.Text

import Maps
import SentryDataTypes
import SentryMaps
import qualified Data.Map as M
import SentryTH



instance Entity Model where
              type EType Model = SentryType
              name = model
              typeS = moddec
              key = modelKey
              

instance Entity SLoc where 
        type EType SLoc = SentryType
        name = lname
        typeS=ltype
        key = lKey
        

   
instance Entity Camera where
        type EType  Camera= SentryType
        name= cname
        typeS = ctype
        key = cKey
        

       
instance Entity Manu where
      type EType Manu = SentryType
      name = mname
      typeS=mtype
      key = mKey
 


   

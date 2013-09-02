module SentryMaps where

import SentryDataTypes
import Maps
import qualified Data.Map as M


type KeyToCamera = KeyToEntityMap Camera
type CameraToKey = EntityToKey Camera

type KeyToManu = KeyToEntityMap Manu
type ManuToKey = EntityToKey Manu

type Key2Loc =  KeyToEntityMap SLoc
type Loc2Key =  EntityToKey SLoc

type Key2Model =  KeyToEntityMap Model
type Model2Key =  EntityToKey Model

{-

data SentryMaps = SentryMaps {
                          k2cam::KeyToCamera
                        , cam2k::CameraToKey
                        , k2man::KeyToManu
                        , man2k::ManuToKey
                        , k2loc::Key2Loc
                        , loc2key::Loc2Key
                        , k2mod::Key2Model
                        , mod2k::Model2Key
 
                        } 
                        -}
                        
               

                  

--getRecFromKey::(Entity a)=>Item (EType a) Int-> KeyToEntityMap a->a
--getRecFromKey k sm = sm M.! k
--getK2E::(Entity a)=>Item a Int->KeyToEntityMap a

--getK2E ( Item Camera )= k2cam SentryMa

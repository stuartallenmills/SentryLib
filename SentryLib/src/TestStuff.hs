
{-# LANGUAGE MultiParamTypeClasses, GADTs, RankNTypes, TemplateHaskell, ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies,  FlexibleContexts, DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module TestStuff where

import qualified Data.Map as M
import Control.Lens
import Maps
import SentryInstances
import SentryDataTypes
import SentryMaps
import SentryTH

import Data.Dynamic

instance   Show Camera where
    show =  name 

instance Show Manu where
    show = name
    
instance Show SLoc where
        show = name
        
instance Eq SentryType where
 (==) (CameraType a) (CameraType  b) = a==b
 (==) (ManuType a) (ManuType b) = a==b  
 (==) (SLocType a) (SLocType b) = a==b
 (==) _ _ = False
 
instance Show SentryType where
 show (CameraType a)  = show a
 show (ManuType a)  = show a
 show (SLocType a)  = show a
 show _ = "I have no idea how to show this"

instance Show DbResultType where
  show (CameraDbResult a) = show a
  show (ManuDbResult a) = show a
  show (SLocDbResult a) = show a
  
defaultCameraNet = CameraNetwork "logonstr" 
                                 "setCompression"
                                 "setFPS"
                                 "setRowCols"
                                  NoInit
                                 "ipAddress"
                                 


defaultImgDef = ImgDef 512 512 MJPEG 50 RgbPlane



cisco ::Manu
cisco = Manu  (  "Cisco")  "Cisco" (ManuType (ManuKey  0)) 

aloc = SLoc "Living Room" "Room" (SLocType (SLocKey 0))


cam = Camera (   "Living Room") "ax432" 15 
                defaultImgDef
                defaultCameraNet
                CNoInit
                (CameraType (CameraKey 0)) -- camera key               
                (ModelType (ModelKey 0)) -- model
                (SLocType (SLocKey 0)) -- location
                (ManuType (ManuKey 0)) -- manu            

cam2 = Camera ( "Den") "ax432" 15 
                defaultImgDef
                defaultCameraNet    
                CNoInit           
                (CameraType (CameraKey 1)) -- camera key               
               (ModelType (ModelKey 0)) -- model
                (SLocType (SLocKey 0)) -- location
                 (ManuType (ManuKey 0))-- manu            



ct = CameraType (CameraKey 0)

mt::MapType
mt = CameraMapType (M.fromList [(CameraKey 0, cam), (CameraKey 1, cam2)])
manudb = ManuMapType (M.fromList [(ManuKey 0, cisco)])
locmap = SLocMapType (M.fromList [(SLocKey 0, aloc)])

tmt = ManuType (ManuKey 0)


bvf val = getResults ( CameraType (CameraKey val)) mt

--camk2manu::SentryType->DbResultType
camk2manu camk = getResults mkey manudb where
                     mkey = getMkey camk
  --                   mkey = (cmKey $ resultValCamera $ getResults camk mt)

camk2manu2 camk = getResults mkey manudb where
                     mkey = (cmKey $ resultValCamera $ getResults camk mt)

getMkey val = (cmKey $ resultValCamera $ getResults (CameraType (CameraKey val)) mt)

getCam val = "Camera:"++s++"  Room:"++r where
                  c= resultValCamera $ bvf  val
                  rm = resultValManu $ getResults (clocKey c) manudb
                  s = show c
                  r = show rm
ck = CameraType (CameraKey 0)

tact = getMkey 0 
                  
lts camk = getResults camk manudb
dlts dbr = resultValCamera dbr
ddlts  = cmKey 
dddlts st = getResults st manudb
ddddlts dbr = resultValManu dbr

tc = lts (CameraType (CameraKey 0))
ttc = dlts tc
tttc = ddlts ttc
ttttc = dddlts tttc
tttttc = ddddlts ttttc


                 

--cmKey cam Prelude.== clocKey cam
{-
cam2man tcam = mapMan M.! cmKey tcam
man2cams man =  cams where 
                 manKey = man2key M.! name man
                 cams = M.foldr  (\x y->(manKey == (cmKey x)):y) []   key2cam
-}
{-
ts = KeyToCameraT key2cam
tk::(Entity a)=> KeyT->a
tk a= M.fromList [(CameraK a, ts)]

getEKey::(EKey a)=> KeyT->a
getEKey (CameraK {cval=a}) = a
-}
{-
eval::KeyT-> SentryMapT
eval (CameraK x) = KeyToCameraT x
eval (ModelK x) = Key2ModelT x
-}


--gk tman = man2key M.! name tman



data  Test c where
  StrT::String-> Test String
  IntT::Int->Test Int
   

data Mta = Mta {_b::Int}

makeLenses ''Mta

makeLenses ''Test

mm = Mta 4
    
testI = IntT 1

        
test2 = StrT "Hello"

get::Test b->b
get (IntT a)= a
get (StrT a) = a

--testArr : [Test a]
--testArr = [testI, test2]

 --idArr::IsEntity
-- idArr = [4::IsEntity, 3::IsEntity]
{-
testEq::Dynamic->Dynamic->Bool
testEq a b = if ta /= tb then False 
          else va==vb where
           va = fromDynamic a
           vb = fromDynamic b
           ta = typeOf va
           tb = typeOf vb
-}
f::Integer->Bool
f = (Prelude.==) 2

fd = toDyn f
bv = toDyn (2::Integer)
cv = toDyn (2::Integer)
gt = dynApply fd bv
--extract::forall a. Maybe Dynamic->Bool
--extract (Just x)= fromDynamic x
--extract  (Nothing)=Nothing

ex::Maybe Int->Int
ex (Just x)= x

data Showable
  where
  MkShowable :: Show a => a -> Showable

instance Show Showable
  where
  showsPrec p (MkShowable a) = showsPrec p a

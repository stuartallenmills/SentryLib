{-# LANGUAGE TypeFamilies, FlexibleContexts, DataKinds, MultiParamTypeClasses, GADTs, RankNTypes  #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell #-}

module SentryTH where

import Language.Haskell.TH
import TemplateDefs
import qualified Data.Map as M
import SentryDataTypes

data T = T {tat::Int}

{-
data Camera = Camera {cameraStuff::Int} deriving (Show, Eq, Ord)
data Manu = Manu {manuStuff::Int} deriving (Show, Eq, Ord)
data Model = Model String  deriving (Show, Eq, Ord)
data Network = Network [String] deriving (Show, Eq, Ord)
-}
--data CameraKey = CameraKey {val::Int}

--type CameraMap = M.Map CameraKey Camera

-- $(kfg "Camera")

--- ks::CameraKey
-- ks = CameraKey 4
--lt = ModelType 3
--nt = NetworkType 1
-- $(testKey "Camera")
$(createKeys ["Camera", "Manu", "Model", "CameraNetwork", "SLoc"])
$(createSentryTypes ["Camera", "Manu", "Model", "CameraNetwork", "SLoc"])
-- $(createDbResults  ["Camera", "Manu", "Model", "CameraNetwork", "SLoc"])
$(createDbResultType  ["Camera", "Manu", "Model", "CameraNetwork", "SLoc"])
$(createMaps  ["Camera", "Manu", "Model", "CameraNetwork", "SLoc"])

$(createMapType ["Camera", "Manu", "Model", "CameraNetwork", "SLoc"])

-- $(getResultsSig)
--getResults::SentryType->MapType->DbResultType
$(doGetResults ["Camera", "Manu", "Model", "CameraNetwork", "SLoc"])
--getResults (CameraType a) (CameraMapType b) =   b M.! a


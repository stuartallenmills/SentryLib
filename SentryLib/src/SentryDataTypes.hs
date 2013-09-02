module  SentryDataTypes where

import Maps

data NetState = Initialized | NoInit | Active deriving (Show, Eq)
data CameraState = CInitialized |CNoInit | CIdle | CActive deriving (Show, Eq)

data CompressionType = MJPEG | H264 deriving (Show, Eq)
data PixelType = RgbPlane | RgbInterleave deriving (Show, Eq)

data CameraNetwork = CameraNetwork {
                        logonStr::StringT
                        ,setCompressionStr::StringT
                        ,setFpsStr::StringT
                        ,setRowCol::StringT
                        ,state::NetState
                        ,ipAddress::StringT} deriving (Show, Eq)



data ImgDef = ImgDef {   rows:: Int
                        ,cols:: Int
                        ,compressType::CompressionType
                        , compressLevel::Float                        
                        ,pixelType::PixelType
                      } deriving (Show, Eq)

data Camera = Camera {   cname::StringT 
                        ,ctype::StringT 
                        ,fps::Double  
                        ,imgdef::ImgDef
                        ,cnetwork::CameraNetwork
                        ,cstate::CameraState
                        ,cKey::EType Camera
                        ,cmodKey::EType Model
                        ,clocKey::EType SLoc 
                        ,cmKey:: EType Manu}  
                        

data Manu = Manu{       mname::StringT 
                        ,mtype::StringT
                        ,mKey::EType Manu}

data Model = Model {   model::StringT 
                      ,moddec::StringT
                      ,modelKey::EType Model 
                      ,manKey::EType Manu
                      }  
                      
data SLoc = SLoc {lname::StringT 
                ,ltype::StringT
                ,lKey::EType SLoc
                }  
               
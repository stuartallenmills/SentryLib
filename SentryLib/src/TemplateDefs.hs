{-# LANGUAGE TypeFamilies, FlexibleContexts,  MultiParamTypeClasses, GADTs, RankNTypes  #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell #-}

module TemplateDefs where

import Language.Haskell.TH
import qualified Data.Map as M

type AMap = M.Map

type Key = Int
 
data TestKey = AKey Int | BKey Int deriving (Eq, Ord, Show)
data TestMap = AMap Int | BMap Int deriving (Eq, Ord, Show)
data TestResult = AType String | BType String deriving (Eq, Ord, Show)

-- data SCameraType = SCameraType String

--type TestMap = M.Map TestKey String

--tmap = M.fromList [(TestKey 0, SCameraType "Hello")]

data TestType = TBType {val::TestKey }

ls::IO Exp
ls = runQ  [e|  M.fromList [(AKey 0, AMap 2), (AKey 1, AMap 3)]  |]

ts::IO [Dec]
ts = runQ [d| 
 getResults (AKey a)  (AMap m)  =  AType  "GoodBye"
 getResults (BKey a)  (BMap m)  =  BType "Hello"
 getResults _ _ = error "All bad"
 |]

--
--buildSentryMap =AppE (VarE Data.Map.Base.fromList) (ListE [TupE [AppE (ConE TemplateDefs.AKey) (LitE (IntegerL 0)),AppE (ConE TemplateDefs.AMap) (LitE (IntegerL 2))],TupE [AppE (ConE TemplateDefs.AKey) (LitE (IntegerL 1)),AppE (ConE TemplateDefs.AMap) (LitE (IntegerL 3))]])

doGetResults::[String]-> DecsQ
doGetResults name= return $ SigD (mkName "getResults") (AppT 
                                          (AppT ArrowT (ConT (mkName "SentryType" ))) 
                                           (AppT 
                                             (AppT ArrowT (ConT (mkName "MapType"))) 
                                             (ConT (mkName "DbResultType"))
                                           )
                                         ) : 
                                         [FunD (mkName "getResults")$ (fmap gv  name)++
                                           [Clause [WildP,WildP] (NormalB (AppE (VarE (mkName "error")) (LitE (StringL "All bad")))) [] ]] 

getResultsSig::DecsQ
getResultsSig = return [SigD (mkName "getResults") (AppT 
                                          (AppT ArrowT (ConT (mkName "SentryType" ))) 
                                           (AppT 
                                             (AppT ArrowT (ConT (mkName "MapType"))) 
                                             (ConT (mkName "DbResultType"))
                                           )
                                         ) ]
                                        
--ats =  FunD getResults [Clause [ConP TemplateDefs.TestType [VarP tk_1],VarP m_2] 
--                                (NormalB (InfixE (Just (VarE m_2)) (VarE Data.Map.Base.!) (Just (VarE tk_1)))) []]

--gv::String->Dec
gv name = Clause [ConP  theType [VarP vartkey],ConP  mape [VarP vartmap]  ]
                        (NormalB (AppE (ConE dbResult ) (InfixE (Just (VarE vartmap)) 
                         (VarE $ mkName "M.!") (Just (VarE vartkey))))) [] where
                               theType = mkName $ name ++ "Type"
                 --              dMB = mkName "TestMap"
                               dbResult = mkName $ name++"DbResult"
                               vartkey = mkName "vartkey"
                               mape = mkName $ name++"MapType"
                               vartmap = mkName "vartmap"
                               

--ls::IO [Dec]
--ls= runQ [d| type TestMap = M.Map TestKey Test |]

fts::IO [Dec]
fts = runQ [d| data CameraKeyT = CameraKeyT Key|]

createSentryTypes::[String]->DecsQ
createSentryTypes n = return  [genDB_ n]

mkSentryMap::Dec
mkSentryMap =TySynD cm [] (AppT (AppT (ConT (mkName "AMap")) (ConT ck)) (ConT nm ))
                        where cm=mkName $ "SentryGlobalMap"
                              ck=mkName $ "SentryType"
                              nm = mkName  "DbResults"
                              

 
-- | genDB creates a list of <name>"Type" constructors, with key values of type <name>Key. for "SentryTypes"
                    
genDB_ ::[String]->Dec                 
genDB_ cname =  DataD [] (mkName "SentryType") [] (fmap keyBuild cname) [] 


keyBuild::String->Con                    
keyBuild bname = RecC nameType [( nameVal, IsStrict, ConT keyType)] where
                nameType = mkName (bname++"Type")
                nameVal = mkName $ "keyVal"++bname
                keyType = mkName $ bname++"Key"


--createDbResults::[String]->DecsQ
--createDbResults names = return  $ map mkResults names 


--mkResults::String->Dec
--mkResults name = DataD [] ck [] [NormalC ck [(NotStrict,ConT ky)]] [mkName "Eq", mkName "Ord", mkName "Show"] where
--                        ck = mkName $ name++"DbResult"
--                        ky = mkName name
               


createDbResultType::[String]->DecsQ
createDbResultType names = return  [DataD [] (mkName "DbResultType") [] (fmap resultsBuild names) [] ]

resultsBuild::String->Con                    
resultsBuild bname = RecC nameType [( nameVal, IsStrict, ConT keyType)] where  --   
                nameType = mkName (bname++"DbResult")
                nameVal = mkName $ "resultVal"++bname
                keyType = mkName  bname




createMapType::[String]->DecsQ
createMapType names =return  [DataD [] (mkName "MapType") [] (fmap mapTypeConstructorBuild names) [] ]

mapTypeConstructorBuild::String->Con
mapTypeConstructorBuild bname =RecC nameType [( nameVal, IsStrict, ConT keyType)] where
                nameType = mkName (bname++"MapType")
                nameVal = mkName $ "mapVal"++bname
                keyType = mkName $ bname++"Map"

--mkKey::String->Con
-- | mkKeyType makes the actual <name>Key types that are used by <name>Type.

createKeys::[String]->DecsQ                       
createKeys names = return $ map mkKey names


mkKey::String->Dec
mkKey name = DataD [] ck [] [NormalC ck [(NotStrict,ConT ky)]] [mkName "Eq", mkName "Ord", mkName "Show"] where
                        ck = mkName $ name++"Key"
                        ky = mkName "Key"


-- | Create the <nameMap> <nameKey> <name>  (BUT there needs to be a "DataResultType" with constructers of <name>ResultType
createMaps::[String]->DecsQ
createMaps names = return $ fmap mkMap names               

mkMap::String->Dec
mkMap name =TySynD cm [] (AppT (AppT (ConT (mkName "AMap")) (ConT ck)) (ConT nm ))
                        where cm=mkName $ name++"Map"
                              ck=mkName $ name++"Key"
                              nm = mkName  name
                    


dbBuild::String->Con                    
dbBuild bname = RecC nameType [( nameVal, IsStrict, ConT keyType)] where
                nameType = mkName (bname++"Map")
                nameVal = mkName $ "val"++bname
                keyType = mkName  "Key"


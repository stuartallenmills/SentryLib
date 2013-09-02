{-# LANGUAGE GADTs, RankNTypes, TypeFamilies , TemplateHaskell, DeriveGeneric #-}

module GPlay where

import  GHC.Generics  


data Tree a = Leaf a | Node a (Tree a) deriving Generic

--instance G.Generic (Tree a) where
--       to  ( G.M1 ( G.L1 ( G.M1 ( G.M1 ( G.K1 g1_apy))))) = Leaf g1_apy

{-
Derived instances:
  instance GHC.Generics.Generic (GPlay.Tree a_aot) where
    GHC.Generics.from (GPlay.Leaf g1_apv)
      = GHC.Generics.M1
          (GHC.Generics.L1
             (GHC.Generics.M1 (GHC.Generics.M1 (GHC.Generics.K1 g1_apv))))
    GHC.Generics.from (GPlay.Node g1_apw g2_apx)
      = GHC.Generics.M1
          (GHC.Generics.R1
             (GHC.Generics.M1
                ((GHC.Generics.:*:)
                   (GHC.Generics.M1 (GHC.Generics.K1 g1_apw))
                   (GHC.Generics.M1 (GHC.Generics.K1 g2_apx)))))
    GHC.Generics.to
      (GHC.Generics.M1 (GHC.Generics.L1 (GHC.Generics.M1 (GHC.Generics.M1 (GHC.Generics.K1 g1_apy)))))
      = GPlay.Leaf g1_apy
    GHC.Generics.to
      (GHC.Generics.M1 (GHC.Generics.R1 (GHC.Generics.M1 (GHC.Generics.:*: (GHC.Generics.M1 (GHC.Generics.K1 g1_apz))
                                                                           (GHC.Generics.M1 (GHC.Generics.K1 g2_apA))))))
      = GPlay.Node g1_apz g2_apA
  
  instance GHC.Generics.Datatype GPlay.D1Tree where
    GHC.Generics.datatypeName _ = "Tree"
    GHC.Generics.moduleName _ = "GPlay"
  
  instance GHC.Generics.Constructor GPlay.C1_0Tree where
    GHC.Generics.conName _ = "Leaf"
  
  instance GHC.Generics.Constructor GPlay.C1_1Tree where
    GHC.Generics.conName _ = "Node"
  

Generic representation:
  
  Generated datatypes for meta-information:
    GPlay.D1Tree
    GPlay.C1_0Tree
    GPlay.C1_1Tree
    GPlay.S1_0_0Tree
    GPlay.S1_1_0Tree
    GPlay.S1_1_1Tree
  
  Representation types:
    type GHC.Generics.Rep (GPlay.Tree a_aot) = GHC.Generics.D1
                                                 GPlay.D1Tree
                                                 (GHC.Generics.C1
                                                    GPlay.C1_0Tree
                                                    (GHC.Generics.S1
                                                       GHC.Generics.NoSelector
                                                       (GHC.Generics.Rec0 a_aot))
                                                  GHC.Generics.:+: GHC.Generics.C1
                                                                     GPlay.C1_1Tree
                                                                     (GHC.Generics.S1
                                                                        GHC.Generics.NoSelector
                                                                        (GHC.Generics.Rec0 a_aot)
                                                                      GHC.Generics.:*: GHC.Generics.S1
                                                                                         GHC.Generics.NoSelector
                                                                                         (GHC.Generics.Rec0
                                                                                            (GPlay.Tree
                                                                                               a_aot))))
                      -}                                                                         
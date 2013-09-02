{-# LANGUAGE GADTs, RankNTypes #-}

module Junkie where



data Test where
   CInt::Int->Test
   CString::String->Test deriving (Eq, Ord, Show)
   
kk = [CInt 3, CString "hi"]

monkey::Test->String
monkey (CInt x) = show x
monkey (CString x) = x   

--zib::forall a. Test->a
zib (CInt x) = x
zib (CString x) = read x::Int


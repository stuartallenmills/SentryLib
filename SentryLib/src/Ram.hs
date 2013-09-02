

module Ram where

import Data.Array  
import Control.Monad.State

data Bit = BitTrue |  BitFalse | Unitialized deriving (Eq, Ord)

instance Show Bit where
   show a |a== BitTrue="T"
          |a== BitFalse="F"
          |a== Unitialized = "U"
          |otherwise = error "The impossible has occured"

type Register  = Array Int Bit 
type InputR = Register
type StateRAM = RAM
type NewState = Register

data Op = Read | Write deriving (Eq, Ord, Show)

--type AR =  AR dat:[Bit,Bit]

type RAM = Array Int Register 

registerSize=16

mkBit::Bit
mkBit = Unitialized

mkRegister16::Register
mkRegister16 = listArray (0,registerSize-1)  $ replicate registerSize mkBit

mkRAM::Int->RAM
mkRAM n = listArray (0, n-1) $ replicate n mkRegister16

setBit :: Bit
setBit = BitTrue

resetBit::Bit
resetBit = BitFalse

writeBit::Bool -> Bit
writeBit val | val  = setBit
             | not  val   = resetBit
             | otherwise = error "The impossible has occured"


writeRegister::[Bool]->Register
writeRegister rin | length rin /= registerSize = error "Bad Register Write"
                  | otherwise = listArray (0,registerSize-1) $  fmap writeBit rin
                  

writeRam::Int->InputR->StateRAM->RAM
writeRam rin reg ram    | rin  >=   snd ( bounds ram ) = error "bad write address"
                        | otherwise = ram // [(rin, reg)]

readRam::Int->StateRAM->Register
readRam tindex ram = ram ! tindex

ram8::RAM
ram8 = mkRAM 8

rTrue = writeRegister (replicate registerSize True)

newRam = writeRam 2 rTrue ram8


doReadRam::Int->State RAM Register
doReadRam tindex = do
                 tr<-get
                 return $ readRam tindex tr 

doWriteRam::Int->Register->State RAM Register
doWriteRam tindex val = do
        tr<-get
        let nram = writeRam tindex val tr
        put nram
        return $ readRam tindex nram
        
doRamOp::Op->Int->Register->RAM->(Register, RAM)
doRamOp op rin reg ram | op == Read = runState ( doReadRam rin ) ram
                       | op == Write = runState (doWriteRam rin reg) ram

zapit ind reg ss = val where 
                        lead = head ss
                        newState = runState (doWriteRam ind reg) lead
                        val = [newState]
                                     

tm = runState ( doWriteRam 0 rTrue ) ram8:tm

kst::[Integer->Integer]
kst = [(+2), (*3)]

gz = fmap (\fx-> fx 3) kst


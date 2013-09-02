module PingTest where

-- Copyright   :  (C) 2012 Edward Kmett, nand`
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  TH, Rank2, NoMonomorphismRestriction
--
-- A simple game of pong using gloss.
-----------------------------------------------------------------------------


import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Control.Monad.State (State, execState, get)
import Control.Monad (when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State



import Data.Set (Set, member, empty, insert, delete)


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import System.Random (randomRs, newStdGen)


-- Some global constants


gameSize        = 300
windowWidth     = 800
windowHeight    = 600
ballRadius      = 0.02
speedIncrease   = 1.2
losingAccuracy  = 0.9
winningAccuracy = 0.1
initialSpeed    = 0.6
paddleWidth     = 0.02
paddleHeight    = 0.3
paddleSpeed     = 1
textSize        = 0.001


-- Pure data type for representing the game state


data Pong = Pong
  { _ballPos   :: Point
  , _ballSpeed :: Vector
  , _paddle1   :: Float
  , _paddle2   :: Float
  , _score     :: (Int, Int)
  , _vectors   :: [Vector]


  -- Since gloss doesn't cover this, we store the set of pressed keys
  , _keys      :: Set Key
  }

-- Some nice lenses to go with it
makeLenses ''Pong

-- Renamed tuple lenses for enhanced clarity with points/vectors



initial :: Pong
initial = Pong (0, 0) (0, 0) 0 0 (0, 0) [] empty

data Ball = Ball        { _pos::Point
                        , _speed::Vector
                        , _radius::Float
                        } deriving Show

makeLenses ''Ball

_x::Simple Lens Vector Float
_x = _1
_y::Simple Lens Vector Float
_y = _2

data World = World {_ball::Ball} deriving Show
--upBall::Float->State Ball ()  ->()

makeLenses ''World

upBall::Float->State Ball  ()
upBall time =  do 
                (u,v)<- use speed
                pos  += (time * u, time*v)
                
aball = Ball (0,0) (1,1) 1

nub t = execState ( upBall t) 

baseW = World aball


--upWorld::State World ()    
upWorld::Float->StateT World IO ()           
upWorld t =  do 
             b <- use ball
             lift $ putStrLn (show b)
 --            zp <- nub t b
             ball .= nub 1 b
             let t'=t+1
             when (t' < 10)  ( upWorld t')
  
--            w.ball .~ nub t w.ball
              
--jz= ballSpeed._1 += 1
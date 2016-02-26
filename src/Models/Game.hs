{-# LANGUAGE TemplateHaskell #-}

module Models.Game where

import Control.Lens
import Data.Map     (Map, showTreeWith)
import Data.Monoid
import Numeric

data Game = Game
          { _money      :: Double
          , _gifts      :: Map String (Sum Integer)
          , _multiplier :: Double
          } deriving Show

makeLensesFor [ ("_money", "lMoney")
              -- , ("_gifts", "lGifts")
              , ("_multiplier", "lMultiplier")
              ]
              ''Game

showGame :: Game -> String
showGame (Game m g x) = "Current money: $" ++ showFFloat (Just 2) m "" ++ "\n"
                     ++ showTreeWith (\ k (Sum s) -> k ++ " => " ++ show s) False True g ++ "\n"
                     ++ "Multiplier: " ++ show x ++ "x"

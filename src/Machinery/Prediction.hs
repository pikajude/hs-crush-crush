{-# LANGUAGE FlexibleContexts #-}

module Machinery.Prediction where

import Control.Lens
import Control.Monad.Writer
import Data.Maybe
import Data.Tuple
import Models.Game
import Reflex.Dom

remaining :: (MonadHold t m, Reflex t)
          => Double -> Dynamic t Game -> m (Dynamic t (Double, String))
remaining price = mapDyn $ \ g ->
    ( min 1 (view lMoney g / price)
    , unwords $ catMaybes $ zipWith pluralize ["day", "hour", "minute", "second"]
                          $ bits $ (max 0 $ price - view lMoney g) / (view lSalary g * view lMultiplier g))
    where
        pluralize "second" 0 = Just "0 seconds"
        pluralize thing 0 = Nothing
        pluralize thing 1 = Just $ "1 " ++ thing
        pluralize thing n = Just $ show n ++ " " ++ thing ++ "s"
        bits n = execWriter $ divby 86400 (ceiling n)
                          >>= divby 3600
                          >>= divby 60
                          >>= divby 1
        divby x n = writer $ fmap (:[]) $ swap $ n `divMod` x

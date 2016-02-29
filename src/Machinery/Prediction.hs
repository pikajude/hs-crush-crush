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
    , unwords $ mapMaybe (uncurry pluralize)
              $ bits $ (/) (max 0 $ price - view lMoney g) (view lSalary g * view lMultiplier g))
    where
        pluralize _ 0 = Nothing
        pluralize thing 1 = Just $ "1 " ++ thing
        pluralize thing n = Just $ show n ++ " " ++ thing ++ "s"
        bits n = execWriter $ foldl (>>=) (return (ceiling n))
                            $ map divby [ (31557600, "year"), (2629800, "month")
                                        , (604800, "week"), (86400, "day")
                                        , (3600, "hour"), (60, "minute")
                                        , (1 :: Integer, "second")]
        divby (x,s) n = writer . fmap ((:[]) . (,) s) . swap $ n `divMod` x

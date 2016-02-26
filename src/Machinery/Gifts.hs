{-# LANGUAGE OverloadedLists #-}

module Machinery.Gifts where

import Control.Lens
import Data.Map     (Map, fromList)
import Data.Monoid
import Models.Game
import Reflex.Dom

gift :: MonadWidget t m => String -> Integer -> Double -> Dynamic t Game -> m (Event t (Double, Map String (Sum Integer, Sum Double)))
gift nm price mult dg = do
    disabler <- mapDyn (\ c -> fromList [("disabled", "1") | view lMoney c < fromIntegral price]) dg
    (buyer, _) <- elDynAttr' "button" disabler $ text $ "Buy " ++ nm ++ " ($" ++ show price ++ ")"
    return $ fmap (const (negate (fromIntegral price), [(nm, (Sum 1, Sum mult))])) (domEvent Click buyer)

myGifts :: [(String, Integer, Double)]
myGifts = [ ("shell", 500, 0.5)
          , ("rose", 950, 2)
          , ("hand lotion", 1805, 5)
          , ("donut", 3430, 10)
          , ("fruit basket", 6516, 20)
          , ("chocolates", 12380, 40)
          , ("book", 23523, 80)
          , ("earrings", 44694, 160)
          ]

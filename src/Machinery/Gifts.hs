{-# LANGUAGE OverloadedLists #-}

module Machinery.Gifts where

import Control.Lens
import Data.Map             (Map, fromList)
import Data.Monoid
import Machinery.Prediction
import Models.Game
import Reflex.Dom

gift :: MonadWidget t m => String -> Integer -> Double -> Dynamic t Game -> m (Event t (Double, Map String (Sum Integer, Sum Double)))
gift nm price mult dg = elAttr "div" [("data-gift", nm)] $ do
    disabler <- mapDyn (\ c -> fromList [("disabled", "1") | view lMoney c < fromIntegral price]) dg
    (buyer, _) <- elDynAttr' "button" disabler $ text $ "Buy " ++ nm ++ " ($" ++ show price ++ ")"

    loadPct <- mapDyn (\ c -> [ ("class", "loader")
                              , ("style", "background-color: green; color: white; text-shadow: 1px 1px #000,-1px 1px #000,1px -1px #000,-1px -1px #000,1px 0 #000,0 1px #000,-1px 0 #000,0 -1px #000; width: "
                                       ++ show (min 100 (fst c * 100))
                                       ++ "%")
                              ]) =<< remaining (fromIntegral price) dg
    elDynAttr "div" loadPct $ dynText =<< mapDyn (\ (a,b) -> if a == 1 then "ready!" else b)
                                      =<< remaining (fromIntegral price) dg

    return $ fmap (\ () -> (negate (fromIntegral price), [(nm, (Sum 1, Sum mult))])) (domEvent Click buyer)

myGifts :: [(String, Integer, Double)]
myGifts = [ ("shell", 500, 0.5)
          , ("rose", 950, 2)
          , ("hand lotion", 1805, 5)
          , ("donut", 3430, 10)
          , ("fruit basket", 6516, 20)
          , ("chocolates", 12380, 40)
          , ("book", 23523, 80)
          , ("earrings", 44694 * 100000, 160)
          ]

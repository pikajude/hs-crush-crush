{-# LANGUAGE OverloadedLists #-}

module Machinery.Gifts where

import Control.Lens
import Data.JSString        (JSString, unpack)
import Data.Map             (Map, fromList)
import Data.Monoid
import Machinery.Prediction
import Models.Game
import Reflex.Dom

meterSuccess :: (Fractional a, Ord a) => a -> String
meterSuccess n | n >= 1 = ""
               | n >= 0.85 = "success"
               | n >= 0.5 = "warning"
               | otherwise = "alert"

gift :: MonadWidget t m => String -> Integer -> Double -> Dynamic t Game -> m (Event t (Double, Map String (Sum Integer, Sum Double)))
gift nm price mult dg = elAttr "div" [("data-gift", nm), ("class", "small-3 columns")] $ do
    disabler <- mapDyn (\ c -> fromList $ [("disabled", "1") | view lMoney c < fromIntegral price] ++ [("class", "expanded button gift-buy")]) dg
    (buyer, _) <- elDynAttr' "button" disabler $ text $ "Buy " ++ nm ++ " ($" ++ show price ++ ")"

    rm <- remaining (fromIntegral price) dg
    loadColor <- mapDyn (\ c -> [ ("class", "progress " ++ meterSuccess (fst c)) ]) rm
    loadPct <- mapDyn (\ c -> [ ("class", "progress-meter")
                              , ("style", "width: " ++ show (min 1 (fst c) * 100) ++ "%")
                              ]) rm
    elDynAttr "div" loadColor $
        elDynAttr "div" loadPct $
            elClass "p" "progress-meter-text" $
                dynText =<< mapDyn (\ (a,b) -> if a == 1 then "ready!" else b)
                        =<< remaining (fromIntegral price) dg

    return $ fmap (\ () -> (negate (fromIntegral price), [(nm, (Sum 1, Sum mult))])) (domEvent Click buyer)

myGifts :: [(String, Integer, Double)]
myGifts = zip3 [ unpack shellEmoji, "🌹", "👏", "🍩", "🍇🍓", "🍫", "📚", "💄", "🍹 ", "💐", "🍰", "🐻", "🍵", "👠", "🐶", "\128255", "👜", "🚗" ]
               (map round $ iterate (* 1.9) (500 :: Double))
               (iterate (* 1.8) 0.5)

foreign import javascript safe "'🐚'" shellEmoji :: JSString

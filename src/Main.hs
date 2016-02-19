{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE RecursiveDo      #-}

import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Time
import Numeric
import Reflex.Dom

{-
type Model = Int

-- we do not return html, it's done by MonadWidget
view :: MonadWidget t m => Dynamic t Model -> m (Event t (Int -> Int))
view model =
  el "div" $ do
    (decrement, _) <- do
        md <- mapDyn _filterGt200 model
        elDynAttr' "button" md $ text "Buy"
    el "div" $ do
      t <- mapDyn show model
      dynText t
    (increment, _) <- el' "button" $ text "+"
    return $ leftmost [ fmap (const (subtract 200)) (domEvent Click decrement)
                      , fmap (const (+ 1)) (domEvent Click increment) ]
    where
        _filterGt200 x | x < 200 = [("disabled", "1")]
                       | otherwise = []
                       -}

ticker :: MonadWidget t m => Double -> m (Dynamic t Double)
ticker fact = mapDyn (/ fact)
          =<< holdDyn 0.0
          =<< fmap (fmap (fromIntegral . _tickInfo_n)) . tickLossy (1 / 60)
          =<< liftIO getCurrentTime

gift :: MonadWidget t m => String -> Integer -> Dynamic t Double -> m (Dynamic t Double)
gift nm price money = do
    disabler <- mapDyn (\ c -> if c < fromIntegral price then [("disabled", "1")] else []) money
    (buyer, _) <- elDynAttr' "button" disabler $ text $ "Buy " ++ nm ++ " ($" ++ show price ++ ")"
    foldDyn id 0 $ fmap (const (subtract (fromIntegral price))) (domEvent Click buyer)

sumDyn :: (MonadHold t m, Reflex t, Num a) => [Dynamic t a] -> m (Dynamic t a)
sumDyn = mapDyn getSum <=< mconcatDyn <=< mapM (mapDyn Sum)

main :: IO ()
main = mainWidget $ el "div" $ do
    rec money <- sumDyn =<< sequence [ticker 19, gift "coat" 50 money, gift "shoes" 200 money]
    el "h3" $ do
        text "Money: "
        dynText =<< mapDyn ((show :: Integer -> String) . floor) money
        text " (actual: "
        dynText =<< mapDyn (($ "") . showFFloat (Just 3)) money
        text ")"

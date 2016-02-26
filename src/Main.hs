{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Map               (Map, fromList, showTreeWith, unionWith)
import Data.Monoid
import Data.Time
import Numeric
import Reflex.Dom

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

ticker :: MonadWidget t m => Double -> m (Event t Double)
ticker fact = do
    t <- liftIO getCurrentTime
    tck <- tickLossy (1 / 60) t
    be <- hold Nothing (fmap Just tck)
    return $ fmap (maybe 0 (\ (a,b) -> fromRational (toRational $ diffUTCTime (_tickInfo_lastUTC b) (_tickInfo_lastUTC a)) * fact)
                  . uncurry (liftM2 (,)))
           $ attach be (fmap Just tck)

gift :: MonadWidget t m => String -> Integer -> Double -> Dynamic t Game -> m (Event t (Double, Map String (Sum Integer, Sum Double)))
gift nm price mult dg = do
    myMoney <- mapDyn (view lMoney) dg
    disabler <- mapDyn (\ c -> fromList [("disabled", "1") | c < fromIntegral price]) myMoney
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

getTotalMultiplier :: Map String (Sum Integer, Sum Double) -> Double
getTotalMultiplier m = 1 + getSum (foldMap snd m)

homeView :: MonadWidget t m => m ()
homeView = do
    rec monetaryBalance <- foldDyn (+) 0 $ mergeWith (+) [giftMoney, stateIncome]
        stateIncome <- fmap (attachWith (\ g n -> n * view lMultiplier g) (current game)) (ticker 100)
        giftCollection <- foldDyn (unionWith mappend) mempty giftThings
        (giftMoney, giftThings) <- splitE . mergeWith (\(a,b) (c,d) -> (a + c, unionWith mappend b d))
                               <$> mapM (\ (g,p,m) -> gift g p m game) myGifts
        game <- [mkDyn|Game $monetaryBalance
                            (fmap fst $giftCollection)
                            (getTotalMultiplier $giftCollection)|]
    el "h3" $ do
        text "Money: "
        dynText =<< mapDyn ((show :: Integer -> String) . floor . view lMoney) game
        el "br" $ text ""
        el "pre" $ dynText =<< mapDyn showGame game

main :: IO ()
main = mainWidget $ tabDisplay "active" "active-li" [("home", ("Home", homeView)), ("away", ("Away", text "Hello, world!"))]

showGame :: Game -> String
showGame (Game m g x) = "Current money: $" ++ showFFloat (Just 2) m "" ++ "\n"
                     ++ showTreeWith (\ k (Sum s) -> k ++ " => " ++ show s) False True g ++ "\n"
                     ++ "Multiplier: " ++ show x ++ "x"

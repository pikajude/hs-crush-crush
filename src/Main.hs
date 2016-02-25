{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE RecursiveDo        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Map               (Map, fromList, showTree, unionWith)
import Data.Monoid
import Data.Time
import Debug.Trace
import Reflex.Dom

data Game = Game
          { _money      :: Double
          , _gifts      :: Map String (Sum Integer)
          , _multiplier :: Integer
          } deriving Show

makeLenses ''Game

ticker :: MonadWidget t m => Double -> m (Event t Double)
ticker fact = do
    t <- liftIO getCurrentTime
    tck <- tickLossy (1 / 60) t
    be <- hold Nothing (fmap Just tck)
    return $ fmap (maybe 0 (\ (a,b) -> fromRational (toRational $ diffUTCTime (_tickInfo_lastUTC b) (_tickInfo_lastUTC a)) * fact)
                  . uncurry (liftM2 (,)))
           $ attach be (fmap Just tck)

gift :: MonadWidget t m => String -> Integer -> Dynamic t Game -> m (Event t (Double, Map String (Sum Integer)))
gift nm price dg = do
    myMoney <- mapDyn (view money) dg
    disabler <- mapDyn (\ c -> fromList [("disabled", "1") | c < fromIntegral price]) myMoney
    (buyer, _) <- elDynAttr' "button" disabler $ text $ "Buy " ++ nm ++ " ($" ++ show price ++ ")"
    return $ fmap (const (negate (fromIntegral price), [(nm, Sum 1)])) (domEvent Click buyer)

appDyn :: (MonadFix m, MonadHold t m, Reflex t) => Game -> [Event t (Game -> Game)] -> m (Dynamic t Game)
appDyn g xs = foldDyn ($) g (mergeWith (.) xs)

myGifts :: [(String, Integer)]
myGifts = [ ("shell", 500)
        , ("rose", 950)
        , ("hand lotion", 1805)
        , ("donut", 3430)
        , ("fruit basket", 6516)
        , ("chocolates", 12380)
        , ("book", 23523)
        , ("earrings", 44694)
        ]

main :: IO ()
main = mainWidget $ el "div" $ do
    rec monetaryBalance <- foldDyn (+) 0 $ mergeWith (+) [giftMoney, stateIncome]
        stateIncome <- fmap (attachWith (\ g n -> n * fromIntegral (1 + length (view gifts g))) (current game)) (ticker 100)
        giftCollection <- foldDyn (unionWith mappend) mempty giftThings
        game <- combineDyn (\m g -> Game m g 1) monetaryBalance giftCollection
        (giftMoney, giftThings) <- splitE . mergeWith (\(a,b) (c,d) -> (a + c, unionWith mappend b d))
                               <$> mapM (\ (g,p) -> gift g p game) myGifts
    el "h3" $ do
        text "Money: "
        dynText =<< mapDyn ((show :: Integer -> String) . floor . view money) game
        el "br" $ text ""
        el "pre" $ dynText =<< mapDyn (showTree . view gifts) game

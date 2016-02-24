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
import Data.Map               (Map, showTree)
import Data.Time
import Reflex.Dom

data Game = Game
          { _money :: Double
          , _gifts :: Map String Integer
          } deriving Show

makeLenses ''Game

ticker :: MonadWidget t m => Double -> m (Event t (Game -> Game))
ticker fact = do
    t <- liftIO getCurrentTime
    tck <- tickLossy (1 / 60) t
    be <- hold Nothing (fmap Just tck)
    return $ fmap ((money +~)
        . maybe 0 (\ (a,b) -> fromRational (toRational $ diffUTCTime (_tickInfo_lastUTC b) (_tickInfo_lastUTC a)) * fact)
        . uncurry (liftM2 (,)))
        $ attach be (fmap Just tck)

gift :: MonadWidget t m => String -> Integer -> Dynamic t Game -> m (Event t (Game -> Game))
gift nm price dg = do
    myMoney <- mapDyn (view money) dg
    disabler <- mapDyn (\ c -> if c < fromIntegral price then [("disabled", "1")] else []) myMoney
    (buyer, _) <- elDynAttr' "button" disabler $ text $ "Buy " ++ nm ++ " ($" ++ show price ++ ")"
    return $ fmap (const ((money -~ fromIntegral price) . (gifts . at nm . non 0 +~ 1))) (domEvent Click buyer)

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
    rec game <- appDyn (Game 0 mempty) =<< sequence (ticker 100 : map (\ (g,p) -> gift g p game) myGifts)
    el "h3" $ do
        text "Money: "
        dynText =<< mapDyn ((show :: Integer -> String) . floor . view money) game
        el "br" $ text ""
        el "pre" $ dynText =<< mapDyn (showTree . view gifts) game

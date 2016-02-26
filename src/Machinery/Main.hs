{-# LANGUAGE RecursiveDo #-}

module Machinery.Main where

import Control.Lens
import Data.Map        (Map, unionWith)
import Data.Monoid
import Machinery.Gifts
import Machinery.Timer
import Models.Game
import Reflex.Dom

getTotalMultiplier :: Map String (Sum Integer, Sum Double) -> Double
getTotalMultiplier m = 1 + getSum (foldMap snd m)

initGame :: MonadWidget t m => m (Dynamic t Game)
initGame = do
    rec monetaryBalance <- foldDyn (+) 0 $ mergeWith (+) [giftMoney, stateIncome]
        stateIncome <- fmap (attachWith (\ g n -> n * view lMultiplier g) (current game)) (ticker 100)
        giftCollection <- foldDyn (unionWith mappend) mempty giftThings
        (giftMoney, giftThings) <- splitE . mergeWith (\ (a,b) (c,d) -> (a + c, unionWith mappend b d))
                               <$> mapM (\ (g,p,m) -> gift g p m game) myGifts
        game <- combineDyn (\ m g -> Game m (fst <$> g) (getTotalMultiplier g))
                    monetaryBalance giftCollection
    return game

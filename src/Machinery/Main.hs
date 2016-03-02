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
    let baseSalary = 0.5
    rec monetaryBalance <- foldDyn (+) 0 $ mergeWith (+) [giftMoney, stateIncome]
        stateIncome <- fmap (attachWith (\ g n -> n * view lSalary g * view lMultiplier g) (current game)) (ticker 1)

        (giftMoney, giftThings) <- elClass "div" "row" $ initGifts game
        giftCollection <- foldDyn (unionWith mappend) mempty giftThings

        game <- combineDyn (\ m g -> let mult = getTotalMultiplier g
                                      in Game m baseSalary (fst <$> g) mult)
                    monetaryBalance giftCollection
    return game
    where
        initGifts game = splitE . mergeWith (\ (a,b) (c,d) -> (a + c, unionWith mappend b d))
                     <$> mapM (\ (g,p,m) -> gift g p m game) myGifts

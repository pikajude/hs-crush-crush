{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE OverloadedLists  #-}

import Control.Lens
import Machinery.Main
import Models.Game
import Reflex.Dom

homeView :: MonadWidget t m => Dynamic t Game -> m ()
homeView game = do
    el "h3" $ do
        text "Money: "
        dynText =<< mapDyn ((show :: Integer -> String) . floor . view lMoney) game
    el "br" $ text ""
    el "pre" $ dynText =<< mapDyn showGame game

main :: IO ()
main = mainWidget $ do
    game <- initGame
    homeView game

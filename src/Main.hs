{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE OverloadedLists  #-}

import Control.Lens
import Machinery.Main
import Models.Game
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.DynTabs

homeView :: MonadWidget t m => Dynamic t Game -> m ()
homeView game = el "h3" $ do
    text "Money: "
    dynText =<< mapDyn ((show :: Integer -> String) . floor . view lMoney) game
    el "br" $ text ""
    el "pre" $ dynText =<< mapDyn showGame game

main :: IO ()
main = mainWidget $ do
    game <- initGame
    tabs <- tabBar "sup" "home" ["home", "away"] (pure ["home", "away"]) (pure "home") (constDyn [])
    tabPane tabs "home" $ homeView game
    tabPane tabs "away" $ text "Hello, world!"

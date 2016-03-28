{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}

module Machinery.Girls where

import Control.Lens  ((??))
import Control.Monad
import Data.Map      (Map, fromList)
import Data.Monoid
import Reflex.Dom

type Screen = forall t m. MonadWidget t m => El t -> Dynamic t Integer -> Workflow t m ()

girl :: MonadWidget t m => String -> Dynamic t Integer -> m ()
girl name bankBalance = elClass "div" "row" $ do
    el "hr" $ return ()
    (clicker, _) <- el "h3" $ el' "a" $ text name
    void $ workflow $ screen0 clicker bankBalance

screen0 :: Screen
screen0 clicker bankBalance = Workflow $ el "div" $ do
    affectionSink <- foldDyn ((min 10 .) . (+)) (0 :: Integer) (1 <$ domEvent Click clicker)
    nexter <- requirements
        [ requirement "Affection" affectionSink 10
        , requirement "Bank balance" bankBalance 100
        ]
    return ((), screen1 clicker bankBalance <$ domEvent Click nexter)

screen1 :: Screen
screen1 clicker bankBalance = Workflow $ el "div" $ do
    affectionSink <- foldDyn ((min 100 .) . (+)) (0 :: Integer) (1 <$ domEvent Click clicker)
    nexter <- requirements
        [ requirement "Affection" affectionSink 100
        , requirement "Bank balance" bankBalance 250
        , requirement "A shoe" (constDyn 0) (1 :: Integer)
        ]
    return ((), undefined <$ domEvent Click nexter)

requirements :: MonadWidget t m => [m (Dynamic t Bool)] -> m (El t)
requirements reqs = do
    rs <- mconcatDyn =<< mapM (mapDyn (:[])) =<< sequence reqs
    (nexter, _) <- (elDynAttr' "button" ?? text "Next")
        =<< mapDyn (<> [("type", "button"), ("class", "success button")])
        =<< displayNone not
        =<< [mkDyn|and $rs|]
    return nexter

requirement :: (MonadWidget t m, Show a, Ord a, Num a) => String -> Dynamic t a -> a -> m (Dynamic t Bool)
requirement reqName currentValue objective = do
    el "dl" $ do
        el "dt" $ text reqName
        el "dd" $ if objective == 1
            then dynText =<< mapDyn (\ x -> if x >= 1 then "Success!" else "No " ++ reqName ++ " yet...") currentValue
            else do
                elClass "span" "value" $ dynText =<< mapDyn (show . min objective) currentValue
                text "  ∕"
                elClass "span" "value" $ text $ show objective
                text " "
                (elDynAttr "span" ?? text "✓")
                    =<< mapDyn (<> [("class", "success badge")])
                    =<< displayNone (< objective) currentValue
    mapDyn (>= objective) currentValue

displayNone :: MonadWidget t m => (a -> Bool) -> Dynamic t a -> m (Dynamic t (Map String String))
displayNone cond inp = [mkDyn|fromList [("style", "display: none") | cond $inp ]|]

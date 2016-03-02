{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import Control.Lens       hiding ((.=))
import Data.ByteString    (ByteString)
import Data.FileEmbed
import Data.Monoid
import Data.Text.Encoding
import Machinery.Main
import Models.Game
import Reflex.Dom
import Stitch

homeView :: MonadWidget t m => Dynamic t Game -> m ()
homeView game = do
    el "h3" $ do
        text "Money: $"
        dynText =<< mapDyn (commas . (show :: Integer -> String) . floor . view lMoney) game
    el "br" $ text ""
    el "pre" $ dynText =<< mapDyn showGame game

commas :: String -> String
commas = reverse . commas' . reverse where
    commas' [a,b,c] = [a,b,c]
    commas' (a:b:c:ds) = a : b : c : ',' : commas' ds
    commas' xs = xs

main :: IO ()
main = mainWidgetWithCss ($(embedFile "bower_components/foundation-sites/dist/foundation.min.css") <> mainCss) $ do
    game <- initGame
    homeView game

mainCss :: ByteString
mainCss = encodeUtf8 $ renderCSS $
    "body" ? do
        ".gift-buy" ?
            "margin-bottom" .= "0"
        ".progress-meter-text" ? do
            "transform" .= "translate(0, -8%)"
            "top" .= "0"
            "left" .= "0"
            "position" .= "relative"
            "text-align" .= "center"

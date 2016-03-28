{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import Control.Lens       hiding ((.=))
import Data.ByteString    (ByteString)
import Data.FileEmbed
import Data.Monoid
import Data.Text          (Text, unpack)
import Data.Text.Encoding
import Machinery.Girls
import Machinery.Main
import Models.Game
import Reflex.Dom
import Stitch

homeView :: MonadWidget t m => Dynamic t Game -> m ()
homeView game = do
    girl "Cassie" =<< mapDyn (floor . view lMoney) game
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
main = mainWidgetWithHead mainHead $ do
    game <- initGame
    homeView game

mainHead = do
    el "style" $ text . unpack $ foundationCss <> mainCss
    elAttr "meta" [("charset", "UTF-8")] $ return ()

foundationCss :: Text
foundationCss = decodeUtf8 $(embedFile "bower_components/foundation-sites/dist/foundation.min.css")

mainCss :: Text
mainCss = renderCSS $
    "body" ? do
        "-webkit-user-select" .= "none"
        ".gift-buy" ? do
            "margin-bottom" .= "0"
            -- "font-family" .= "\"Apple Color Emoji\",\"Android Emoji\",\"Segoe UI Emoji\",\"EmojiSymbols\",\"Symbola\",\"Inconsolata\",\"Consolas\",\"Ubuntu Mono\",\"Menlo\",monospace;"
        ".progress-meter-text" ? do
            "transform" .= "translate(0, -8%)"
            "top" .= "0"
            "left" .= "0"
            "position" .= "relative"
            "text-align" .= "center"

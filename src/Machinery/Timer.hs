module Machinery.Timer where

import Control.Monad
import Control.Monad.IO.Class
import Data.Time
import Reflex.Dom

ticker :: MonadWidget t m => Double -> m (Event t Double)
ticker fact = do
    t <- liftIO getCurrentTime
    tck <- tickLossy (1 / 60) t
    be <- hold Nothing (fmap Just tck)
    return $ (maybe 0 (\ (a,b) -> fromRational (toRational $ diffUTCTime (_tickInfo_lastUTC b) (_tickInfo_lastUTC a)) * fact)
                  . uncurry (liftM2 (,)))
           <$> attach be (fmap Just tck)

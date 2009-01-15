-----------------------------------------------------------------------------------------
{-| Module      : AlgbRewrites.TIRewrite1
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module AlgbRewrites.TIRewrite2 where


import Core2Algb(TableInfos (..))
import AlgbRewrites.AlgbRewrite2


rewrite :: TableInfos -> TableInfos
rewrite  (TI (e, cols, subs)) =
            let
                subs' = map f subs
                f (c, ti) = (c, AlgbRewrites.TIRewrite2.rewrite ti)
                e' = AlgbRewrites.AlgbRewrite2.rewrite e
            in
                (TI (e', cols, subs'))
                


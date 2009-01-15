-----------------------------------------------------------------------------------------
{-| Module      : AlgbRewrites.TIRewrite1
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module AlgbRewrites.TIRewrite1 where


import Core2Algb(TableInfos (..))
import AlgbRewrites.AlgbRewrite1


rewrite :: TableInfos -> TableInfos
rewrite  (TI (e, cols, subs)) =
            let
                subs' = map f subs
                f (c, ti) = (c, AlgbRewrites.TIRewrite1.rewrite ti)
                e' = AlgbRewrites.AlgbRewrite1.rewrite e
            in
                (TI (e', cols, subs'))
                


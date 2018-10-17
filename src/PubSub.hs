{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module PubSub
    (
      PubSub
    , newPubSub
    , publish
    , subscribe
    , fetchViews
    ) where

import EventSource(computeProjection, Projection)

import Data.Map(empty, Map, alter, lookupMax)
import Data.Maybe(fromJust)

data PubSub i e p = PubSub { 
                    getPubEvents :: [(i, e)]
                  , getSubsribers :: [(Projection i e p, p)]
                  }

newPubSub :: PubSub i e p
newPubSub = PubSub [] []

publish :: i -> e -> PubSub i e p -> PubSub i e p
publish i e s = s { getPubEvents = (i, e):getPubEvents s }

subscribe :: Show p => Projection i e p -> p -> PubSub i e p -> PubSub i e p
subscribe p d s = s { getSubsribers = (p, d):getSubsribers s }

fetchViews :: PubSub a e p -> [p]
fetchViews s = map (\(p,d) -> computeProjection p d es) (getSubsribers s)
  where es = reverse $ getPubEvents s

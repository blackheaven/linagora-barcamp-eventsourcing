{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module PubSub
    (
      PubSub
    , newPubSub
    , publish
    , subscribe
    , fetchViews
    ) where

import EventSource(computeProjection, Projection, Event)

data PubSub e a = PubSub { 
                  getPubEvents :: [Event e]
                , getSubsribers :: [(Projection e a, a)]
                }

newPubSub :: PubSub e p
newPubSub = PubSub [] []

publish :: Event e -> PubSub e a -> PubSub e a
publish e s = s { getPubEvents = e:getPubEvents s }

subscribe :: Projection e a -> a -> PubSub e a -> PubSub e a
subscribe p d s = s { getSubsribers = (p, d):getSubsribers s }

fetchViews :: PubSub e a -> [a]
fetchViews s = map (\(p,d) -> computeProjection p d es) (getSubsribers s)
  where es = reverse $ getPubEvents s

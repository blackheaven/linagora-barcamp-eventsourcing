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

publish :: Eq e => Event e -> PubSub e a -> PubSub e a
publish e s = s { getPubEvents = newEvents }
  where newEvents = if notElem e oldEvents then e:oldEvents else oldEvents
        oldEvents = getPubEvents s

subscribe :: Projection e a -> a -> PubSub e a -> PubSub e a
subscribe p d s = s { getSubsribers = (p, d):getSubsribers s }

fetchViews :: Eq e => PubSub e a -> [a]
fetchViews s = map (\(p,d) -> computeProjection p d es) (getSubsribers s)
  where es = reverse $ getPubEvents s

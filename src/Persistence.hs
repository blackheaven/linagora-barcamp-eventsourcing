module Persistence
    (
      Persister
    , newPersister
    , persist
    , fetch
    ) where

import EventSource(AggregateId, Aggregate(..))

import Data.Map as M (empty, Map, alter, lookup)

data Persister a c e = Persister (M.Map AggregateId (Aggregate a c e))

persist :: Eq e => Persister a c e -> Aggregate a c e -> Maybe (Persister a c e)
persist (Persister p) a = (const $ Persister newP) <$> M.lookup (aggregateId a) newP
  where newP = M.alter (maybe (Just a) update) (aggregateId a) p
        update o = if follows (events a) (events o) then Just a else Nothing
        follows x y = drop (length x - length y) x == y

newPersister :: Persister a c e
newPersister = Persister M.empty

fetch :: Persister a c e -> AggregateId -> Maybe (Aggregate a c e)
fetch (Persister p) i = M.lookup i p

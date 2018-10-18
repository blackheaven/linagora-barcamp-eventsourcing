module EventSource
    (
      Aggregate(..)
    , AggregateId(..)
    , applyCommand
    , applyCommands
    , applyEvents
    , computeProjection
    , Decision(..)
    , Projection(..)
    , Event(..)
    , EventId(..)
    ) where

import Data.Foldable(foldl')

data Aggregate a c e = Aggregate {
                       aggregateId :: AggregateId
                     , projection :: a
                     , events :: [Event e]
                     , applyCommand' :: Decision a c e
                     , applyEvent' :: Projection e (Aggregate a c e)
                     }

newtype AggregateId = AggregateId Integer deriving (Show, Eq, Ord)

instance (Show a, Show e) => Show (Aggregate a c e) where
    show x = "Aggregate " ++ show (aggregateId x) ++ " (" ++ show (projection x) ++ ") : " ++ show (reverse $ events x)

instance Eq a => Eq (Aggregate a e c) where
    (Aggregate _ a _ _ _) == (Aggregate _ b _ _ _) = a == b

data Event e = Event {
               eventId :: EventId
             , eventAggregateId :: AggregateId
             , eventValue :: e
             } deriving (Show)

instance Eq e => Eq (Event e) where
    (Event a _ _) == (Event b _ _) = a == b

newtype EventId = EventId Integer deriving (Show, Eq, Ord)

newtype Decision a c e = Decision { extractCommandApplier :: Aggregate a c e -> [e -> Event e] -> c -> [Event e] }

newtype Projection e a = Projection { extractEventApplier :: a -> Event e -> a }

applyCommand :: Aggregate a c e -> [e -> Event e] -> c -> [Event e]
applyCommand a is e = (extractCommandApplier . applyCommand') a a is e

applyCommands :: Eq e => Aggregate a c e -> [e -> Event e] -> [c] ->  [Event e]
applyCommands a is xs = concat $ reverse $ fst $ snd $ foldl' apply (a, ([],is)) xs
  where apply :: Eq e => (Aggregate a c e, ([[Event e]], [e -> Event e])) -> c -> (Aggregate a c e, ([[Event e]], [e -> Event e]))
        apply (s, (es,iss)) c = let evs = applyCommand s iss c in (applyEvents s evs, (evs:es, drop (length evs) iss))

applyEvents :: Eq e => Aggregate a c e -> [Event e] -> Aggregate a c e
applyEvents a xs = computeProjection (ensureIdemPotence $ applyEvent' a) a xs

ensureIdemPotence :: Eq e => Projection e (Aggregate a c e) -> Projection e (Aggregate a c e)
ensureIdemPotence p = Projection $ \a e -> if notElem e (events a)
                                             then extractEventApplier p a e
                                             else a

computeProjection :: Projection e a -> a -> [Event e] -> a
computeProjection p i e = foldl' (extractEventApplier p) i e

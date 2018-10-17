module EventSource
    (
      Aggregate(..)
    , applyCommand
    , applyCommands
    , applyEvent
    , applyEvents
    , CommandApplier(..)
    , EventApplier(..)
    ) where

import Data.Foldable(foldl')

data Aggregate a c e = Aggregate {
                       value :: a
                     , applyCommand' :: CommandApplier a c e
                     , applyEvent' :: EventApplier a c e
                     }

instance Show a => Show (Aggregate a e c) where
    show x = "Aggregate " ++ show (value x)

instance Eq a => Eq (Aggregate a e c) where
    (Aggregate a _ _) == (Aggregate b _ _) = a == b

newtype CommandApplier a c e = CommandApplier { extractCommandApplier :: a -> c -> [e] }

newtype EventApplier a c e = EventApplier { extractEventApplier :: a -> e -> Aggregate a c e }

applyCommand :: Aggregate a c e -> c -> [e]
applyCommand a xs = (extractCommandApplier . applyCommand') a (value a) xs

applyCommands :: Aggregate a c e -> [c] -> [e]
applyCommands a xs = concat $ reverse $ snd $ foldl' apply (a, []) xs
  where apply (s, es) c = let events = applyCommand s c in (applyEvents s events, events:es)

applyEvent :: Aggregate a c e -> e -> Aggregate a c e
applyEvent a xs = (extractEventApplier . applyEvent') a (value a) xs

applyEvents :: Aggregate a c e -> [e] -> Aggregate a c e
applyEvents a xs = foldl' applyEvent a xs

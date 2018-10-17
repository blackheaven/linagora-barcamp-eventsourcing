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
                       projection :: a
                     , events :: [e]
                     , applyCommand' :: CommandApplier a c e
                     , applyEvent' :: EventApplier a c e
                     }

instance (Show a, Show e) => Show (Aggregate a c e) where
    show x = "Aggregate " ++ show (projection x) ++ " : " ++ show (reverse $ events x)

instance Eq a => Eq (Aggregate a e c) where
    (Aggregate a _ _ _) == (Aggregate b _ _ _) = a == b

newtype CommandApplier a c e = CommandApplier { extractCommandApplier :: Aggregate a c e -> c -> [e] }

newtype EventApplier a c e = EventApplier { extractEventApplier :: Aggregate a c e -> e -> Aggregate a c e }

applyCommand :: Aggregate a c e -> c -> [e]
applyCommand a xs = (extractCommandApplier . applyCommand') a a xs

applyCommands :: Aggregate a c e -> [c] -> [e]
applyCommands a xs = concat $ reverse $ snd $ foldl' apply (a, []) xs
  where apply (s, es) c = let evs = applyCommand s c in (applyEvents s evs, evs:es)

applyEvent :: Aggregate a c e -> e -> Aggregate a c e
applyEvent a xs = (extractEventApplier . applyEvent') a a xs

applyEvents :: Aggregate a c e -> [e] -> Aggregate a c e
applyEvents a xs = foldl' applyEvent a xs

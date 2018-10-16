-- | A library to do stuff.
module Lib
    (
      workflow
    , newWorkflow
    , applyCommand
    , applyCommands
    , applyEvent
    , applyEvents
    , User(..)
    , WorkflowCommand(..)
    , WorkflowEvent(..)
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

type CommandApplier a c e = a -> c -> [e]

type EventApplier a c e = a -> e -> Aggregate a c e

workflow :: Aggregate Workflow WorkflowCommand WorkflowEvent
workflow = instanciateWorkflow newWorkflow

instanciateWorkflow :: Workflow -> Aggregate Workflow WorkflowCommand WorkflowEvent
instanciateWorkflow x = Aggregate x applyCommandWorkflow applyEventWorkflow

newtype User = User String
    deriving (Show, Eq)

data Workflow = Workflow {
                assigned :: Maybe User
              } deriving (Show, Eq)

data WorkflowCommand = AssignTask User
                     | AchieveTask
                     | RestartTask
                     | UnassignTask
                     | PostoneTask
                     | StartTask
    deriving (Show, Eq)

data WorkflowEvent = AssignedTask User
                   | Done
                   | TaskReDone
                   | UnassignedTasked
                   | PostponedTask
                   | TaskStarted
    deriving (Show, Eq)

newWorkflow :: Workflow
newWorkflow = Workflow Nothing

applyCommandWorkflow :: CommandApplier Workflow WorkflowCommand WorkflowEvent
applyCommandWorkflow x c = case c of
                                AssignTask u -> if assigned x == Just u then [] else [AssignedTask u]
                                _ -> undefined

applyEventWorkflow :: EventApplier Workflow WorkflowCommand WorkflowEvent
applyEventWorkflow x e = case e of
                              AssignedTask u -> instanciateWorkflow $ x { assigned = Just u }
                              _ -> undefined

applyCommand :: Aggregate a c e -> c -> [e]
applyCommand a xs = applyCommand' a (value a) xs

applyCommands :: Aggregate a c e -> [c] -> [e]
applyCommands a xs = concat $ reverse $ snd $ foldl' apply (a, []) xs
  where apply (s, es) c = let events = applyCommand s c in (applyEvents s events, events:es)

applyEvent :: Aggregate a c e -> e -> Aggregate a c e
applyEvent a xs = applyEvent' a (value a) xs

applyEvents :: Aggregate a c e -> [e] -> Aggregate a c e
applyEvents a xs = foldl' applyEvent a xs

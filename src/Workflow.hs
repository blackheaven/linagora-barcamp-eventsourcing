module Workflow
    (
      workflow
    , newWorkflow
    , User(..)
    , WorkflowCommand(..)
    , WorkflowEvent(..)
    ) where

import EventSource(Aggregate(..), CommandApplier(..), EventApplier(..))

type WorkflowTyper a = a Workflow WorkflowCommand WorkflowEvent

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
                   | UnassignedTask
                   | PostponedTask
                   | TaskStarted
    deriving (Show, Eq)

newWorkflow :: Workflow
newWorkflow = Workflow Nothing

workflow :: WorkflowTyper Aggregate
workflow = instanciateWorkflow newWorkflow

instanciateWorkflow :: Workflow -> WorkflowTyper Aggregate
instanciateWorkflow x = Aggregate x applyCommandWorkflow applyEventWorkflow

applyCommandWorkflow :: WorkflowTyper CommandApplier
applyCommandWorkflow = CommandApplier $ \ x c -> case c of
                                                      AssignTask u -> if assigned x == Just u then [] else [AssignedTask u]
                                                      UnassignTask -> maybe [] (const [UnassignedTask]) (assigned x)
                                                      _ -> undefined

applyEventWorkflow :: WorkflowTyper EventApplier
applyEventWorkflow = EventApplier $ \x e -> case e of
                                                  AssignedTask u -> instanciateWorkflow $ x { assigned = Just u }
                                                  UnassignedTask -> instanciateWorkflow $ x { assigned = Nothing }
                                                  _ -> undefined

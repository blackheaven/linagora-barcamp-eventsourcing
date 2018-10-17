module Workflow
    (
      workflow
    , newWorkflow
    , WorkflowTyper
    , User(..)
    , WorkflowCommand(..)
    , WorkflowEvent(..)
    ) where

import EventSource(Aggregate(..), CommandApplier(..), EventApplier(..))

type WorkflowTyper a = a Workflow WorkflowCommand WorkflowEvent

newtype User = User String
    deriving (Show, Eq)

data Status = Waiting
            | Started
            | Archived
    deriving (Show, Eq)

data Workflow = Workflow {
                assigned :: Maybe User
              , status :: Status
              } deriving (Show, Eq)

data WorkflowCommand = AssignTask User
                     | AchieveTask
                     | RestartTask
                     | UnassignTask
                     | PostponeTask
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
newWorkflow = Workflow Nothing Waiting

workflow :: WorkflowTyper Aggregate
workflow = instanciateWorkflow newWorkflow

instanciateWorkflow :: Workflow -> WorkflowTyper Aggregate
instanciateWorkflow w = Aggregate w applyCommandWorkflow applyEventWorkflow

applyCommandWorkflow :: WorkflowTyper CommandApplier
applyCommandWorkflow = CommandApplier $ \ w c -> case c of
                                                      AssignTask u -> if assigned w == Just u   then [] else [AssignedTask u]
                                                      AchieveTask  -> if status w   == Started  then [Done]          else []
                                                      RestartTask  -> if status w   == Archived then [TaskReDone]    else []
                                                      UnassignTask ->    maybe [] (const [UnassignedTask]) (assigned w)
                                                      PostponeTask -> if status w   == Started  then [PostponedTask] else []
                                                      StartTask    -> if status w   == Waiting  then [TaskStarted]   else []

applyEventWorkflow :: WorkflowTyper EventApplier
applyEventWorkflow = EventApplier $ \w e -> case e of
                                                  AssignedTask u -> instanciateWorkflow $ w { assigned = Just u }
                                                  Done           -> instanciateWorkflow $ w { status = Archived }
                                                  TaskReDone     -> instanciateWorkflow $ w { status = Waiting }
                                                  UnassignedTask -> instanciateWorkflow $ w { assigned = Nothing }
                                                  PostponedTask  -> instanciateWorkflow $ w { status = Waiting }
                                                  TaskStarted    -> instanciateWorkflow $ w { status = Started }

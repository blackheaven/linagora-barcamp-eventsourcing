module TaskWorkflow
    (
      waitingTaskWorkflow
    , newTaskWorkflow
    , TaskWorkflowTyper
    , User(..)
    , TaskWorkflowCommand(..)
    , TaskWorkflowEvent(..)
    , TaskWorkflow(..)
    ) where

import EventSource(Aggregate(..), AggregateId, Decision(..), Event(..), Projection(..))

type TaskWorkflowTyper a = a TaskWorkflow TaskWorkflowCommand TaskWorkflowEvent
type TaskWorkflowTyper' a = a TaskWorkflowEvent (TaskWorkflowTyper Aggregate)

newtype User = User String
    deriving (Show, Eq)

data Status = Waiting
            | Started
            | Archived
    deriving (Show, Eq)

data TaskWorkflow = TaskWorkflow {
                    assigned :: Maybe User
                  , status :: Status
                  } deriving (Show, Eq)

data TaskWorkflowCommand = AssignTask User
                         | AchieveTask
                         | RestartTask
                         | UnassignTask
                         | PostponeTask
                         | StartTask
    deriving (Show, Eq)

data TaskWorkflowEvent = AssignedTask User
                       | Done
                       | TaskReDone
                       | UnassignedTask
                       | PostponedTask
                       | TaskStarted
    deriving (Show, Eq)

newTaskWorkflow :: TaskWorkflow
newTaskWorkflow = TaskWorkflow Nothing Waiting

waitingTaskWorkflow :: AggregateId -> TaskWorkflowTyper Aggregate
waitingTaskWorkflow x = Aggregate x newTaskWorkflow [] applyCommandTaskWorkflow applyEventTaskWorkflow

applyCommandTaskWorkflow :: TaskWorkflowTyper Decision
applyCommandTaskWorkflow = Decision $ \a is c -> zipWith ($) is $
    case c of
        AssignTask u -> onNotAssigned a (Just u) [AssignedTask u]
        AchieveTask  -> onStatus a Started [Done]
        RestartTask  -> onStatus a Archived [TaskReDone]
        UnassignTask -> onNotAssigned a Nothing [UnassignedTask]
        PostponeTask -> onStatus a Started [PostponedTask]
        StartTask    -> onStatus a Waiting [TaskStarted]
    where onStatus a v x = if status (projection a) == v then x else []
          onNotAssigned a v x = if assigned (projection a) == v then [] else x

applyEventTaskWorkflow :: TaskWorkflowTyper' Projection
applyEventTaskWorkflow = Projection $ \a e -> case eventValue e of
                                                   AssignedTask u -> updateTaskWorkflow a (\t -> t { assigned = Just u }) e
                                                   Done           -> updateTaskWorkflow a (\t -> t { status = Archived }) e
                                                   TaskReDone     -> updateTaskWorkflow a (\t -> t { status = Waiting }) e
                                                   UnassignedTask -> updateTaskWorkflow a (\t -> t { assigned = Nothing }) e
                                                   PostponedTask  -> updateTaskWorkflow a (\t -> t { status = Waiting }) e
                                                   TaskStarted    -> updateTaskWorkflow a (\t -> t { status = Started }) e
    where updateTaskWorkflow a t e = a { projection = t (projection a), events = e:(events a) }

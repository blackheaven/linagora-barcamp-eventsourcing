module TaskWorkflow
    (
      waitingTaskWorkflow
    , newTaskWorkflow
    , TaskWorkflowTyper
    , User(..)
    , TaskWorkflowCommand(..)
    , TaskWorkflowEvent(..)
    ) where

import EventSource(Aggregate(..), CommandApplier(..), EventApplier(..))

type TaskWorkflowTyper a = a TaskWorkflow TaskWorkflowCommand TaskWorkflowEvent

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

waitingTaskWorkflow :: TaskWorkflowTyper Aggregate
waitingTaskWorkflow = Aggregate newTaskWorkflow [] applyCommandTaskWorkflow applyEventTaskWorkflow

applyCommandTaskWorkflow :: TaskWorkflowTyper CommandApplier
applyCommandTaskWorkflow = CommandApplier $ \a c -> case c of
                                                         AssignTask u -> onNotAssigned a (Just u) [AssignedTask u]
                                                         AchieveTask  -> onStatus a Started [Done]
                                                         RestartTask  -> onStatus a Archived [TaskReDone]
                                                         UnassignTask -> onNotAssigned a Nothing [UnassignedTask]
                                                         PostponeTask -> onStatus a Started [PostponedTask]
                                                         StartTask    -> onStatus a Waiting [TaskStarted]
    where onStatus a v x = if status (projection a) == v then x else []
          onNotAssigned a v x = if assigned (projection a) == v then [] else x

applyEventTaskWorkflow :: TaskWorkflowTyper EventApplier
applyEventTaskWorkflow = EventApplier $ \a e -> case e of
                                                     AssignedTask u -> updateTaskWorkflow a (\t -> t { assigned = Just u }) e
                                                     Done           -> updateTaskWorkflow a (\t -> t { status = Archived }) e
                                                     TaskReDone     -> updateTaskWorkflow a (\t -> t { status = Waiting }) e
                                                     UnassignedTask -> updateTaskWorkflow a (\t -> t { assigned = Nothing }) e
                                                     PostponedTask  -> updateTaskWorkflow a (\t -> t { status = Waiting }) e
                                                     TaskStarted    -> updateTaskWorkflow a (\t -> t { status = Started }) e

updateTaskWorkflow :: TaskWorkflowTyper Aggregate -> (TaskWorkflow -> TaskWorkflow) -> TaskWorkflowEvent -> TaskWorkflowTyper Aggregate
updateTaskWorkflow a t e = a { projection = t (projection a), events = e:(events a) }

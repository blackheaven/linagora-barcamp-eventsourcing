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
waitingTaskWorkflow = instanciateTaskWorkflow newTaskWorkflow

instanciateTaskWorkflow :: TaskWorkflow -> TaskWorkflowTyper Aggregate
instanciateTaskWorkflow w = Aggregate w applyCommandTaskWorkflow applyEventTaskWorkflow

applyCommandTaskWorkflow :: TaskWorkflowTyper CommandApplier
applyCommandTaskWorkflow = CommandApplier $ \ w c -> case c of
                                                          AssignTask u -> if assigned w == Just u   then [] else [AssignedTask u]
                                                          AchieveTask  -> if status w   == Started  then [Done]          else []
                                                          RestartTask  -> if status w   == Archived then [TaskReDone]    else []
                                                          UnassignTask ->    maybe [] (const [UnassignedTask]) (assigned w)
                                                          PostponeTask -> if status w   == Started  then [PostponedTask] else []
                                                          StartTask    -> if status w   == Waiting  then [TaskStarted]   else []

applyEventTaskWorkflow :: TaskWorkflowTyper EventApplier
applyEventTaskWorkflow = EventApplier $ \w e -> case e of
                                                     AssignedTask u -> instanciateTaskWorkflow $ w { assigned = Just u }
                                                     Done           -> instanciateTaskWorkflow $ w { status = Archived }
                                                     TaskReDone     -> instanciateTaskWorkflow $ w { status = Waiting }
                                                     UnassignedTask -> instanciateTaskWorkflow $ w { assigned = Nothing }
                                                     PostponedTask  -> instanciateTaskWorkflow $ w { status = Waiting }
                                                     TaskStarted    -> instanciateTaskWorkflow $ w { status = Started }

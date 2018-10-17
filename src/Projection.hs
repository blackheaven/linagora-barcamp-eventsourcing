module Projection
    (
      hotPotatoe
    , newHotPotatoe
    , TasksStats
    , getPotato
    ) where

import EventSource(Projection(..))
import TaskWorkflow(TaskWorkflowId(..), TaskWorkflowEvent(..))

import Data.Map(empty, Map, alter, lookupMax)
import Data.Maybe(fromJust)

data TasksStats = TasksStats (Map TaskWorkflowId Integer)

hotPotatoe :: Projection TaskWorkflowId TaskWorkflowEvent TasksStats
hotPotatoe = Projection $ \(TasksStats s) (i,e) -> TasksStats $ if isAssignation e
                                                                  then alter (Just . maybe 1 (+ 1)) i s
                                                                  else s

newHotPotatoe :: TasksStats
newHotPotatoe = TasksStats empty

getPotato :: TasksStats -> TaskWorkflowId
getPotato (TasksStats m) = fst $ fromJust $ lookupMax m

isAssignation :: TaskWorkflowEvent -> Bool
isAssignation e = case e of
                       AssignedTask _ -> True
                       UnassignedTask -> True
                       _              -> False

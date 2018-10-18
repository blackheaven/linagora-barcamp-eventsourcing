module HotPotatoe
    (
      hotPotatoe
    , newHotPotatoe
    , TasksStats
    , getPotato
    ) where

import EventSource(Event(..), AggregateId, Projection(..))
import TaskWorkflow(TaskWorkflowEvent(..))

import Data.Map(empty, Map, alter, lookupMax)
import Data.Maybe(fromJust)

data TasksStats = TasksStats (Map AggregateId Integer)

hotPotatoe :: Projection TaskWorkflowEvent TasksStats
hotPotatoe = Projection $ \(TasksStats s) e -> TasksStats $ if isAssignation (eventValue e)
                                                                  then alter (Just . maybe 1 (+ 1)) (eventAggregateId e) s
                                                                  else s

newHotPotatoe :: TasksStats
newHotPotatoe = TasksStats empty

getPotato :: TasksStats -> AggregateId
getPotato (TasksStats m) = fst $ fromJust $ lookupMax m

isAssignation :: TaskWorkflowEvent -> Bool
isAssignation e = case e of
                       AssignedTask _ -> True
                       UnassignedTask -> True
                       _              -> False

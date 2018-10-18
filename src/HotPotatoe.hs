module HotPotatoe
    (
      hotPotatoe
    , newHotPotatoe
    , TasksStats
    , getPotato
    ) where

import EventSource(Event(..), AggregateId, Projection(..))
import TaskWorkflow(TaskWorkflowEvent(..))

import Data.Function(on)
import Data.List(maximumBy)
import Data.Map(empty, Map, alter, assocs)

data TasksStats = TasksStats (Map AggregateId Integer) deriving (Show, Eq)

hotPotatoe :: Projection TaskWorkflowEvent TasksStats
hotPotatoe = Projection $ \(TasksStats s) e -> TasksStats $ if isAssignation (eventValue e)
                                                                  then alter (Just . maybe 1 (+ 1)) (eventAggregateId e) s
                                                                  else s

newHotPotatoe :: TasksStats
newHotPotatoe = TasksStats empty

getPotato :: TasksStats -> AggregateId
getPotato (TasksStats m) = fst $ maximumBy (compare `on` snd) $ assocs m

isAssignation :: TaskWorkflowEvent -> Bool
isAssignation e = case e of
                       AssignedTask _ -> True
                       UnassignedTask -> True
                       _              -> False

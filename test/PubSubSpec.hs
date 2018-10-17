module PubSubSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import EventSource
import Projection
import TaskWorkflow
import PubSub

taskId1 :: TaskWorkflowId
taskId1 = TaskWorkflowId 1

taskId2 :: TaskWorkflowId
taskId2 = TaskWorkflowId 2

user1 :: User
user1 = User "alice"

user2 :: User
user2 = User "bob"

task1 :: TaskWorkflowTyper Aggregate
task1 = waitingTaskWorkflow taskId1

task2 :: TaskWorkflowTyper Aggregate
task2 = waitingTaskWorkflow taskId2

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Projection" $ do
    describe "HotPotatoe" $ do
      it "given three valid events on two tasks the projection should give the most hitted one" $ do
        getPotato (computeProjection hotPotatoe newHotPotatoe [(taskId1, AssignedTask user1), (taskId2, AssignedTask user2), (taskId1, UnassignedTask)]) `shouldNotBe` taskId1
      it "given one valid event and two invalids on two tasks the projection should give the hitted one" $ do
        getPotato (computeProjection hotPotatoe newHotPotatoe [(taskId1, Done), (taskId2, AssignedTask user2), (taskId1, PostponedTask)]) `shouldNotBe` taskId1

module HotPotatoeSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import EventSource
import HotPotatoe
import TaskWorkflow

taskId1 :: AggregateId
taskId1 = AggregateId 1

taskId2 :: AggregateId
taskId2 = AggregateId 2

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
  describe "HotPotatoe" $ do
    it "given three valid events on two tasks the projection should give the most hitted one" $ do
      getPotato (computeProjection hotPotatoe newHotPotatoe [Event (EventId 1) taskId1 (AssignedTask user1), Event (EventId 2) taskId2 (AssignedTask user2), Event (EventId 2) taskId1 UnassignedTask]) `shouldNotBe` taskId1
    it "given one valid event and two invalids on two tasks the projection should give the hitted one" $ do
      getPotato (computeProjection hotPotatoe newHotPotatoe [Event (EventId 1) taskId1 Done, Event (EventId 1) taskId2 (AssignedTask user2), Event (EventId 2) taskId1 PostponedTask]) `shouldNotBe` taskId1

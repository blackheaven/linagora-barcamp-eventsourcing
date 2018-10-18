module PubSubSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import EventSource
import HotPotatoe
import TaskWorkflow
import PubSub

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

pubSubHotP :: PubSub TaskWorkflowEvent TasksStats
pubSubHotP = subscribe hotPotatoe newHotPotatoe newPubSub

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "PubSub" $ do
    describe "HotPotatoe" $ do
      it "given three valid events on two tasks the projection should give the most hitted one" $ do
        (map getPotato . fetchViews) (foldr publish pubSubHotP [Event (EventId 1) taskId1 (AssignedTask user1), Event (EventId 2) taskId2 (AssignedTask user2), Event (EventId 3) taskId1 UnassignedTask]) `shouldBe` [taskId1]
      it "given three valid events and two duplicated on two tasks the projection should give the most hitted one" $ do
        (map getPotato . fetchViews)  (foldr publish pubSubHotP [Event (EventId 1) taskId1 (AssignedTask user1), Event (EventId 2) taskId2 (AssignedTask user2), Event (EventId 3) taskId1 UnassignedTask, Event (EventId 2) taskId2 (AssignedTask user2), Event (EventId 2) taskId2 (AssignedTask user2)]) `shouldBe` [taskId1]

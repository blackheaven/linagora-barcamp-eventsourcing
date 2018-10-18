module PersistenceSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Maybe(fromJust, isJust)

import EventSource
import Persistence
import TaskWorkflow

task1 :: AggregateId
task1 = AggregateId 1

waitingTask :: TaskWorkflowTyper Aggregate
waitingTask = waitingTaskWorkflow task1

startedTask :: TaskWorkflowTyper Aggregate
startedTask = applyEvents waitingTask [Event (EventId 1) task1 TaskStarted]

achievedTask :: TaskWorkflowTyper Aggregate
achievedTask = applyEvents startedTask [Event (EventId 2) task1 Done]

postponedTask :: TaskWorkflowTyper Aggregate
postponedTask = applyEvents startedTask [Event (EventId 3) task1 PostponedTask]

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Persistence" $ do
    describe "Creation" $ do
      it "Persisting a new aggregate should generate a Persister" $ do
        isJust (persist newPersister achievedTask) `shouldBe` True
      it "Persisting a new aggregate should generate a Persister with this aggregate fetchable" $ do
        ((persist newPersister achievedTask) >>= (\p -> fetch p task1))  `shouldBe` Just achievedTask
    describe "Linear updating" $ do
      let filledPersister = fromJust $ persist newPersister waitingTask
      it "Updating an aggregate should generate a Persister" $ do
        isJust (persist filledPersister achievedTask) `shouldBe` True
      it "Updating an aggregate should generate a Persister with this aggregate fetchable in this version" $ do
        ((persist filledPersister achievedTask) >>= (\p -> fetch p task1))  `shouldBe` Just achievedTask
    describe "Conflict updating" $ do
      let filledPersister = fromJust $ persist newPersister waitingTask
      it "Updating an aggregate with an old version should not generate a Persister" $ do
        isJust (persist filledPersister achievedTask >>= \p -> persist p postponedTask) `shouldBe` False


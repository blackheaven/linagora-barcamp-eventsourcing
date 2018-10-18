module TaskWorkflowSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import EventSource
import TaskWorkflow

task1 :: AggregateId
task1 = AggregateId 1

user1 :: User
user1 = User "alice"

user2 :: User
user2 = User "bob"

waitingTask :: TaskWorkflowTyper Aggregate
waitingTask = waitingTaskWorkflow task1

startedTask :: TaskWorkflowTyper Aggregate
startedTask = applyEvents waitingTask [Event (EventId 1) task1 TaskStarted]

achievedTask :: TaskWorkflowTyper Aggregate
achievedTask = applyEvents startedTask [Event (EventId 2) task1 Done]

eventsCreator :: [e -> Event e]
eventsCreator = map (\i -> \e -> Event (EventId i) task1 e) [5..]

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Workflow" $ do
    describe "Commands" $ do
      describe "AssignTask" $ do
        it "one assign should produce an assigned task" $ do
          applyCommands waitingTask eventsCreator [AssignTask user1] `shouldBe` [Event (EventId 5) task1 (AssignedTask user1)]
        it "assigning two different users should produce two assigned tasks" $ do
          applyCommands waitingTask eventsCreator [AssignTask user1, AssignTask user2] `shouldBe` [Event (EventId 5) task1 (AssignedTask user1), Event (EventId 6) task1 (AssignedTask user2)]
        it "assigning two identical users should produce one assigned task" $ do
          applyCommands waitingTask eventsCreator [AssignTask user1, AssignTask user1] `shouldBe` [Event (EventId 5) task1 (AssignedTask user1)]
      describe "UnassignTask" $ do
        it "unassign a non-assigned task should produce nothing" $ do
          applyCommands waitingTask eventsCreator [UnassignTask] `shouldBe` []
        it "unassign an assigned task should produce an assigned task" $ do
          applyCommands waitingTask eventsCreator [AssignTask user1, UnassignTask] `shouldBe` [Event (EventId 5) task1 (AssignedTask user1), Event (EventId 6) task1 UnassignedTask]
      describe "StartTask" $ do
        it "starting a task should produce a started task" $ do
          applyCommands waitingTask eventsCreator [StartTask] `shouldBe` [Event (EventId 5) task1 TaskStarted]
        it "starting a task twice should produce only one started task" $ do
          applyCommands waitingTask eventsCreator [StartTask, StartTask] `shouldBe` [Event (EventId 5) task1 TaskStarted]
        it "starting a done task should produce nothing" $ do
          applyCommands achievedTask eventsCreator [StartTask] `shouldBe` []
      describe "AchieveTask" $ do
        it "achieving a waiting task should produce nothing" $ do
          applyCommands waitingTask eventsCreator [AchieveTask] `shouldBe` []
        it "achieving a started task should produce a done task" $ do
          applyCommands startedTask eventsCreator [AchieveTask] `shouldBe` [Event (EventId 5) task1 Done]
        it "achieving a started task twice should produce only one done task" $ do
          applyCommands startedTask eventsCreator [AchieveTask, AchieveTask] `shouldBe` [Event (EventId 5) task1 Done]
      describe "RestartTask" $ do
        it "restarting a waiting task should produce nothing" $ do
          applyCommands waitingTask eventsCreator [RestartTask] `shouldBe` []
        it "restarting a started task should produce nothing" $ do
          applyCommands startedTask eventsCreator [RestartTask] `shouldBe` []
        it "restarting a done task should produce a redone task" $ do
          applyCommands achievedTask eventsCreator [RestartTask] `shouldBe` [Event (EventId 5) task1 TaskReDone]
        it "restarting a done task twice should produce only one redone task" $ do
          applyCommands achievedTask eventsCreator [RestartTask, RestartTask] `shouldBe` [Event (EventId 5) task1 TaskReDone]
      describe "PostponeTask" $ do
        it "postponing a waiting task should produce nothing" $ do
          applyCommands waitingTask eventsCreator [PostponeTask] `shouldBe` []
        it "postponing a done task should produce nothing" $ do
          applyCommands achievedTask eventsCreator [PostponeTask] `shouldBe` []
        it "postponing a started task should produce a postponed task" $ do
          applyCommands startedTask eventsCreator [PostponeTask] `shouldBe` [Event (EventId 5) task1 PostponedTask]
        it "postponing a started task twice should produce only one postponed task" $ do
          applyCommands startedTask eventsCreator [PostponeTask, PostponeTask] `shouldBe` [Event (EventId 5) task1 PostponedTask]
    describe "Event" $ do
      describe "AssignedTask" $ do
        it "one assign should change the default value" $ do
          applyEvents waitingTask [Event (EventId 5) task1 (AssignedTask user1)] `shouldNotBe` waitingTask
        it "two assigns should be the same as taking only the last one" $ do
          applyEvents waitingTask [Event (EventId 5) task1 (AssignedTask user1), Event (EventId 6) task1 (AssignedTask user2)] `shouldBe` applyEvents waitingTask [Event (EventId 5) task1 (AssignedTask user2)]
      describe "UnassignedTask" $ do
        it "unassign a non-assigned task  should equivalent to the default value" $ do
          applyEvents waitingTask [Event (EventId 5) task1 UnassignedTask] `shouldBe` waitingTask
        it "unassign an assigned task  should equivalent to the default value" $ do
          applyEvents waitingTask [Event (EventId 5) task1 (AssignedTask user1), Event (EventId 6) task1 UnassignedTask] `shouldBe` waitingTask
      describe "TaskStarted" $ do
        it "one start should change the default value" $ do
          applyEvents waitingTask [Event (EventId 5) task1 TaskStarted] `shouldNotBe` waitingTask
      describe "Done" $ do
        it "one done on a started task should be equivalent to a done task" $ do
          applyEvents startedTask [Event (EventId 5) task1 Done] `shouldBe` achievedTask
      describe "TaskReDone" $ do
        it "one redone on an achieved task should be equivalent to the default value" $ do
          applyEvents achievedTask [Event (EventId 5) task1 TaskReDone] `shouldBe` waitingTask
      describe "PostponedTask" $ do
        it "one postpone on an achieved task should be equivalent to the default value" $ do
          applyEvents startedTask [Event (EventId 5) task1 PostponedTask] `shouldBe` waitingTask
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x

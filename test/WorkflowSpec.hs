module WorkflowSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import EventSource
import Workflow

user1 :: User
user1 = User "alice"

user2 :: User
user2 = User "bob"

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Workflow" $ do
    describe "Commands" $ do
      describe "AssignTask" $ do
        it "one assign should produce an assigned task" $ do
          applyCommands workflow [AssignTask user1] `shouldBe` [AssignedTask user1]
        it "assigning two different users should produce two assigned tasks" $ do
          applyCommands workflow [AssignTask user1, AssignTask user2] `shouldBe` [AssignedTask user1, AssignedTask user2]
        it "assigning two identical users should produce one assigned task" $ do
          applyCommands workflow [AssignTask user1, AssignTask user1] `shouldBe` [AssignedTask user1]
      describe "UnassignTask" $ do
        it "unassign a non-assigned task should produce nothing" $ do
          applyCommands workflow [UnassignTask] `shouldBe` []
        it "unassign an assigned task should produce an assigned task" $ do
          applyCommands workflow [AssignTask user1, UnassignTask] `shouldBe` [AssignedTask user1, UnassignedTask]
    describe "Event" $ do
      describe "AssignTask" $ do
        it "one assign should change the default value" $ do
          applyEvents workflow [AssignedTask user1] `shouldNotBe` workflow
        it "two assigns should be the same as taking only the last one" $ do
          applyEvents workflow [AssignedTask user1, AssignedTask user2] `shouldBe` applyEvents workflow [AssignedTask user2]
      describe "UnassignTask" $ do
        it "unassign a non-assigned task  should equivalent to the default value" $ do
          applyEvents workflow [UnassignedTask] `shouldBe` workflow
        it "unassign an assigned task  should equivalent to the default value" $ do
          applyEvents workflow [AssignedTask user1, UnassignedTask] `shouldBe` workflow
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x

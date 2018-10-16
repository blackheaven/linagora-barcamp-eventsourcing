module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Lib

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
        it "one assign should produce one event" $ do
          applyCommands workflow [AssignTask user1] `shouldBe` [AssignedTask user1]
        it "two distinct assigns should produce two events" $ do
          applyCommands workflow [AssignTask user1, AssignTask user2] `shouldBe` [AssignedTask user1, AssignedTask user2]
        it "two identical assigns should produce one event" $ do
          applyCommands workflow [AssignTask user1, AssignTask user1] `shouldBe` [AssignedTask user1]
    describe "Event" $ do
      describe "AssignTask" $ do
        it "one assign should change the default value" $ do
          applyEvents workflow [AssignedTask user1] `shouldNotBe` workflow
        it "two assigns should be the same as taking only the last one" $ do
          applyEvents workflow [AssignedTask user1, AssignedTask user2] `shouldBe` applyEvents workflow [AssignedTask user2]
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x

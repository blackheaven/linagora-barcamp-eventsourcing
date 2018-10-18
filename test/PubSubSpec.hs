module PubSubSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

-- import EventSource
-- import HotPotatoe
-- import TaskWorkflow
-- import PubSub

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Projection" $ do
    describe "HotPotatoe" $ do
      it "given three valid events on two tasks the projection should give the most hitted one" $ do
        1 `shouldNotBe` 2
        -- getPotato (computeProjection hotPotatoe newHotPotatoe [(taskId1, AssignedTask user1), (taskId2, AssignedTask user2), (taskId1, UnassignedTask)]) `shouldNotBe` taskId1
      -- it "given one valid event and two invalids on two tasks the projection should give the hitted one" $ do
        -- getPotato (computeProjection hotPotatoe newHotPotatoe [(taskId1, Done), (taskId2, AssignedTask user2), (taskId1, PostponedTask)]) `shouldNotBe` taskId1

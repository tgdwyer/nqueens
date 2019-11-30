import Test.Hspec
import Test.QuickCheck
import Data.List (sort)
import Lib (checkBoard, eightQueens)

-- Q . . . . . . .
-- . . . . Q . . .
-- . Q . . . . . .
-- . . . . . Q . .
-- . . Q . . . . .
-- . . . . . . Q .
-- . . . Q . . . .
-- . . . . . . . Q
diagonal = [0,2,4,6,1,3,5,7]

-- Q . . . Q . . .
-- . . . . . . . Q
-- . Q . . . . . .
-- . . . . . Q . .
-- . . Q . . . . .
-- . . . . . . Q .
-- . . . Q . . . .
-- . . . . . . . .
horizontal = [0,2,4,6,0,3,5,1]

-- . . . . . Q . .
-- . . Q . . . . .
-- Q . . . . . . .
-- . . . . . . Q .
-- . . . . Q . . .
-- . . . . . . . Q
-- . Q . . . . . .
-- . . . Q . . . .
valid = [2,6,1,7,4,0,3,5]

alldiff :: Ord a => [a] -> Bool
alldiff l = and $ zipWith (/=) s $ tail s
            where s = sort l

main :: IO ()
main = hspec $
    describe "checkBoard" $ do
        it "alldiff" $
            alldiff [1,2,3] `shouldBe` True
        it "alldiff" $
            alldiff [1,2,1] `shouldBe` False
        it "diagonal" $
            checkBoard diagonal `shouldBe` False
        it "horizontal" $
            checkBoard horizontal `shouldBe` False
        it "valid" $
            checkBoard valid `shouldBe` True
        it "eightQueens: 92 solutions" $
            length eightQueens `shouldBe` 92
        it "eightQueens: all valid" $
            all checkBoard eightQueens

module Chapter1Spec where
import Chapter1
import Test.Tasty
import Test.Tasty.QuickCheck as QC

chapter1Quick = testGroup "-> QuickCheck: " [
        QC.testProperty "reverse == myReverse" $ 
          \list -> reverse (list :: [Int]) == myReverse list
        , QC.testProperty "filter == myFilter"  $
          \list -> filter (/= 0) (list :: [Int]) == myFilter (/= 0) list
        , QC.testProperty "foldr f e . filter p == foldFilter" $
          \list -> (foldr (-) 0 . filter odd) (list :: [Int]) == foldFilter (-) 0 odd list
        , QC.testProperty "myTakeWhile == takeWhile" $
          \list -> takeWhile odd (list :: [Int]) == myTakeWhile odd list
        , QC.testProperty "dropWhile == myDropWhileEnd" $
          \list -> dropWhile odd (list :: [Int]) == dropWhile odd list
        , QC.testProperty "apply n (+ 3) a == n * 3 + a" $ applyTest]
applyTest :: Positive Int -> Positive Int -> Bool
applyTest (Positive n) (Positive a) = apply n (+ 3) a == n * 3 + a

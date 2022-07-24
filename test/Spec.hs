import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 should be 4" $
      (2 + 2) `shouldBe` 4
    -- QuickCheck
    it "x + 1 is always greater than x" $
      property $ \x -> x + 1 > (x :: Int)
    -- Wrong assertion
    it "x + 0 is always greater than x" $
      property $ \x -> x + 0 > (x :: Int)
  describe "divideBy" $ do
    it "15 divided by 3 is 5" $
      divideBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 with reminder 2" $
      divideBy 22 5 `shouldBe` (4, 2)
  describe "multiplies" $ do
    it "6 multiplied by 2 is 12" $
      multiplies 6 2 `shouldBe` 12
    it "2 multiplied by 6 is 12" $
      multiplies 2 6 `shouldBe` 12
    it "17 multiplied by 3 is 51" $
      multiplies 17 3 `shouldBe` 51

divideBy :: Integral a => a -> a -> (a, a)
divideBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise =
        go (n - d) d (count + 1)

multiplies :: (Eq a, Num a) => a -> a -> a
multiplies a 1 = a
multiplies a b = multiplies a (b - 1) + a

-- -----------------

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  -- frequency :: [(Int, Gen a)] -> Gen a
  frequency [(1, return Nothing), (3, return (Just a))]

newtype Test = Test (Int -> Int)

hum :: Test -> Test -> Test
hum (Test f) (Test g) = Test $ (\x -> f $ g x)

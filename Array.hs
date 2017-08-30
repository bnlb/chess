module Array (
  slice
) where


-- Inclusive slice
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

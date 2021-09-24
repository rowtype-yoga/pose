module Issue.Eight where

findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex _ Nil = Nothing
findLastIndex pred xs = go 0 Nothing xs
  where
  go :: Int -> Maybe Int -> List a -> Maybe Int
  go _ res Nil = res
  go i res (y : ys) = go (i + 1) newRes ys
  where
    newRes = if (pred y) then Just i else res
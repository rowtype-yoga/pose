module Where where

foo = x
  where
  x = y
  y = 1

funk = x

  where
  x = y

  y = 1

girl = x
  where
  x = y

  y = 1

boy = x
  where

  x = y

  -- breaks
  y = 1

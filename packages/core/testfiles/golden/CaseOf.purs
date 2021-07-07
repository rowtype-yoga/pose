module Case.Of where

x = case _ of
  A -> 12
  B 23 -> 23
  C g
    | g > 4 -> 44

y = case _ of

  A -> 12

  B 23 -> 23

  b@(C g)
    | g > 4 -> c + g

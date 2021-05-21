module RecordUpdates where

foo x = x { bar { baz = 10
                }
          , qux = "hi", gar = do 10
          }

update state = 
  identity
    $ state 
      { bar = 10
      , qux = "hi"
      }
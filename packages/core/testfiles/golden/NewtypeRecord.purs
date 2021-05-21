module NewtypeRecord where

newtype Foo
  = Foo {}

foo :: Foo
foo = Foo {}

empty =
  { --onPress: handler_ do loadSearch
  }

newtype Bar
  = Bar { first :: String, second :: String }

uBar = case _ of
  ( Bar
      { first
      , second
      }
  ) -> first

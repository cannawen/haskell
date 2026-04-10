-- newtype ApiKey = MakeAPIKey { getAPIKey :: String }
type ApiKey = String
type FoundPerson = Bool
type PersonId = Integer
type Person = String

idToPerson :: ApiKey -> PersonId -> Person
idToPerson _ 0 = "Canna"
idToPerson _ 1 = "Michael"
idToPerson _ _ = "Raf"

getAPI :: FoundPerson -> (FoundPerson, ApiKey)
getAPI foundPerson = (foundPerson, apiKey)
  where
    apiKey = "my_api_key"

getPeopleIds :: FoundPerson -> ApiKey -> (FoundPerson, [PersonId])
getPeopleIds foundPerson apiKey = (True, [0, 1, 2])

getPeople :: FoundPerson -> ApiKey -> [PersonId] -> (FoundPerson, [Person])
getPeople foundPerson apiKey peopleIds =
  (foundPerson, fmap (idToPerson apiKey) peopleIds)

previousMain :: IO ()
previousMain = print (foundPerson3, people)
  where
  foundPerson0 = False
  (foundPerson1, apiKey) = getAPI foundPerson0
  (foundPerson2, peopleIds) =
    getPeopleIds foundPerson1 apiKey
  (foundPerson3, people) =
    getPeople foundPerson2 apiKey peopleIds

-- appx.: M a = ReaderT ApiKey (State FoundPerson a)
-- or with IO: M a = ReaderT ApiKey (StateT FoundPerson IO a)
newtype M a = MakeM { getM :: ApiKey -> FoundPerson -> (FoundPerson, a) }

instance Functor M where
  fmap :: (a -> b) -> M a -> M b
  fmap f (MakeM g) =
    MakeM (\apiKey foundPerson -> fmap f (g apiKey foundPerson))

instance Applicative M where
  pure x =
    MakeM (\_ foundPerson -> (foundPerson, x))
  (<*>) m1 m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2)))

instance Monad M where
  (>>=) :: forall a b. M a -> (a -> M b) -> M b
  (>>=) x f = flattened
    where
      preFlattened :: M (M b)
      preFlattened = fmap f x

      flattened :: M b
      flattened = join preFlattened
      -- flattened = join (fmap f x)

      join :: M (M b) -> M b
      join (MakeM g) = MakeM h
        where
          h :: ApiKey -> FoundPerson -> (FoundPerson, b)
          h apiKey foundPersonBefore = (foundPersonAfter, out)
            where
              ranG :: (FoundPerson, M b)
              ranG = g apiKey foundPersonBefore

              unwrappedG :: (FoundPerson, b)
              unwrappedG = getM (snd ranG) apiKey (fst ranG)

              foundPersonAfter :: Bool
              foundPersonAfter = fst unwrappedG

              out :: b
              out = snd unwrappedG

runM :: M a -> (FoundPerson, a)
runM (MakeM f) = f apiKey foundPerson
  where
    apiKey = "my_api_key"
    foundPerson = False

-- getPeopleIds :: FoundPerson -> ApiKey -> (FoundPerson, [PersonId])
getPeopleIdsM :: M [PersonId]
getPeopleIdsM =
  MakeM (\apiKey foundPerson -> getPeopleIds foundPerson apiKey)

-- getPeople :: FoundPerson -> ApiKey -> [PersonId] -> (FoundPerson, [Person])
getPeopleM :: [PersonId] -> M [Person]
getPeopleM peopleIds =
  MakeM (\apiKey foundPerson -> getPeople foundPerson apiKey peopleIds)

main :: IO ()
main = print $ runM $ do
  peopleIds <- getPeopleIdsM
  getPeopleM peopleIds


--------
-- runM_ :: M a -> IO ()

-- main :: IO ()
-- main = runM $ do
--   peopleIds <- getPeopleIdsM
--   people <- getPeopleM peopleIds
--   liftIO $ print people

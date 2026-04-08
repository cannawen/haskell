type ApiKey = String
type PersonId = Integer
type Person = String

idToPerson :: ApiKey -> PersonId -> Person
idToPerson _ 0 = "Canna"
idToPerson _ 1 = "Michael"
idToPerson _ _ = "Raf"

getAPI :: Bool -> (Bool, ApiKey)
getAPI foundPerson = (foundPerson, "my_api_key") 

getPeopleIds :: Bool -> ApiKey -> (Bool, [PersonId])
getPeopleIds foundPerson apiKey = (True, [0, 1, 2])

getPeople :: Bool -> ApiKey -> [PersonId] -> (Bool, [Person])
getPeople foundPerson apiKey peopleIds = (foundPerson, map (idToPerson apiKey) peopleIds)

main :: IO ()
main = print (foundPerson3, people)
  where
  foundPerson0 = False
  (foundPerson1, apiKey) = getAPI foundPerson0
  (foundPerson2, peopleIds) = getPeopleIds foundPerson1 apiKey
  (foundPerson3, people) = getPeople foundPerson2 apiKey peopleIds

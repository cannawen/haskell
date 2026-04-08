type ApiKey = String
type PersonId = Integer
type Person = String

idToPerson :: ApiKey -> PersonId -> Person
idToPerson "my_api_key" 0 = "Canna"
idToPerson "my_api_key" 1 = "Michael"
idToPerson "my_api_key" 2 = "Raf"
idToPerson "my_api_key" _ = "Person not found"
idToPerson _ _ = "Unauthorized access"

getAPI :: ApiKey
getAPI = "my_api_key"

getPeopleIds :: ApiKey -> [PersonId]
getPeopleIds apiKey = [0, 1, 2]

getPeople ::  ApiKey -> [PersonId] -> [Person]
getPeople apiKey peopleIds = map (idToPerson apiKey) peopleIds

main :: IO ()
main = print people
  where
  apiKey = getAPI
  peopleIds = getPeopleIds apiKey
  people = getPeople apiKey peopleIds
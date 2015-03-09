# Swagger Generated Python client


Usage example, based on the swagger petstore:

```python
# include the client module
from client import *

# build a client connection.  In this example, we are passing the hostname as arg0, and
# sending a header with name `api_key` and value `special-key` on each call to the api.

client = swagger.ApiClient('http://petstore.swagger.io/v2', 'api_key', 'special-key')

# create the PetApi class with the client we just created
petApi = PetApi.PetApi(client)

# call the API and fetch a pet, with petId=3
pet = petApi.getPetById(petId=3)

# write it into pretty JSON
json = client.sanitizeForSerialization(pet)
print json
{'category': {'category': None, 'status': None, 'name': 'string', 'tags': None, 'photoUrls': None, 'id': 0L}, 'status': {'category': None, 'status': None, 'name': None, 'tags': None, 'photoUrls': None, 'id': None}, 'name': 'foogly', 'tags': [{'id': 0L, 'name': 'string'}], 'photoUrls': ['string'], 'id': 3L}
```

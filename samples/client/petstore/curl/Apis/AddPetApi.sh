#!/bin/sh

## AddPetApi

        ### Add a new pet to the store
        # addPet

        curl -X POST http://petstore.swagger.io/v2/pet \
        -H 'Content-Type: application/json' \ -H 'Content-Type: application/xml' \ 
        -H 'Accept: application/json' \ -H 'Accept: application/xml' \ 
        -H "x-API-key: YOUR_API_KEY"


#!/bin/sh

## UpdatePetWithFormApi

        ### Updates a pet in the store with form data
        # updatePetWithForm

        curl -X POST http://petstore.swagger.io/v2/pet/{petId} \
        -H 'Content-Type: application/x-www-form-urlencoded' \ 
        -H 'Accept: application/x-www-form-urlencoded' \ 
        -H "x-API-key: YOUR_API_KEY"


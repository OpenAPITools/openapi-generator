#!/bin/sh

## UpdateUserApi

        ### Updated user
        # updateUser

        curl -X PUT http://petstore.swagger.io/v2/user/{username} \
        -H 'Content-Type: application/json' \ 
        -H 'Accept: application/json' \ 
        -H "x-API-key: YOUR_API_KEY"


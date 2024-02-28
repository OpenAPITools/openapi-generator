#!/bin/sh

## CreateUserApi

        ### Create user
        # createUser

        curl -X POST http://petstore.swagger.io/v2/user \
        -H 'Content-Type: application/json' \ 
        -H 'Accept: application/json' \ 
        -H "x-API-key: YOUR_API_KEY"


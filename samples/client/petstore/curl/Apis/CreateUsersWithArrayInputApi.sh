#!/bin/sh

## CreateUsersWithArrayInputApi

        ### Creates list of users with given input array
        # createUsersWithArrayInput

        curl -X POST http://petstore.swagger.io/v2/user/createWithArray \
        -H 'Content-Type: application/json' \ 
        -H 'Accept: application/json' \ 
        -H "x-API-key: YOUR_API_KEY"


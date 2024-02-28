#!/bin/sh

## CreateUsersWithListInputApi

        ### Creates list of users with given input array
        # createUsersWithListInput

        curl -X POST http://petstore.swagger.io/v2/user/createWithList \
        -H 'Content-Type: application/json' \ 
        -H 'Accept: application/json' \ 
        -H "x-API-key: YOUR_API_KEY"


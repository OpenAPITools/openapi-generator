#!/bin/sh

## PlaceOrderApi

        ### Place an order for a pet
        # placeOrder

        curl -X POST http://petstore.swagger.io/v2/store/order \
        -H 'Content-Type: application/json' \ 
        -H 'Accept: application/json' \ 
        -H "x-API-key: YOUR_API_KEY"


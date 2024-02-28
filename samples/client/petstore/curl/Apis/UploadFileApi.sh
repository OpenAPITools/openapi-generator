#!/bin/sh

## UploadFileApi

        ### uploads an image
        # uploadFile

        curl -X POST http://petstore.swagger.io/v2/pet/{petId}/uploadImage \
        -H 'Content-Type: multipart/form-data' \ 
        -H 'Accept: multipart/form-data' \ 
        -H "x-API-key: YOUR_API_KEY"


module github.com/OpenAPITools/openapi-generator/samples/openapi3/client/petstore/go

go 1.16

replace go-petstore => ./go-petstore

require (
	github.com/stretchr/testify v1.7.0
	go-petstore v0.0.0-00010101000000-000000000000
	golang.org/x/oauth2 v0.0.0-20210323180902-22b0adad7558
)

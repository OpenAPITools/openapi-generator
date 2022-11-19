module github.com/OpenAPITools/openapi-generator/samples/openapi3/client/petstore/go

go 1.16

replace go-petstore => ./go-petstore

require (
	github.com/stretchr/testify v1.8.1
	go-petstore v0.0.0-00010101000000-000000000000
	golang.org/x/oauth2 v0.1.0
)

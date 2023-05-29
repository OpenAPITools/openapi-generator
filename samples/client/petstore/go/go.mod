module github.com/OpenAPITools/openapi-generator/samples/client/petstore/go

replace github.com/OpenAPITools/openapi-generator/samples/client/petstore/go/go-petstore => ./go-petstore

go 1.13

require (
	github.com/OpenAPITools/openapi-generator/samples/client/petstore/go/go-petstore v0.0.0-00010101000000-000000000000
	github.com/go-openapi/swag v0.22.3
	github.com/stretchr/testify v1.8.1
	golang.org/x/net v0.2.0 // indirect
	golang.org/x/oauth2 v0.2.0
)
